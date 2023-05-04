{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Dosh.LSP.Document where

import Control.Monad.Extra (whenM)
import Control.Monad.State (StateT, get, gets, lift, modify)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Sequence.Zipper (SeqZipper, backWhile, forwardWhile)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope (Rope)
import Data.Text.Utf16.Rope qualified as Rope
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Dosh.GHC.Lexer qualified as GHC
import Dosh.GHC.Parser qualified as GHC
import Dosh.Prelude hiding (toList)
import Dosh.Util (tshow, withLineNumbers)
import GHC.Driver.Session qualified as GHC
import GHC.Exts (IsList (..))
import Language.LSP.Client.Session (Session, changeDoc, documentContents)
import Language.LSP.Types (TextDocumentContentChangeEvent (..), TextDocumentIdentifier (..), UInt, Diagnostic)
import Language.LSP.Types qualified as LSP
import Language.LSP.Types.Lens (HasCharacter (character), end, line, range, start)

data ChunkMetadata = ChunkMetadata
    { firstLine :: UInt
    , lastLine :: UInt
    , dirty :: Bool
    }
    deriving stock (Generic, Show)

mergeChunks :: ChunkMetadata -> ChunkMetadata -> Either String ChunkMetadata
mergeChunks a b
    | a.lastLine /= b.firstLine = Left "Cannot merge non-adjacent chunks"
    | otherwise = Right a{lastLine = b.lastLine}

moveChunkLines :: Int -> ChunkMetadata -> ChunkMetadata
moveChunkLines dy = #firstLine %~ (+- dy) >>> #lastLine %~ (+- dy)

(+-) :: UInt -> Int -> UInt
(+-) u i
    | i >= 0 = u + fromIntegral i
    | otherwise = u - fromIntegral (abs i)

data Document = Document
    { identifier :: TextDocumentIdentifier
    , language :: Text
    , chunks :: SeqZipper ChunkMetadata
    , expressionLines :: HashSet UInt
    , getSessionDynFlags :: IO GHC.DynFlags
    }
    deriving stock (Generic)

newDocument :: TextDocumentIdentifier -> Document
newDocument identifier = do
    Document
        { identifier
        , language = mempty
        , chunks = mempty
        , expressionLines = mempty
        , getSessionDynFlags = undefined
        }

handleUpdates :: [TextDocumentContentChangeEvent] -> StateT Document Session ()
handleUpdates events = do
    updates <- mapM normaliseUpdate events
    identifier <- gets identifier
    lift $ changeDoc identifier updates
    gets (toList . chunks) >>= normaliseChunks >>= modify . set #chunks . fromList
    chunks <- gets chunks
    contents :: Rope <-
        maybe (error "handleUpdates: document contents not found") pure
            =<< lift (documentContents identifier)
    expressionLines <- gets expressionLines
    liftIO $
        Text.writeFile "lsp-contents.hs" $
            Text.unlines
                [ withLineNumbers (Rope.toText contents)
                , "\nChunks:"
                , Text.unlines $ tshow <$> toList chunks
                , "\nExpression lines:"
                , Text.unlines $ tshow <$> toList expressionLines
                ]
  where
    -- Moves the update locations forward by prefix length, if the first and/or last line of the update fall on prefixed expr lines.
    -- Removes prefixed line locations that are being overwritten by the update.
    -- Translates prefixed line locations and chunks AFTER the update by the number of lines affected by this update.
    -- Ensures that there is a chunk that wholly contains this update.
    -- Notably, does not create any new prefixed lines.
    normaliseUpdate :: TextDocumentContentChangeEvent -> StateT Document Session TextDocumentContentChangeEvent
    normaliseUpdate u@(view range -> Just r) = do
        d <- get
        let afterPrefix x = x . filtered (view line >>> (`HashSet.member` d.expressionLines)) . character %~ (+ fromIntegral prefixLength)
            u' = u & range . traverse %~ (afterPrefix start >>> afterPrefix end)
            startLine, endLine, endLine' :: UInt
            startLine = r ^. start . line
            endLine = r ^. end . line
            endLine' = startLine +- Text.count "\n" u._text
            dy :: Int
            dy = fromIntegral endLine' - fromIntegral endLine
            ifMultiline :: (Choice p, Applicative f) => Optic' p f a a
            ifMultiline = filtered (const $ dy /= 0)
        modify $
            ifMultiline . #expressionLines
                %~ ( HashSet.filter (\y -> not $ y > startLine && y <= endLine)
                        >>> HashSet.map (\y -> if y > startLine then y +- dy else y)
                   )
                >>> #chunks
                    %~ ( goToLine startLine
                            >>> #after
                                %~ ( ifMultiline . dropping 1 traverse %~ moveChunkLines dy
                                        >>> ensureChunkContains startLine endLine'
                                   )
                       )
        pure u'
    normaliseUpdate u = pure u
    ensureChunkContains :: UInt -> UInt -> Seq ChunkMetadata -> Seq ChunkMetadata
    ensureChunkContains firstLine lastLine =
        filtered Seq.null .~ Seq.singleton ChunkMetadata{dirty = True, ..}
            >>> Seq.adjust (#firstLine %~ min firstLine >>> #lastLine %~ max lastLine >>> #dirty .~ True) 0
    normaliseChunks :: [ChunkMetadata] -> StateT Document Session [ChunkMetadata]
    normaliseChunks (a : b : c) | a.lastLine == b.firstLine = normaliseChunks $ a{lastLine = b.lastLine, dirty = True} : c
    normaliseChunks (c : cs) | c.dirty = mappend <$> reparseChunk c <*> normaliseChunks cs
    normaliseChunks (c : cs) = (c :) <$> normaliseChunks cs
    normaliseChunks cs = pure cs

-- Moves diagnostics left on virtually prefixed lines.
normaliseDiagnostics :: Document -> [Diagnostic] -> [Diagnostic]
normaliseDiagnostics doc = fmap $ range %~ mangle start . mangle end
    where
        isExprLine = flip HashSet.member doc.expressionLines . view line
        mangle l = l . filtered isExprLine . character . filtered (>= fromIntegral prefixLength) %~ (+- (-prefixLength))

exprPrefix :: UUID -> Text
exprPrefix u = "_" <> Text.replace "-" "_" (UUID.toText u) <> " = "

prefixLength :: Int
prefixLength = Text.length $ exprPrefix UUID.nil

newExprPrefix :: IO Text
newExprPrefix = exprPrefix <$> UUID.nextRandom

prefixLine :: UInt -> StateT Document Session ()
prefixLine n = whenM (gets $ not . HashSet.member n . expressionLines) $ do
    identifier <- gets identifier
    prefix <- liftIO newExprPrefix
    let loc =
            LSP.Position
                { _line = n
                , _character = 0
                }
    modify $ #expressionLines %~ HashSet.insert n
    lift $ changeDoc
            identifier
            [ TextDocumentContentChangeEvent
                { _range =
                    Just
                        LSP.Range
                            { _start = loc
                            , _end = loc
                            }
                , _rangeLength = Nothing
                , _text = prefix
                }
            ]

unprefixLine :: UInt -> StateT Document Session ()
unprefixLine n = whenM (gets $ HashSet.member n . expressionLines) $ do
    identifier <- gets identifier
    modify $ #expressionLines %~ HashSet.delete n
    lift $
        changeDoc
            identifier
            [ TextDocumentContentChangeEvent
                { _range =
                    Just
                        LSP.Range
                            { _start =
                                LSP.Position
                                    { _line = n
                                    , _character = 0
                                    }
                            , _end =
                                LSP.Position
                                    { _line = n
                                    , _character = fromIntegral prefixLength
                                    }
                            }
                , _rangeLength = Nothing
                , _text = ""
                }
            ]

reparseChunk :: ChunkMetadata -> StateT Document Session [ChunkMetadata]
reparseChunk c = do
    mapM_ unprefixLine [c.firstLine .. c.lastLine]
    contents :: Text <-
        maybe (error "handleUpdates: document contents not found") (pure . chunkContents c)
            =<< lift . documentContents
            =<< gets identifier
    dynFlags <- gets getSessionDynFlags >>= liftIO
    let chunk = GHC.chunkFromText "<interactive>" (fromIntegral c.firstLine) contents
        chunks = GHC.splitChunks chunk
    forM chunks $ \c -> do
        let firstLine = fromIntegral $ GHC.firstLine c
        let lastLine = fromIntegral $ GHC.lastLine c
        case GHC.parseExprChunk dynFlags c of
            Just (GHC.ExpressionChunk exprs) ->
                forM_ (filter (not . Text.null . GHC.unLoc) exprs) $
                    prefixLine . fromIntegral . GHC.firstLine
            Nothing -> mapM_ unprefixLine [firstLine .. lastLine]
            _ -> undefined

        pure
            ChunkMetadata
                { firstLine = fromIntegral $ GHC.firstLine c
                , lastLine = fromIntegral $ GHC.lastLine c
                , dirty = False
                }
  where
    ropeContents :: UInt -> UInt -> Rope -> Text
    ropeContents firstLine lastLine =
        Text.intercalate "\n"
            . take (fromIntegral $ lastLine - firstLine + 1)
            . Rope.lines
            . snd
            . Rope.splitAtLine (fromIntegral firstLine)
    chunkContents :: ChunkMetadata -> Rope -> Text
    chunkContents ChunkMetadata{firstLine, lastLine} = ropeContents firstLine lastLine

-- multiChunkContents :: [ChunkMetadata] -> Rope -> Text
-- multiChunkContents cs = ropeContents (minimum $ firstLine <$> cs) (maximum $ lastLine <$> cs)

goToLine :: UInt -> SeqZipper ChunkMetadata -> SeqZipper ChunkMetadata
goToLine r = forwardWhile chunkContains >>> backWhile chunkContains
  where
    chunkContains ChunkMetadata{..} = r >= firstLine && r <= lastLine
