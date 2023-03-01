{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.ParserSpec where

import Data.List (intercalate)
import Data.List.Extra (nub)
import Data.Text qualified as Text
import Dosh.GHC.Parser
import Dosh.GHC.Server (withGhc)
import Dosh.Prelude hiding (elements)
import Dosh.Util
import Test.Hspec
import Test.Hspec.Expectations.Extra
import Test.QuickCheck

data Source
    = PragmaSource Text Text
    | ImportSource Text
    | DeclarationSource Text
    | ExpressionSource Text Text
    deriving stock (Generic, Eq)

instance Show Source where
    show (PragmaSource x y) = Text.unpack $ "{-# " <> x <> " " <> y <> " #-}"
    show (ImportSource x) = Text.unpack $ "import " <> x
    show (DeclarationSource x) = Text.unpack x
    show (ExpressionSource x _) = Text.unpack x

arbitraryPragma :: Gen Source
arbitraryPragma = PragmaSource "LANGUAGE" <$> elements extensions
  where
    extensions :: [Text]
    extensions = foldr (\e acc -> e : ("No" <> e) : acc) [] ["OverloadedStrings", "OverloadedLabels"]

arbitraryImport :: Gen Source
arbitraryImport = ImportSource <$> elements ["Data.Default", "Data.Text"]

arbitraryDeclaration :: Gen Source
arbitraryDeclaration =
    pure $
        DeclarationSource $
            Text.intercalate
                "\n"
                [ "square :: Int -> Int"
                , "square x = x^2"
                ]

arbitraryExpression :: Gen Source
arbitraryExpression =
    uncurry ExpressionSource
        <$> elements
            [ ("True", "True\n")
            ,
                ( Text.intercalate
                    "\n"
                    [ "fromIntegral $"
                    , "  let x = 1"
                    , "      y = 2"
                    , "   in x + y"
                    ]
                , "3\n"
                )
            , ("putStrLn \"Hello!\"", "Hello!\n")
            ]

instance Arbitrary Source where
    arbitrary =
        oneof
            [ arbitraryPragma
            , arbitraryImport
            , arbitraryDeclaration
            , arbitraryExpression
            ]

data SourceChunk
    = ModuleSourceChunk {srcs :: [Source]}
    | DeclarationSourceChunk {srcs :: [Source]}
    | ExpressionSourceChunk {srcs :: [Source]}
    deriving stock (Eq)

arbitraryModuleChunk :: Gen SourceChunk
arbitraryModuleChunk = do
    pragmas <- listOf arbitraryPragma
    decls <- listOf arbitraryDeclaration
    pure $ ModuleSourceChunk $ pragmas <> [DeclarationSource "module Foo.Bar where"] <> decls

arbitraryDeclarationChunk :: Gen SourceChunk
arbitraryDeclarationChunk = do
    decls <- nub <$> listOf1 arbitraryDeclaration
    pure $ DeclarationSourceChunk decls

arbitraryExpressionChunk :: Gen SourceChunk
arbitraryExpressionChunk = do
    exprs <- listOf1 arbitraryExpression
    pure $ ExpressionSourceChunk exprs

instance Arbitrary SourceChunk where
    arbitrary = oneof [{-arbitraryModuleChunk, -} arbitraryDeclarationChunk, arbitraryExpressionChunk]

instance Show SourceChunk where
    show (ModuleSourceChunk inputs) = intercalate "\n\n" $ show <$> inputs
    show (DeclarationSourceChunk inputs) = intercalate "\n" $ show <$> inputs
    show (ExpressionSourceChunk inputs) = intercalate "\n" $ show <$> inputs

newtype SourceCode = SourceCode {chunks :: [SourceChunk]}

instance Show SourceCode where
    show SourceCode{..} = intercalate "\n\n" $ show <$> chunks

instance Arbitrary SourceCode where
    arbitrary = SourceCode <$> arbitrary

instance Eq SomeException where
    a == b = tshow a == tshow b

atLine :: Text -> Int -> Code
atLine = flip $ locatedText "<test>"

instance Arbitrary Text where
    arbitrary = Text.pack <$> listOf (elements "xxx \n")

startLine :: RealSrcSpan -> Int
startLine = srcLocLine . realSrcSpanStart

endLine :: RealSrcSpan -> Int
endLine = srcLocLine . realSrcSpanEnd

locate :: Text -> Expectation
locate (flip atLine 1 -> L loc code) = do
    srcSpanFile loc `shouldBe` "<test>"
    srcLocLine start `shouldBe` 1
    srcLocCol start `shouldBe` 1
    srcLocLine end `shouldBe` length codeLines
    srcLocCol end `shouldBe` max 1 (Text.length (last codeLines))
  where
    start = realSrcSpanStart loc
    end = realSrcSpanEnd loc
    codeLines = Text.splitOn "\n" code

shouldStartOnLine :: RealSrcSpan -> Int -> Expectation
loc `shouldStartOnLine` l = startLine loc `shouldBe` l

chunksAreNotAdjacent :: Text -> Expectation
chunksAreNotAdjacent (flip atLine 1 -> c) = do
    Right chunks <- withGhc $ splitChunks c
    forM_ (zip chunks $ tail chunks) $ \(L loc1 _, L loc2 _) -> do
        loc2 `shouldStartOnLine` (endLine loc1 + 1)

expressionsAreAdjacent :: Text -> Expectation
expressionsAreAdjacent (flip atLine 1 -> c) = do
    let exprs = splitExpressions c
    forM_ (zip exprs $ tail exprs) $ \(L loc1 expr1, L loc2 _) -> do
        loc2 `shouldStartOnLine` (endLine loc1 + 1)
        expr1 `shouldNotEndWithText` "\n"

data ChunkType = Module | Declaration | Expression
    deriving stock (Show, Eq)

parseChunks :: SourceCode -> Expectation
parseChunks c@(flip atLine 1 . tshow -> code) = do
    Right parsedChunks <- withGhc $ splitChunks code
    length parsedChunks `shouldBe` max 1 (length c.chunks)
    zipWithM_ compareChunks parsedChunks c.chunks
  where
    compareChunks :: CodeChunk -> SourceChunk -> Expectation
    compareChunks (describeParsed . unLoc -> parsed) (describeSource -> source) = parsed `shouldBe` source
    describeSource ModuleSourceChunk{} = Module
    describeSource DeclarationSourceChunk{} = Declaration
    describeSource ExpressionSourceChunk{} = Expression
    describeParsed ModuleChunk{} = Module
    describeParsed DeclarationChunk{} = Declaration
    describeParsed ExpressionChunk{} = Expression

spec :: Spec
spec = do
    it "correctly locates code" $ property locate
    it "does not produce adjacent chunks" $ property chunksAreNotAdjacent
    it "produces adjacent expressions" $ property expressionsAreAdjacent
    it "correctly parses chunks" $ property parseChunks
