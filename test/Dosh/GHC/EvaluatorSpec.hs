module Dosh.GHC.EvaluatorSpec where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (hGetSome)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Text qualified as Text
import Dosh.GHC.Client ()
import Dosh.GHC.Evaluator qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.Prelude hiding (elements, log)
import Dosh.Util
import GHC (Ghc)
import Test.Hspec
import Test.QuickCheck

runGhcSession :: Ghc () -> IO (ByteString, [Text])
runGhcSession a = do
    output <- newMVar ""
    errors <- newMVar []
    let reportError :: SomeException -> IO ()
        reportError = modifyMVar_ errors . (pure .) . (:) . tshow
    (input, outputH, _) <- GHC.server' reportError
    input $ do
        let exec = a >> GHC.evaluate "mapM_ hFlush [stdout, stderr]"
        let log = forever $ liftIO $ do
                content <- hGetSome outputH defaultChunkSize
                modifyMVar_ output $ pure . (<> content)
        raceWithDelay_ 1000 exec log
    (,) <$> takeMVar output <*> takeMVar errors

data Input
    = Pragma Text Text
    | Import Text
    | Declaration Text
    | Expression Text
    deriving stock (Eq)

instance Show Input where
    show (Pragma x y) = Text.unpack $ "{-# " <> x <> " " <> y <> " #-}"
    show (Import x) = Text.unpack $ "import " <> x
    show (Declaration x) = Text.unpack x
    show (Expression x) = Text.unpack x

arbitraryPragma :: Gen Input
arbitraryPragma = Pragma "LANGUAGE" <$> elements extensions
  where
    extensions :: [Text]
    extensions = foldr (\e acc -> e : ("No" <> e) : acc) [] ["OverloadedStrings", "OverloadedLabels"]

arbitraryImport :: Gen Input
arbitraryImport = pure $ Import "Data.Default"

arbitraryDeclaration :: Gen Input
arbitraryDeclaration =
    pure $
        Declaration $
            Text.unlines
                [ "square :: Int -> Int"
                , "square x = x^2"
                ]

arbitraryExpression :: Gen Input
arbitraryExpression = pure $ Expression "True"

instance Arbitrary Input where
    arbitrary =
        oneof
            [ arbitraryPragma
            , arbitraryImport
            , arbitraryDeclaration
            , arbitraryExpression
            ]

newtype Chunk = Chunk [Input]
    deriving stock (Eq)

instance Arbitrary Chunk where
    arbitrary = Chunk <$> arbitrary

toValidText :: Chunk -> Text
toValidText (Chunk inputs) = foldr addInput "" $ zip inputs (tail $ cycle inputs)
    where
        addInput :: (Input, Input) -> Text -> Text
        addInput (i@Expression{},Declaration{}) = ((tshow i <> "\n\n") <>)
        addInput (i@Expression{},Import{}) = ((tshow i <> "\n\n") <>)
        addInput (i@Declaration{},Expression{}) = ((tshow i <> "\n\n") <>)
        addInput (i@Import{},Expression{}) = ((tshow i <> "\n\n") <>)
        addInput (i,_) = ((tshow i <> "\n") <>)

instance Show Chunk where
    show = Text.unpack . toValidText

evalValidChunks :: Chunk -> Expectation
evalValidChunks (toValidText -> chunk) = runGhcSession (GHC.evaluate chunk) `shouldReturn` ("", [])

spec :: Spec
spec = do
    it "evaluates empty string" $ property $ do
        runGhcSession (GHC.evaluate "") `shouldReturn` ("", [])
    it "evaluates hello world" $ property $ do
        runGhcSession (GHC.evaluate "putStrLn \"Hello world!\"") `shouldReturn` ("Hello world!\n", [])
    it "evaluates well-formed chunks" $ property evalValidChunks
