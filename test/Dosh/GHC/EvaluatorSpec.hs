{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Dosh.GHC.EvaluatorSpec where

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Development.IDE.GHC.Compat (Ghc)
import Dosh.GHC.Evaluator qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.Prelude hiding (elements)
import Dosh.Util
import Test.Hspec
import Test.QuickCheck

runGhcSession :: Ghc () -> IO (ByteString, ByteString, [SomeException])
runGhcSession action = do
    GHC.testServer $ do
        action
        GHC.evaluate "mapM_ hFlush [stdout, stderr]"

data Input
    = Pragma Text Text
    | Import Text
    | Declaration Text
    | Expression Text Text
    deriving stock (Generic, Eq)

instance Show Input where
    show (Pragma x y) = Text.unpack $ "{-# " <> x <> " " <> y <> " #-}"
    show (Import x) = Text.unpack $ "import " <> x
    show (Declaration x) = Text.unpack x
    show (Expression x _) = Text.unpack x

arbitraryPragma :: Gen Input
arbitraryPragma = Pragma "LANGUAGE" <$> elements extensions
  where
    extensions :: [Text]
    extensions = foldr (\e acc -> e : ("No" <> e) : acc) [] ["OverloadedStrings", "OverloadedLabels"]

arbitraryImport :: Gen Input
arbitraryImport = Import <$> elements ["Data.Default", "Data.Text"]

arbitraryDeclaration :: Gen Input
arbitraryDeclaration =
    pure $
        Declaration $
            Text.unlines
                [ "square :: Int -> Int"
                , "square x = x^2"
                ]

arbitraryExpression :: Gen Input
arbitraryExpression =
    uncurry Expression
        <$> elements
            [ ("True", "True\n")
            ,
                ( Text.intercalate "\n"
                    [ "fromIntegral $"
                    , "  let x = 1"
                    , "      y = 2"
                    , "   in x + y"
                    ]
                , "3\n"
                )
            , ("putStrLn \"Hello!\"", "Hello!\n")
            ]

instance Arbitrary Input where
    arbitrary =
        oneof
            [ arbitraryPragma
            , arbitraryImport
            , arbitraryDeclaration
            , arbitraryExpression
            ]

newtype Chunk = Chunk {inputs :: [Input]}
    deriving stock (Eq)

instance Arbitrary Chunk where
    arbitrary = Chunk <$> arbitrary

toValidText :: Chunk -> Text
toValidText (Chunk inputs) = foldr addInput "" $ zip inputs (tail $ cycle inputs)
  where
    addInput :: (Input, Input) -> Text -> Text
    addInput (i@Expression{}, Expression{}) = ((tshow i <> "\n") <>)
    addInput (i@Expression{}, _) = ((tshow i <> "\n\n") <>)
    addInput (i, Expression{}) = ((tshow i <> "\n\n") <>)
    addInput (i, _) = ((tshow i <> "\n") <>)

instance Show Chunk where
    show = Text.unpack . toValidText

instance Eq SomeException where
    a == b = tshow a == tshow b

evalValidChunks :: Chunk -> Expectation
evalValidChunks chunk =
    withTimeout (1_000_000 + 100_000 * length chunk.inputs) $
        runGhcSession (GHC.evaluate $ toValidText chunk) `shouldReturn` (expectedOutput, "", [])
  where
    expectedOutput = mconcat $ output <$> chunk.inputs
    output (Expression _ o) = Text.encodeUtf8 o
    output _ = ""

quietly :: Property -> IO ()
quietly = quickCheckWithResult (stdArgs{chatty=False}) >=> \case
    Success{} -> pure ()
    Failure{output} -> expectationFailure output
    GaveUp{output} -> expectationFailure output
    NoExpectedFailure{output} -> expectationFailure output

spec :: Spec
spec = it "evaluates chunks" $ quietly $ property evalValidChunks
