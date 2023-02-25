module Test.Hspec.Expectations.Extra where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec
import Prelude

expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Expectation
compareWith comparator errorDesc result expected = expectTrue errorMsg (comparator expected result)
  where
    errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

shouldContainText :: Text -> Text -> Expectation
a `shouldContainText` b = Text.unpack a `shouldContain` Text.unpack b

shouldStartWithText :: Text -> Text -> Expectation
a `shouldStartWithText` b = Text.unpack a `shouldStartWith` Text.unpack b

shouldEndWithText :: Text -> Text -> Expectation
a `shouldEndWithText` b = Text.unpack a `shouldEndWith` Text.unpack b

shouldNotStartWith :: (Eq a, Show a) => [a] -> [a] -> Expectation
shouldNotStartWith = compareWith ((not .) . isPrefixOf) "should not start with"

shouldNotStartWithText :: Text -> Text -> Expectation
a `shouldNotStartWithText` b = Text.unpack a `shouldNotStartWith` Text.unpack b
