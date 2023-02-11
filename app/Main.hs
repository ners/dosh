module Main where

import Control.Monad
import Data.Functor
import Data.Generics.Labels ()
import Data.Text (Text)
import Development.IDE (Pretty (pretty), Priority (..), WithPriority (..))
import Development.IDE.Main (Log)
import Dosh.LSP.Server
import Dosh.Notebook
import Dosh.Server
import Dosh.Util
import Reflex.Network (networkView)
import Reflex.Vty

main :: IO ()
main = mainWidget $ initManager_ $ do
    io <- languageServer
    stderr :: Dynamic t Text <-
        foldDyn (flip mappend) "" $
            io.onLog <&> logLine
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- networkView (notebook io <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        grout (fixed $ pure 10) $ boxStatic roundedBoxStyle $ text $ current stderr
        void <$> ctrldPressed

logLine :: WithPriority Log -> Text
logLine WithPriority{priority = Error, payload} = tshow (pretty payload) <> "\n"
logLine WithPriority{priority = Warning, payload} = tshow (pretty payload) <> "\n"
logLine _ = ""
