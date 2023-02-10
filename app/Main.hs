module Main where

import Control.Monad
import Data.Generics.Labels ()
import Dosh.LSP.Server
import Dosh.Notebook
import Dosh.Util
import Reflex.Network (networkView)
import Reflex.Vty

main :: IO ()
main = mainWidget $ initManager_ $ do
    io <- languageServer
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- networkView (notebook io <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
