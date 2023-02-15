module Main where

import Control.Monad
import Data.Generics.Labels ()
import Dosh.GHC.Client qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.Notebook
import Dosh.Util
import Reflex.Network (networkView)
import Reflex.Vty

main :: IO ()
main = mainWidget $ do
    ghcServer <- GHC.server
    ghcClient <- GHC.client ghcServer
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- networkView (notebook ghcClient <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
