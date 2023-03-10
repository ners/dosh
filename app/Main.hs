module Main where

import Dosh.GHC.Client qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.Notebook
import Dosh.Prelude
import Dosh.Util
import Reflex.Network (networkView)
import Reflex.Vty

main :: IO ()
main = mainWidget $ do
    ghcServer <- GHC.server
    ghcClient <- GHC.client ghcServer
    initialNotebook <- newNotebook
    initManager_ $ mdo
        dn :: Dynamic t Notebook <- holdDyn initialNotebook u
        u :: Event t Notebook <- holdUniqDyn dn >>= networkView . fmap (notebook ghcClient) >>= switchHold never
        -- grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
