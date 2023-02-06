module MyLib (someFunc) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text
import Dosh.Notebook
import Reflex
import Reflex.ExternalRef
import Reflex.Vty
import Util

echoServer
    :: (Reflex t, MonadIO m, PerformEvent t m, TriggerEvent t m)
    => m (ExternalRef t Text, ExternalRef t Text)
echoServer = do
    i <- newExternalRef
    o <- newExternalRef
    void $ liftIO $ forkIO $ forever $ do
        incomingText <- readExternalRef i
        forM_ (Text.inits incomingText) $ \prefix -> do
            writeExternalRef o prefix
            liftIO $ threadDelay 100_000
    pure (i, o)

someFunc :: IO ()
someFunc = mainWidget $ do
    (i, o) <- echoServer
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- dyn (notebook i o <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
