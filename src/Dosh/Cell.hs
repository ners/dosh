{-# LANGUAGE LambdaCase #-}

module Dosh.Cell where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Zipper qualified as TZ
import Dosh.Util
import GHC.Generics (Generic)
import Reflex
import Reflex.Vty

data Cell = Cell
    { number :: Int
    , input :: Text
    , output :: Maybe Text
    }
    deriving stock (Show, Generic)

newCell :: Int -> Cell
newCell number =
    Cell
        { number
        , input = ""
        , output = Nothing
        }

evaluateCell
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadHold t (Performable m)
       , MonadFix (Performable m)
       , MonadFix m
       )
    => IoServer t
    -> Event t Cell
    -> m (Event t Text)
evaluateCell IoServer{..} ec = do
    performEvent_ $ eval <$> ec
    updated <$> foldDyn (flip mappend) "" response
  where
    eval :: Cell -> Performable m ()
    eval = liftIO . query . (.input)

cell
    :: forall t m
     . ( Reflex t
       , PerformEvent t m
       , HasFocusReader t m
       , HasImageWriter t m
       , HasInput t m
       , HasLayout t m
       , HasDisplayRegion t m
       , MonadFix m
       , HasTheme t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadHold t (Performable m)
       , MonadFix (Performable m)
       )
    => IoServer t
    -> Cell
    -> m (Event t Cell)
cell io c = do
    update <- grout (fixed $ pure 1) $ row $ do
        let ps1 = "In[" <> tshow c.number <> "]: "
        grout (fixed $ pure $ Text.length ps1) $ text $ pure ps1
        TextInput{..} <- grout flex $ textInput def{_textInputConfig_initialValue = TZ.fromText c.input}
        let edited :: Dynamic t Cell
            edited = set #input <$> _textInput_value <*> pure c
        onEnter :: Event t Cell <- tagPromptlyDyn edited <$> enterPressed
        evaluated :: Dynamic t Cell <- do
            responseDyn :: Dynamic t Text <- evaluateCell io onEnter >>= foldDyn (flip mappend) ""
            pure $ zipDynWith (#output ?~) responseDyn edited
        pure $ updated evaluated
    forM_ c.output $ \(output :: Text) -> do
        grout (fixed $ pure 1) $ row $ do
            let ps1 = "Out[" <> tshow c.number <> "]: "
            grout (fixed $ pure $ Text.length ps1) $ text $ pure ps1
            grout flex $ text $ pure output
    pure update
