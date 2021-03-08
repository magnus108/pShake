{-# LANGUAGE RecursiveDo #-}
module Lib.Client.CamerasTab
    ( camerasTab
    , CamerasTab(..)
    )
where

import Lib.Client.Utils
import           Utils.Comonad
import           Control.Conditional            ( (?<>) )
import qualified Control.Lens                  as Lens
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation
import qualified Utils.ListZipper              as ListZipper

import qualified Lib.Model.Data                as Data
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


data CamerasTab = CamerasTab
    { _container :: Element
    , _selection :: Event Camera.Cameras
    }

instance Widget CamerasTab where
    getElement = _container


camerasTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior
           ( Data.Data
                 String
                 (ListZipper.ListZipper (Camera.Camera, [UI Element]))
           )
    -> UI CamerasTab
camerasTab bTranslations bTransMode errorView loadingView notAskedView bCameras = mdo

    (eSelection, hSelection) <- liftIO $ newEvent

    let bDisplay = pure $ \center (camera' :: ListZipper.ListZipper (Camera.Camera, [UI Element])) -> do
                display <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ (snd (extract camera'))

                UI.on UI.click display $ \_ -> do
                    liftIO $ hSelection (Data.Data (fmap fst camera'))

                return $ display

    let bDisplay'' = (\f mode z -> case mode of
                            ClientTranslation.Translating ->
                                [UI.div #+ (concat (fmap snd (ListZipper.toList z)))]
                            ClientTranslation.Normal -> do
                                [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend f z)]
                     ) <$> bDisplay <*> bTransMode


    let _selection = fmap Camera.Cameras $ filterJust $ Data.toJust <$> eSelection

    _container <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView <*> bDisplay'' <*> bCameras)

    return CamerasTab { .. }
