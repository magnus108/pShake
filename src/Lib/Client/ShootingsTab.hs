{-# LANGUAGE RecursiveDo #-}
module Lib.Client.ShootingsTab
    ( shootingsTab
    , ShootingsTab(..)
    )
where


import Lib.Client.Utils
import           Utils.Comonad
import           Control.Conditional            ( (?<>) )
import qualified Control.Lens                  as Lens
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation
import qualified Utils.ListZipper              as ListZipper

import qualified Lib.Model.Data                as Data
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI



data ShootingsTab = ShootingsTab
    { _container :: Element
    , _selection :: Event Shooting.Shootings
    }

instance Widget ShootingsTab where
    getElement = _container

shootingsTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior
           ( Data.Data
                 String
                 (ListZipper.ListZipper (Shooting.Shooting, [UI Element]))
           )
    -> UI ShootingsTab
shootingsTab bTranslations bTransMode errorView loadingView notAskedView bShootings = mdo

    (eSelection, hSelection) <- liftIO $ newEvent

    let bDisplay = pure $ \center (shooting' :: ListZipper.ListZipper (Shooting.Shooting, [UI Element])) -> do
                display <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ (snd (extract shooting'))

                UI.on UI.click display $ \_ -> do
                    liftIO $ hSelection (Data.Data (fmap fst shooting'))

                return $ display

    let bDisplay'' = (\f mode z -> case mode of
                            ClientTranslation.Translating ->
                                [UI.div #+ (concat (fmap snd (ListZipper.toList z)))]
                            ClientTranslation.Normal -> do
                                [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend f z)]
                     ) <$> bDisplay <*> bTransMode


    let _selection = fmap Shooting.Shootings $ filterJust $ Data.toJust <$> eSelection

    _container <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView <*> bDisplay'' <*> bShootings)

    return ShootingsTab { .. }

