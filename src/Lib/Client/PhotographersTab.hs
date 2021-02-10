{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.PhotographersTab
    ( photographersTab
    , PhotographersTab(..)
    )
where
import           Utils.Comonad

import qualified Lib.Client.Input.Text         as Entry
import qualified Utils.ListZipper              as ListZipper
import qualified Lib.Client.Pop.Popup         as Popup
import           Control.Conditional            ( (?<>) )
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Client.Select.Dropdown as Dropdown

import qualified Lib.Client.Translation.Translation
                                               as Translation

import qualified Data.HashMap.Strict           as HashMap

import qualified Lib.Model.Data                as Data
import qualified Relude.Unsafe                 as Unsafe
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Client.Picker.Picker      as Picker
import qualified Control.Lens                  as Lens
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )


data PhotographersTab = PhotographersTab
    { _container :: Element
    , _selection :: Event Photographer.Photographers
    , _eH3 :: (Event String, Behavior String)
    , _eH2 :: (Event String, Behavior String)
    , _eH1 :: (Event String, Behavior String)
    }

instance Widget PhotographersTab where
    getElement = _container


photographersTab
    :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String Photographer.Photographers)
    -> UI PhotographersTab
photographersTab bTranslations bTransMode bPhotographers = do

    let bZipper = Lens.view Photographer.unPhotographers <<$>> bPhotographers

    let bDisplayOpen = pure $ \center photographer -> do
                text <- UI.span # set text (photographer ^. Photographer.name)
                UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ fmap element [text]

    let bDisplayClosed = pure $ \photographer -> do
                    text <- UI.span # set text (photographer ^. Photographer.name)
                    icon <-
                        UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
                    UI.button
                        #. "button"
                        #+ fmap element [text, icon]

    (errorView, _eH1) <- Translation.translation2 bTranslations bTransMode (pure "error")
    (loadingView, _eH2) <- Translation.translation2 bTranslations bTransMode (pure "loading")
    (notAskedView, _eH3) <- Translation.translation2 bTranslations bTransMode (pure "notAsked")

    (opened, bView, eSelection) <- Dropdown.dropdown3
        bTranslations
        bTransMode
        bDisplayClosed
        bDisplayOpen


    let
        state = mkWriteAttr $ \(data', view, error', loading, notAsked) x -> void $ do
            case data' of
                Data.NotAsked -> notAsked x
                Data.Loading -> loading x
                Data.Failure _ -> error' x
                Data.Data zipper -> view x zipper


    _container <- UI.div
        # sink state ((,,,,) <$> bZipper <*> bView <*> errorView <*> loadingView <*> notAskedView)

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Photographer.Photographers
                <$> eSelection

    return PhotographersTab { .. }
