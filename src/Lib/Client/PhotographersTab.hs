{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Lib.Client.PhotographersTab
    ( photographersTab
    , PhotographersTab(..)
    )
where
import qualified Lib.Client.Pop.Popup2         as Popup
import Control.Conditional ((?<>))
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Client.Select.Dropdown2 as Dropdown

import qualified Lib.Client.Translation.Translation2
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
    }

instance Widget PhotographersTab where
    getElement = _container



translation text' openButton popup = mkWriteAttr $ \(tm, pm) x -> void $ do
        case (tm,pm) of
            (Translation.Translating, Popup.Open) ->  return x # set children [popup]
            (Translation.Translating, Popup.Closed) -> return x # set children [openButton]
            _ -> return x # set children [text']


photographersTab
    :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String Photographer.Photographers)
    -> UI PhotographersTab
photographersTab bTranslations bTransMode bPhotographers = do

    ((textNotAsked, openButtonNotAsked, popupNotAsked), bPopModeNotAsked, tTransNotAsked) <- Translation.translation2 bTranslations bTransMode (pure "notAsked")
    notAsked <- UI.div # sink (translation textNotAsked openButtonNotAsked popupNotAsked) ((,) <$> bTransMode <*> (facts bPopModeNotAsked))

    ((textLoading, openButtonLoading, popupLoading), bPopModeLoading, tTransLoading) <- Translation.translation2 bTranslations bTransMode (pure "loading")
    loading <- UI.div # sink (translation textLoading openButtonLoading popupLoading) ((,) <$> bTransMode <*> (facts bPopModeLoading))

    ((textError, openButtonError, popupError), bPopModeError, tTransError) <- Translation.translation2 bTranslations bTransMode (pure "error")
    error' <- UI.div # sink (translation textError openButtonError popupError) ((,) <$> bTransMode <*> (facts bPopModeError))


    let bZipper = fmap (Lens.view Photographer.unPhotographers) <$> bPhotographers

    let bDisplay = pure $ \mode center photographer -> do
            text <- UI.span # set text (photographer ^. Photographer.name)
            icon <-
                UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
            UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                #+ fmap element ([text] <> not (mode == Dropdown.Open) ?<> [icon])

    ((closed, open), tDropMode, eSelection) <- Dropdown.dropdown2 bTranslations bTransMode bZipper bDisplay

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Photographer.Photographers
                <$> eSelection

    let state = mkWriteAttr $ \(data', dropMode) x -> void $ do
                    case data' of
                        Data.NotAsked -> return x # set children [notAsked]
                        Data.Loading -> return x # set children [loading]
                        Data.Failure _ -> return x # set children [error']
                        Data.Data _ -> do -- wierd
                            case dropMode of
                                Dropdown.Open ->
                                    return x # set children [open]
                                Dropdown.Closed ->
                                    return x # set children [closed]

    _container <- UI.div # sink state ((,) <$> bPhotographers <*> (facts tDropMode))

    return PhotographersTab { .. }

