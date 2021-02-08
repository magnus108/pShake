{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Lib.Client.PhotographersTab
    ( photographersTab
    , PhotographersTab(..)
    )
where

import qualified Lib.Client.Input.Text         as Entry
import qualified Utils.ListZipper              as ListZipper
import qualified Lib.Client.Pop.Popup2         as Popup
import           Control.Conditional            ( (?<>) )
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Client.Select.Dropdown2   as Dropdown

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

    let bZipper =
            fmap (Lens.view Photographer.unPhotographers) <$> bPhotographers

    let
        bDisplay = pure $ \mode center photographer -> do
            text <- UI.span # set text (photographer ^. Photographer.name)
            icon <-
                UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
            UI.button
                #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                #+ fmap element
                        ([text] <> not (mode == Dropdown.Open) ?<> [icon])

    -- HANDLER MÅ IKKE KOMME MED UD..
    (bItem, hSelection, hPopup, eSelection) <- Dropdown.dropdown2
        bTranslations
        bTransMode
        bZipper
        bDisplay

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Photographer.Photographers
                <$> eSelection




    open   <- UI.div #. "buttons has-addons"
    closed <- UI.div
    transError <- Translation.translation bTranslations bTransMode (pure "error")
    transNotAsked <- Translation.translation bTranslations bTransMode (pure "notAsked")
    transLoading <- Translation.translation bTranslations bTransMode (pure "loading")

    let
        state = mkWriteAttr $ \(data', tm, pmError, pmLoading, pmNotAsked) x -> void $ do
            case data' of
                Data.NotAsked  -> do
                    case (tm, pmNotAsked) of
                        (Translation.Translating, Popup.Open) -> do
                            return x # set children [Translation._input transNotAsked, Translation._close transNotAsked]
                        (Translation.Translating, Popup.Closed) ->
                            return x # set children [Translation._open transNotAsked]
                        _ -> return x # set children [Translation._text transNotAsked]
                Data.Loading   -> do
                    case (tm, pmLoading) of
                        (Translation.Translating, Popup.Open) -> do
                            return x # set children [Translation._input transLoading, Translation._close transLoading]
                        (Translation.Translating, Popup.Closed) ->
                            return x # set children [Translation._open transLoading]
                        _ -> return x # set children [Translation._text transLoading]
                Data.Failure _ -> do
                    case (tm, pmError) of
                        (Translation.Translating, Popup.Open) -> do
                            return x # set children [Translation._input transError, Translation._close transError]
                        (Translation.Translating, Popup.Closed) ->
                            return x # set children [Translation._open transError]
                        _ -> return x # set children [Translation._text transError]
                Data.Data (d, s, z) -> do -- wierd
                    case s of
                        Dropdown.Open -> do
                            let
                                children' = ListZipper.toList
                                    (ListZipper.bextend (d hSelection s) z)
                            return x
                                #  set children []
                                #+ [element open # set children [] #+ children']
                        Dropdown.Closed -> do
                            let child = d hPopup s False z
                            return x
                                #  set children []
                                #+ [element closed # set children [] #+ [child]]


    _container <- UI.div
        # sink state ((,,,,) <$> bItem <*> bTransMode <*> (facts (Translation._tPopup transError)) <*> (facts (Translation._tPopup transLoading)) <*> (facts (Translation._tPopup  transNotAsked)))

    let _eH2 = (Translation._eInput transLoading, Translation._bKey transLoading)
    let _eH3 = (Translation._eInput transNotAsked, Translation._bKey transNotAsked)
    let _eH1 = (Translation._eInput transError, Translation._bKey transError)

    return PhotographersTab { .. }

