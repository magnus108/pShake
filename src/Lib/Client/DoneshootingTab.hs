{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DoneshootingTab
    ( doneshootingTab
    , DoneshootingTab(..)
    )
where

import Lib.Client.Utils
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Lib.Model.Data                as Data

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Doneshooting                as Doneshooting
import qualified Lib.Client.Picker.Picker      as Picker
import qualified Control.Lens                  as Lens


data DoneshootingTab = DoneshootingTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DoneshootingTab where
    getElement = _container

doneshootingTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior (Element -> [UI Element])
    -> Behavior (Data.Data String Doneshooting.Doneshooting)
    -> UI DoneshootingTab
doneshootingTab bTranslations bTransMode errorView loadingView notAskedView pickView bDoneshooting = mdo
    _selector  <- UI.button #. "button" 

    let bZipper = Lens.view Doneshooting.unDoneshooting <<$>> bDoneshooting

    let bDisplay = pure $ \filepath -> [element _selector # set children [] #+ [UI.string filepath, UI.span #. "icon" #+ [UI.mkElement "i" #. "far fa-file"]]]
 
    let bErrorDisplay = (\p -> UI.div #+ (p _selector)) <$> pickView
    let errorView' = (\vs ev -> ev : vs) <$> errorView <*> bErrorDisplay

    _container <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView' <*> bDisplay <*> bZipper)
    let _selection = UI.click _selector

    return DoneshootingTab { .. }
