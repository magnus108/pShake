{-# LANGUAGE RecursiveDo #-}
module Lib.Client.SessionsTab
    ( sessionsTab
    , SessionsTab(..)
    )
where

import Lib.Client.Utils
import           Utils.Comonad
import           Control.Conditional            ( (?<>) )
import qualified Control.Lens                  as Lens
import qualified Lib.Model.Session              as Session
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation
import qualified Utils.ListZipper              as ListZipper

import qualified Lib.Model.Data                as Data
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


data SessionsTab = SessionsTab
    { _container :: Element
    , _selection :: Event Session.Sessions
    }

instance Widget SessionsTab where
    getElement = _container


sessionsTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior
           ( Data.Data
                 String
                 (ListZipper.ListZipper (Session.Session, [UI Element]))
           )
    -> UI SessionsTab
sessionsTab bTranslations bTransMode errorView loadingView notAskedView bSessions = mdo

    (eSelection, hSelection) <- liftIO $ newEvent

    let bDisplay = pure $ \center (session' :: ListZipper.ListZipper (Session.Session, [UI Element])) -> do
                display <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ (snd (extract session'))

                UI.on UI.click display $ \_ -> do
                    liftIO $ hSelection (Data.Data (fmap fst session'))

                return $ display

    let bDisplay'' = (\f mode z -> case mode of
                            ClientTranslation.Translating ->
                                [UI.div #+ (concat (fmap snd (ListZipper.toList z)))]
                            ClientTranslation.Normal -> do
                                [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend f z)]
                     ) <$> bDisplay <*> bTransMode


    let _selection = fmap Session.Sessions $ filterJust $ Data.toJust <$> eSelection

    _container <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView <*> bDisplay'' <*> bSessions)

    return SessionsTab { .. }
