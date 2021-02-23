{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Input.Text
    ( entry
    , userText
    , TextEntry(..)
    )
where

import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI



data TextEntry = TextEntry
    { _elementTE :: Element
    , _userTE    :: Tidings String
    }

instance Widget TextEntry where
    getElement = _elementTE

userText :: TextEntry -> Tidings String
userText = _userTE

entry :: Behavior String -> UI TextEntry
entry bValue = do
    input    <- UI.input #. "input"


    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    window <- askWindow

    liftIOLater $ runUI window $ do
        value' <- currentValue bValue
        void $ element input # set value value'

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    let _elementTE = input
        _userTE    = tidings bValue $ UI.valueChange input

    return TextEntry { .. }


