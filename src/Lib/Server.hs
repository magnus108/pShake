module Lib.Server
    ( runServer
    )
where


import           Lib.App                        ( Env(..) )

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI

runServer :: MonadIO m => Env msg -> m ()
runServer Env {..} = liftIO $ do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           }
        $ \win -> do
              content <- UI.p # set text "bob"
              void $ UI.getBody win # set children [content]
