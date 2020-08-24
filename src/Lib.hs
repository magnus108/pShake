module Lib
    ( main
    )
where

import           Lib.Config                     ( loadConfig )
import           Lib.App                        ( mkEnv )
import           Lib.Server                     ( runServer )

main :: Int -> FilePath -> IO ()
main port root = do
    loadConfig root "config.json" >>= mkEnv port >>= runServer
