module Main where

import qualified Lib
import Text.Read (read)
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering)


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port, root] <- getArgs
    Lib.main (read port) root
