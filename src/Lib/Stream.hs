{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Stream
    ( StreamAPI
    , streamAPI
    , streamDocs
    , photographersStream
    , printSourceIO
    , app6
    , streamPhotographers
    )
where

import           Servant.API
import           Servant.Client
import           Servant.Server
import           Servant.Types.SourceT          ( foreach
                                                , source
                                                )


import qualified Servant.Client.Streaming      as S

import qualified Lib.Model.Photographer        as Photographer
import qualified Utils.ListZipper              as ListZipper

import           Servant.Docs                   ( API, docs)


type StreamAPI
    = "photographersStream" :> StreamGet NewlineFraming JSON (SourceIO Photographer.Photographers)


printSourceIO :: Show a => ClientEnv -> S.ClientM (SourceIO a) -> IO ()
printSourceIO env c = S.withClientM c env $ \e -> case e of
    Left  err -> putStrLn $ "Error: " ++ show err
    Right rs  -> foreach fail print rs


streamAPI :: Proxy StreamAPI
streamAPI = Proxy

streamDocs :: API
streamDocs = docs streamAPI


photographersStream :: S.ClientM (SourceIO Photographer.Photographers)
photographersStream = S.client streamAPI


streamPhotographers :: SourceIO Photographer.Photographers
streamPhotographers =
    source
        $ [ Photographer.Photographers
                (ListZipper.ListZipper
                    []
                    (Photographer.Photographer "bob" "cat")
                    []
                )
          ]

app6 :: Application
app6 = serve streamAPI (return streamPhotographers)
