{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib (getPosition, getInstruments) where

import Console.Options
import Control.Monad.Except
import BitMEX.API
import BitMEX.Types
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (Scheme(Https), ServantError)
import Servant.Common.BaseUrl (BaseUrl(..))

getPosition :: FlagParam String -> Action (IO ())
getPosition fs toParam = do
  instruments <- getInstrumentsRequest (instrumentsRequest createBitMEXClient)
  --print . show $ fs
  --print . show $ toParam
  print . show $ instruments

getInstruments :: IO ()
getInstruments = do
  let BitMEXBackend {..} = createBitMEXClient
  instruments <- getInstrumentsRequest instrumentGetActive
  print . show $ instruments

instrumentsRequest :: BitMEXBackend BitMEXClient -> BitMEXClient [Instrument]
--instrumentsRequest client = instrumentGetActive client :: BitMEXClient [Instrument]
instrumentsRequest BitMEXBackend { instrumentGetActive = a, .. } = a


getInstrumentsRequest :: BitMEXClient [Instrument] -> IO [Instrument]
getInstrumentsRequest client = do
  either <- runExceptT (runBitMEXClientBy client)
  r <- handleError either
  return r

handleError :: Either ServantError [Instrument] -> IO [Instrument]
handleError (Left e) = do
  print e
  return []

handleError (Right i) = pure i


runBitMEXClientBy :: BitMEXClient a -> ExceptT ServantError IO a
runBitMEXClientBy client = do
  manager <- liftIO $ newManager tlsManagerSettings
  runClient client manager $ BaseUrl Https "www.bitmex.com" 443 "/api/v1"