{-# LANGUAGE OverloadedStrings #-}

module Snowplow.Tracker
    ( Tracker
    , trackPageView
    , createTracker
    ) where

import Data.ByteString
import Data.Maybe (fromMaybe)

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS

import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import Network.HTTP.Types.Status

import Control.Monad (forever, when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent hiding (readChan, writeChan)
import Control.Concurrent.BoundedChan

import Iglu.Core
import Snowplow.Event


type Context = SelfDescribingJson

type SelfDescribingEvent = SelfDescribingJson


data Tracker = Tracker {
  encodeContexts :: Bool,
  collectorUri   :: String,
  manager        :: Manager,
  queue          :: BoundedChan SnowplowEvent,
  emitterThread  :: ThreadId
}

createEmitter :: Manager -> BoundedChan SnowplowEvent -> T.Text -> IO ThreadId
createEmitter manager queue collectorUri = forkIO $ forever pull
  where pull = do
          event <- readChan queue
          stm   <- getTimestamp
          let request = eventToRequest collectorUri $ event { deviceSentTimestamp = Just stm }
          retry <- case request of 
            Nothing -> return False
            Just r  -> fmap shouldRetry $ send manager r
          when retry $ writeChan queue event

createTracker :: Bool -> Maybe ManagerSettings -> Int -> String -> IO Tracker
createTracker encodeContexts managerSettings queueCapacity collectorUri = do
  trackerChannel <- newBoundedChan queueCapacity
  manager        <- newManager $ fromMaybe tlsManagerSettings managerSettings
  emitterThread  <- createEmitter manager trackerChannel $ T.pack collectorUri
  return $ Tracker encodeContexts collectorUri manager trackerChannel emitterThread

send :: Manager -> Request -> IO Status
send manager request = runResourceT $ do
  response <- http request manager
  return $ responseStatus response 

shouldRetry :: Status -> Bool
shouldRetry status = status == status200

eventToRequest :: T.Text -> SnowplowEvent -> Maybe Request -- should be MonadThrow
eventToRequest collectorEndpoint event = fmap setQuery initialUrl
  where fullUrl    = T.concat [collectorEndpoint, T.pack "/i?"]
        initialUrl = parseUrl $ T.unpack fullUrl 
        setQuery   = setQueryString (eventToUrl event)

track :: Tracker -> SnowplowEvent -> IO ()
track (Tracker encode curl _ queue _) event = do
  dtm <- getTimestamp
  let timestampedEvent = event { deviceCreatedTimestamp = Just dtm }
  let eventWithContexts = normalizeContexts encode timestampedEvent
  writeChan queue eventWithContexts

trackPageView :: Tracker -> String -> String -> [Context] -> IO ()
trackPageView tracker url page contexts = track tracker $ pageView url page contexts

-- Should not be exposed to user-space as doesn't set timestamp
pageView :: String -> String -> [Context] -> SnowplowEvent
pageView url page contexts = emptyEvent { eventType = PageView, url = Just url, pageTitle = Just page, contexts = contexts }

trackSelfDescribingEvent :: SelfDescribingEvent -> [Context] -> IO ()
trackSelfDescribingEvent event contexts = undefined

getTimestamp :: IO Integer
getTimestamp = round `fmap` getPOSIXTime

