{-# LANGUAGE OverloadedStrings #-}

module Snowplow.Tracker
    ( Tracker (..)
    , SnowplowEvent (..)
    , trackPageView
    , eventToUrl
    , emptyEvent
    , createEmitter
    , eventToRequest
    , Tracker
    ) where

import qualified Data.Text as T

import qualified Data.HashMap.Strict as HS


import Control.Monad.Trans.Resource
import Network.HTTP.Conduit

import Control.Monad (void, forever)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent hiding (readChan, writeChan)
import Control.Concurrent.BoundedChan
import Data.Aeson

-- import Data.IORef
-- import System.IO.Unsafe

-- Iglu

data SchemaVer = SchemaVer { 
  model :: Int,
  revision :: Int,
  addition :: Int
} deriving (Show, Eq)

instance ToJSON SchemaVer where
  toJSON v = String $ T.pack $ show $ model v


data SchemaRef = SchemaRef {
  vendor :: String,
  name :: String,
  format :: String,
  version :: SchemaVer
} deriving (Show, Eq)

instance ToJSON SchemaRef where
  toJSON v = undefined


data SelfDescribingJson = SelfDescribingJson {
  schema :: SchemaRef,
  jsonData :: Value
} deriving (Show, Eq)


instance ToJSON SelfDescribingJson where
  toJSON v = undefined

-- Snowplow

type Context = SelfDescribingJson
type SelfDescribingEvent = SelfDescribingJson


-- Technically, queue is very separate entity

data Tracker = Tracker {
  collectorUri :: String,
  queue :: BoundedChan SnowplowEvent
}


-- We should have an emitter, who is responsible for 
-- queing event queue and doing actual sending


data SnowplowEvent = SnowplowEvent {
  trackerVersion :: Maybe String,
  encodedContexts :: Maybe [Context],
  url :: Maybe String,
  pageTitle :: Maybe String,
  deviceCreatedTimestamp :: Maybe String
} deriving (Show, Eq)

instance ToJSON SnowplowEvent where
  toJSON e = object [
    "tv"   .= trackerVersion e,
    "url"  .= url e,
    "cx"   .= encodedContexts e,
    "page" .= pageTitle e,
    "dtm"  .= deviceCreatedTimestamp e]


emptyEvent :: SnowplowEvent
emptyEvent = SnowplowEvent { 
  trackerVersion  = Just "haskell-0.1.0.0",
  encodedContexts = Nothing,
  url = Nothing,
  pageTitle = Nothing,
  deviceCreatedTimestamp = Nothing
}

createEmitter :: Tracker -> IO ()
createEmitter tracker = void $ forkIO $ forever pull
  where pull = do
          event <- readChan $ queue tracker 
          let req = eventToRequest (T.pack $ collectorUri tracker) event
          print req

-- foo :: IO ()
-- foo = do
--   request <- parseUrl "http://google.com/"
--   manager <- newManager tlsManagerSettings
--   runResourceT $ do
--       resource <- http request manager
--       r <- withIO alloc 3
--       fmap void r


-- alloc :: IO ()
-- alloc = print "hello"
-- 
-- rele :: a -> IO ()
-- rele _ = return ()


-- runResourceT allocates

eventToRequest :: T.Text -> SnowplowEvent -> Maybe Request
eventToRequest collectorEndpoint event = parseUrl $ T.unpack fullUrl
  where fullUrl = T.concat [collectorEndpoint, T.pack "?", eventToUrl event]


eventToUrl :: SnowplowEvent -> T.Text
eventToUrl e = urlizePayload $ normalizeGetPayload $ removeNulls $ toJSON e

trackPageView :: Tracker -> String -> String -> IO ()
trackPageView (Tracker curl q) url page = writeChan q $ pageView url page

-- Should not be exposed to user-space as doesn't set timestamp
pageView :: String -> String -> SnowplowEvent
pageView url page = emptyEvent { url = Just url, pageTitle = Just page }

pageView' :: String -> String -> IO SnowplowEvent
pageView' url page = do
  dtm <- getDtm
  return (pageView url page) { deviceCreatedTimestamp = Just dtm }


trackSelfDescribingEvent :: SelfDescribingEvent -> [Context] -> IO ()
trackSelfDescribingEvent event contexts = undefined

-- It'll send event once, we need it constantly pulling
emit :: Tracker -> IO ()
emit tracker = do
  event <- readChan $ queue tracker 
  return ()

removeNulls :: Value -> HS.HashMap T.Text Value
removeNulls json = case json of
  Object hm -> filterNulls hm
  _ -> HS.fromList []
  where filterNulls m = HS.filterWithKey predicate m
        predicate _ v = case v of
            Null -> False
            _ -> True

normalizeGetPayload :: HS.HashMap T.Text Value -> HS.HashMap T.Text T.Text
normalizeGetPayload payload = HS.map encode payload
  where encode v = case v of
            String s -> s
            o -> encode o
  
urlizePayload :: HS.HashMap T.Text T.Text -> T.Text
urlizePayload hm = T.intercalate (T.pack "&") (fmap join $ HS.toList hm) 
  where join pair = case pair of
            (k, v) -> T.concat [k, T.pack "=", v]


getDtm :: IO String
getDtm = return "my-dtm"
