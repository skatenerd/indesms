{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Web.Scotty
import Data.Monoid (mconcat)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Read as R
import qualified Data.Text as T
import Data.Ratio
import GHC.Generics
import qualified Twilio.Messages as TM
import qualified Twilio as TW
import Control.Monad.Trans (liftIO)
import Geography.Geocoding.Google (geoEncode, geoDecode)
import qualified Network.Wreq as WREQ
import qualified Control.Lens as CL
import Control.Lens ((^.))
import qualified Data.List as DL
import Data.Ord
import System.Environment (getEnv)


data Stations = Stations { features :: [Station] } deriving (Show, Eq, Generic)
data Station = Station { geometry :: Geometry, properties :: Properties } deriving (Show, Eq, Generic)

-- this is lng/lat vs lat/lng
-- this data structure should be more descriptive, and contain a custom fromJSON
data Geometry = Geometry { coordinates :: (Double, Double) } deriving (Show, Eq, Generic)

data Properties = Properties { addressStreet :: String, bikesAvailable :: Int, docksAvailable :: Int } deriving (Show, Eq, Generic)
instance Aeson.FromJSON Stations
instance Aeson.FromJSON Station
instance Aeson.FromJSON Geometry
instance Aeson.FromJSON Properties

-- in meters
physicalDistance (lat1Deg, lng1Deg) (lat2Deg, lng2Deg) =
  let latDeltaDeg = (lat1Deg - lat2Deg)
      lngDeltaDeg = (lng1Deg - lng2Deg)
      latDeltaRad = toRadians latDeltaDeg
      lngDeltaRad = toRadians lngDeltaDeg
      lat1Rad = toRadians lat1Deg
      lat2Rad = toRadians lat2Deg
      toRadians x = (2 * pi) * (x / 360)
      haversineBody = ((sin (latDeltaRad / 2)) ** 2) + ((cos lat1Rad) * (cos lat2Rad) * ((lngDeltaRad / 2) ** 2))
      c = 2 * (atan2 (sqrt haversineBody) (sqrt (1 - haversineBody)))
      earthRadius = 6371000
   in earthRadius * c

stationDistance userLocation station =
  let (stationLng, stationLat) = coordinates $ geometry station
  in physicalDistance userLocation (stationLat, stationLng)

showStation station =
  let address = addressStreet (properties station)
      bikes = show $ bikesAvailable (properties station)
      docks = show $ docksAvailable (properties station)
  in "ADDRESS: " ++ address ++ "\n BIKES: " ++ bikes ++ "\n EMPTY SLOTS: " ++ docks ++ "\n"

getAnswer stationData (Left _) = "We're sorry, we encounterd an error"
getAnswer stationData (Right uesrLocation) =
  let stations = features stationData
      -- lol is it lat/lng or lng/lat?
      sorted = DL.sortOn (stationDistance uesrLocation) stations
  in "NEAREST INDEGO STATIONS ARE:\n" ++ (concatMap showStation $ take 3 sorted)

main :: IO ()
main = scotty 4567 $ do
  post "/" $ do
    body :: String <- param "Body"
    sender :: T.Text <- param "From"
    stationData <- liftIO $ (WREQ.asJSON =<< WREQ.get "https://www.rideindego.com/stations/json/" :: IO (WREQ.Response Stations))
    liftIO $ print stationData
    place <- liftIO $ geoEncode (body ++ " Philadelphia PA")
    myPhoneNumber <- liftIO $ getEnv "PHONE_NUMBER"
    let answer = getAnswer (stationData ^. WREQ.responseBody) place
    liftIO $ TW.runTwilio' (getEnv "ACCOUNT_SID") (getEnv "AUTH_TOKEN") $ do
      let body = TM.PostMessage sender (T.pack myPhoneNumber) $ T.pack answer
      TM.post body
    html $ "TY"
