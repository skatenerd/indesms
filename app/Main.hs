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
data Geometry = Geometry { coordinates :: (Double, Double) } deriving (Show, Eq, Generic)
data Properties = Properties { addressStreet :: String, bikesAvailable :: Int, docksAvailable :: Int } deriving (Show, Eq, Generic)
instance Aeson.FromJSON Stations
instance Aeson.FromJSON Station
instance Aeson.FromJSON Geometry
instance Aeson.FromJSON Properties

-- *Main Lib Network.Wreq Data.Aeson Control.Lens>
--

cartesianDistance (x1, y1) (x2, y2) = sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))

stationDistance userLocation station =
  let stationLocation = coordinates $ geometry station
  in cartesianDistance userLocation stationLocation

showStation station =
  let address = addressStreet (properties station)
      bikes = show $ bikesAvailable (properties station)
      docks = show $ docksAvailable (properties station)
  in "ADDRESS: " ++ address ++ "\n BIKES: " ++ bikes ++ "\n EMPTY SLOTS: " ++ docks ++ "\n"

getAnswer stationData (Left _) = "We're sorry, we encounterd an error"
getAnswer stationData (Right (userFoo, userBar)) =
  let stations = features stationData
      -- lol is it lat/lng or lng/lat?
      sorted = DL.sortOn (stationDistance (userBar, userFoo)) stations
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
