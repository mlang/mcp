{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}
module WhereAmI (whereAmI, Location(..)) where

import Control.Exception
import Control.Lens ((&), (^.), (.~))
import Data.Aeson (FromJSON)
import Geo.Computations
import GHC.Generics (Generic)
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutNone)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq

data GeoIPException = GeoIPFailed String deriving Show

instance Exception GeoIPException

data Body = Body {
  businessName :: String
, businessWebsite :: String
, city :: String
, continent :: String
, country :: String
, countryCode :: String
, ipName :: String
, ipType :: String
, isp :: String
, lat :: String
, lon :: String
, org :: String
, query :: String
, region :: String
, status :: String
} deriving (Eq, Generic, Show)
instance FromJSON Body

data Location = Location {
  businessName :: String
, businessWebsite :: String
, city :: String
, continent :: String
, country :: String
, countryCode :: String
, ipName :: String
, ipType :: String
, isp :: String
, point :: Point
, org :: String
, query :: String
, region :: String
} deriving (Eq, Show)

tls :: Options
tls = defaults & header "Accept" .~ ["application/json"]
               & manager .~ Left noTimeout where
  noTimeout = tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }

whereAmI :: IO Location
whereAmI = do
  r <- asJSON =<< getWith tls "https://extreme-ip-lookup.com/json/"
  let Body {..} = r ^. responseBody
  pure $ case status of
    "success" -> Location businessName businessWebsite
                          city continent country countryCode
                          ipName ipType isp
                          (pt (read lat) (read lon) Nothing Nothing)
                          org query region
    _ -> throw $ GeoIPFailed status
