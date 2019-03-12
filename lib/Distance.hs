{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Distance where

import qualified Geo.Computations as GPS
import qualified ICAO
import qualified WhereAmI as GeoIP

class Distance p r | p -> r where
  distance :: p -> p -> r

instance Distance GPS.Point GPS.Distance where
  distance = GPS.distance

instance Distance ICAO.ICAOCode GPS.Distance where
  distance p q = GPS.distance (ICAO.point p) (ICAO.point q)

data IP = IP String | ExternalIP

instance Distance IP (IO GPS.Distance) where
  distance (IP p) (IP q) = GPS.distance <$> (GeoIP.point <$> (GeoIP.whereIs p))
                                        <*> (GeoIP.point <$> (GeoIP.whereIs q))
  distance ExternalIP (IP q) = GPS.distance <$> (GeoIP.point <$> GeoIP.whereAmI)
                                            <*> (GeoIP.point <$> (GeoIP.whereIs q))
  distance (IP p) ExternalIP = GPS.distance <$> (GeoIP.point <$> (GeoIP.whereIs p))
                                            <*> (GeoIP.point <$> GeoIP.whereAmI)
  distance ExternalIP ExternalIP = pure 0
