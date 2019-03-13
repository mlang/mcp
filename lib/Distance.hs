{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Distance where

import qualified Geo.Computations as GPS
import qualified ICAO
import qualified WhereAmI as GeoIP

data IP = IP String | ExternalIP

class Distance p q r | p q -> r where
  distance :: p -> q -> r

instance Distance GPS.Point GPS.Point GPS.Distance where
  distance = GPS.distance

instance Distance GPS.Point ICAO.ICAOCode GPS.Distance where
  distance p q = GPS.distance p (ICAO.point q)

instance Distance ICAO.ICAOCode GPS.Point GPS.Distance where
  distance p q = GPS.distance (ICAO.point p) q

instance Distance ICAO.ICAOCode ICAO.ICAOCode GPS.Distance where
  distance p q = GPS.distance (ICAO.point p) (ICAO.point q)

instance Distance IP IP (IO GPS.Distance) where
  distance (IP p) (IP q) = GPS.distance <$> (GeoIP.point <$> (GeoIP.whereIs p))
                                        <*> (GeoIP.point <$> (GeoIP.whereIs q))
  distance ExternalIP (IP q) = GPS.distance <$> (GeoIP.point <$> GeoIP.whereAmI)
                                            <*> (GeoIP.point <$> (GeoIP.whereIs q))
  distance (IP p) ExternalIP = GPS.distance <$> (GeoIP.point <$> (GeoIP.whereIs p))
                                            <*> (GeoIP.point <$> GeoIP.whereAmI)
  distance ExternalIP ExternalIP = pure 0

instance Distance IP GPS.Point (IO GPS.Distance) where
  distance (IP p) q = GPS.distance <$> (GeoIP.point <$> (GeoIP.whereIs p))
                                   <*> pure q
  distance ExternalIP q = GPS.distance <$> (GeoIP.point <$> GeoIP.whereAmI)
                                       <*> pure q

instance Distance IP ICAO.ICAOCode (IO GPS.Distance) where
  distance (IP p) q = GPS.distance <$> (GeoIP.point <$> (GeoIP.whereIs p))
                                   <*> pure (ICAO.point q)
  distance ExternalIP q = GPS.distance <$> (GeoIP.point <$> GeoIP.whereAmI)
                                       <*> pure (ICAO.point q)
