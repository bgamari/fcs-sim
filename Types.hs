module Types where

import Linear
import Control.Lens

type Viscosity = Double     -- ^ centipoise
type Diffusivity = Double   -- ^ nm^2 / ns
type Time = Double          -- ^ nanoseconds
type Length = Double        -- ^ nanometers
type BeamSize = V3 Length
type BoxSize = V3 Length

data Spherical a = Spherical { _r      :: !a    -- ^ radial
                             , _theta  :: !a    -- ^ inclination
                             , _phi    :: !a    -- ^ azimuth
                             }
                 deriving (Show, Eq, Ord)

sphericalV3 :: RealFloat a => Iso' (Spherical a) (V3 a)
sphericalV3 = iso from to
  where
    from (Spherical r theta phi) = V3 x y z
      where
        x = r * sin theta * cos phi
        y = r * sin theta * sin phi
        z = r * cos theta
    to (V3 x y z) = Spherical r theta phi
      where
        r = sqrt (x^2 + y^2 + z^2)
        theta = acos (z / r)
        phi = atan2 y x
{-# INLINE sphericalV3 #-}
