{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module HomArray
    ( HomArray(..)
    , slice
    , length
    , replicateM
    ) where

import GHC.TypeLits
import Data.Proxy

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Sized as VGS
import Data.Finite
import Prelude hiding (length)

-- | An fixed-size array that can be unboxed.
newtype HomArray (n :: Nat) a = HomArray { unHomArray :: VGS.Vector VU.Vector n a }

length :: forall n a. KnownNat n => HomArray n a -> Int
length _ = fromIntegral $ natVal (Proxy @n)

instance (VU.Unbox a, (1 + n') ~ n, VG.Vector (VGS.Vector V.Vector n) (VU.Vector a), KnownNat n)
      => VU.Unbox (HomArray n a)

newtype instance VUM.MVector s (HomArray n a) = MV_HomArray (VGS.Vector V.Vector n (VUM.MVector s a))
newtype instance VU.Vector (HomArray n a) = V_HomArray (VGS.Vector V.Vector n (VU.Vector a))

instance (KnownNat n, (1 + n') ~ n, VU.Unbox a, VGM.MVector VUM.MVector a)
      => VGM.MVector VUM.MVector (HomArray n a) where
    basicLength (MV_HomArray v) = VGM.basicLength $ VGS.head v
    basicUnsafeSlice i j (MV_HomArray v) = MV_HomArray $ VGS.map (VGM.basicUnsafeSlice i j) v
    basicOverlaps (MV_HomArray u) (MV_HomArray v) = or $ VGS.zipWith VGM.basicOverlaps u v
    basicUnsafeNew len = MV_HomArray <$> VGS.replicateM (VGM.basicUnsafeNew len)
    basicInitialize (MV_HomArray v) = VGS.mapM_ VGM.basicInitialize v
    basicUnsafeRead (MV_HomArray v) i = HomArray <$> VGS.generateM (\j -> VGS.index v j `VGM.basicUnsafeRead` i)
    basicUnsafeWrite (MV_HomArray v) i (HomArray u) = VGS.imapM_ (\j x -> VGM.basicUnsafeWrite (VGS.index v j) i x) u
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicInitialize #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}

instance (KnownNat n, (1 + n') ~ n, VUM.Unbox a, VG.Vector VU.Vector a)
      => VG.Vector VU.Vector (HomArray n a) where
    basicUnsafeFreeze (MV_HomArray v) = V_HomArray <$> VGS.mapM VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_HomArray v) = MV_HomArray <$> VGS.mapM VG.basicUnsafeThaw v
    basicLength (V_HomArray v) = VG.basicLength $ VGS.head v
    basicUnsafeSlice i j (V_HomArray v) = V_HomArray $ VGS.map (VG.basicUnsafeSlice i j) v
    basicUnsafeIndexM (V_HomArray v) i = HomArray . VGS.convert <$> VGS.mapM (`VG.basicUnsafeIndexM` i) v
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}

slice :: (KnownNat n, VG.Vector (VGS.Vector V.Vector n) (VU.Vector a))
      => Finite n -> VU.Vector (HomArray n a) -> VU.Vector a
slice n (V_HomArray v) = v VG.! fromIntegral n

replicateM :: (Monad m, KnownNat n, VG.Vector VU.Vector a) => m a -> m (HomArray n a)
replicateM action = HomArray <$> VGS.replicateM action
