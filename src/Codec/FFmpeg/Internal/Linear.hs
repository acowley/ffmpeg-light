{-# LANGUAGE ScopedTypeVariables #-}
-- | Minimal operations on small vector types.
module Codec.FFmpeg.Internal.Linear where
import Foreign.C.Types
import Foreign.Ptr (castPtr)
import Foreign.Storable

-- | A two-component vector
data V2 a = V2 !a !a

-- | A three-component vector
data V3 a = V3 !a !a !a

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Storable a => Storable (V3 a) where
  sizeOf _ = 3 * sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  peek ptr = let ptr' = castPtr ptr
             in V3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
  poke ptr (V3 x y z) = let ptr' = castPtr ptr
                        in do poke ptr' x
                              pokeElemOff ptr' 1 y
                              pokeElemOff ptr' 2 z

-- | Quadrance between two 3D points.
qd :: V3 CInt -> V3 CInt -> CInt
qd (V3 x1 y1 z1) (V3 x2 y2 z2) = let dx = x2 - x1
                                     dy = y2 - y1
                                     dz = z2 - z1
                                 in dx * dx + dy * dy + dz * dz

-- | A four-component vector
data V4 a = V4 !a !a !a !a

instance Functor V4 where
  fmap f (V4 x y z w) = V4 (f x) (f y) (f z) (f w)

instance Storable a => Storable (V4 a) where
  sizeOf _ = 4 * sizeOf (undefined::a)
  alignment _ = alignment (undefined::a)
  peek ptr = let ptr' = castPtr ptr
             in V4 <$> peek ptr' <*> peekElemOff ptr' 1
                   <*> peekElemOff ptr' 2 <*> peekElemOff ptr' 3
  poke ptr (V4 x y z w) = let ptr' = castPtr ptr
                          in do poke ptr' x
                                pokeElemOff ptr' 1 y
                                pokeElemOff ptr' 2 z
                                pokeElemOff ptr' 3 w
