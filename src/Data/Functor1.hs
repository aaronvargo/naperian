{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Data.Functor1 where

import           Control.Applicative  (Const (..))
import           Data.Proxy
import           Data.Type.Coercion

-- | Objects in the category @(Hask -> Hask) -> Hask@
class Functor1 w where
  -- | @
  -- map1 f . map1 g = map1 (f . g)
  -- map1 id = id
  -- @
  map1 :: (forall a. f a -> g a) -> w f -> w g

  -- | mapCoerce1 c = map1 (coerceWith c)
  mapCoerce1 :: (forall x. Coercion (f x) (g x)) -> w f -> w g
  mapCoerce1 f = map1 (coerceWith f)

instance Functor1 Proxy where
  map1 _ Proxy = Proxy

#if MIN_VERSION_base(4,9,0)
-- | since base-4.9
instance Functor1 (Const a) where
  map1 _ = Const . getConst
#endif
