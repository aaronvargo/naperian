{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Naperian
  ( module Data.Functor1
  , Naperian(..)
  , nindex
  -- * Default Definitions
  -- ** Naperian
  , Distribute1
  , distributeTabulate
  , distributeRepresentable
  , distributeIso
  , distributeCoerce
  -- ** Functor
  , fmapCotraverse1
  -- ** Apply/Applicative\/MonadZip
  , zipWithNap
  , apNap
  , pureNap
  -- ** Bind/Monad
  , bindNap
  -- ** Distributive
  , distributeNap
  , collectNap
  -- ** Representable
  , Logarithm(..)
  , tabulateLog
  , indexLog
  ) where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Comonad.Trans.Traced
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Distributive
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Rep
import Data.Functor1
import Data.Functor1.Applied
import Data.Stream.Infinite
import Data.Type.Coercion
import GHC.Generics hiding (Rep)

{- |

A more powerful form of 'Distributive' functor, which is equal in power to a
'Representable' functor (for some 'Rep'), but which can be implemented
asymptotically more efficiently for instances which don't support random access.

A functor is Naperian/Representable iff it's isomorphic to @(->) r@ for some
@r@. Such a functor can be thought of as a container of a fixed size, where @r@
is the type of positions in the container. By representing a position as a
function of type @forall x. f x -> x@, which gets the value at that position, a
Naperian/Representable functor can equivalently be shown to be one for which @f@
is isomorphic to @(->) (forall x. f x -> x)@

These isomorphisms are equivalent to 'distribute1' + 'fmap', but the latter can
be implemented more efficiently for containers which don't support random
access.

-}

class Distributive f => Naperian f where
  {-# MINIMAL distribute1 #-}

  -- |
  -- @
  -- 'distribute1' . 'Applied' = 'fmap' ('Applied' . 'Identity')
  -- 'distribute1' ('Const' x) = 'Const' x '<$' xs
  -- @
  distribute1 :: Functor1 w => w f -> f (w Identity)
  default distribute1 ::
    (Generic1 f, Naperian (Rep1 f), Functor1 w)
    => w f -> f (w Identity)
  distribute1 = distributeIso (from1 :: f a -> Rep1 f a) to1

  -- | @'cotraverse1' f = 'fmap' f . 'distribute1'@
  cotraverse1 :: Functor1 w => (w Identity -> a) -> w f -> f a
  cotraverse1 f = fmap f . distribute1

  -- | @'collect1' f = 'distribute1' . 'map1' f@
  collect1 :: Functor1 w => (forall x. g x -> f x) -> w g -> f (w Identity)
  collect1 f = distribute1 . map1 f

  -- | @'twiddle1' f g = 'fmap' f . 'distribute1' . 'map1' g@
  twiddle1 ::
       Functor1 w => (w Identity -> a) -> (forall x. g x -> f x) -> w g -> f a
  twiddle1 f g = fmap f . distribute1 . map1 g

  -- | @
  -- 'ntabulate' . 'nindex' = 'id'
  -- 'nindex' . 'ntabulate' = 'id'
  -- @
  ntabulate :: ((forall x. f x -> x) -> a) -> f a
  ntabulate f = cotraverse1 (\(TabulateArg g) -> g runIdentity) (TabulateArg f)

newtype TabulateArg a f = TabulateArg ((forall x. f x -> x) -> a)
instance Functor1 (TabulateArg a) where
  map1 f (TabulateArg g) = TabulateArg $ \h -> g (h . f)

-- | Inverse of 'ntabulate'
nindex :: f a -> (forall x. f x -> x) -> a
nindex x f = f x

-- * Default Definitions

-- | Alias for the type of 'distribute1'
type Distribute1 f = forall w. Functor1 w => w f -> f (w Identity)

-- | Derive 'distribute1' given an implementation of 'ntabulate'
distributeTabulate :: Naperian f => Distribute1 f
distributeTabulate w = ntabulate $ \f -> map1 (Identity . f) w

-- | Derive 'distribute1' given an instance of 'Representable'
distributeRepresentable :: Representable f => Distribute1 f
distributeRepresentable w = tabulate $ \f -> map1 (Identity . (`index` f)) w

-- | Derive 'distribute1' via an isomorphism
distributeIso ::
     Naperian g
  => (forall x. f x -> g x)
  -> (forall x. g x -> f x)
  -> Distribute1 f
distributeIso t frm = frm . distribute1 . map1 t

-- | Derive 'distribute1' via a coercion
distributeCoerce ::
     forall g f. Naperian g
  => (forall x. Coercion (g x) (f x))
  -> Distribute1 f
distributeCoerce x = coerceWith x . distribute1 . mapCoerce1 (sym x)

-- | Derive 'fmap' given an implementation of 'cotraverse1'. Note that an
-- implementation of 'distribute1' is /not/ sufficient!
fmapCotraverse1 :: Naperian f => (a -> b) -> f a -> f b
fmapCotraverse1 f = cotraverse1 (f . runIdentity . runApplied) . Applied

data PairOf a b f = PairOf (f a) (f b)
instance Functor1 (PairOf a b) where
  map1 f (PairOf x y) = PairOf (f x) (f y)

zipWithNap :: Naperian f => (a -> b -> c) -> f a -> f b -> f c
zipWithNap f as bs =
  cotraverse1 (\(PairOf (Identity a) (Identity b)) -> f a b) (PairOf as bs)

apNap :: Naperian f => f (a -> b) -> f a -> f b
apNap = zipWithNap ($)

-- Used instead of Const for compatibility with base < 4.9
newtype Const1 a (f :: * -> *) = Const1 { runConst1 :: a }
instance Functor1 (Const1 a) where
  map1 _ (Const1 x) = Const1 x

pureNap :: Naperian f => a -> f a
pureNap = cotraverse1 runConst1 . Const1

data BindArgs a b f = BindArgs (f a) (a -> f b)
instance Functor1 (BindArgs a b) where
  map1 f (BindArgs x g) = BindArgs (f x) (f . g)

bindNap :: Naperian f => f a -> (a -> f b) -> f b
bindNap as f =
  cotraverse1 (\(BindArgs (Identity a) g) -> runIdentity (g a)) (BindArgs as f)

newtype Composed g a f = Composed { runComposed :: g (f a) }
instance Functor g => Functor1 (Composed g a) where
  map1 f = Composed . fmap f . runComposed

distributeNap :: (Naperian f, Functor w) => w (f a) -> f (w a)
distributeNap = cotraverse1 (fmap runIdentity . runComposed) . Composed

collectNap :: (Naperian f, Functor w) => (a -> f b) -> w a -> f (w b)
collectNap f = distributeNap . fmap f

newtype Logarithm f = Logarithm { runLogarithm :: forall x. f x -> x }

tabulateLog :: Naperian f => (Logarithm f -> a) -> f a
tabulateLog f = ntabulate $ \x -> f (Logarithm x)

indexLog :: f a -> Logarithm f -> a
indexLog x (Logarithm f) = f x

-- * Instances

instance Naperian Identity where
  distribute1 = Identity

instance Naperian ((->) e) where
  distribute1 w e = map1 (Identity . ($ e)) w

instance (Naperian f, Naperian g) => Naperian (Product f g) where
  distribute1 =
    Pair <$> collect1 (\(Pair x _) -> x) <*> collect1 (\(Pair _ y) -> y)

newtype AppCompose w g f = AppCompose { runAppCompose :: w (Compose f g) }
instance Functor1 w => Functor1 (AppCompose w g) where
  map1 f = AppCompose . map1 (Compose . f . getCompose) . runAppCompose

instance (Naperian f, Naperian g) => Naperian (Compose f g) where
  distribute1 =
    Compose .
    cotraverse1 (collect1 (runIdentity . getCompose) . runAppCompose) .
    AppCompose

instance Naperian f => Naperian (IdentityT f) where
  distribute1 = distributeCoerce (Coercion :: Coercion (f x) (IdentityT f x))

instance Naperian f => Naperian (ReaderT e f) where
  distribute1 =
    distributeCoerce
      (Coercion :: Coercion (Compose ((->) e) f x) (ReaderT e f x))

instance Naperian w => Naperian (TracedT s w) where
  distribute1 =
    distributeCoerce
      (Coercion :: Coercion (Compose w ((->) s) x) (TracedT s w x))

instance Naperian f => Naperian (Cofree f) where
  distribute1 =
    distributeIso
      (\(x :< xs) -> Pair (Identity x) (Compose xs))
      (\(Pair (Identity x) (Compose xs)) -> x :< xs)

instance Naperian Stream where
  distribute1 =
    distributeIso
      (\(x :> xs) -> Pair (Identity x) xs)
      (\(Pair (Identity x) xs) -> x :> xs)

#if MIN_VERSION_distributive(0,5,1)
-- | since distributive-0.5.1
instance Naperian U1 where
  distribute1 _ = U1

-- | since distributive-0.5.1
instance (Naperian f, Naperian g) => Naperian (f :*: g) where
  distribute1 = distributeIso (\(x :*: y) -> Pair x y) (\(Pair x y) -> x :*: y)

-- | since distributive-0.5.1
instance (Naperian f, Naperian g) => Naperian (f :.: g) where
  distribute1 =
    distributeCoerce (Coercion :: Coercion (Compose f g x) ((:.:) f g x))

-- | since distributive-0.5.1
instance Naperian Par1 where
  distribute1 = distributeCoerce (Coercion :: Coercion (Identity x) (Par1 x))

-- | since distributive-0.5.1
instance Naperian f => Naperian (Rec1 f) where
  distribute1 = distributeCoerce (Coercion :: Coercion (f x) (Rec1 f x))

-- | since distributive-0.5.1
instance Naperian f => Naperian (M1 i c f) where
  distribute1 = distributeCoerce (Coercion :: Coercion (f x) (M1 i c f x))
#endif
