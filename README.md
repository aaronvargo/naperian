# naperian

This package provides `Naperian` functors, a more powerful form of
`Distributive` functor which is equal in power to a `Representable` functor (for
some `Rep`), but which can be implemented asymptotically more efficiently for
instances which don't support random access.

`Distributive` functors allow distribution of `Functor`s:

```haskell
distribute :: (Distributive f, Functor g) => g (f a) -> f (g a)
```

With `Distributive`, you can, for example, zip two containers by distributing
the `Pair` `Functor`:

```haskell
data Pair a = Pair a a deriving Functor

zipDistributive :: Distributive f => f a -> f a -> f (a, a)
zipDistributive xs ys = fmap f $ distribute (Pair xs ys)
  where f (Pair x y) = (x, y)
```

Note that the two containers must have elements of the same type. `Naperian`,
however, allows the containers to have elements of different types:

```haskell
zipNaperian :: Naperian f => f a -> f b -> f (a, b)
```

It does so by allowing distribution of *`Functor1`s*, where a `Functor1` is a
functor from `Hask -> Hask` to `Hask`:

```haskell
class Functor1 w where
  map1 :: (forall a. f a -> g a) -> w f -> w g

distribute1 :: (Naperian f, Functor1 w) => w f -> f (w Identity)
```

The more polymorphic zip can then be implemented by distributing the `Pair1` `Functor1`:

```haskell
data Pair1 a b f = Pair1 (f a) (f b)
instance Functor1 (Pair1 a b) where ...

zipNaperian :: Naperian f => f a -> f b -> f (a, b)
zipNaperian as bs = fmap f $ distribute1 (Pair1 as bs)
  where f (Pair1 (Identity a) (Identity b)) = (a, b)
```

`Naperian` functors can be shown to be equivalent to `Representable` functors,
for some `Rep`, by selecting `Rep f = ∀x. f x -> x`. That is, a position in a
`Naperian` container can be represented as a function which gets the value at
that position. `tabulate` can then be derived using the `Functor1`:

```haskell
newtype TabulateArg a f = TabulateArg ((forall x. f x -> x) -> a)
```

The rest is left as an exercise for the reader.
