---
title : Programming in Haskell with Via
date  : 2019-05-31
---


Programming in Haskell with Wrappers
====================================


Haskell is a wonderful language and one of the features that makes it so great is its typeclass system. One of the simplest is `Semigroup`, which just defines an associative binary function. 

```Haskell
class Semigroup a where
	(<>) :: a -> a -> a
```

Semigroups arise frequently, but we quickly run into an issue when a type has two equally natural definition of `(<>)`.  For example, `Bool` is a semigroup with `(<>) = (||)` or `(<>) = (&&)`. Idris solves this with named typeclasses.  In Haskell, we have to do a little more work. 

```Haskell
newtype All = All { getAll :: Bool }
newtype Any = Any { getAny :: Bool } 
```

Now we can define a `Semigroup` and `Monoid` instance for each.

``` haskell
instance Semigroup All where
	(All p) <> (All q) = All (p && q)
instance Monoid Conj where 
	mempty = All True

instance Semigroup Any where
	(Any p) <> (Any q) = Any (p && q)
instance Monoid Any where
	mempty = Any False
```

Now we can rewrite functions using the monoid instances.

```haskell
and :: [Bool] -> Bool
and = getAll . mconcat . map All
--    foldl (&&) True
 
or :: [Bool] -> Bool
or = getAll . mconcat . map All
--    foldl (||) False 
```

In both cases, we wrap our input, apply `mconcat` and unwrap. Simply by changing our choice of type to wrap in, we radically change the behavior. Our "wrap" and "unwrap" operations do not actually modify any data, they are just used to modify the type. Haskell has a typeclass in `Data.Coerce` just for this. 


``` Haskell 
class Coercible a b where
	coerce :: a -> b


instance Coercible Any Bool where
	coerce = getAny

instance Coercible Bool Any where
	coerce = Any


instance Coercible All Bool where
	coerce = getAll
	
instance Coercible Bool Allwhere
	coerce = All
```

The class `Coercible` is not a typical typeclass, we cannot actually write instances of it ourselves. The compiler generates the instances automatically and requires that the types have identical representations. What makes `coerce` so useful is that Haskell know it does nothing to the actual values. So, `(coerce :: Bool -> Any) True` gets optimized away and is not actually called. Simple newtypes are not the only instances of `Coercible` we’re interested in. For example, `Coercible a b` implies `Coercible [a] [b]`. 

Now we can rewrite both `and` and `or` as `coerce . mconcat . coerce` and making GHC pick the correct monoid by giving type hints. The extension `TypeApplications` makes this much more convenient. 

This pattern of coercion, application, and coercion is extremely commoon.

```haskell
via :: forall a b c d . (Coercible c a, Coercible b d)
    => (a -> b) -> c -> d
via f = (coerce :: b -> d)  . f . (coerce :: c -> a) 


and', or' :: [Bool] -> Bool
and' = via @_ @All mconcat
or'  = via @_ @Any mconcat
```

The type `Int` has a similar ambiguity to `Bool` in that we could define `(<>) = (+)` or `(<>) = (*)`. The type `Sum` and `Product` are defined in `Data.Monoid` and have `Monoid` instances as follows.

```haskell 
newtype Sum a = Sum { getSum :: a }

instance Num a => Monoid (Sum a) where
	mappend = via @a (+)
	mempty  = Sum 0


newtype Product a = Product { getProd :: a }

instance Num a => Monoid (Product a) where
	mappend = via @a (*)
	mempty  = Product 1
```

Now we can rewrite `product` and `sum` in this style. 

```haskell
product, sum :: forall a. Num a => [a] -> a
product = via @_ @(Product a) mconcat
sum     = via @_ @(Sum a) mconcat
```

For any ordered type `a`, we have two natural semigroups with `(<>) = min` and `(<>) = max`. So, we have wrappers `Min` and `Max` as below in `Data.Semigroup`.

```Haskell 
newtype Min a = Min { getMin :: a } 

instance Ord a => Semigroup (Min a) where
	(<>) = via @a min


newtype Max a = Max { getMax :: a } 

instance Ord a => Semigroup (Max a) where
	(<>) = via @a max 
```

Now we can see that `minimum` and `maximum` are both just `sconcat` with respect to different semigroups. 

```haskell
maximum :: forall a. Ord a => NonEmpty a -> a
maximum = via @_ @(Max a) sconcat

minimum :: forall a. Ord a => NonEmpty a -> a
minimum = via @_ @(Min a) sconcat
```

For any non-commutative semigroup, we can define a new semigroup by reversing the arguments.

```haskell 
newtype Dual a = Dual { getDual :: a }

instance Semigroup a => Semigroup (Dual a) where
	(<>) = via $ flip (<>)
```

We can use this to append lists in reverse order. 

```haskell
reverse :: forall a. [a] -> [a]
reverse = via @_ @(Dual [a]) mconcat . fmap (:[])
```

Note that we have to `fmap (: [])` because we do not have `Coercible a [a]` as they have different internal representations.

We can use more than just `mconcat`. These wrappers show up wherever we find typeclass instances with no “most natural” definition, for instance, `(<*>) :: [a -> b] -> [a] -> [b]`. We have an alternate definition using `ZipList`. 

```haskell 
instance Applicative ZipList where
	(ZipList fs) <*> (ZipList xs) = zipWith ($) fs xs
	pure x = ZipList [x]
```

We have a `Coercible (ZipList a) [a]` instance, which gives us the following.

```haskell
transpose :: forall a. [[a]] -> [[a]]
transpose = via @(ZipList (ZipList a)) sequenceA


zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = via @(ZipList a) . liftA2
```

These wrappers also allow us to write more expressive constraints. For example, the `Num` class is a famously poor design that implies an additive group, a multiplicative semigroup as well as `abs` and `signum` for some reason. We can define a much more reasonable hierarchy of structures and overloaded functions.

```haskell
(+) :: forall a. Monoid (Sum a) => a -> a -> a
(+) = via @(Sum a) (<>)

zero :: forall a. Monoid (Sum a) => a
zero = (coerce :: Sum a -> a) mempty


(*) :: forall a. Monoid (Product a) => a -> a -> a
(*) = via @(Product a) (<>)
 
unit :: forall a. Monoid (Product a) => a
unit = (coerce :: Product a -> a) mempty


class Monoid g => Grp g where
	inv :: g -> g

(-) :: forall a. Grp (Sum a) => a -> a -> a
a - b = a + (via @(Sum a) inv $ b)
 
(/) :: forall a. Grp (Product a) => a -> a -> a
a / b = a * (via @(Product a) inv $ b)
```
