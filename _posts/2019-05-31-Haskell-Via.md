---
title : Programming in Haskell with Via
date  : 2019-05-31
---


Haskell is a wonderful language and one of the features that makes it so great is its typeclass system. 
Two of the simplest and most useful are `Monoiod` and `Semigroup`.

```Haskell
class Semigroup a where
	(<>) :: a -> a -> a -- must be associative
	
class Semigroup a => Monoid a where
	mempty :: a -- identity element
```

Semigroups are ubiquitous,  but we quickly run into an issue when a type has two equally natural definition of `(<>)`.
For example, `Bool` is a semigroup with `(<>) = (||)` or `(<>) = (&&)`. 
Idris solves this with named typeclasses.
In Haskell, we have to do a little more work. 

```Haskell
newtype All = All { getAll :: Bool }
newtype Any = Any { getAny :: Bool } 
```

Now we can define a `Semigroup` and `Monoid` instance for each.

``` haskell
instance Semigroup All where
	(All p) <> (All q) = All (p && q)
instance Monoid All where 
	mempty = All True

instance Semigroup Any where
	(Any p) <> (Any q) = Any (p && q)
instance Monoid Any where
	mempty = Any False
```

Using these, we can write `and` and `or` with `mconcat`.

```haskell
and :: [Bool] -> Bool
and = getAll . mconcat . map All
--    foldl (&&) True
 
or :: [Bool] -> Bool
or = getAny . mconcat . map Any
--    foldl (||) False 
```

In both cases, we wrap our input to select a particular `Monoid` instance, apply `mconcat` and revert back to the bare type. 
Our "wrap" and "unwrap" operations do not actually modify any data, they just modify the type. 
Haskell has a typeclass in `Data.Coerce` just for this. 


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

The class `Coercible` is not a typical typeclass, we cannot actually write instances of it ourselves. 
The compiler generates the instances automatically and requires that the types have *identical* representations. 
What makes `coerce` so useful is that Haskell know it does nothing to the actual values, so `(coerce :: Bool -> Any) True` gets optimized away and is not actually called. 
We are not limited to just plain newtypes.
For example, `Coercible a b` implies `Coercible [a] [b]`. 

Now we can rewrite both `and` and `or` as `coerce . mconcat . coerce` and force GHC to use the correct monoid by giving type hints. The `TypeApplications` extension makes this more convenient. 

This pattern of coercion, application, then coercion is extremely common.

```haskell
via :: forall a b c d 
	. (Coercible c a, Coercible b d)
    => (a -> b) -> c -> d
via f = (coerce :: b -> d)  . f . (coerce :: c -> a) 


and', or' :: [Bool] -> Bool
and' = via @[All] @All mconcat
or'  = via @[Any] @Any mconcat
```

Here we gave both types, but `via @[All]` or `via @_ @All` would work as well, GHC will infer the rest.
Iceland Jack, author of [DerivingVia](https://github.com/ghc-proposals/ghc-proposals/pull/120), recently proposed [ApplyingVia](https://github.com/ghc-proposals/ghc-proposals/pull/218), a language extension that extends this approach and bakes it into GHC.
I strongly encourage reading through his proposal, there are some incredible examples in there.
This post is meant to give a rough sense of the style of `ApplyingVia` using relatively straight forward haskell. 


The type `Num a => a` has a similar ambiguity to `Bool` in that we could define `(<>) = (+)` or `(<>) = (*)`. 
In `Data.Monoiod` we have the types `Sum` and `Product` to give us those `Monoid` instances.

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

Note the explicit `forall a`, we need `a` to be in scope so we can pass it to `via`.
For any ordered type `a`, we have two natural semigroups with `(<>) = min` and `(<>) = max`. 
Ind `Data.Semigroup` we ind types `Min` and `Max` as below. 

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
minimum :: forall a. Ord a => NonEmpty a -> a
minimum = via @_ @(Min a) sconcat

maximum :: forall a. Ord a => NonEmpty a -> a
maximum = via @_ @(Max a) sconcat
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

We can use more than just `mconcat`. These wrappers show up wherever we find typeclass instances with no “most natural” definition. For instance, `(<*>) :: [a -> b] -> [a] -> [b]` has two obvious possible definitions. 
We can give an alternate definition with the `ZipList` type. 

```haskell 
instance Applicative ZipList where
	(ZipList fs) <*> (ZipList xs) = zipWith fs xs
	pure x = ZipList [x]
```

We have a `Coercible (ZipList a) [a]` instance, which allows for the following.

```haskell
transpose :: forall a. [[a]] -> [[a]]
transpose = via @(ZipList (ZipList a)) sequenceA


zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = via @(ZipList a) . liftA2
```

These wrappers also allow us to write more expressive constraints. 
For example, the `Num` class is a famously poor design that implies an additive group, a multiplicative semigroup as well as `abs` and `signum` for some reason. We can define a much more reasonable hierarchy of structures and overloaded functions.

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

