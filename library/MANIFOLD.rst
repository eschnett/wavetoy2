Composing manifolds
===================

Manifolds and fields should be composable in the following ways:

- Gluing manifolds together either at a boundary, or with an overlap,
  as in:
  ::
     (m1 -> f, m2 -> f) -> ((m1 :+: m2) -> f)
     (m1 -> f, m2 -> f) -> (Either m1 m2 -> f)
  This is "either".

- If glued manifolds overlap, then there needs to be an isomorphism
  mapping between the respectiver overlapse. We might additionally
  require a "decomposition of unity" that describes which manifold is
  authoritative in what regions. The latter may rather need to be
  described on the level of the fields.

- "Concatenating" manifolds, as in: a 1d array of 1d arrays can be a
  2d array. Question: Is this a product? (Probabyl not.) Is this
  currying?
  ::
     (m1 -> m2 -> f) -> ((m1 :*: m2) -> f)
     (m1 -> m2 -> f) -> ((m1, m2) -> f)

- Products of fields, as in:
  ::
     (m -> f1, m -> f2) -> (m -> (f1, f2))

- Sums of fields? This seems easy, but not useful; this transformation
  is not bijective:
  ::
     Either (m -> f1) (m -> f2) -> (m -> Either f1 f2)

- Pullbacks:
  ::
     (n -> m) -> (m -> f) -> (n -> f)



Overall questions:

- Are fields maps from a manifold to a vector space? Or to a more
  general algebraic structure?

- Can fields be represented as functors? Profunctors? Arrows?
  Categories?

- Manifolds are probably not representable. Fields probably are.

- What is a presheaf?



Examining various paths
=======================

direct
------
::
   Field :: f a
   data FieldSum f g a = FieldSum (f a) (g a)
   data FieldProduct f g a = FieldSum (f (g a))
   type FieldSum = Data.Functor.Product
   type FieldProduct = Data.Functor.Compose



as function
-----------
::
   Field :: m a -> f b
   newtype FieldSum m f g a = FieldSum (m a -> (f a, g a))
   newtype FieldProduct m f g a = FieldProduct (m a -> f (g a))



as profunctor, a generalized function
-------------------------------------
[problematic]
::
   import Data.Profunctor   -- profunctors
   instance Profunctor Field

   Product in tangent space:
   Field m v -> Field m w -> Field m (v,w)
   -- (straightforward -- functor product (?))
   requires some kind of applicative structure?

   Gluing:
   Field m v -> Field n v -> Field (m|n) v
   -- (straightforward -- contravariant functor sum (?))
   requires some kind of co-applicative structure?

   Tangent space access (?):
   Field m n -> Field n v -> Field m v
   Data.Profunctor.Procompose

   Chaining:
   Field m (Field n v) -> Field (m,n) v
   (need generalisation of uncurry')
   (maybe via Closed?)

   Pullback:
   (n -> m) -> Field m v -> Field n v
   (straightforward -- contravariant functor map)

   is this useful: Data.Profunctor.Cayley?

   evaluate:
   Sieve?



as category
-----------
::
   import Control.Category
   Field :: Category (m a) (f b)



as category
-----------
::
   import Data.Category   -- data-category
   Field :: Category (m a) (f b)
   Field :: Category m v
   newtype FieldSum m v w = FieldSum m (v + w)
   newtype FieldProduct m n v = FieldProduct (m, n) v

   Gluing (coproduct) (this is not gluing unless v ~ w):
   FieldSum (Field m v) (Field n w) = FieldSum (Field m v | Field n w)

   Tangent space (uncurry?):
   FieldProduct (Field m n) (Field n v) = FieldProduct (Field (m,n) v)

   Outer product (categorical product):
   FieldProduct (Field m v) (Field n w) = FieldProduct (Field (m,n) (v,w))

   Product in tangent space (functor product):
   FieldProduct (Field m v) (Field m w) = FieldProduct (Field m (v * w))

   Pullback (contravariance!):
   (m -> n) -> Field n v -> Field m v



as arrow
--------
[unworkable]
(and thus also as category, since all arrows are categories)
(and thus also as profunctors, since all arrows are profunctors)
::
   import Control.Arrow
   import Control.Arrow.Extras   -- arrow-extras
   instance Arrow Field

   Product in tangent space:
   Field m v -> Field m w -> Field m (v,w)
   (&&&)

   Gluing:
   Field m v -> Field n v -> Field (m|n) v
   (|||)

   Tangent space access (?):
   Field m n -> Field n v -> Field m v
   (categorical composition)

   Chaining:
   Field m (Field n v) -> Field (m,n) v
   (&&&) . (>*>)

   Pullback:
   (n -> m) -> Field m v -> Field n v
   (arrow composition after converting (n -> m) into arrow,
   or precomposition with a pure function)

   evaluate:
   Field m v -> m -> v
   unArr? (opposite of arr?) doesn't exist. probably Field-specific.

   could add more categorical notions -- cartesian, closed, etc.

Conclusion: Arrows do not work. In an arrow, operations do not change
the representation, requiring a too-generic reqresentation. While we
need operations such as (&&&), these should be provided by a new type
(comparable to e.g. Compose for functors), not by an operation that
leaves types unchanged.

Back to profunctors? Or category?
