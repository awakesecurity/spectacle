module Test.Laws.Lens
  ( -- * Lens Laws
    laws,
    injective,
    surjective,
    idempotent,
  )
where

import Lens.Micro (Lens', set)
import Lens.Micro.Extras (view)

import Hedgehog (Gen, PropertyT, diff, forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ---------------------------------------------------------------------------------------------------------------------

laws :: (Show a, Eq a, Show b, Eq b) => Lens' a b -> String -> Gen a -> Gen b -> TestTree
laws p desc gen1 gen0 =
  testGroup
    (if null desc then "lens laws" else desc ++ " - lens laws")
    [ testProperty "1. injectivity" $ property (injective p gen1 gen0)
    , testProperty "2. surjectivity" $ property (surjective p gen1)
    , testProperty "3. idempotency" $ property (idempotent p gen1 gen0)
    ]

-- | Setters are injective. You get back what you put in:
--
-- @
-- 'view' p ('set' p x fx) == 'set' p x fx
-- @
injective :: (Monad m, Show a, Eq b, Show b) => Lens' a b -> Gen a -> Gen b -> PropertyT m ()
injective p gen1 gen0 = do
  fx <- forAll gen1
  x <- forAll gen0
  diff (view p (set p x fx)) (==) x

-- | Setters are surjective. Putting back what you got doesn't change anything:
--
-- @
-- 'set' p ('view' p fx) fx == fx
-- @
surjective :: (Monad m, Eq a, Show a) => Lens' a b -> Gen a -> PropertyT m ()
surjective p gen1 = do
  fx <- forAll gen1
  diff (set p (view p fx) fx) (==) fx

-- | Setters are idempotent. Setting twice is the same as setting once:
--
-- @
-- 'set' p x ('set' p x fx) == 'set' p x fx
-- @
idempotent :: (Monad m, Eq a, Show a, Show b) => Lens' a b -> Gen a -> Gen b -> PropertyT m ()
idempotent p gen1 gen0 = do
  fx <- forAll gen1
  x <- forAll gen0
  diff (set p x (set p x fx)) (==) (set p x fx)
