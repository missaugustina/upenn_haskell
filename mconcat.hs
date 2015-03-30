import Data.Monoid

-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--         -- Defined in `Data.Monoid'

myConcat :: Monoid a => [a] -> a
myConcat = foldl mappend mempty

