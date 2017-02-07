module Utilities where

-- Takes a tuple containing two functions and a tuple containing two
-- arguments. If the arguments are valid for the given functions, the
-- nth function is mapped to the nth argument.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Takes a function and a Maybe value. If the value is not Nothing, the
-- function is applied to the value.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes two Maybe arguments. Returns the first argument if it is not 
-- Nothing, otherwise it returns the second argument.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Takes a function which returns a Maybe value and another value argument. 
-- If the function applied to the second argument returns a value (not Nothing), 
-- the result of the function is returned, otherwise the second argument is
-- returned.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Takes a function and value as arguments. Recursively runs the function
-- on the value argument until the result is equal to the argument.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Takes a real fraction and a list as arguments. Returns the nth element
-- where n-1 is equal to the fraction times the length of the list rounded
-- down.
-- Example: pick 0.7 [2,4,6,8,10]
-- n-1 = floor ( 0.7 * 5 = 3.5 ) = 3 <=> n = 4
-- returns the 4rd element of the array which is equal to 8
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
