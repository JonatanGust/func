{-
    Part 0
    Code/function given by Lab
-}

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

{-
    Part 1
    n+1 steps, one step for each recusrion/decrement and then one for basecase.
-}


{-
    Part 2
-}
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 0 k = 0
power1 n k = product(replicate (fromInteger k) (fromInteger n))

{-
    Part 3
-}
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 0 k = 0
power2 n k | even k = power2 (n*n) (div k 2)--(n*n)^(k/2)
power2 n k | odd k  = n * (power2 (n) (k-1)) --n*(n^(k-1))

{-
    Part 4
    A - Test cases
        -Negative base
        -Posetive base
        -Posetive exponent
        -Odd vs even
        -Basecases
-}

--B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power1 n k' == power2 n k') && (power1 n k' == power n k')
    where k' = abs(k) -- <- part D

--C
test_powers = (prop_powers (-3) 5) && (prop_powers (-1) 0) && (prop_powers 0 5) && (prop_powers 2 5) && (prop_powers 3 5)

--D
import Test.QuickCheck