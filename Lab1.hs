
import Test.QuickCheck
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
    k+1 steps, one step for each recusrion/decrement of k and
    then one step for the basecase.
-}


--Part 2
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product(replicate (fromInteger k) (fromInteger n))


--Part 3
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)    --(n*n)^(k/2)
power2 n k = n * (power2 (n) (k-1))    --n*(n^(k-1))

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
prop_powers n k = (power1 n k == power2 n k)&&(power1 n k== power n k)

--C - test the things from A
test_powers :: Bool
test_powers = (prop_powers (-3) 5)&&(prop_powers (-1) 0)&&
                (prop_powers 0 5)&&(prop_powers 2 6)&&(prop_powers 2 5)

--D
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (power1 n k' == power2 n k')&&(power1 n k' == power n k')
    where k' = abs(k) -- <- part D
{-
    import of QC
    We havn't defined our functions for negative exponents (just like the
    given function) and thus we exlude those cases from our test.
-}