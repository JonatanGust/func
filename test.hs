

{-
    Part 1
    n+1 steps, one step for each recusrion/decrement and then one for basecase.
-}


import Test.QuickCheck
power1 :: Integer -> Integer -> Integer
power1 0 k = 0
power1 n 0 = 1
power1 n k | k < 0 = error "power: negative argument"
power1 n k = product(replicate (fromInteger k) (fromInteger n))




power2 :: Integer -> Integer -> Integer
power2 n k | k < 0 = error "power: negative argument"
power2 n k | k == 0 = 1
power2 n k | even k = power2 (n*n) (div k 2)--(n*n)^(k/2)
power2 n k | odd k  = n * (power2 (n) (k-1)) --n*(n^(k-1))



power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

{-
    Test cases
    -Negative base
        -Negative exponent
        -Posetive base
        -Posetive exponent
    -Non integers
    -Odd vs even
    -Basecases
-}

prop_powers :: Integer -> Integer -> Bool
prop_powers n k = if(power1 n k == power2 n k) then(power1 n k == power n k)  else(False)

