import Test.QuickCheck
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)


power1:: Integer -> Integer -> Integer
power1 n 0 = 1
power1 0 k = 0
power1 n k | k < 0 = error "power: negative argument"
power1 n k  = product(replicate (fromInteger k) (fromInteger n))


power2:: Integer -> Integer -> Integer
power2 n k | k == 0 = 1
power2 n k | k < 0 = error "power: negative argument"
power2 n k | even k = power2 (n*n) (div k 2)
power2 n k | odd k = power2 n (k-1) * n

prop_powers n k = power n k == power1 n k  && power n k == power2 n k

testcases = [(n,k) | n <- [0..10], k<-[0..10]]



testfunc2 = (prop_powers 2 3) && (prop_powers 2 4)

prop_powers' n k = power n k' == power1 n k'  && power n k' == power2 n k'
    where k' = abs(k)

{-

Negativa siffror
inte integers
udda/jämna k
basfall


-}