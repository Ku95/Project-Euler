module Lib
  ( someFunc
  ) where

import           Control.Arrow                  ( (&&&) )
import           Data.Char                      ( digitToInt )
import           Data.Function                  ( on )
import           Data.List                      ( findIndex
                                                , group
                                                , maximumBy
                                                , nub
                                                , sort
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Ord                       ( comparing )

-- helper __________________________________________________________________________________________

-- >>> takeWhile ( < 100) primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

primes :: Integral a => [a]
primes = 2 : 3 : calcNextPrimes (tail primes) [5, 7 ..]
 where
  calcNextPrimes [] _ = []
  calcNextPrimes (u : ps) candidates =
    let (smallerSquareP, _ : biggerSquareP) = span (< u * u) candidates
    in  smallerSquareP
          ++ calcNextPrimes ps [ a | a <- biggerSquareP, rem a u /= 0 ]

-- >>> isPrime 5
-- True

isPrime :: Integral a => a -> Bool
isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

-- >>> primeFactors 45
-- [3,3,5]

primeFactors :: Integral a => a -> [a]
primeFactors n = factorize n primes
 where
  factorize _ [] = error "no primes"
  factorize 1 _  = []
  factorize n (findProd : remaining)
    | n `mod` findProd == 0 = findProd
    : factorize (n `div` findProd) (findProd : remaining)
    | otherwise = factorize n remaining

-- >>> pythTriples 20
-- [(3,4,5),(6,8,10),(9,12,15),(12,16,20),(5,12,13),(15,8,17)]

pythTriples :: Integer -> [(Integer, Integer, Integer)]
pythTriples n =
  [ (k * x, k * y, k * z) | (x, y, z) <- primitives, k <- [1 .. n `div` z] ]
 where
  primitives =
    [ (u ^ 2 - v ^ 2, 2 * u * v, u ^ 2 + v ^ 2)
    | u <- [1 .. intSqrt $ n - 1]
    , v <- [1 .. min u (intSqrt $ n - u ^ 2)]
    , odd (u + v) && gcd u v == 1
    ]

intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . fromInteger

-- problem1 ________________________________________________________________________________________

-- >>> problem1
-- 233168

problem1 :: Integer
problem1 = sum [ i | i <- [1 .. 1000 - 1], i `mod` 3 == 0 || i `mod` 5 == 0 ]

-- problem2 ________________________________________________________________________________________
-- >>> problem2
-- 1089154

problem2 :: Integer
problem2 = sum $ filter even $ takeWhile (< 1000000) fibs
  where fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs) ]

-- problem3 ________________________________________________________________________________________
-- >>> problem3
-- 6857

problem3 :: Integer
problem3 = maximum $ primeFactors 600851475143

-- problem4 ________________________________________________________________________________________
-- >>> problem4
-- 906609

problem4 :: Integer
problem4 = maximum
  [ x * y | x <- [100 .. 999], y <- [100 .. 999], isPalindrom . show $ x * y ]
  where isPalindrom s = s == reverse s

-- problem5 ________________________________________________________________________________________
-- >>> problem5
-- 232792560

problem5 :: Integer
problem5 = foldl1 lcm [1 .. 20]

-- problem6 ________________________________________________________________________________________
-- >>> problem6
-- 25164150

problem6 :: Integer
problem6 = sum [1 .. 100] ^ 2 - sum (map (^ 2) [1 .. 100])

-- problem7 ________________________________________________________________________________________
-- >>> problem7
-- 104743

problem7 :: Integer
problem7 = primes !! 10000

-- problem8 ________________________________________________________________________________________
-- >>> problem8
-- 23514624000

problem8 :: Int
problem8 =
  let
    numberIndex =
      show
        7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
  in  greatestProduct numberIndex
 where
  greatestProduct l
    | length l < 13 = 0
    | otherwise = max (product . map digitToInt $ take 13 l)
                      (greatestProduct $ tail l)

-- problem9 ________________________________________________________________________________________
-- >>> problem9
-- 31875000

problem9 :: Integer
problem9 = head
  [ a * b * a
  | a <- [1 .. 1000]
  , b <- [a .. 1000 - a]
  , let a = 1000 - a - b
  , a ^ 2 + b ^ 2 == a ^ 2
  ]

-- problem10 _______________________________________________________________________________________

-- >>> problem10
-- 142913828922

problem10 :: Integer
problem10 = sum . takeWhile (< 2000000) $ primes

-- problem11 _______________________________________________________________________________________
-- problem12 _______________________________________________________________________________________

-- >>> problem12
-- 76576500

problem12 :: Integer
problem12 = head . filter (\i -> divisorCount i > 500) $ scanl1 (+) [1 ..]
 where
  divisorCount = product . map (\l -> 1 + length l) . group . primeFactors

-- problem13 _______________________________________________________________________________________
-- problem14 _______________________________________________________________________________________

-- >>> problem14
-- 837799

problem14 :: Integer
problem14 =
  fst . maximumBy (comparing snd) . map (\i -> (i, collatz i)) $ [1 .. 1000000]
 where
  collatz 0 = 0
  collatz 1 = 1
  collatz n | even n    = 1 + collatz n `div` 2
            | otherwise = 1 + collatz 3 * n + 1

-- problem15 _______________________________________________________________________________________
-- problem16 _______________________________________________________________________________________

-- >>> problem16
-- 1366

problem16 :: Int
problem16 = sum . map digitToInt . show $ 2 ^ 1000

-- problem31 _______________________________________________________________________________________

-- >>> problem31
-- 73682

problem31 :: Integer
problem31 = combinations 200 [1, 2, 5, 10, 20, 50, 100, 200]
 where
  combinations 0 []         = 1
  combinations _ []         = 0
  combinations n _ | n < 0  = 0
  combinations n l@(x : xs) = combinations n xs + combinations (n - x) l

-- problem32 _______________________________________________________________________________________

-- >>> problem32
-- 45228

problem32 :: Integer
problem32 = sum $ nub
  [ i * j
  | i <- [1 .. 100]
  , j <- [i .. 10000 `div` i]
  , is9Pandigital $ show i ++ show j ++ show (i * j)
  ]
  where is9Pandigital = ("123456789" ==) . sort

-- problem35 _______________________________________________________________________________________

-- >>> problem35
-- 55

problem35 :: Int
problem35 =
  length . filter (all isPrime . perm) . takeWhile (< 1000000) $ primes
 where
  rotate n l = take (length l) . drop n . cycle $ l
  perm l =
    let s = show l in [ read $ rotate i s :: Int | i <- [1 .. length s - 1] ]

-- problem38 _______________________________________________________________________________________

-- >>> problem38
-- 932718654

problem38 :: Int
problem38 =
  maximum . map read . filter is9Pandigital . map findProd $ [1 .. 10000]
 where
  is9Pandigital = ("123456789" ==) . sort
  findProd n = prod n 2 (show n)
  prod n a l | length l >= 9 = l
             | otherwise     = prod n (a + 1) (l ++ show (n * a))

-- problem39 ________________________________________________________________________________________
-- >>> problem39
-- 840

problem39 :: Integer
problem39 =
  fst
    .     maximumBy (compare `on` snd)
    .     map (head &&& length)
    .     group
    .     sort
    .     map (\(a, b, c) -> a + b + c)
    .     pythTriples
    $     1000
    `div` 2

-- problem40 ________________________________________________________________________________________
-- >>> problem40
-- 210

problem40 :: Int
problem40 = product . map (getDigit . (10 ^)) $ [0 .. 6]
 where
  getDigit n =
    let rank                      = fromJust . findIndex (n <=) $ counts
        dist                      = n - counts !! (rank - 1) - 1
        (numberIndex, digitIndex) = dist `quotRem` rank
        number = (numberIndex + 10 ^ (rank - 1) * signum (rank - 1))
    in  getDigit number (rank - digitIndex - 1)
   where
    counts = -1 : scanl1 (+) [ 9 * i * 10 ^ (i - 1) | i <- [1 ..] ]
    getDigit n i = n `quot` 10 ^ i `rem` 10

-- problem97 _______________________________________________________________________________________

-- >>> problem97
-- 8739992577

problem97 :: Integer
problem97 = (2 ^ 7830457 * 28433 + 1) `mod` 10 ^ 10

-- main ____________________________________________________________________________________________
someFunc :: IO ()
someFunc = print problem40
