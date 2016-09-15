import Mp1
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.List as List

{-------- QUICKCHECKS --------}

prop_mytake :: Int -> [Int] -> Bool
prop_mytake n xs = (mytake n xs) == (take n xs)

prop_mydrop :: Int -> [Int] -> Bool
prop_mydrop n xs = (mydrop n xs) == (drop n xs)

prop_rev :: [Int] -> Bool
prop_rev xs = (rev xs == reverse xs)

prop_app :: Int -> [Int] -> Bool
prop_app n xs = (app (take (length xs `div` 3) xs) (drop (length xs `div` 3) xs)) == xs

prop_add :: (Ord a) => a -> [a] -> Bool
prop_add n ys = List.nub (List.insert n ys') == (add n ys')
  where ys' = List.nub (List.sort ys)

prop_union :: (Ord a) => [a] -> [a] -> Bool
prop_union xs ys = List.sort (List.union xs' ys') == (union xs' ys')
  where xs' = List.nub (List.sort xs)
        ys' = List.nub (List.sort ys)

prop_intersect :: (Ord a) => [a] -> [a] -> Bool
prop_intersect xs ys = (List.intersect xs' ys') == (intersect xs' ys')
  where xs' = List.nub (List.sort xs)
        ys' = List.nub (List.sort ys)

prop_powerset :: [Int] -> Bool
prop_powerset xs = (2^(length xs')) == length ps && (List.sort ps == ps)
  where xs' = take 10 (List.nub (List.sort (xs)))
        ps  = powerset xs'

prop_inclist :: [Int] -> Bool
prop_inclist xs = [ (x + 1) | x <- xs ] == (inclist xs)

prop_sumlist :: [Int] -> Bool
prop_sumlist xs = sumlist xs == List.sum xs

prop_myzip :: [Int] -> [Int] -> Bool
prop_myzip xs ys = myzip xs ys == List.zip xs ys

prop_addpairs :: [Int] -> [Int] -> Bool
prop_addpairs xs ys = zipWith (+) xs ys == addpairs xs ys

prop_ones :: Int -> Bool
prop_ones n
  | n < 0 = prop_ones ((-1) * n)
  | otherwise = sum (take n ones) == toInteger n

prop_nats :: Int -> Bool
prop_nats n
  | n < 0 = prop_nats ((-1) * n)
  | otherwise = sum (take n nats) == toInteger (div (n * (n+1)) 2)

prop_fib :: Int -> Bool
prop_fib n
  | n < 0 = prop_fib ((-1) * n)
  | n == 0 = [] == take n fib
  | n == 1 = [1] == take n fib
  | n == 2 = [1, 1] == take n fib
  | otherwise = sum (take 2 (drop (n-3) fib)) == head (drop (n-1) fib)

prop_inclist' :: [Int] -> Bool
prop_inclist' xs = [ (x + 1) | x <- xs ] == (inclist' xs)

prop_sumlist' :: [Int] -> Bool
prop_sumlist' xs = sumlist' xs == List.sum xs

prop_listcons :: [Int] -> Bool
prop_listcons xs = xs == cons2list (list2cons xs)
