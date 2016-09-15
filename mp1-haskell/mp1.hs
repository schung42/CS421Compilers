module Mp1 where

data Cons a = Cons a (Cons a)
            | Nil
  deriving (Show,Eq)

data Exp = IntExp Int
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show,Eq)

{-
 - You'll want to go through and put in the type signatures first, if you want
 - to check your work incrementally with quickCheck. (quickCheck is a way to
 - create random tests to validate your work.)
 -
 - To use our quickChecks, you'll want to load up mp1check by going:
 -   :l mp1check
 -
 - then for each property in mp1check, you'll want to test with:
 -   > quickCheck PROP_NAME
 -
 - For example, to test mytake, you would run:
 -   > quickCheck prop_mytake
 -}

mytake :: Int -> [Int] -> [Int]
mytake n xx  
    | n <= 0 = []
    | length xx == 0 = []
    | otherwise = head xx : mytake (n-1) (tail xx)

mydrop :: Int -> [Int] -> [Int]
mydrop n xx
    | n <=0 = xx
    | length xx == 0 = []
    | otherwise = mydrop (n-1) (tail xx)

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

app :: [Int] -> [Int] -> [Int]
app xx yy = xx ++ yy

add :: Ord a => a -> [a] -> [a]
add a [] = [a]
add a (x:xs)
    | a < x = a : x : xs
    | a == x = x : xs
    | otherwise = x : add a xs

union :: Ord a => [a] -> [a] -> [a]
union a [] = a
union [] a = a
union (a:as) b = add a (union as b)


{--intersect :: Ord a => [a] -> [a] -> [a]
intersect [] a = [] 
intersect a [] = []
intersect (a:as) (x:xs)
    | temp == (x:xs) = intersect as xs
    | otherwise = intersect as xs
    where temp = add a xs--}
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] a = [] 
intersect a [] = []
intersect (a:as) (x:xs)
    | a == x = x : xs
    | otherwise = intersect as xs

powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ [x| x<-powerset xs]
--powerset (x:xs) = [ add x a| a<-powerset(xs)] ++ powerset xs


inclist :: (Num a) => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x + 1) : inclist xs


sumlist :: (Num t) => [t] -> t
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

myzip :: [t] -> [t1] -> [(t, t1)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs a [] = []
addpairs [] a = []
addpairs (a:as) (b:bs) = a + b : addpairs as bs

{--| do it with myzip here, you bum
--}
ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
--nats = temp 1
--    where temp x = x : (x+1)
nats = 1 : [ x + 1 | x <-nats]

fib :: [Integer]
fib = 1 : 1 : addpairs fib (tail fib)

list2cons :: [a] -> Cons a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs) 

cons2list :: Cons a -> [a]
cons2list Nil = []
cons2list (Cons a as) = a : cons2list (as)

eval :: Exp -> Int
eval (IntExp a) = a
eval (PlusExp [a,b]) = (eval a) + (eval b)
eval (MultExp [a,b]) = (eval a) * (eval b)

{--|
map does the same operation on the entire list. takes two parameters:
(function) and (list you want to operate on)
--}
inclist' :: (Num a) => [a] -> [a]
inclist' xx = map(\x -> (x+1)) xx

{--|
foldr takes two parameters (the function you want to apply recursively),
the starting value (accumulator) and the (list you want to operate on)
--}
sumlist' :: (Num t) => [t] -> t
sumlist' xx = foldr(+) 0 xx

list2cons' :: [a] -> Cons a
list2cons' xx = foldr(Cons) Nil xx
