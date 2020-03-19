-- ntice
{- porovnaj_cas (h1,m1,s1,h2,m2,s2) | h1 == h2 && m1 == m2 && s1 == s2 = "Su rovnake"
-}

-- Zoznamy
-- 1.
kvRovnica :: (Float, Float, Float) -> (Float, Float)
kvRovnica (a, b, c) = (x1, x2)
                where
			                 x1 = e + sqrt d / (2 * a)
			                 x2 = e - sqrt d / (2 * a)
			                 d = b * b - 4 * a * c
			                 e = - b / (2 * a)

-- 2.
maxi :: Ord a => [a] -> a
maxi [x] = x
maxi (x:y:ys) | x <= y = maxi(y:ys)
                 | otherwise = maxi (x:ys)
-- 3.
prvok :: Eq a => a -> [a] -> Bool
prvok _ [] = False
prvok x (y:ys) | x == y = True
              | otherwise = prvok x ys

-- 4.
usporiadany :: Ord a => [a] -> Bool
usporiadany [] = True
usporiadany [x] = True
usporiadany (x:y:xs) = x <= y && usporiadany (y:xs)  


-- 5.
listToSet :: Eq a => [a] -> [a]

-- 6.
insert :: Int -> [Int] -> [Int]
insert x [] = [x] 
insert x (y:ys) = if x <= y
                  then x:y:ys
                  else y : insert x ys

-- 7.
bezprvku _ [] = [] 
bezprvku a (b:bc) | a == b    = bc 
                     | otherwise = b : bezprvku a bc

-- 8.
zluc :: Ord a => [a] -> [a] -> [a]
zluc [] [] = []
zluc (h:prvy) (c:druhy)  | h <= c = h:merge prvy (c:druhy)
                | h > c = c:zluc (h:prvy) druhy

-- 9.
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort [x] = [x]
insertSort (x:xs) = insert $ insertSort xs

-- 10.
import Data.List (minimum, delete)

MinSort :: Ord t => [t] -> [t]
MinSort [] = []
MinSort xs = let { x = minimum xs } 
           in  x : minSort (delete x xs)

-- 11.
mergeSort :: Ord a => [a] -> [a]
mergeSort ls = merges $ map (\x -> [x]) ls