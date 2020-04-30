-- 1. Naprogramujte funkciu map pomocou funkcií foldr a foldl. Funkcie nazvite napr. mapr, mapl. Vyskúšajte mapovanie pre nekonečný zoznam:
-- take 3 (mapr (+1) [1..])
mapr f xs = foldr (\x acc -> (f x):acc) [] xs
-- take 3 (mapl (+1) [1..])
mapl f (xs)   = foldl (\acc x -> acc++[(f x)]) [] xs

------------------------------------------------------------------
-- 2. Naprogramujte funkciu filter pomocou funkcií foldr a foldl. Funkcie nazvite napr. filterr, filterl. Vyskúšajte filtrovanie pre nekonečný zoznam:
-- head (filterr (>5) [1..])
filterr f xs = foldr (\ x acc -> if f x then x:acc else acc) [] xs
-- head (filterl (>5) [1..])
filterl f xs = foldl (\acc x -> if f x then acc ++ [x] else acc) [] xs

------------------------------------------------------------------
-- 3. Naprogramujte funkciu reverse pomocou funkcií foldr a foldl. Funkcie nazvite napr. revr, revl.
revr xs = foldr (\ x acc -> acc ++ [x]) [] xs
revl xs = foldl (\ acc x -> [x] ++ acc) [] xs

------------------------------------------------------------------
-- 4. Naprogramujte funkciu sucet :: Num a => [a] -> a na výpočet súčtu prvkov v zozname pomocou rekurzie a pattern matchingu. Upravte ju na funkciu s chvostovou rekurziou.
sucet [] = 0
sucet [x] = x
sucet (x:xs) = x + sucet(xs)

-- How does recursion works?
{-
sucet [1,2,3] = 
1+ sucet [2,3] =
1 + (2 + sucet [3]) =
1 + (2 + (3 + sucet[])) = 
1 + (2 + (3 + 0)) =
1 + (2 + 3) =
1 + 5 =
6
-}

-- Using Tail recursion
-- sumTR [1,2,3] 0
sumTR [] acc = acc
sumTR (x:xs) acc = sumTR xs x+acc

main = do
  print(mapr (+1) [1,2,3])          -- [2,3,4]
  print(mapl (+1) [1,2,3])          -- [2,3,4]
  print(take 3 (mapr (+1) [1..]))   -- prints first 3 numbers of infinite list
  print([1..1000])                  -- prints numbers from 1 to 1000
  print(filterr (>5) [1..10])       -- [6,7,8,9,10]
  print(filterl (>5) [1..10])       -- [6,7,8,9,10]
  print(head (filterr (>5) [1..]))  -- 6
  print(head (filterl (>5) [1..]))  -- 6
  print(revr [1..10])               -- [10,9,8,7,6,5,4,3,2,1]
  print(revl [1..10])               -- [10,9,8,7,6,5,4,3,2,1]
  print(sucet [1,2,3])              -- 6
  print(sumTR [1..3] 0)             -- 6 (Tail recursion)
