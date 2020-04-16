-- 1. tupleList
-- tupleList :: Integral a => [a] -> [(a,Bool)]
tupleList [] = []
tupleList (x:xs) | even x == True = (x, True):(tupleList xs)
                 | otherwise = (x, False):(tupleList xs)
-- tupleList(x:xs) = (x, even x):(tupleList xs) -- Da sa aj takto jednoducho
tupleListMap xs = map (\x -> (x, even x)) xs

-- ===================================================================
-- 2. Mucha
-- mucha :: Char -> String
-- mucha c = map (\x -> if any (==x) ['a','e','i','o','u','y'] then c else x) "Sedi mucha na stene"
-- Tu zistujem, ci je na vstupe samohlaska a ak nie, tak vypise error 
mucha c = if any (==c) ['a','e','i','o','u','y']
          then map (\x -> if any (==x) ['a','e','i','o','u','y'] 
                          then c else x) "Sedi mucha na stene"
          else error "Zadany znak nie je samohlaska"
{- Alebo aj takto sa to da
mucha c = if elem c "aeiouy"
          then map (\x -> if any (==x) ['a','e','i','o','u','y'] 
                          then c else x) "Sedi mucha na stene"
          else error "Zadany znak nie je samohlaska"
-}
-- ===================================================================
-- 3. mini :: Ord a => [a] -> a
-- Nájde minimum v zadanom zozname prvkov.
-- acc = akumulator na docasny vysledok
--je dany prvok mensi ako docasny vysledok?
miniFL (x:xs) = foldl (\acc x -> if x < acc then x else acc ) x xs

-- scanl aj vypise
miniSCL (x:xs) = scanl (\acc x -> if x < acc then x else acc) x xs

-- pre folding sprava
-- akumulator je napravo
miniFR (x:xs) = foldr (\x acc -> if x < acc then x else acc) x xs

-- scanr pre vypis sprava
miniSCR (x:xs) = scanr (\x acc -> if x < acc then x else acc) x xs

-- ===================================================================
-- 4. Funkcia prvok :: Eq a => a -> [a] -> Bool zistí, či sa daný prvok nachádza v zozname.
prvok _ [] = False
prvok x (y:ys) | x == y = True
               | otherwise = prvok x(ys)
               
prvokFL a xs = foldr (\acc x -> if x == a then True else acc) False xs
prvokFR a xs = foldr (\x acc -> if x == a then True else acc) False xs
-- cez filter Any
prvokAny a xs = any (==a) xs

main = do
  print(tupleList[1,3,2,-1,7,0])
  print(tupleListMap[1,3,2,-1,7,0])
  print(miniFL [1,2,3,6,-5])
  print(miniSCL([1,2,3,6,-5]))
  print(miniFR([1,2,3,6,-5]))
  print(miniSCR([1,2,3,6,-5]))
  print(prvok 5 [1,54,8,45,6])
  print(prvokFR 5 [1,54,8,5,6])
  print(prvokFR 5 [1,54,8,45,6])
