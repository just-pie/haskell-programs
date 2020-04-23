-- 1. tupleList
-- Vytvorí zo zoznamu celých čísiel zoznam dvojíc (číslo, parita čísla).
-- tupleList :: Integral a => [a] -> [(a,Bool)]
tupleList [] = []
tupleList (x:xs) | even x == True = (x, True):(tupleList xs)
                 | otherwise = (x, False):(tupleList xs)
-- tupleList(x:xs) = (x, even x):(tupleList xs) -- Da sa aj takto jednoducho
tupleListMap xs = map (\x -> (x, even x)) xs

-- ======================================================================================
-- 2. Mucha
-- Funkcia mucha :: Char -> String vo vete "Sedi mucha na stene" nahradí všetky samohlásky zadanou samohláskou.
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

-- ======================================================================================
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

-- ======================================================================================
-- 4. Funkcia prvok
-- prvok :: Eq a => a -> [a] -> Bool
-- Zistí, či sa daný prvok nachádza v zozname.
prvok _ [] = False
prvok x (y:ys) | x == y = True
               | otherwise = prvok x(ys)
               
prvokFL a xs = foldr (\acc x -> if x == a then True else acc) False xs
prvokFR a xs = foldr (\x acc -> if x == a then True else acc) False xs
-- cez filter Any - vracia vzdy True alebo False, nevytvara zoznamy!!!
prvokAny a xs = any (==a) xs

-- =======================================================================================
-- 5. Funkcia doubleList
-- doubleList :: Eq a => [a] -> [a] 
-- Zdvojnásobí výskyt každého prvku v zozname.
doubleListFR xs = foldr (\x acc -> x:x:acc) [] xs
-- pri foldingu nalavo musime zretazit zoznam so zoznamom a tak pridam x na koniec zoznamu
doubleListFL xs = foldl(\acc x -> acc ++ [x,x]) [] xs
-- variacia pre scanr a scanl
doubleListSCR xs = scanr (\x acc -> x:x:acc) [] xs
doubleListSCL xs = scanl(\acc x -> acc ++ [x,x]) [] xs

-- =======================================================================================
-- 6. Funkcia zaporne
-- zaporne :: (Num a, Ord a) => [a] -> [a]
-- Vráti podzoznam obsahujúci záporné čísla z daného zoznamu čísiel.
zaporne xs = foldr (\x acc -> if x < 0 then x:acc else acc) [] xs
zaporne2 xs = filter (<0) xs -- to iste, len cez filter

-- =======================================================================================
-- 7. Funkcia pocetZap
-- pocetZap :: (Num a, Ord b, Num b) => [b] -> a 
-- Zistí počet záporných čísiel v zozname.
-- Potrebujem to cislo ktore mam v akumulatore ulozene zvysit o 1
pocetZap xs = foldl (\acc x -> if x < 0 then acc+1 else acc) 0 xs
-- cez filtrovaciu funkciu
pocetZap2 xs = length(filter (<0) xs)

-- =======================================================================================
-- 8. Funkcia binToDec
-- binToDec :: Num a => [Char] -> a 
-- Prevedie binárne číslo zadané ako String na desiatkové číslo.
binToDecFL xs = foldl (\acc x -> if x=='0' then acc*2 else acc*2+1) 0 xs
-- pre scanl
binToDecSCL xs = scanl (\acc x -> if x=='0' then acc*2 else acc*2+1) 0 xs

-- =======================================================================================
-- 9. Funkcia hexToDec
-- hexToDec:: Num a => [Char] -> a 
-- Prevedie hexadecimálne číslo zadané ako String na desiatkové číslo.
hexToDec xs = foldl (\acc x -> acc*16 + digitToInt x ) 0 xs
  where digitToInt d | d == '0' = 0
                     | d == '1' = 1
                     | d == '2' = 2
                     | d == '3' = 3
                     | d == '4' = 4
                     | d == '5' = 5
                     | d == '6' = 6
                     | d == '7' = 7
                     | d == '8' = 8
                     | d == '9' = 9
                     | d == 'A' = 10
                     | d == 'B' = 11
                     | d == 'C' = 12
                     | d == 'D' = 13
                     | d == 'E' = 14
                     | d == 'F' = 15


main = do
  print(tupleList[1,3,2,-1,7,0])        -- [(1,False),(3,False),(2,True),(-1,False),(7,False),(0,True)]
  print(tupleListMap[1,3,2,-1,7,0])     -- [(1,False),(3,False),(2,True),(-1,False),(7,False),(0,True)][(1,False),(3,False),(2,True),(-1,False),(7,False),(0,True)]
  print(miniFL [1,2,3,6,-5])            -- -5 
  print(miniSCL([1,2,3,6,-5]))          -- [1,1,1,1,-5]
  print(miniFR([1,2,3,6,-5]))           -- -5 
  print(miniSCR([1,2,3,6,-5]))          -- [-5,-5,-5,-5,1]
  print(prvok 5 [1,54,8,45,6])          -- False
  print(prvokFR 5 [1,54,8,5,6])         -- True
  print(prvokFR 5 [1,54,8,45,6])        -- False
  print(doubleList [1,54,8,5,6])        -- [1,1,54,54,8,8,5,5,6,6]
  print(zaporne [1,2,3,-4,-5])          -- [-4,-5]
  print(zaporne2 [1,2,3,-4,-5])         -- [-4,-5]
  print(pocetZap [1,2,3,-4,-5])         -- 2
  print(binToDecFL "10")                -- 2
  print(binToDecSCL "10")               -- [0,1,2]
  print(hexToDec("1A"))                 -- 26
