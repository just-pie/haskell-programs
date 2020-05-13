-- cvicenie1.hs

absolutna x = if x < 0 then x*(-1) else x

absolutnaGE x | x < 0 = x*(-1)
              | otherwise = x

znamienko x = if x < 0 then -1 else if x == 0 then 0 else 1

znamienkoGE x | x < 0 = -1
              | x == 0 = 0
              | otherwise = 1

mocnina a n = if n == 0 then 1 else a * mocnina a (n-1)

mocninaGE a n | n == 0 = 1
              | a == 0 = 1      
              | n < 0 = a * mocninaGE a (absolutnaGE n)
              | otherwise = a * mocninaGE a (n-1)

mocninaLogGE a n | n == 0 = 1
                 | n `mod` 2 == 1 = a * mocninaLogGE a (n-1)
                 | otherwise = mocninaLogGE (a*a) (div n 2)

urocenaSuma suma urok roky | roky == 0 = suma
                           | urok == 0 = suma
                           | suma == 0 = 0
                           | otherwise = urocenaSuma (suma+((suma/100)*urok)) urok (roky-1)

---------------------------------------------------------------------------------------

-- distancne_zadanie.hs a distancne_zadanie_precvicovanie.hs
-- 1. Napíšte funkciu, ktorá porovná dva časy zadané ako trojice hodina, minúta, sekunda a zistí, či prvý čas je skôr ako druhý.
skor (h1,m1,s1) (h2,m2,s2) | h1 < h2 = True
                           | h1 > h2 = False
                           | m1 < m2 = True
                           | m1 > m2 = False
                           | s1 < s2 = True
                           | s1 > s2 = False
                           | otherwise = False
-- 2. apíšte funkciu, ktorá 24-hodinový čas prevedie na 12-hodinový.
prevod_hodin (h,m,s) | h > 12 = (h-12,m,s," PM")
                     | h == 0 = (h+12,m,s," AM")
                     | h == 12 = (h,m,s," PM")
                     | otherwise = (h,m,s,"AM")

---------------------------------------------------------------------------------------

-- 1. Napíšte funkciu kvRovnica :: (Ord a, Floating a) => (a,a,a) -> [a], ktorá nájde riešenie kvadratickej rovnice ax2+bx+c=0 danej parametrami a,b,c.
kvRovnica (a,b,c) | d < 0 = [] -- nema riesenie
                  | d == 0 = [-b/(2*a)] -- jeden koren 
                  | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)] -- dve riesenia
  where d = b*b - 4*a*c           

-- 2. Funkcia maxi :: Ord a => [a] -> a nájde maximum zo zadaných prvkov zoznamu.
maxi :: Ord a => [a] -> a
maxi [x] = x
maxi (x:y:ys) | x <= y = maxi(y:ys)
                 | otherwise = maxi (x:ys)

-- 3. Funkcia prvok :: Eq a => a -> [a] -> Bool zistí, či zadaná hodnota je prvkom zadaného zoznamu.
prvok :: Eq a => a -> [a] -> Bool
prvok _ [] = False
prvok x (y:ys) | x == y = True
               | otherwise = prvok x ys

-- 4. Funkcia usporiadany :: Ord a => [a] -> Bool zistí, či je zadaný zoznam usporiadaný vzostupne (od najmenšieho po najväčší).
usporiadany :: Ord a => [a] -> Bool
usporiadany [] = True
usporiadany [x] = True
usporiadany (x:y:xs) = x <= y && usporiadany (y:xs)  


-- 5. Funkcia listToSet :: Eq a => [a] -> [a] transformuje zoznam na množinu, teda na zoznam bez opakujúcich sa prvkov.
list_to_set [] = []
list_to_set [x] = [x]
list_to_set (x:xs) | prvok x xs = list_to_set(xs)
                   | otherwise = x:(list_to_set(xs))

listtoset [] = []
listtoset (x:xs) = x:listtoset (filter ((/=) x) xs)

-- 6. Funkcia insert :: Ord a => a -> [a] -> [a] vloží zadaný prvok na správne miesto do zadaného usporiadaného zoznamu.
insert :: Int -> [Int] -> [Int]
insert x [] = [x] 
insert x (y:ys) = if x <= y
                  then x:y:ys
                  else y : insert x ys

-- 7. Funkcia bezPrvku :: Eq a => a -> [a] -> [a] odstráni prvý výskyt zadaného prvku zo zadaného zoznamu. Ak sa v zozname nenachádza, neodstráni nič.
bezPrvku _ [] = [] -- _ je akykolvek prvok
bezPrvku x (y:ys) | x == y = (ys)
                  | otherwise = y : (bezPrvku x ys)

-- 8. Funkcia zluc :: Ord a => [a] -> [a] -> [a] zlúči dva zadané usporiadané zoznamy do jedného usporiadaného zoznamu.
zluc :: Ord a => [a] -> [a] -> [a]
zluc (x:xs) (y:ys) = if x < y
                        then x:(zluc xs (y:ys))
                        else y:(zluc (x:xs) ys)
zluc [] xs = xs
zluc xs [] = xs


-- 9. Funkcia insertSort :: Ord a => [a] -> [a] usporiada zadaný zoznam vkladaním.
insertSort :: Ord a => [a]->[a]
insertSort [] = []
insertSort (x:xs) = insert x (isort xs)

insert :: Ord a=> a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
               | x <= y    = x:y:ys
               | otherwise = y: insert x ys

-- 10. minSort
mini [x] = x
mini (x:y:ys) | x <= y = mini (x:ys)
              | otherwise = mini (y:ys)

minSort [] = []
minSort [x] = [x]
minSort xs = m : (minSort (bezPrvku  m xs))
  where m = mini xs

-----------------------------------------------------------------------------------------------
-----------------------------    funkcie_vyssieho_radu.hs     ---------------------------------
-----------------------------------------------------------------------------------------------
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
-- je dany prvok mensi ako docasny vysledok?
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
-- kratsie riesenie
hextodec xs = foldl(\acc x -> if ord x <= 57 then acc*16 + (ord x - 48) else acc*16 + (ord x - 55)) 0 xs

-- =======================================================================================
-- 10. Funkcia unpackList 
-- unpackList :: Num a => [(a,b)] -> [b] 
-- Vytvorí zo zadaného zoznamu dvojíc (počet opakovaní, prvok) zoznam prvkov s daným počtom opakovaní.
-- Napríklad:
-- unpackList [(4,0),(2,1),(3,0)] = [0,0,0,0,1,1,0,0,0]

-- folding left
unpackListL xs = foldl (\acc x -> acc ++ unpackTuple x ) [] xs
  where unpackTuple (p@pocetOpakovani,c) | p <= 0 = []
                                         | otherwise  = unpackTuple (p-1, c) ++ [c]

-- foldind right
-- ( . ) Y ( . )
unpackListR xs = foldr (\x acc ->  (unpackTuple x) ++ acc) [] xs
  where unpackTuple (p@pocetOpakovani,c) | p <= 0 = []
                                         | otherwise  = unpackTuple (p-1, c) ++ [c]





-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- PRIKLADY Z PREZENCTACII
-- Pascalov trojuholnik
pascal 0 = [1]
pascal n = zipWith (+) ([0] ++ pascal (n-1)) (pascal (n-1) ++ [0])

-- Eratostenovo sito
sito (p:xs) = p:sito [n | n <- xs, mod n p /= 0]
sito _ = []


-----------------------------------------------------------------------------
-- spojenie dvoch zoznamov
spoj :: [a]->[a]->[a]
spoj xs ys =concat[xs,ys]

-- reverse zoznamu, obrateny zoznam
reverse :: Ord a => [a]->[a]
reverse [] = []
reverse (x:xs) = rev xs ++[x]

-- vymaze opakujuce sa prvky zoznamu, duplikaty
vymaz_duplikat :: Eq a => [a] -> [a]
vymaz_duplikat []     = []
vymaz_duplikat (x:xs) = x : filter (/= x) (vymaz_duplikat xs)

-- vypise permutacie False a True v zozname
-- Priklad: print(bools 2)  -> [[False,False],[False,True],[True,False],[True,True]]
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) acc ++ map (True:) acc
          where acc = bools (n-1)          
