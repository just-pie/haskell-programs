{--
Štandardné funkcie na prácu so zoznamami:
dlzka :: [a] -> Int length
prvy :: [a] -> a head
bezpr :: [a] -> [a] tail
posl :: [a] -> a last
bezposl :: [a] -> [a] init
prvych :: Int -> [a] -> [a] take
bezprvych :: Int -> [a] -> [a] drop
prvok :: a -> [a] -> Bool elem
sucet :: Num a => [a] -> a sum
obrat :: [a] -> [a] reverse
maxi :: Ord a => [a] -> a maximum

el = [] prázdny zoznam
l = [1..] nekonečný zoznam

Konštruktory (:), [], ++
1:[] = [1]
1:2:[] = [1,2]
1:[2,3] = [1,2,3]
[1,2] ++ [3] = [1,2,3]

Selektory
head [1,2,3] = 1 init [1,2,3] = [1,2]
tail [1,2,3] = [2,3] last [1,2,3] = 3
[1,2,3]!!0 = 1 [1,2,3]!!1 = 2 [1,2,3]!!2 = 3
take 2 [1,2,3,4,5] = [1,2]
drop 2 [1,2,3,4,5] = [3,4,5]

_ je Wildcard - unifikuje sa s ľubovoľnou hodnotou daného typu

[] – prázdny zoznam
(x:xs) – neprázdny zoznam
(x:y:ys) – zoznam dĺžky aspoň 2

Aliasing
z@(x:xs) – vzor pre celok (z) aj jeho časti (x:xs)

Zip transformuje dva zoznamy na zoznam dvojíc
zip :: [a] -> [b] -> (a,b)
zip [1,2,3] [4,5,6] => [(1,4),(2,5),(3,6)]

Unzip transformuje zoznam dvojíc do dvojice zoznamov
(výsledok funkcie nemôžu byť dva zoznamy)
unzip :: [(a,b)] -> ([a],[b])
unzip [(1,4),(2,5),(3,6)] => ([1,2,3],[4,5,6])
--}

--------------------------------------------------------------------------
--------------------------     cvicenie1.hs    ---------------------------
--------------------------------------------------------------------------
-- wildcard vo funkcii, ktorá vráti prvý prvok zoznamu
head (x:_) = x
-- aliasing vo funkcii, ktorá zdvojí prvý prvok na začiatku zoznamu
dva_prve z@(x:xs) = x:z
-- List comprehension - gnerovanie zoznamov
xs = concat [[1,2],[3],[4,5,6]] -- [1,2,3,4,5,6]

-- Guearded equations
postupnost d h krok = [d, (d+krok)..h]

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

-- Zoznam susedných dvojíc v zozname
dvojice xs = zip (init xs) (tail xs)
dvojice [1,2,3,4] = [(1,2),(2,3),(3,4)] 

---------------------------------------------------------------------------------------
------------    distancne_zadanie.hs a distancne_zadanie_precvicovanie.hs    ----------
---------------------------------------------------------------------------------------
-- 1. Napíšte funkciu, ktorá porovná dva časy zadané ako trojice hodina, minúta, sekunda a zistí, či prvý čas je skôr ako druhý.
skor (h1,m1,s1) (h2,m2,s2) | h1 < h2 = True
                           | h1 > h2 = False
                           | m1 < m2 = True
                           | m1 > m2 = False
                           | s1 < s2 = True
                           | s1 > s2 = False
                           | otherwise = False
-- 2. Napíšte funkciu, ktorá 24-hodinový čas prevedie na 12-hodinový.
prevod_hodin (h,m,s) | h > 12 = (h-12,m,s," PM")
                     | h == 0 = (h+12,m,s," AM")
                     | h == 12 = (h,m,s," PM")
                     | otherwise = (h,m,s,"AM")

-- 3. Funkcia velV :: Floating a => (a,a) -> a na výpočet veľkosti rovinného vektora.
velV  :: Floating a => (a,a) -> a 
velV (a1,a2) = sqrt (a1^2 + a2^2) 

-- 4. Definujte funkciu na výpočet vzdialenosti dvoch bodov v rovine.
vzdialenost :: Floating a => (a,a) -> (a,a) -> a
vzdialenost a1 a2 b1 b2 = absolutna (sqrt (b1-a1)^2 + (b2-a2)^2)

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

-- 3. Funkcia prvok :: Eq a => a -> [a] -> Bool zistí, či zadaná hodnota je prvkom zadaného zoznamu. Elem, elem
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


-- DALSIE FUNKCIE VYSSIEHO RADU
-- Zdvojnasobi cisla v zozname
doubleList xs = map (\x -> 2 * x) xs
-- print(doubleList [1,2,3]) -- [2,4,6]

-- funkcia na spojenie dvoch zoznamov pomocou foldr
xs ++ ys = foldr (:) ys xs
-- [1,2,3] ++ [4,5]  -- [1,2,3,4,5]

-- Funkcia OR pomocou foldr
or xs = foldr (||) False xs
-- print(or [False, False, True])  -- TRUE

-- Zistenie dlzky zoznamu pomocou foldr
length xs = foldr (\x y -> 1 + y) 0 xs

-- Hornerova schema
horner xs = foldl (\x y -> x*10 + y) 0 xs

-- FILTROVANIE
-- definovanie pomocou list comprehension
filter p xs = [x | x <- xs, p x]
--definovanie pomocou pattern matching a rekurzie
filter _ [] = []
filter p (x:xs) | p x = x : filter p xs
                | otherwise = filter p xs

-- Vrati prvky zo zoznamu, pre ktore plati podmienka
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x : (takeWhile p xs)
                   | otherwise = []
                   
-- Odstrani prvky zo zoznamu, pre ktore plati podmienka
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p z@(x:xs) | p x = dropWhile p xs
                     | otherwise = z

-- Existuje prvok v zozname, pre ktorá platí podmienka?
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) | p x = True
             | otherwise = any p xs
-- print(any (== 5) [1,2,3,4])  -- False

--Platí podmienka pre všetky prvky v zozname?
all :: (a -> Bool) -> [a] -> Bool
all _ [] = False
all p (x:xs) | p x = all p xs
             | otherwise = False
-- print(all (< 5) [1,2,3,4])  -- True

-----------------------------------------------------------------------------------------------
-----------------------------    vyhodnocovanie_vyrazov.hs    ---------------------------------
-----------------------------------------------------------------------------------------------

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

------------------------------------------------------------------
-- 5. Naprogramujte funkciu pocet :: Num a => [a] -> Int na výpočet počtu prvkov v zozname pomocou rekurzie a pattern matchingu. Upravte ju na funkciu s chvostovou rekurziou.
pocet [] = 0
pocet (x:xs) = 1 + pocet xs

-- Using Tail recursion
pocetTR [] acc = acc
pocetTR (x:xs) acc = pocetTR xs acc + 1
-- vramci parametrov zadam funkciu

-- How does it work?
{-
pocet [1,2,3] = 1 + pocet [2,3] =
              = 1 + (1 + pocet [3]) =
              = 1 + (1 + (1 + pocet [])) = 
              = 1 + (1 + (1 + 0)) =
              = 1 + (1 + 1) = 
              = 1 + 2 = 
              = 3
-}

{-
pocetTR [1,2,3] 0 = pocetTR [2,3] 1 =
                  = pocetTR [3] 2 =
                  = pocetTR [] 2+1 = 
                  = 3
-}

------------------------------------------------------------------
-- 6. Naprogramujte funkciu sucpoc :: Num a => [a] -> (a,Int) -> (a,Int), ktorej výsledkom bude dvojica súčet a počet prvkov zoznamu. Funkcia bude chvostovo rekurzívna s parametrami zoznam a dvojica s akumulovaným výsledkom.
-- sucpoc [1,2,3] (0,0) = (6,3)

sucpos [] (accS, accP) = (accS, accP)
sucpos (x:xs) (accS, accP) = sucpos xs (accS+x, accP+1)
sucpoc2 xs = sucpos xs (0,0) -- toto nam zjednodusuje print, nemusim pisat este dve nuly do printu

------------------------------------------------------------------
-- 7. Pomocou funkcie sucpoc vypočítajte aritmetický priemer prvkov zoznamu.
aritPriemer[] = 0
aritPriemer (x:xs) = fst (sucpoc2 xs) / snd (sucpoc2 xs)


------------------------------------------------------------------
-- 8. Napíšte funkciu s chvostovou rekurziou na výpočet faktoriálu.

faktorial 0 = 1
faktorial x = x * faktorial (x-1) 

factorialTR 0 acc = acc
factorialTR x acc = factorialTR (x−1) (x*acc)

------------------------------------------------------------------
-- 9. Napíšte funkciu s chvostovou rekurziou na výpočet Fibonacciho čísla.
fibTR 0 acc1 _ = acc1 -- nezaujima nas acc2 takm pouzijeme _ - wilcart
fibTR 1 _ acc2 = acc2
fibTR x acc1 acc2 = fibTR (x-1) acc2 (acc1+acc2)
fibTR2 x = fibTR x 0 1


-- DALSIE ZADANIA Z PREZENTACIE
fib a b = a : (fib b (a+b))

-- Funkcia iterate :: (a -> a) -> a -> [a] vráti nekonečný zoznam opakovaných aplikácií funkcie f na parameter x [x, f x, f (f x), ...]
iterate f x = x : (iterate f (f x))

-- UROKY
-- Hodnota vkladu 1000€ úročeného raz ročne 3,5% po dobu 3 roky:
iterate (*1.035) 1000 !! 3

-- Zostatok dlžnej sumy z pôžičky 1000€ pri mesačnej splátke 20€ a ročnom úroku 10% po 3 rokoch:
iterate (\x->(x-240)*1.10) 1000 !! 3

-- Funkcia zip :: [a] -> [b] -> [(a,b)] vezme dva zoznamy a vráti zoznam dvojíc korešpondujúcich prvkov. Ak je niektorý zo vstupných zoznamov kratší, zvyšok druhého zoznamu sa ignoruje.
zip (a:as) (b:bs) = (a,b):(zip as bs)
zip _ _ = []

-- Funkcia zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] vezme binárnu funkciu a dva zoznamy a vráti zoznam výsledkov binárnej funkcie na príslušných prvkoch zoznamov.
zipWith f (a:as) (b:bs) = (f a b) : (zipWith f as bs)
zipWith _ _ _ = []

-- Fibonacci s funkciou zipWith
fibs = 1 : 1 : (zipWith (+) fibs (tail fibs))
-- prvých 10:
take 10 fibs
-- desiate:
fibs !! 10
-- najmenšie štvorciferné:
head (dropWhile (< 1000) fibs))
-- najväčšie štvorciferné:
last (takeWhile (< 10000) fibs)

-- Pascalov trojuholnik
pascal = iterate riadok [1] where riadok r = zipWith (+) (r++[0]) ([0]++r)  -- POZOR - bude sa to robit donekonecna!

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
-------------------------      Ulohy na precvicenie      --------------------
-----------------------------------------------------------------------------
-- 1. Funkcia fakt :: Num a => a -> a vypočíta pre zadané číslo jeho faktoriál. 
fakt x | x < 2 = 1
       | otherwise = x * fakt(x-1)

-- 2. Funkcia mocnina :: Num a => a -> a -> a vypočíta pre zadaný základ z a exponent e mocninu ze. 
mocnina a n | n == 0 = 1
            | otherwise = a ^ n 

-- 3. Funkcia nsd :: (Ord a, Num a) => a -> a -> a, ktorá vráti najväčší spoločný deliteľ dvoch zadaných čísiel.
-- nsd 34 18 = 2
nsd a b = gcd a b   
-- euklidovym algoritmom
myGCD :: Integer -> Integer -> Integer
myGCD a b
      | b == 0     = abs a
      | otherwise  = myGCD b (a `mod` b)

-- 4. Funkcia sudelitelne :: (Ord a, Num a) => a -> a -> Bool, ktorá zistí, či zadané dve čísla sú súdeliteľné, t.j. či majú spoločného deliteľa rôzneho od 1.
-- sudelitelne 34 18 = True
sudelitelne a b | nsd a b <= 1 = False
                | otherwise = True


-- 5. Funkcia prvocislo :: Num a => a -> Bool, ktorá zistí, či zadané číslo je prvočíslo.
celociselnedelitele n = [ x | x <- [1..n], n `mod`x ==0 ]
prvocislo n | celociselnedelitele n == [1,n] = True
            | otherwise = False

--  6. Funkcia skor :: (a,b,c) -> (d,e,f) -> Bool, ktorá porovná dva časy a zistí, či prvý je skôr ako druhý. 
skor (h,m,s) (h2,m2,s2) | h > h2 =True
                        | h==h2 && m >= m2 && s>=s2 = True
                        | otherwise = False
                        
-- 7. Funkcia cas12 :: (a,b,c) -> (a,b,c,[Char]), ktorá prevedie 24-hodinový čas na 12-hodinový.  
cas12 (h,m,s)  | h > 12 = (h-12,m,s)
               | otherwise = (h,m,s)

--  8. Funkcia velV :: Floating a => (a,a) -> a na výpočet veľkosti rovinného vektora. 
--absolutnah x | x < 0 = x * (-1)
--             | otherwise = x
velV(a1,a2)=sqrt(a1^2+a^2)

--9. Funkcia vzdialenost :: Floating a => (a,a) -> (a,a) -> a na výpočet vzdialenosti dvoch bodov v rovine. 
absolutnah x | x < 0 = x * (-1)
             | otherwise = x
vzdialenost a1 a2 b1 b2 = absolutnah ( sqrt( (b1-a1)^2 + (b2-a2)^2 ) )


--10. Funkcia korene :: Floating a => (a,a,a) -> [a] na výpočet koreňov kvadratickej funkcie. 
korene a b c = if d < 0  
                  then error "0"
               else (x,y) where
                  x = e + sqrt d / (2 * a)
                  y = e - sqrt d / (2 * a)
                  d = b * b - 4 * a * c
                  e = - b / (2 * a)

--12. Funkcia usporiadany :: Ord a => [a] -> Bool, ktorá zistí, či je zadaný zoznam usporiadaný vzostupne
usporiadany [] = True
usporiadany [x] = True
usporiadany (x:xs) | head (x:xs) < (x:xs)!!2 = True
                   | otherwise = False

--13. Funkcia delitele :: Int -> [Int], ktorá vráti zoznam vlastných deliteľov zadaného čísla. 
delitele n = [x | x <- [1..n-1], n `mod` x ==0 ]

--14. Funkcia pocet_zap :: [Int] -> Int, ktorá vráti počet záporných čísiel v danom zozname. 
pocet_zap [x] = if x < 0 then 1 else 0
pocet_zap (x:xs) = length [z | z <-(x:xs), z < 0]

-- 15. Funkcia isSet :: Eq a => [a] -> Bool, ktorá zistí, či zadaný zoznam neobsahuje opakujúce sa prvky. 
isSet []     = True
isSet (x:xs) = x `notElem` xs && isSet xs

-- 16. Funkcia listToSet :: Eq a => [a] -> [a], ktorá zadaný zoznam prevedie na zoznam s neopakujúcimi sa prvkami.
listToSet :: Eq a => [a] -> [a]
listToSet [] = []
listToSet (x:xs) = x : listToSet (filter (/=x) xs)

-- 17. Funkcia digitToInt :: Char -> Int, ktorá zadaný znak (číslicu) z intervalu ’0’..’9’, ’A’..’F’ prevedie na číslo 0..15, výsledkom pre iný znak bude chyba.
digitToInt x | x == '0' = 0
             | x == '1' = 1
             | x == '2' = 2
             | x == '3' = 3
             | x == '4' = 4
             | x == '5' = 5
             | x == '6' = 6
             | x == '7' = 7
             | x == '8' = 8
             | x == '9' = 9
             | x == 'A' = 10
             | x == 'B' = 11
             | x == 'C' = 12
             | x == 'D' = 13
             | x == 'E' = 14
             | x == 'F' = 15
             | otherwise = error "chyba"

--18. Funkcia binToDec :: [Char] -> Int, ktorá zadaný reťazec núl a jednotiek (binárne číslo) prevedie na desiatkové číslo.
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

-- 19. Funkcia hexToDec :: [Char] -> Int, ktorá zadaný reťazec (hexadecimálne číslo) prevedie na desiatkové číslo
hexChar :: Char -> Integer
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise = 0

parseHex :: String -> Integer
parseHex hxStr | length hxStr /= 0 = (hexChar(last(hxStr)))+(16*parseHex(init(hxStr)))
               | otherwise = 0 

-- 20. Funkcia decToBin :: Integral a => a -> [Char], ktorá zadané celé nezáporné číslo v desiatkovej sústave prevedie na binárne číslo (reťazec núl a jednotiek).  

decToBin 0 = "0"
decToBin n | n `mod` 2 == 1 = decToBin (n `div` 2) ++ "1"
           | n `mod` 2 == 0 = decToBin (n `div` 2) ++ "0"

--21. Funkcia copy :: [Char] -> Int -> Int -> [Char] má tri parametre: reťazec r, index i, číslo d, vráti časť reťazca r od indexu i dĺžky d. 

--22. Funkcia delete :: [Char] -> Int -> Int -> [Char] má tri parametre: reťazec r, index i, číslo d, vymaže z reťazca r časť od indexu i dĺžky d. 

--23. Funkcia group :: [a] -> [[a]] , ktorá v zadanom zozname zoskupí do podzoznamov po sebe nasledujúce rovnaké prvky. 
group :: (Eq a) => [a] ->  [[a]]
group [] = []
group (x:xs) = first : group second
  where y = span (== x) $ x:xs
        first = fst $ y
        second = snd $ y

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- OSTATNE
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

-- alebo -> efektivnejsie
compress :: Eq a => [a] -> [a]
compress = map head . group   

-- vypise permutacie False a True v zozname
-- Priklad: print(bools 2)  -> [[False,False],[False,True],[True,False],[True,True]]
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) acc ++ map (True:) acc
          where acc = bools (n-1)          

-- Vypise pocet parnych cisel, vypise pocet neparnych cisel
parne :: [Int] -> Int
parne xs = length [ x | x <- xs, even x ]

neparne :: [Int] -> Int
neparne xs = length [ x | x <- xs, odd x ]

-- Vypise list parnych cisel, vypise list neparnych cisel
vypis_parne :: [Int] -> [Int]
vypis_parne xs = [ x | x <- xs, odd x ]

vypis_neparne :: [Int] -> [Int]
vypis_neparne xs = [ x | x <- xs, odd x ]

-- Eulerova funkcia - pocet nesudelitelnych cisel
eulerFunkcia n = length [x | x <- [1..n], gcd n x == 1]

-- Ci su dve cisla delitelne
divisible x y | x/y == 0 = True
              | otherwise = False
              
-- Zisti, ci ej zadane cislo Palindrom
isPalindromeHelper :: [Integer] -> [Integer] -> Bool
isPalindromeHelper (x:xs) (y:ys)
  | x == y && xs == [] && ys == [] = True
  | x == y    = isPalindromeHelper xs ys
  | x /= y    = False
  | otherwise = False

isPalindrome :: [Integer] -> Bool
isPalindrome ls = isPalindromeHelper ls reversed
  where reversed = reverse ls
  
-- Najde i-ty element listu
nthElement :: Int -> [Int] -> Int
nthElement x xs
  | x > (length xs) = 0
  | x == 0          = head xs
  | otherwise       = nthElement (x - 1) (tail xs)
  
 -- vrati prvy prvok zo zoznamu
prvy :: [Integer] -> Integer
prvy (x : []) = x
prvy (x : xs) = x

-- vrati posledny prvok zo zoznamu
posledny :: [Integer] -> Integer
posledny (x : []) = x
posledny (x : xs) = lastElem xs

-- Vlozi cislo do zoznamu an urcitu poziciu
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys     1 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)
-- alebo
insertAtX x xs n = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- Sucet prvkov v zozname
sucet xs = sum xs 

-- Duplikuje kazdy prvok v zozname dvakrat
dupli xs = concat [[x,x] | x <- xs]

-- Duplikuje kazdy prvok v zozname n-krat
repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n

-- Vymaze kazdy n-ty prvok zo zoznamu
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [ i | (i,c) <- ( zip xs [1,2..]), (mod c n) /= 0]

-- Rozdeli zoznam na 2 zoznamy od urciteho prvku
split xs n = (take n xs, drop n xs)

-- Faktorial
faktorial n = if n == 0 then 1 else n * faktorial (n-1)


