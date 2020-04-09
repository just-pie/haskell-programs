-- 1. porovnanie casov
skor (h1,m1,s1) (h2,m2,s2) | h1 < h2 = True
                           | h1 > h2 = False
                           | m1 < m2 = True
                           | m1 > m2 = False
                           | s1 < s2 = True
                           | s1 > s2 = False
                           | otherwise = False
-- 2. 24h na 12h
prevod_hodin (h,m,s) | h > 12 = (h-12,m,s," PM")
                     | h == 0 = (h+12,m,s," AM")
                     | h == 12 = (h,m,s," PM")
                     | otherwise = (h,m,s,"AM")

-- 1. kvadraticka rovnica
kvRovnica (a,b,c) | d < 0 = [] -- nema riesenie
                  | d == 0 = [-b/(2*a)] -- jeden koren 
                  | otherwise = [(-b + sqrt d)/(2*a), (-b - sqrt d)/(2*a)] -- dve riesenia
  where d = b*b - 4*a*c           

-- 5. list to set
prvok _ [] = False
prvok x (y:ys) | x == y = True
               | otherwise = prvok x ys

list_to_set [] = []
list_to_set [x] = [x]
list_to_set (x:xs) | prvok x xs = list_to_set(xs)
                   | otherwise = x:(list_to_set(xs))

listtoset [] = []
listtoset (x:xs) = x:listtoset (filter ((/=) x) xs)

-- 10. minSort
mini [x] = x
mini (x:y:ys) | x <= y = mini (x:ys)
              | otherwise = mini (y:ys)

bezPrvku _ [] = [] -- _ je akykolvek prvok
bezPrvku x (y:ys) | x == y = (ys)
                  | otherwise = y : (bezPrvku x ys)

minSort [] = []
minSort [x] = [x]
minSort xs = m : (minSort (bezPrvku  m xs))
  where m = mini xs

main = do
  print("Haskell")
  print(minSort[1,3,-1,7,0])
