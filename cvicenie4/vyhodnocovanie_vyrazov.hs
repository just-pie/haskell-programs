-- 1. Naprogramujte funkciu map pomocou funkcií foldr a foldl. Funkcie nazvite napr. mapr, mapl. Vyskúšajte mapovanie pre nekonečný zoznam:
-- take 3 (mapr (+1) [1..])
mapr f xs = foldr (\x acc -> (f x):acc) [] xs
-- take 3 (mapl (+1) [1..])
mapl f (xs)   = foldl (\acc x -> acc++[(f x)]) [] xs

main = do
  print(mapr (+1) [1,2,3])          -- [2,3,4]
  print(mapl (+1) [1,2,3])          -- [2,3,4]
  print(take 3 (mapr (+1) [1..]))   -- prints first 3 numbers of infinite list
  print([1..1000])                  -- prints numbers from 1 to 1000
