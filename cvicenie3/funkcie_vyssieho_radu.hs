-- 1. tupleList
-- tupleList :: Integral a => [a] -> [(a,Bool)]
tupleList [] = []
tupleList (x:xs) | even x == True = (x, True):(tupleList xs)
                 | otherwise = (x, False):(tupleList xs)
-- tupleList(x:xs) = (x, even x):(tupleList xs) -- Da sa aj takto jednoducho
tupleListMap xs = map (\x -> (x, even x)) xs

-- 2. Mucha
-- mucha :: Char -> String
mucha c = map (\x -> if any (==x) ['a','e','i','o','u','y'] then c else x) "Sedi mucha na stene"

main = do
  print(tupleList[1,3,2,-1,7,0])
  print(tupleListMap[1,3,2,-1,7,0])
