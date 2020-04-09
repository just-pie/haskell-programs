tupleList [] = []
tupleList (x:xs) | even x == True = (x, True):(tupleList xs)
                 | otherwise = (x, False):(tupleList xs)
-- tupleList(x:xs) = (x, even x):(tupleList xs) -- Da sa aj takto jednoducho
tupleListMap xs = map (\x -> (x, even x)) xs
main = do
  print(tupleList[1,3,2,-1,7,0])
  print(tupleListMap[1,3,2,-1,7,0])
