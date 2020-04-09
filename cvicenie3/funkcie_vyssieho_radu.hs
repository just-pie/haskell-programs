tupleList [] = []
tupleList (x:xs) | even x == True = (x, True):(tupleList xs)
                 | otherwise = (x, False):(tupleList xs)

main = do
  print(tupleList[1,3,2,-1,7,0])
