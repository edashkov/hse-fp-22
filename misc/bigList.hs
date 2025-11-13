bigList = [1..200000000]

main =  print $ foldl (+) 0 bigList
