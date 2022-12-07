ls1 :: [Int]
ls1 = [3,1,4,1,5]
ls2 :: [Int]
ls2 = [1,6,1,8,0]
terceiro :: [Int] -> Int
terceiro ls = head(tail(tail ls))

bissexto :: Int -> Bool
bissexto x 
 |mod x 400 == 0 || mod x 4 ==0 = True
 |mod x 100 /= 0  = False
 |otherwise = False