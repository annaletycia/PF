not_logico :: Bool -> Bool
not_logico False = True
not_logico True = False

notLogico :: Bool -> Bool
notLogico x
 |x == True = False
 |otherwise = True