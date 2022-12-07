horario :: Int -> (Int, Int, Int)
horario x = (div x 3600, div (mod x 3600) 60, mod (mod x 3600) 60 )