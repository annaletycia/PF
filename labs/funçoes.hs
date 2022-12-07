volumeEsfera :: Float->Float
volumeEsfera r = (r^3) * pi * (4/3)

hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt(x ^ 2 + y ^ 2)

distPontos :: Float -> Float -> Float -> Float -> Float 
distPontos x1 y1 x2 y2 = sqrt ((x2-x1)^2+(y2-y1)^2)
quadrado :: Float -> Float
quadrado x = x^2

quartapot :: Float -> Float 
quartapot x = quadrado(quadrado x)

funçaoL :: Bool -> Bool -> Bool
funçaoL p q = (q||q)&& not(p&&q)