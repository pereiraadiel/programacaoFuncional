-- EXERCICIO 1
ou :: Bool -> Bool -> Bool
-- operador "ou" com guardas
ou x y 
  | x == True = True
  | y == True = True
  | otherwise = False

-- operador ou com if else
-- ou x y = 
--   if(x == True) then True
--   else if (y == True) then True
--   else False

-- operador "ou" com casamento de padroes (1)
-- ou True False = True
-- ou False True = True
-- ou True True = True 
-- ou False False = False

-- operador "ou" com casamento de padroes (2)
-- ou True _  = True
-- ou _ True = True
-- ou False False = False

-- operador "ou" com casamento de padroes (3)
-- ou False False = False
-- ou _ _ = True

-- EXERCIO 2
-- Distancia entre dois pontos no espaco
type Ponto = (Float, Float, Float)

quadrado :: Float -> Float
quadrado x = x*x

distancia :: Ponto -> Ponto -> Float
distancia (ax,ay,az) (bx,by,bz) = 
  sqrt( quadrado (bx - ax) + quadrado (by - ay) + quadrado (bz - az) )

-- EXERCICIO 3
-- Avaliar expressoes

-- EXERCICIO 4
-- Fatorial implementado com guardas
fatorial :: Int -> Int
fatorial x 
  | x == 2 = x
  | otherwise = x * fatorial (x-1)

-- Fatorial implementado com casamento de padroes
fatorial 2 = 2
fatorial x = x * fatorial (x-1)

-- EXERCICIO 5
-- Fibonacci recursivo
fibonacci :: Int -> Int
fibonacci x 
  | x == 0 = 1
  | x == 1 = 1
  | otherwise = fibonacci (x-1) + fibonacci(x-2)