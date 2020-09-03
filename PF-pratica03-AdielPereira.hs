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

-- EXERCICIO 6
-- triangulares
triangulares :: Int -> Int
triangulares 1 = 0
triangulares 2 = 1
triangulares 3 = 3
triangulares x = (triangulares (x-1)) + (x-1)

-- EXERCICIO 7
-- fibonacci com tupla?????

passo (1,1) = (1,2)
passo (x,y) = (y, x+y)

n_par x = passo(x,x+1)

fibo 1 = (1,1)

-- EXERCICIO 8
-- potencia2
potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 x = potencia2 (x-1) * 2

-- EXERCICIO 9
-- prodIntervalo
-- prodIntervalo :: Int -> Int -> Int


-- EXERCICIO 11
-- restoDiv e divInteira
restoDiv :: Int -> Int -> Int
restoDiv x y = 
  if(x == y) then 0
  else if(x < y) then x
  else restoDiv (x-y) y

divInteira :: Int -> Int -> Int
divInteira x y = 
  if(x == y) then 1
  else if (x < y) then 0
  else 1 + divInteira (x-y) y

-- EXERCICIO 12

-- EXERCICIO 13
-- binomial
binomial :: (Int, Int) -> Int
-- definicao de binomial recursiva:
binomial (n,k) 
  | k == 0 = 1
  | n == k = 1
  | otherwise = binomial (n-1,k) + binomial (n-1, k-1)

-- definicao de binomial com casamento de padroes
bnomial :: (Int, Int) -> Int
bnomial (n, 0) = 1
bnomial (n,k) = if(k==n) then 1
  else bnomial(n-1,k) + bnomial(n-1,k-1)

