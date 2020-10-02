-- Aluno: Adiel - 11721BCC008
-- Aluno: Eliabe - 11721BCC032


import Data.List
--1)
triangulo::(Int,Int,Int)->String
triangulo(x,y,z)
    |x==60 && y==60 && z==60 = "equilatero"
    |(x==90 || z==90 || y==90) && (x+y+z)==180 && x>0 && y>0 && z>0 = "retangulo"
    |(x>90 || z>90 || y>90) && (x+y+z)==180 && x>0 && y>0 && z>0 = "obtuso"
    |(x+y+z)==180 && x>0 && y>0 && z>0 = "simples"
    |otherwise = "nao_triangulo"

--2)
a_igual_zero::Float->Float->Float
a_igual_zero b c = -(c/b)

delta::Float->Float->Float->Float
delta a b c = (b*b)-(4*a*c)

x_um::Float->Float->Float->Float
x_um a b c = ((-b)+sqrt(delta a b c))/(2*a)

x_dois::Float->Float->Float->Float
x_dois a b c = ((-b)- sqrt(delta a b c))/(2*a)

equacao :: Float->Float->Float->(Float,Float)
equacao a b c 
    | a == 0 = ((a_igual_zero b c), a)
    | otherwise = ((x_um a b c), (x_dois a b c))
--3)
type Data = (Int,Int,Int)

idade::(Data,Data)->Int
idade ((d1,m1,a1),(d2,m2,a2))
    |m1<m2 || (m1==m2 && d1<d2) = (a1-a2)-1
    |otherwise = (a1-a2)

passagem::Float->(Data,Data)->Float
passagem preco datas
    |(idade(datas)) < 2 = (preco*0.15)
    |(idade(datas)) <= 10 = (preco*0.40)
    |(idade(datas)) >= 70 = (preco*0.50)
    |otherwise = preco


--4)

gera1 :: [Int]
gera1 = [x^2 | x <- [1..15], odd x, x > 4, x< 15]

gera2 :: [(Int,Int)]
gera2 = [(x,y) | x <- [1..15], y <- [1..15], x >= 1, x <= 4, y >= x, y <=x*2 ]
-- precisa pegar o primeiro do intervalo para y na gera2

gera3 :: [Int]
gera3 = [ x | x <- [1..15], y <- [10..15], x <= y]

gera4 :: [(Int, Int)]
gera4 = [ (x,y) | x <- [1..15], y <- [1..16], odd x, y == x+1]

somaTuplas :: [(Int, Int)] -> [Int]
somaTuplas [] = []
somaTuplas (x:xs) = somaTupla x : somaTuplas xs 

somaTupla :: (Int,Int) -> Int
somaTupla (x,y) = x+y

gera5 :: [Int]
gera5 = [ x | x <- somaTuplas(gera4)]


--5)a)
contaNegM2::[Int]->Int
contaNegM2 x = length([ y|y<-x,even y, y<0 ])

--b)
listaNegM2::[Int]->[Int]
listaNegM2 x = [ y|y<-x,even y, y<0 ]

--6) 
distancias :: [(Float,Float)] -> [Float]
distancias [] = []
distancias ((x,y):xys) = (sqrt (x^2 + y^2)) : (distancias xys)


distancias_compreencao :: [(Float,Float)] -> [Float]
distancias_compreencao [] = []
distancias_compreencao x = [ z | z <- distancias x ]


--7)
fatores::Int->[Int]
fatores x = [ y| y <- [1..x] , (mod x y) == 0 ]

primos::Int->Int->[Int]
primos x y = [ z| z <- [x..y], length(fatores z) == 2 ]

--8)
-- mmc

mdc::Int->Int->Int
mdc a b 
  | a < b = mdc b a        
  | b == 0 = a        
  | otherwise = mdc b (mod a b)
  
mmc2::Int->Int->Int
mmc2 x y = (x * y) `div` (mdc x y)

mmc::Int->Int->Int->Int
mmc x y z = mmc2 x (mmc2 y z)


--9)
listimpares::Int->[Float]
listimpares n = [ fromIntegral y | y<-[1, 3..n]]

listpares::Int->[Float]
listpares n = [ fromIntegral y | y<-[2,4..n]]

divimpares::Float->Int->[Float]
divimpares x n = [ y/x | y<-(listimpares n) ]

divpares::Float->Int->[Float]
divpares x n = [ y/x | y<-(listpares n) ]

calculaserie::Float->Int->Float
calculaserie x n = sum(divimpares x n)+sum(divpares x n)

--10)
divide x 
  | mod x 3 == 0 && mod x 5 == 0 = "FizzBuzz"
  | mod x 3 == 0 = "Fizz"
  | mod x 5 == 0 = "Buzz"
  | otherwise = "No"

fizzbuzz :: Int -> [String]
fizzbuzz n = [divide x | x <- [1..n] ]


--11)

contador _ _ [] (count1, count2) = (count1, count2)
contador x y (h:t) (count1, count2)
    | x == h = contador x y t (count1 + 1, count2)
    | y == h = contador x y t (count1, count2 + 1)
    | otherwise = contador x y t (count1, count2)

conta_ocorrencia x y list = contador x y list (0, 0)



--12)
pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = 
  if (a == x) then True
  else pertence a z

unicaOcorrencia :: Int -> [Int] -> Bool
unicaOcorrencia x [] = False
unicaOcorrencia x (a:as) = 
  if (x == a && not (unicaOcorrencia x as)) then True
  else unicaOcorrencia x as


--13)
intercala::[t]->[t]->[t]
intercala x y = concat(transpose [x,y])

--14)
-- Agenda de contatos
-- Nome, Endereco, Telefone, Email
type Contato = (String, String, String, String)

-- para os testes
agenda :: [Contato]
agenda = [("Teste", "Rua 1", "3490903231", "teste@mail.com"),
  ("Teste1", "Rua 1", "3490903231", "teste1@mail.com")]

getNome :: Contato -> String
getNome (nome, _, _, _) = nome

getEmail :: Contato -> String
getEmail (_,_,_, email) = email

agendaGetNome :: [Contato] -> String -> String
agendaGetNome [] email= "Email Desconhecido"
agendaGetNome (ag:ags) email 
  | getEmail ag == email = getNome ag 
  | otherwise = agendaGetNome ags email


--15)
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa",  1.66, 27,'F'),("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S') ]

nome::Pessoa->String
nome (x,_,_,_) = x

altura::Pessoa->Float
altura (_,x,_,_) = x

idadep::Pessoa->Int
idadep (_,_,x,_) = x

est_civil::Pessoa->Char
est_civil (_,_,_,x) = x


lista_alturas::[Pessoa]->[Float]
lista_alturas x = [ altura y | y<-x]

soma::[Float]->Float
soma [x] = x
soma (x:xs) = x + soma xs

media::[Float]->Float
media x = (soma x)/(fromIntegral (length x))


lista_idades::[Pessoa]->[Int]
lista_idades x = [ idadep y | y<-x]

mais_velha::[Pessoa]->Int
mais_velha x = maximum(lista_idades x )

pessoa_mais_velha::[Pessoa]->Pessoa
pessoa_mais_velha x = head([y | y<-x, idadep y == mais_velha x])

--função de altura media
altura_media::[Pessoa]->Float
altura_media x = media(lista_alturas(x))

--função de idade da pessoa mais nova
mais_nova::[Pessoa]->Int
mais_nova x = minimum(lista_idades x )

--função que retorna nome e estado civil da pessoa mais velha
info_mais_velha::[Pessoa]->(String,Char)
info_mais_velha x = (nome(pessoa_mais_velha x),est_civil(pessoa_mais_velha x))

--função para mostrar pessoas com idade >= 50 anos
pessoa_maior_50::[Pessoa]->[Pessoa]
pessoa_maior_50 x = [y | y<-x, idadep y >= 50]

--função para mostrar casadas com idade superior a i
casadas::[Pessoa]->Int->[Pessoa]
casadas x i = [y | y<-x, est_civil y == 'C', idadep y > i]

--16)
insere_ord a [] = [a]
insere_ord a (b:bs) = 
  if(a < b) then a:b:bs
  else b: insere_ord a bs


--17)
reverte::[t]->[t]
reverte [] = []
reverte (x:xs) = reverte xs ++ [x]


--18)
sem_repetidos:: Eq t => [t]->[t]
sem_repetidos [] = []
sem_repetidos [x] = [x]
sem_repetidos (x:xs) = x:[ y|y<-sem_repetidos(xs), y /= x]

--19)
disponiveis::[Int]
disponiveis = [1,2,5,10,20,50,100]

notasTroco::Int->[[Int]]
notasTroco 0 = [[]]
notasTroco x = [ y:ys | y <- disponiveis, x >= y, ys <- notasTroco(x-y)]

--20)
remover:: Eq t => t->[t]->[t]
remover x [] = []
remover x (y:ys)
    |x == y = ys
    |otherwise = (y:(remover x ys))


locais::[Int]->[[Int]]
locais [] = [[]]
locais list = [ x:y | x<-list, y<-locais(remover x list) ]

captura::Int->Int->[Int]->Bool
captura _ x [] = False
captura x y (z:zs)
    |(y == z) || (z == (y+x)) || (z == (y-x)) = True
    |otherwise = captura(x+1) y zs

check_captura::[Int]->Bool
check_captura [] = True
check_captura (x:xs)
    |(captura 1 x xs) == False = check_captura xs
    |otherwise = False

locais_validos::[[Int]]->[[Int]]
locais_validos [] = []
locais_validos (x:xs)
    |(check_captura x) == False = locais_validos xs
    |otherwise = (x:(locais_validos xs))

nRainhas::Int->[[Int]]
nRainhas x = locais_validos(locais([1..x]))

