dobro :: Int -> Int
dobro x = x * 2

quadruplicar :: Int -> Int
quadruplicar x = dobro (dobro x)

hipotenusa :: Double -> Double -> Double
hipotenusa a b = sqrt((a ^ 2) + (b ^ 2)) 

distancia :: Double -> Double -> Double -> Double -> Double 
distancia ax ay bx by = sqrt ((bx - ax) ^ 2 + (by - ay) ^2)

conversao :: Double -> (Double, Double, Double)
conversao x = (x, x*3.96, x*4.96)

bissexto:: Int -> Bool
bissexto x | (mod x 400 == 0) = True
           | (mod x 4 == 0) && (mod x 100 /= 0) = True
           | otherwise = False

-- Data = Dia, Mes, Ano
type Data = (Int, Int, Int);

validaData:: Data -> Bool
validaData (d,m,a)
  | d >=1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 ||
    m == 10 || m == 12) = True
  | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
  | d >= 1 && d <= 28 && m == 2 && not (bissexto a) = True
  | d >= 1 && d <= 29 && m == 2 && (bissexto a) = True
  | otherwise = False


precede:: Data -> Data -> Bool
precede (dx,mx,ax) (dy,my,ay) = 
  if( validaData (dx,mx,ax) && validaData (dy,my,ay)) then 
    if(ax > ay) then False
    else if(mx > my) then False
    else if(dx > dy) then False
    else True
  else False

-- Livro = Codigo, Titulo, Autor, Editora, AnoLancamento
type Livro = (String, String, String, String, Int);
-- Pessoa = Identificacao, Nome, Email, Telefone
type Pessoa = (String, String, String, String);

-- Imprestimo = CodLivro, CodLeitor, DataEmp, DataDev, Situacao
type Imprestimo = (String, String, Data, Data, Int);

-- Livros = Lista de Livros
type Livros = [Livro]
-- Pessoas = Lista de Pessoas
type Pessoas = [Pessoa]

situacaoImprestimo :: Imprestimo -> Data -> String
situacaoImprestimo (_, _, _, (dx,mx,ax), _) (dy,my,ay) = 
  if precede (dx,mx,ax) (dy,my,ay) then "Em atraso"
  else "Em dia"