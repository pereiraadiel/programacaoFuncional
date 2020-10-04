l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6] 


--1)
--1.1)
selecao_foldr [] = []
selecao_foldr xs = [x] ++ selecao_foldr (remove x xs)
    where x = foldr1 min xs

remove a [] = []
remove a (x:xs)
    |  a==x = xs
    | otherwise = x:(remove a xs)

minimo [] = undefined
minimo [x] = x
minimo (x:xs)
    | x <= (minimo xs) = x
    | otherwise = minimo xs

--1.2)
insere_ordenado x [] = [x]
insere_ordenado x (y:ys)
    | x <= y = (x:y:ys)
    | otherwise = y: (insere_ordenado x ys)

insercao [] = []
insercao (x:xs) = insere_ordenado x (insercao xs)   

insercao_foldr xs = foldr insere_ordenado [] xs

--1.3)
quick [] = []
quick (x:xs) = (quick (filter (<x) xs)) ++ [x]  ++ (quick (filter (>x) xs))

--2)
--2.1)
bubble_sort1 [] = []
bubble_sort1 lista = bubbleOrd1 (lista,True) (length lista)

bubbleOrd1 (lista,False) _ = lista
bubbleOrd1 (lista,True) 0 = lista
bubbleOrd1 (lista,True) n  = bubbleOrd1 (troca1 (lista,True)) (n-1)

troca1 (list,False) = (list,False)
troca1 ([x],True) = ([x],True)
troca1 ((x:y:zs),True) =
    if x>y 
        then aux (troca1((x:zs),True)) y 
        else ((x:y:zs),False)
        where
            aux (list, bool) y = (y:list, bool)

--2.2)
bubble_sort2 [] = []
bubble_sort2 lista = bubbleOrd2 lista (length lista)

bubbleOrd2 lista 0 = lista
bubbleOrd2 lista n = bubbleOrd2 (troca2 (lista,(n-1))) (n-1)

troca2::([Int],Int)->[Int]
troca2 (list,0) = list
troca2 ([x],_) = [x]
troca2 ((x:y:zs),n)
    |x>y = y:troca2((x:zs),(n-1)) 
    |otherwise = x:troca2((y:zs),(n-1))

--2.3)
bubble_sort3 [] = []
bubble_sort3 lista = bubbleOrd3 (lista,True) (length lista)

bubbleOrd3 (lista,False) _ = lista 
bubbleOrd3 (lista,True) 0 = lista 
bubbleOrd3 (lista,True) n = bubbleOrd3 (troca3 (lista,(n-1),True)) (n-1)

troca3 (list,_,False) = (list,False)
troca3 (list,0,True) = (list,True)
troca3 ([x],n,True) = ([x],True)
troca3 ((x:y:zs),n,True) =
    if x > y
        then aux ( troca3 ((x : zs), (n-1),True)) y
        else ((x: y : zs),False)
      where
        aux (list, bool) a = (a : list,bool)

--bubble sort com contador e finalização antecipada
bubble_sortcont1 [] = ([],0)
bubble_sortcont1 lista = bubbleOrdcont1 (lista,0,True) (length lista)

bubbleOrdcont1 (lista,cont,True) 0 = (lista,cont) 
bubbleOrdcont1 (lista,cont,False) _ = (lista,cont) 
bubbleOrdcont1 (lista,cont,True) n = bubbleOrdcont1 (trocacont1 (lista,cont,True)) (n-1)

trocacont1 (list,cont,False) = (list,cont,False)
trocacont1 ([x],cont,True) = ([x],cont,True)
trocacont1 ((x:y:zs),cont,True) =
    if x > y
        then aux ( trocacont1 ((x : zs), cont + 1,True)) y
        else ((x: y : zs), cont + 1,False)
        where
        aux (list, count, bool) a = (a : list, count,bool)

--bubble sort com contador e desconsiderando ultimo elemento
bubble_sortcont2 [] = ([],0)
bubble_sortcont2 lista = bubbleOrdcont2 (lista,0) (length lista)

bubbleOrdcont2 (lista,cont) 0 = (lista,cont) 
bubbleOrdcont2 (lista,cont) n = bubbleOrdcont2 (trocacont2 (lista,cont,(n-1))) (n-1)

trocacont2 (list,cont,0) = (list,cont)
trocacont2 ([x],cont,n) = ([x],cont)
trocacont2 ((x:y:zs),cont,n) =
    if x > y
        then aux ( trocacont2 ((x : zs), cont + 1, (n-1))) y
        else aux ( trocacont2 ((y : zs), cont + 1, (n-1))) x
      where
        aux (list, count) a = (a : list, count)

-- --bubble sort com contador, finalização antecipada e desconsiderando ultimo elemento
bubble_sortcont3 [] = ([],0)
bubble_sortcont3 lista = bubbleOrdcont3 (lista,0,True) (length lista)

bubbleOrdcont3 (lista,cont,False) _ = (lista,cont) 
bubbleOrdcont3 (lista,cont,True) 0 = (lista,cont) 
bubbleOrdcont3 (lista,cont,True) n = bubbleOrdcont3 (trocacont3 (lista,cont,(n-1),True)) (n-1)

trocacont3 (list,cont,_,False) = (list,cont,False)
trocacont3 (list,cont,0,True) = (list,cont,True)
trocacont3 ([x],cont,n,True) = ([x],cont,True)
trocacont3 ((x:y:zs),cont,n,True) =
    if x > y
        then aux ( trocacont3 ((x : zs), cont + 1, (n-1),True)) y
        else ((x: y : zs), cont + 1,False)
      where
        aux (list, count, bool) a = (a : list, count,bool)

-- O algoritmo mais util seria o bubble_sortcon2 pois ele sempre ira organizar a lista não importa a organização da lista
-- o bubble_sortcont1 sempre ira parar apos no maximo 2 loops na lista pois ira achar o ultimo numero da lista e ira terminar a execução
-- o bubble_sortcont 3 apenas consegue organizar listas que estão de organizadas de maneira decrescente

--3)
--3.1)
selecao_var_1 :: (Ord a) => [a] -> [a]
selecao_var_1 [] = []
selecao_var_1 xs = x:selecao_var_1(remove x xs)
    where x = minimo xs

--3.2)
remove_menor a [] = []
remove_menor a (x:xs)
    | x == (minimo xs) && a == x = xs
    | otherwise = x:(remove_menor a xs)

-- remove a [] = []
-- remove a (x:xs)
--     |  a==x = xs
--     | otherwise = x:(remove a xs)

-- minimo [] = undefined
-- minimo [x] = x
-- minimo (x:xs)
--     | x <= (minimo xs) = x
--     | otherwise = minimo xs


--4)
quick_sort::(Ord a)=>[a]->[a]
quick_sort [] = []
quick_sort (s:xs) = quick_sort [x | x<-xs, x<s ] ++ [s] ++ quick_sort [ x|x<-xs,x>=s ]

quick_sortcont :: (Ord a) => [a] -> ([a], Int)
quick_sortcont [] = ([], 0)
quick_sortcont (piv : xs) =
  let (left, n_L) = quick_sortcont2 xs 0 (<= piv)
      (right, n_R) = quick_sortcont2 xs 0 (> piv)
      (sorted_L, n1_L) = quick_sortcont left
      (sorted_R, n1_R) = quick_sortcont right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

quick_sortcont2:: [a] -> Int -> (a -> Bool) -> ([a], Int)
quick_sortcont2 [] n _ = ([], n)
quick_sortcont2 (x : xs) n cond =
    if (cond x)
    then add (quick_sortcont2 xs (n + 1) cond) x
    else quick_sortcont2 xs (n + 1) cond
    where
    add (list, n) y = (y : list, n)



--4.1)
divide list s = ([x | x<-list, x<s ],[ x|x<-list,x>s ])

--4.2)
dividepivo3 list = ([x | x<-list, x<pivo list ],[ x|x<-list,x>pivo list])

pivo list = 
    if (length list)<3
        then head list
        else mediana (take 3 list )

mediana list = (quick_sort list)!!1

--4.1 com contador)
dividecont [] _ = (([],[]), 0)
dividecont list elem =
  let (left, n_L) = dividecont2 list 0 elem
      (right, n_R) = dividecont3 list 0 elem
   in ((left,right), n_L + n_R)

dividecont2 [] n _ = ([], n)
dividecont2 (x:xs) n elem =
    if (x < elem)
        then add (dividecont2 xs (n + 1) elem) x
        else dividecont2 xs (n + 1) elem
        where
            add (list, n) y = (y : list, n)

dividecont3 [] n _ = ([], n)
dividecont3 (x:xs) n elem =
    if (x > elem)
        then add (dividecont3 xs (n + 1) elem) x
        else dividecont3 xs (n + 1) elem
        where
            add (list, n) y = (y : list, n)

--4.2 com contador)



pivo1 list = 
    if (length list)<3
        then ((head list), 0)
        else medianacont (take 3 list )

medianacont list = aux ((quick_sortcont list))
    where
        aux ((list, n)) = ((list!!1),n)

dividepivocont [] = (([],[]), 0)
dividepivocont list =
    let (elem,n) = pivo1 list
        (left, n_L) = dividecont5 list 0 elem
        (right, n_R) = dividecont6 list 0 elem
    in ((left,right), n_L + n_R + n)
        
dividecont5 [] n _ = ([], n)
dividecont5 (x:xs) n elem =
    if (x < elem)
        then add (dividecont5 xs (n + 1) elem) x
        else dividecont5 xs (n + 1) elem
        where
            add (list, n) y = (y : list, n)

dividecont6 [] n _ = ([], n)
dividecont6 (x:xs) n elem =
    if (x > elem)
        then add (dividecont6 xs (n + 1) elem) x
        else dividecont6 xs (n + 1) elem
        where
            add (list, n) y = (y : list, n)

-- o algoritimo de dividecont é mais efcaz, pois ele não precisa achar a mediana dos primeiros elementos
-- logo esse usa menos 6 comparaçoes

--6)
data Exp a =
    Val a -- um numero
    | Add (Exp a) (Exp a) -- soma de duas expressoes
    | Sub (Exp a) (Exp a) -- subtração
    | Mul (Exp a) (Exp a)
    | Pow (Exp a) (Exp a)

avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pow exp1 exp2) = (avalia exp1) ^ (avalia exp2)



ex1::Exp Int
ex1 = Mul (Add (Val 3) (Val 12)) (Pow (Sub (Val 15) (Val 5)) (Mul (Val 1) (Val 3)))
ex2::Exp Int
ex2 = Sub (Val 0) (Mul (Sub (Val 5) (Add (Val 8)(Add (Val 6) (Val 1)))) (Add(Val 2)(Pow (Val 6) (Val 2))))


--7)
type Hora = (String,Int,Int)

--7.a)
horasDecorridas :: Hora -> Int
horasDecorridas (p,h,_) 
    | p == "AM" = h
    | otherwise = h + 12

minutosDecorridos :: Hora -> Int
minutosDecorridos (p,h,m)
    | p == "AM" = m + (h*60)
    | otherwise = m + (h*60) + (12*60)

segundosDecorridos :: Hora -> Int
segundosDecorridos (p,h,m)
    | p == "AM" = (m*60) + (h*3600)
    | otherwise = (m*60) + (h*3600) + (12*3600)

--7.b)
horasDecorridas_b :: Hora -> String
horasDecorridas_b (p,h,m) 
    | h < 0 || h > 11 = "Hora Invalida"
    | m < 0 || m > 59 = "Minutos Invalidos"
    | p == "AM" = show h
    | otherwise = show (h + 12)

minutosDecorridos_b :: Hora -> String
minutosDecorridos_b (p,h,m)
    | h < 0 || h > 11 = "Hora Invalida"
    | m < 0 || m > 59 = "Minutos Invalidos"
    | p == "AM" = show (m + (h*60))
    | otherwise = show (m + (h*60) + (12*60))

segundosDecorridos_b :: Hora -> String
segundosDecorridos_b (p,h,m)
    | h < 0 || h > 11 = "Hora Invalida"
    | m < 0 || m > 59 = "Minutos Invalidos"
    | p == "AM" = show ((m*60) + (h*3600))
    | otherwise = show ((m*60) + (h*3600) + (12*3600))

--7.c) Feito!

--8)
type Data = (Int,Int,Int)
data Mensagem = Msg{ contato::String,  mensagem::String, datamsg::Data, hora::Hora, app::String}  deriving (Show,Eq, Ord) 


ms1::Mensagem
ms2::Mensagem
ms3::Mensagem
ms4::Mensagem
ms5::Mensagem
ms6::Mensagem
ms7::Mensagem
ms8::Mensagem
ms9::Mensagem
ms10::Mensagem
ms11::Mensagem
ms12::Mensagem
ms13::Mensagem
ms14::Mensagem
ms15::Mensagem
ms16::Mensagem
ms17::Mensagem
ms18::Mensagem
ms19::Mensagem
ms20::Mensagem
ms21::Mensagem
ms22::Mensagem
ms23::Mensagem
ms24::Mensagem
ms25::Mensagem
ms26::Mensagem
ms27::Mensagem
ms28::Mensagem
ms29::Mensagem
ms30::Mensagem

ms1 = Msg {contato="Eliabe", mensagem="teste de mensagem 1",datamsg=(21,09,2020),hora=("AM",09,30), app="WhatsApp"}
ms2 = Msg {contato="Felipe", mensagem= "teste de mensagem 2",datamsg=(21,09,2020),hora=("AM",09,32),app="Facebook"}
ms3 = Msg {contato= "Marcos", mensagem="teste de mensagem 3",datamsg=(21,09,2020),hora=("AM",09,34), app="WhatsApp"}
ms4 = Msg {contato= "Heitor", mensagem="teste de mensagem 4",datamsg=(21,09,2020),hora=("AM",11,20), app="WhatsApp"}
ms5 = Msg {contato= "Felipe", mensagem="teste de mensagem 5",datamsg=(21,09,2020),hora=("AM",11,30), app="Facebook"}
ms6 = Msg {contato= "Eduardo", mensagem="teste de mensagem 6",datamsg=(21,09,2020),hora=("PM",00,20), app="Facebook"}
ms7 = Msg {contato= "Marcos", mensagem="teste de mensagem 7",datamsg=(21,09,2020),hora=("PM",01,10), app="Linkedin"}
ms8 = Msg {contato= "Rafael", mensagem="teste de mensagem 8",datamsg=(21,09,2020),hora=("PM",01,12), app="Facebook"}
ms9 = Msg {contato= "Lucas", mensagem="teste de mensagem 9",datamsg=(21,09,2020),hora=("PM",08,22), app="Facebook"}
ms10 = Msg {contato= "Eliabe", mensagem="teste de mensagem 10",datamsg=(21,09,2020),hora=("PM",08,22), app="WhatsApp"}
ms11 = Msg {contato= "Vinicius", mensagem="teste de mensagem 11",datamsg=(21,09,2020),hora=("PM",09,24), app="Linkedin"}
ms12 = Msg {contato= "Marcelo", mensagem="teste de mensagem 12",datamsg=(21,09,2020),hora=("PM",10,30), app="WhatsApp"}
ms13 = Msg {contato= "992569854", mensagem="teste de mensagem 13",datamsg=(21,09,2020),hora=("PM",10,31), app="Facebook"}
ms14 = Msg {contato= "Eliabe", mensagem="teste de mensagem 14",datamsg=(21,09,2020),hora=("PM",10,32), app="Facebook"}
ms15 = Msg {contato= "marcos", mensagem="teste de mensagem 15",datamsg=(22,09,2020),hora=("AM",08,33), app="Facebook"}
ms16 = Msg {contato= "996857432", mensagem="teste de mensagem 16",datamsg=(22,09,2020),hora=("AM",09,00), app="WhatsApp"}
ms17 = Msg {contato= "Vinicius", mensagem="teste de mensagem 17",datamsg=(22,09,2020),hora=("AM",10,34), app="Linkedin"}
ms18 = Msg {contato= "Felipe", mensagem="teste de mensagem 18",datamsg=(22,09,2020),hora=("AM",11,34), app="WhatsApp"}
ms19 = Msg {contato= "992569854", mensagem="teste de mensagem 19",datamsg=(22,09,2020),hora=("AM",11,40), app="Facebook"}
ms20 = Msg {contato= "996857432", mensagem="teste de mensagem 20",datamsg=(22,09,2020),hora=("PM",01,22), app="Facebook"}
ms21 = Msg {contato= "Vinicius", mensagem="teste de mensagem 21",datamsg=(22,09,2020),hora=("PM",01,23), app="Facebook"}
ms22 = Msg {contato= "Heitor", mensagem="teste de mensagem 22",datamsg=(22,09,2020),hora=("PM",02,23), app="WhatsApp"}
ms23 = Msg {contato= "Rafael", mensagem="teste de mensagem 23",datamsg=(22,09,2020),hora=("PM",02,24), app="Linkedin"}
ms24 = Msg {contato= "Eliabe", mensagem="teste de mensagem 24",datamsg=(22,09,2020),hora=("PM",02,25), app="Facebook"}
ms25 = Msg {contato= "Heitor", mensagem="teste de mensagem 25",datamsg=(22,09,2020),hora=("PM",02,25), app="Facebook"}
ms26 = Msg {contato= "Vinicius", mensagem="teste de mensagem 26",datamsg=(22,09,2020),hora=("PM",03,21), app="Facebook"}
ms27 = Msg {contato= "Rafael", mensagem="teste de mensagem 27",datamsg=(22,09,2020),hora=("PM",03,22), app="WhatsApp"}
ms28 = Msg {contato= "Rafael", mensagem="teste de mensagem 28",datamsg=(22,09,2020),hora=("PM",03,23), app="Linkedin"}
ms29 = Msg {contato= "Heitor", mensagem="teste de mensagem 29",datamsg=(22,09,2020),hora=("PM",03,24), app="Facebook"}
ms30 = Msg {contato= "Vinicius", mensagem="teste de mensagem 30",datamsg=(22,09,2020),hora=("PM",03,35), app="Facebook"}

--8.b)
type Lista = [Mensagem]
teste = [ms1,ms2,ms3,ms4,ms5,ms6,ms7,ms8,ms9,ms10,ms11,ms12,ms13,ms14,ms15,ms16,ms17,ms18,ms19,ms20,ms21,ms22,ms23,ms24,ms25,ms26,ms27,ms28,ms29,ms30]

fraseteste (Msg{contato=contact }) = "nome: " ++ contact

bubble_sortmsg [] = []
bubble_sortmsg lista = bubbleOrdmsg lista (length lista)

bubbleOrdmsg lista 0 = lista
bubbleOrdmsg lista n = bubbleOrdmsg (trocamsg lista) (n-1)

trocamsg [x] = [x]
trocamsg (x:y:zs)
    |contato x>contato y = y:trocamsg(x:zs) 
    |otherwise = x:trocamsg(y:zs)

--8.c)
-- quick_sortdatahr::[Mensagem]->[Mensagem]
quick_sortdatahr [] = []
quick_sortdatahr (s:xs) = quick_sortdatahr [x | x<-xs, compararmsgs x s ] ++ [s] ++ quick_sortdatahr [ x|x<-xs, (compararmsgs x s) == False ]

getdiadata::Mensagem->Int
getdiadata x = aux( datamsg x)
    where
        aux (x,y,z) = x

gettp::Mensagem->String
gettp x = aux( hora x)
    where
        aux (tp,h,m) = tp

gethr::Mensagem->Int
gethr x = aux( hora x)
    where
        aux (tp,h,m) = h

getmin::Mensagem->Int
getmin x = aux( hora x)
    where
        aux (tp,h,m) = m

compararmsgs x s
    |(getdiadata x < getdiadata s) || (getdiadata x == getdiadata s && gettp x < gettp s) || (getdiadata x == getdiadata s && gettp x == gettp s && gethr x < gethr s) || (getdiadata x == getdiadata s && gettp x == gettp s && gethr x == gethr s && getmin x <= getmin s) = True
    |otherwise = False

--8d)

duas_ultimas_msgs x = drop ((length x)-2) x

mensagens_de_contato::String->[Mensagem]
mensagens_de_contato x = [ y | y<-teste, (contato y)==x]

ultimas_msgs x = duas_ultimas_msgs(quick_sortdatahr(mensagens_de_contato x))

--10)

data ArvBinEA a = Vazia | Folha a | NoEA (Char, ArvBinEA a, ArvBinEA a) deriving (Show) 

ea::ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

calcula::ArvBinEA Float->Float
calcula Vazia = 0
calcula (Folha x) = x
calcula (NoEA (op, esq, dir))
    |op=='*' = (calcula esq)*(calcula dir)
    |op=='/' = (calcula esq)/(calcula dir)
    |op=='+' = (calcula esq)+(calcula dir)
    |otherwise = (calcula esq)-(calcula dir)