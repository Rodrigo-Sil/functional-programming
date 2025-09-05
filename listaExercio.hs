-- 1) Escreva uma função sucessor n que retorne o sucessor de n (usando apenas operadores)


sucessor :: Int -> Int

sucessor n = n + 1


-- 2) Escreva uma função antecessor n que retorne o antecessor de n (usando apenas operadores)

antecessor :: Int -> Int

antecessor n = n - 1

-- 3) Escreva uma função areaRet b h que retorne a sua área de um retângulo de base b e altura h.

areaRet :: Num a => a -> a -> a

areaRet b h = b * h


-- 4) Escreva uma função funcSegGrau a b c x que retorne o resultado de uma função do segundo grau f(x) = Ax^2 + Bx + C ao receber os coeficientes a, b, c e um valor de x.

funcSegGrau a b c x = (a * (x ** 2)) + b * x + c 

-- (** para float e ^ para inteiros) 

-- 5) Escreva uma função delta a b c que calcule o valor do discriminante delta de uma equação do segundo grau Ax2 + Bx + C = 0 , ao receber os coeficientes a, b, e c.

delta :: Floating a => a -> a -> a -> a

delta a b c = (b ** 2) - (4 * a * c)

-- (colocar o numero entre parenteses se for negativo na hora da execução)


-- 6) Utilizando a função delta da questão anterior e guardas, crie uma função raizesReais a b c que retorne strings de mensagem indicando a quantidade de raízes reais diferentes que a equação Ax2 + Bx + C = 0 possui

raizesReais :: Float -> Float -> Float -> String
raizesReais a b c
  | d < 0     = "Nao existem raizes reais"
  | d == 0    = "Existe uma raiz real (duas raizes iguais)"
  | d > 0     = "Existem duas raizes reais distintas"
  where d = delta a b c


-- 7) Escreva uma função pitagoras a b que receba os dois catetos de um triângulo retângulo e retorne o valor da hipotenusa

pitagoras :: Float -> Float -> Float

pitagoras a b = ((a ** 2) + (b ** 2)) ** (1 / 2)

-- usando sqrt

pitagorasSqrt :: Float -> Float -> Float

pitagorasSqrt a b = sqrt (a ** 2 + b ** 2)


-- 8) Escreva as seguintes funções para calcular diversas medidas de um cilindro. A cada função nova, vá
--utilizando as funções de itens anteriores quando for aplicável.

-- a) circunferencia raio:

circunferencia :: Floating a => a -> a

circunferencia raio =
  let pi = 3.14
  in 2 * pi * raio
  -- Usar o let para definir uma variavel
  -- in indica onde aquela defiição sera usada
  
  
-- b) areaCirculo raio:

areaCirculo :: Floating a => a -> a

areaCirculo raio = 
  let pi = 3.14
  in pi * raio ** 2


-- c) volumeCilindro raioBase altura:

volumeCilindro :: Floating a => a -> a -> a

volumeCilindro raioBase altura = areaBase * altura
 where areaBase = areaCirculo raioBase

-- d) areaTotalCilindro raioBase altura:

areaTotalCilindro :: Floating a => a -> a -> a 

areaTotalCilindro raioBase altura = 2 * areaBase + areaLateral
  where areaBase = areaCirculo raioBase
        areaLateral = (circunferencia raioBase) * altura
        

-- 9) Escreva uma função modulo v que recebe uma tupla v = (x, y), que representa um vetor com
-- coordenadas x e y partindo da origem de um sistema de coordenadas e retorne seu comprimento


moduloV :: Floating a => (a, a) -> a

moduloV (x, y) = sqrt (x ^ 2 + y ^2)

-- 10)Escreva uma função distancia a b que recebe duas tuplas a = (ax, ay) e b = (bx, by), que
-- representam as coordenadas de pontos e retorne a distância entre os dois pontos

distancia :: Num a => (a, a) -> (a, a) -> (a, a)

distancia (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)


-- 11) Escreva uma funcao parImpar n que receba um número inteiro n e retorne uma string “par” ou “impar”

parImpar :: Int -> String

parImpar n
  | mod n 2 == 0   = "par"
  | otherwise      = "impar"
  
-- 12) Uma universidade avalia seus cursos através de diversos fatores e atribui uma nota de 0 a 10 ao final da
-- avaliação. Para fins de divulgação, os cursos são classificados segundo uma classificação descrita na tabela
-- abaixo:

--	       ________________________________
--             |    [8, 10]      |      A     | 
--             |    [7, 8 [      |      B     |
--             |    [6, 7 [      |      C     |
--             |    [5, 6 [      |      D     |
--             |    [0, 5 [      |      E     |
--	       |_________________|____________|


-- Escreva uma funcao avaliacaoCurso nota que recebe a nota do curso e retorne seu conceito


avaliacaoCurso :: Float -> Char

avaliacaoCurso n
  | n >= 0  &&  n < 5    = 'E'
  | n >= 5  &&  n < 6    = 'D'
  | n >= 6  &&  n < 7    = 'C'
  | n >= 7  &&  n < 8    = 'B'
  | n >= 8  &&  n <= 10  = 'A'
  | otherwise            = error "Nota invalida"
  
  
-- 13) Escreva uma função situacaoAluno n1 n2 n3 que recebe 3 notas, calcule a média e retorne a situação
-- do aluno, em formato de string, segundo uma das seguintes condições:

situacaoAluno n1 n2 n3
  | d >= 7     =  "aprovado"
  | 4 <= d && d < 7 =  "avaliação final"
  | d < 4      =  "reprovado"
  | otherwise  =  error "Nota invalida"
  where d =  (n1 + n2 + n3) / 3
  
-- 14) Escreva uma função vogal l que receba um caractere l e informe se é uma vogal ou não.

vogal l
  | elem l "aeiouAEIOU"  == True  =  "Vogal"
  | otherwise =  "Consoante"
  
-- 15) Escreva uma função maior a b c que receba três números inteiros e retorne o maior dentre eles.

maior :: Int -> Int -> Int -> Int

maior a b c 
  | a >= b && a >= c    =  a
  | b >= a && b >= c    =  b
  | otherwise           =  c


-- 16) Escreva uma função sortTres a b c que receba três números e retorne uma lista com eles em ordem
-- crescente

menor a b c 
  | a <= b && a <= c    =  a
  | b <= a && b <= c    =  b
  | otherwise           =  c
  

numeroMedio a b c
  | (a >= b && a <= c) || (a <= b && a >= c) = a
  | (b >= a && b <= c) || (b <= a && b >= c) = b
  | otherwise                                = c


sortTres a b c = [menor a b c, numeroMedio a b c, maior a b c] 


-- 17) Escreva uma função colisaoPontoAABB pMin pMax p que receba as coordenadas pMin e pMax de dois
-- pontos que definem uma caixa alinhada com os eixos x e y e um terceiro ponto p e retorne se p é interno ou
-- externo à caixa formada



colisaoPontoAABB (x1, y1) (x2, y2) (x, y)
  | (xMin <= x && x <= xMax) && (yMin <= y && y <= yMax)   =   "interno"
  | otherwise                                              =   "externo"
  where
  xMin
   | x1 <= x2  =  x1
   |otherwise  =  x2
  xMax 
   | x1 >= x2  = x1
   | otherwise = x2
  yMin 
   | y1 <= y2  =  y1
   |otherwise  =  y2
  yMax 
   | y1 >= y2  = y1
   | otherwise = y2


-- 19) Escreva as seguintes funções em Haskell:


-- a) dobroList :: [Int] -> [Int]: recebe uma lista de inteiros e retorna a lista com o dobro do valor
-- de cada elemento

-- recursivamente

dobroListR :: [Int] -> [Int]

dobroListR [] = []

dobroListR (a:as) = (2 * a) : (dobroList as)

-- compreensão de lista

dobroListL :: [Int] -> [Int]

dobroListL lista  = [ 2 * x | x <- lista ]

-- Função de alta ordem

dobroList :: [Int] -> [Int]

dobroList l = map (2 *) l

-- b) contaList :: [a] a -> Int: → recebe uma lista e um valor e retorna a quantidade de vezes que
-- esse valor aparece dentro da lista

contaList [] x = 0
contaList (a:as) x
 | a == x    = 1 + contaList as x
 | otherwise = contaList as x
 
-- c) filtraPares :: [Int] -> [Int]: recebe uma lista de inteiros e retorna uma lista contendo
-- somente os números pares da lista entrada

-- recursivamente

filtraParesR [] = []
filtraParesR (a:as)
    | mod a 2 == 0    = a:(filtraParesR as)
    | otherwise         = filtraParesR as
    
-- compreensão de listas


filtraParesL lista = [ x | x <- lista, mod x 2 == 0 ]

-- função de alta ordem

filtraPares lista = filter even lista

-- d) multiplos :: Int -> [Int]: recebe um número inteiro e retorna a lista de múltiplos desse
-- número.

multiplos n = [x * n | x <- [1..n]]

-- e) divisores :: Int -> [Int]: recebe um número inteiro e retorna a lista de números divisores
-- desse número.

divisores n = [x | x <- [1..n], mod n x == 0]

-- f) mmc :: Int -> Int -> Int: recebe dois números inteiros e retorna o mínimo múltiplo comum
-- entre eles

mmc m n = head [x | x <- [1..], mod x m == 0, mod x n == 0]

--compreensao de lista
dobroList l = [2*x | x <- l]

--recursiva
dobroLista [] = []
dobroLista (a:as) = 2*a(dobroLista as)

n
--contaLista(compreensão de lista)

contaLista l n = length [x | x <- l, x == n]

--contaLista (recursiva)
contaList [] n = 0
contaList (a:as) n
	| n == a     = 1 + contaList as n
	| otherise =       contaList as 

--divisores

divisores n = [x | x <- [1..n], mod n x == 0]

--primo

primo n = length (divisores n) <= 2


--Lista de primos ate n

primoList n = take n [x | x <- [1..], primo x]
