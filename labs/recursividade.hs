--funçao recursiva:

fatorial :: Int->Int
fatorial n
 | n==0 =1 --caso base
 | otherwise = n * fatorial (n-1) --caso recursivo
 
somaN :: Int -> Int
somaN n
 | n == 0 = 0
 | otherwise = n + somaN (n-1)
--soma dos numeros n até 1
 
mdc :: (Int,Int)-> Int
mdc (m,n)
  | n == 0 = m
  | otherwise = mdc (n, m `mod`n)
--mdc(m,n) = mdc(n, m mod n) se n for maior que zero

fibonacci :: Int->Int
fibonacci x
 | x==1 = 1
 | x==2 = 1
 | otherwise = fibonacci(x-1)+ fibonacci(x-2)
--fibonacci: soma dos dois números anteriores a x.
 
produto :: [Float] -> Float
produto [] = 1
produto (x:xs) = x * produto xs

{--Anna Letycia Fernandes Reis
--12011BSI235

type Dia = Int
type Mes = Int
type Ano = Int
type Data = (Dia,Mes,Ano)
{-por meio da construção type, são feitas algumas declarações de sinônimos para facilitar a leitura
e o desenvolvimento do código-}

bissexto:: Int-> Bool
bissexto x 
 | (mod x 400 == 0) = True
 | (mod x 4 == 0) && (mod x 100 /= 0) = True
 | otherwise = False
{-essa função recebe uma ano de entrada e de saída devolve se tal ano é ou não bissexto.
Foi definido nas guardas que para que um ano seja bissexto ele deve ser divisível por 4 e 400 e
não divisível por 100. Caso contrário, ele não é bissexto-}

numDeDiasEmCadaMesDeUmAno :: Ano -> [Int]
numDeDiasEmCadaMesDeUmAno ano = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
 where feb 
        | bissexto ano = 29
        | otherwise = 28
{-essa função recebe um ano de entrada e devolve a quantidade de dias de cada mês daquele ano,
considerando se ele é ou não bissexto, por meio da implementação da função bissexto-}

numDeDias :: Data -> Int
numDeDias (dia,mes,ano) =
 dia + sum (take (mes-1) (numDeDiasEmCadaMesDeUmAno ano)) + (ano-2001) * 365 + (ano-2001) `div` 4
{-essa função recebe uma data de entrada (dia,mês,ano) e devolve o número de dias de 2001 até o ano
indicado, usando a combinação da função numDeDiasEmCadaMesDeUmAno e da bissexto. Então ela pega o 
número de dias da data, mais os dias dos outros meses daquele ano, mais os dias dos anos entre 2001
e o indicado, e soma tudo-}

nomeDoDia :: Int -> String
nomeDoDia x
 |x == 0 = "Domingo"
 |x == 1 = "Segunda"
 |x == 2 = "Terca"
 |x == 3 = "Quarta"
 |x == 4 = "Quinta"
 |x == 5 = "Sexta"
 |x == 6 = "Sabado"
 |otherwise = nomeDoDia (x-7)
{-essa função vai indicar o nome do dia dado um valor entre 0 e 6, ou seja, os sete dias da semana-}
 
diaDaSemana :: Data -> String
diaDaSemana (dia,mes,ano)          
  | dia <= head(reverse (take mes (numDeDiasEmCadaMesDeUmAno ano))) && (mes <= 12) = nomeDoDia (numDeDias (dia,mes,ano))
  | otherwise = "Data invalida"
{-por fim, essa função combina as funções nomeDoDia e numDeDias, para descobrir por meio da entrada
de uma data, qual o dia da semana. Isso é feito calculando o módulo do número de dias daquela data 
até 2001 e o valor 7, sendo o resultado comparado a um dos valores da função nomeDoDia.-}

{-diaDaSemana :: Data -> String
diaDaSemana (dia,mes,ano) 
  | dia <= head(reverse (take mes(numDeDiasEmCadaMesDeUmAno ano))) && (mes <= 12) = nomeDoDia(numDeDias(dia,mes,ano))
  | otherwise = "Data invalida"-}
-}