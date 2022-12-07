--Anna Letycia Fernandes Reis
--12011BSI235

--Trabalho PF 2 usando funções de ordem superior

{-por meio da construção type, são feitas algumas declarações de sinônimos para facilitar a leitura e o desenvolvimento do código-}
type Nome = String
type Preco = Int
type CodBar = Int

type BaseDeDados = [(CodBar,Nome,Preco)]

type ListaDeCodigos = [CodBar]
type Recibo = [(Nome,Preco)]

{-função que simplifica a base de dados dos produtos, especificando código de barra, nome e preço de cada item-}
listaDeProdutos :: BaseDeDados
listaDeProdutos = [(1234, "Oleo DoBom, 1l" , 195), (4756, "Chocolate Cazzeiro, 250g" , 180), (3216, "Arroz DoBom, 5Kg", 213), (5823, "Balas Pedregulho, 1Kg" , 379), (4719, "Queijo Mineirim, 1Kg" , 449), (6832, "Iogurte Maravilha, 1Kg" , 499), (1112, "Rapadura QuebraDente, 1Kg", 80),(1111, "Sal Donorte, 1Kg", 221), (1113, "Cafe  DoBom, 1Kg", 285),(1115, "Biscoito Bibi, 1Kg", 80), (3814, "Sorvete QGelo, 1l", 695)]

{-admitindo que o comprimento de cada linha no recibo seja de 30 caracteres, forma-se a tamLinha-}
tamLinha :: Int
tamLinha = 30

{-essa função vai receber o preço do produto e convertê-lo em uma string, mediante o comando show, do preço em reais. Tal que, x é o quociente da divisão e resto é o resto, essa separação é necessária para representar os centavos-}
formataCentavos :: Preco -> String
formataCentavos preco = show x ++
                        "." ++
                        (if resto < 10 then "0" else "") ++
                        show resto
    where
     x = div preco 100
     resto = mod preco 100

{-Essa função representa apenas um produto em uma só linha e formata para que cada linha tenha 30 caracteres, tal que recebe o recibo e resulta em uma strig da conta do supermercado-}
formataLinha :: (Nome,Preco) -> String
formataLinha (nome,preco) = nome ++
                            (replicate qtdepts '.') ++
                            precoStr ++
                             "\n"
    where
    precoStr = formataCentavos preco
    qtdepts = tamLinha - length nome - length precoStr

{-Já essa função aplica a formataLinha em cada item da lista, ou seja, retorna uma string contendo a conta dos elementos indicados do supermercado-}
formataLinhas :: [(Nome,Preco)] -> String
formataLinhas itens = foldr (++) "" (map formataLinha itens)

{-Recebe o preço total da compra e resulta em uma string contendo a soma da conta do supermercado-}
formataTotal :: Preco -> String
formataTotal preco = "\nTotal" ++  
            (replicate qtdepts '.') ++
             precoStr  
    where
     precoStr = formataCentavos preco
     qtdepts = tamLinha - length "Total" - length precoStr

{-Essa função realiza o calculo do total, localizando o segundo elemento do recibo (preço) pela função map e somando o preço de cada um por meio da função sum-}
geraTotal :: Recibo -> Preco
geraTotal itens = sum (map snd itens)

{-Essa função recebe o recibo dos itens comprados e retorna uma string contendo a conta do supermercado formatada-}
formataRecibo :: Recibo -> String
formataRecibo itens = "Supermercado QLegal\n" ++
                    "\n" ++
                    formataLinhas itens ++
                    formataTotal (geraTotal itens)   

{-Função que será implementada na achaItem, e tem como retorno o par (Nome,Preco) que corresponde ao código de barra na base de dados, tal que cod2 é o código de barras indicado e cod1 os codigos dos itens-}
acha :: BaseDeDados -> CodBar -> (Nome, Preco)
acha [] _ = ("Item desconhecido", 0)
acha ((cod1,nome,preco):resto) cod2    | cod1 == cod2 = (nome,preco)
                                       | otherwise = acha resto cod2

{-Essa função implementa a acha e serve para localizar um item na base de dados-}
achaItem :: CodBar -> (Nome,Preco)
achaItem n = acha listaDeProdutos n

{-A função fazRecibo aplica achaItem para todo item na lista de entrada e retorna o Recibo (Nome,Preco) de cada um-}
fazRecibo :: ListaDeCodigos -> Recibo
fazRecibo itens = map achaItem itens

{-Essa função é definida pela fazRecibo e formataRecibo, e gerando o Recibo estruturado de uma compra de supermercado-}
geraRecibo :: ListaDeCodigos -> String
geraRecibo itens = formataRecibo (fazRecibo itens)

{-Função main alternativa-}
main :: IO ()
main = do
 putStrLn ("Digite os códigos de barras em um lista:")
 x <- getLine
 putStrLn ((geraRecibo (read x :: ListaDeCodigos)))
 putStrLn ("Digite algo para sair")
 y <- getLine
 putStrLn ("End")