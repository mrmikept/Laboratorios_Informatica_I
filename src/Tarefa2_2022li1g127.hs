{- |
Module      : Tarefa2_2022li1g127
Description : Geração contínua de um mapa
Copyright   : Diego Alejandro Guzmán Rios <a98425@alunos.uminho.pt>
              Mike Stephane Melendes Pinto <a89292@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g127 where

import LI12223
import Tarefa1_2022li1g127

{-|A função 'estendeMapa' cria um novo 'Terreno' aleatorio no mapa utilizando as funções 'escolheTerreno' e 'escolheObstaculo'.

== Exemplos de utilização:
>>> estendeMapa (Mapa 2 [(Relva, [Arvore, Nenhum]), (Rio _, [Nenhum, Tronco])]) _
[(Estrada _, [Carro, Nenhum]), (Rio _, [Nenhum, Tronco])]

>>> estendeMapa (Mapa 2 [(Rio _, [Tronco, Nenhum]), (Rio _, [Nenhum, Tronco])]) _
[(Rio _, [Tronco, Nenhum]), (Rio _, [Nenhum, Tronco])]
-}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa larg l) al = (Mapa larg (((terrenoEscolhido , listaObstaculos):l)))
                            where terrenoEscolhido = escolheTerreno(Mapa larg l)(al)
                                  listaObstaculos = escolheObstaculo(larg)(larg)(al)(escolheTerreno(Mapa larg l)(al))([])


{-|A função 'alteraVelocidade' recebe um 'Terreno' e três inteiros e altera o valor da velocidade dos terrenos usando a divisão dos valores inteiros recebidos.

== Exemplos de utilização:
>>> alteraVelocidade (Rio 0) 35 5
Rio 7

>>> alteraVelocidade (Relva) 25 8
Relva
-}
alteraVelocidade :: Terreno -> Int -> Int -> Int -> Terreno
alteraVelocidade (terreno) maior menor larg |terreno == Relva = terreno
                                            |maior == 0 = alteraVelocidade(terreno)(1)(menor)(larg)
                                            |menor == 0 = alteraVelocidade(terreno)(maior)(1)(larg)
                                            |(maior `div` menor) >= larg = alteraVelocidade(terreno)(maior `div` 2)(menor)(larg)
                                            |maior < menor = alteraVelocidade(terreno)(menor)(maior)(larg)                                       
                                            |terreno == (Estrada 0) = (Estrada (maior `div` menor))
                                            |terreno == (Rio 0) = (Rio (maior `div` menor))

{-|A função 'escolheTerreno' utiliza a ajuda das funções 'proximosTerrenosValidos', 'funcaoValorAleatorio' e 'alteraVelocidade' para escolher um 'Terreno' de uma lista de próximos 'Terrenos' válidos.

== Exemplos de utilização:
>>> escolheTerreno (Mapa 2 [(Relva, [Nenhum, Arvore]), (Relva, [Nenhum, Arvore])]) 22
[(Relva, [Arvore, Nenhum]), (Relva, [Nenhum, Arvore])]

>>> escolheTerreno (Mapa 2 [(Relva, [Nenhum, Arvore]), (Relva, [Nenhum, Arvore])]) 82
[(Rio _, [Tronco, Nenhum]), (Relva, [Nenhum, Arvore])]
-}
escolheTerreno :: Mapa -> Int -> Terreno
escolheTerreno (Mapa larg []) al = alteraVelocidade(listaTValidos !! indice)(larg*2)(al)(larg)
                                    where listaTValidos = proximosTerrenosValidos(Mapa larg [])
                                          tamListaTValidos = (length(listaTValidos))
                                          indice = funcaoValorAleatorio(al)(tamListaTValidos)(tamListaTValidos)
escolheTerreno (Mapa larg ((Relva, l):t)) al = alteraVelocidade(listaTValidos !! indice)(larg*2)(al)(larg)
                                            where listaTValidos = proximosTerrenosValidos(Mapa larg ((Relva, l):t))
                                                  tamListaTValidos = (length(listaTValidos))
                                                  indice = funcaoValorAleatorio(al)(tamListaTValidos)(tamListaTValidos)

escolheTerreno (Mapa larg ((Estrada velocidade, l):t)) al = alteraVelocidade(listaTValidos !! indice)(larg*2)(al)(larg)
                                            where listaTValidos = proximosTerrenosValidos(Mapa larg ((Estrada velocidade, l):t))
                                                  tamListaTValidos = (length(listaTValidos))  
                                                  indice = funcaoValorAleatorio(al)(tamListaTValidos)(tamListaTValidos)

escolheTerreno (Mapa larg ((Rio velocidade, l):t)) al | (listaTValidos !! indice) /= Rio 0 = alteraVelocidade(listaTValidos !! indice)(larg*2)(al)(larg)
                                                      | velocidade < 0 = alteraVelocidade(listaTValidos !! indice)(larg*2)(al)(larg)
                                                      | velocidade > 0 = alteraVelocidade(listaTValidos !! indice)(-larg*2)(al)(larg)
                                                      where listaTValidos = proximosTerrenosValidos(Mapa larg ((Rio velocidade, l):t))
                                                            tamListaTValidos = (length(listaTValidos))  
                                                            indice = funcaoValorAleatorio(al)(tamListaTValidos)(tamListaTValidos)


{-|A função 'escolheObstaculo' utiliza a ajuda das funções 'proximosObstaculosValidos', 'funcaoValorAleatorio' para para escolher um 'Obstaculo' de uma lista de próximos 'Obstaculo' válidos.

== Exemplos de utilização:
>>> escolheObstaculo (2)(1)(_)(Relva)[]
[Nenhum, Arvore]

>>> escolheObstaculo (2)(1)(_)(Rio _)[]
[Nenhum, Tronco]
-}
escolheObstaculo :: Int -> Int -> Int -> Terreno -> [Obstaculo] -> [Obstaculo]
escolheObstaculo larg count al terreno l | count == 0 = l
                                         | count > 0 = escolheObstaculo(larg)(count-1)(al)(terreno)((listaObstaculosValidos !! indice):l)
                                        where listaObstaculosValidos = proximosObstaculosValidos(larg)(terreno, l)
                                              tamListaObstaculos = length(listaObstaculosValidos)
                                              indice = funcaoValorAleatorio(al)(count)(tamListaObstaculos)

{-|A função 'funcaoValorAleatorio' utiliza a função mod para gerar um numero pseudo-aleatorio atráves do tamanho da lista.

== Exemplos de utilização:
>>> funcaoValorAleatorio 8 2 2
0

>>> funcaoValorAleatorio 5 2 3
1
-}
funcaoValorAleatorio :: Int -> Int -> Int -> Int
funcaoValorAleatorio maior menor limite | menor == 0 = 0  
                                        | maior < menor = funcaoValorAleatorio(menor)(maior)(limite)
                                        | (maior `mod` menor) >= limite = funcaoValorAleatorio(maior `mod` menor)(menor)(limite)
                                        | otherwise = maior `mod` menor

{-|A função 'proximosTerrenosValidos' utiliza a ajuda das funções 'relvaContiguos', 'estradaContiguos' e 'riosContiguos' para verificar qual 'Terreno' pode ser gerado dentro das restrições.

== Exemplos de utilização:
>>> proximosTerrenosValidos (Mapa 4 [(Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_])])
[Rio _, Relva]

>>> proximosTerrenosValidos (Mapa 4 [(Rio _, [_]), (Rio _, [_]), (Rio _, [_]), (Estrada _, [_])])
[Rio _, Estrada _, Relva]
-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0,Estrada 0, Relva]
proximosTerrenosValidos (Mapa larg ((Relva, _):t)) | relvaContiguos(2)(Mapa larg t) == False = [Rio 0, Estrada 0]
                                                   | otherwise = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa larg ((Estrada velocidade, _):t)) | estradaContiguos(2)(Mapa larg t) == False = [Rio 0, Relva]
                                                                | otherwise = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa larg ((Rio velocidade, _):t)) | riosContiguos(2)(Mapa larg t) == False = [Relva, Estrada 0]
                                                            | otherwise = [Rio 0, Estrada 0, Relva]

{-|A função 'proximosObstaculosValidos' utiliza a ajuda das funções 'verificaNenhum', 'compTroncos' e 'compCarros' para verificar qual 'Obstaculo' pode ser gerado dentro das restrições.

== Exemplos de utilização:
>>> proximosObstaculosValidos 4 (Relva, [])
[Arvore, Nenhum]

>>> proximosObstaculosValidos 2 (Rio _, [Tronco])
[Nenhum]
-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos larg (Relva, l) | larg == length(l) = []
                                          | larg - length(l) == 1 && verificaNenhum(l) == False = [Nenhum]
                                          | otherwise = [Arvore, Nenhum]
proximosObstaculosValidos larg (Rio velocidade, l) | larg == length(l) = []
                                                   | larg - length(l) == 1 && verificaNenhum(l) == False = [Nenhum]
                                                   | compTroncos(1)(l) == False = [Nenhum]
                                                   | otherwise = [Tronco,Nenhum]
proximosObstaculosValidos larg (Estrada velocidade, l) | larg == length(l) = []
                                                       | larg - length(l) == 1 && verificaNenhum(l) == False = [Nenhum]
                                                       | compCarros(1)(l) == False = [Nenhum]
                                                       | otherwise = [Carro,Nenhum]
