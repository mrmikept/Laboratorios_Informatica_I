{- |
Module      : Tarefa4_2022li1g127
Description : Determinar se o jogo terminou
Copyright   : Diego Alejandro Guzmán Rios <a98425@alunos.uminho.pt>
              Mike Stephane Melendes Pinto <a89292@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g127 where

import LI12223
import Tarefa3_2022li1g127

{-|A função 'jogoTerminou' é utilizada para saber quando o 'Jogo' termina.

== Exemplos de utilizacao:
>>> jogoTerminou (Jogo(Jogador(1,1)) Mapa 2 [(Relva, [Arvore, Nenhum]), (Relva, [Arvore, Nenhum])])
False

>>> jogoTerminou (Jogo(Jogador(1,1)) Mapa 2 [(Estrada 2, [Nenhum, Carro]), (Relva, [Arvore, Nenhum])])
True
-}
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) mapa) | verificaForaMapa(Jogo (Jogador (x,y)) mapa) = True
                                         | verificaPosJogadorEObstaculo(terreno)(obstaculo) = True
                                         | otherwise = False
                                where terreno = fst(verificaTerrenoCoord(mapa)(y))
                                      obstaculo = posicaoMapa(mapa)(x,y)

{-|A função 'verificaForaMapa' verifica se o 'Jogador' saiu do 'Mapa'.

== Exemplos de utilizacao:
>>> verificaForaMapa (Jogo(Jogador (3,2)) (Mapa 2 _))
True

>>> verificaForaMapa (Jogo(Jogador (2,2)) (Mapa 2 _))
False
-}
verificaForaMapa :: Jogo -> Bool
verificaForaMapa (Jogo (Jogador (x,y)) (Mapa larg l)) | x < 0 || y < 0 = True
                                                      | x >= larg || y >= length(l) = True
                                                      | otherwise = False

{-|A função 'verificaPosJogadorEObstaculo' verifica a posição do 'Jogador' e do 'Obstaculo'.

== Exemplos de utilizacao:
>>> verificaPosJogadorEObstaculo Relva _
False

>>> verificaPosJogadorEObstaculo (Estrada 2) Carro
True
-}
verificaPosJogadorEObstaculo :: Terreno -> Obstaculo -> Bool
verificaPosJogadorEObstaculo Relva _ = False
verificaPosJogadorEObstaculo (Estrada velocidade) obstaculo | obstaculo == Carro = True
                                                            | otherwise = False
verificaPosJogadorEObstaculo (Rio velocidade) obstaculo | obstaculo == Nenhum = True
                                                        | otherwise = False

