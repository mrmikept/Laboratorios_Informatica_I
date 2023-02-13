module Tarefa1_2022li1g127_Spec where

import LI12223
import Tarefa1_2022li1g127
import Test.HUnit

mapa1 = (Mapa 5 [(Rio 5, [Tronco,Nenhum,Tronco,Nenhum,Nenhum]),
                 (Relva, [Arvore,Arvore,Arvore,Nenhum,Arvore]),
                 (Estrada 2, [Carro,Carro,Carro,Nenhum,Carro]),
                 (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Nenhum]),
                 (Rio (-2), [Tronco, Nenhum, Tronco, Tronco, Nenhum])])--Exemplo Mapa Valido

mapa2 = (Mapa 2 [(Estrada 4, [Arvore, Nenhum]),
                 (Rio (-4), [Tronco, Nenhum])]) --Exemplo Mapa Invalido por obstaculos em terrenos improprios

mapa3 = (Mapa 3 [(Estrada 2, [Carro, Carro, Carro]),
                 (Estrada 3, [Carro, Nenhum, Tronco]),
                 (Rio 2,[Tronco, Tronco, Tronco])]) -- Mapa Invalido por ausencia de Obstaculos 'Nenhum'

mapa4 = (Mapa 7 [(Rio 3, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum]),
                 (Estrada (-10), [Carro, Carro, Carro, Carro, Nenhum])]) --Mapa Invalido por "comprimento de obstaculos"

mapa5 = (Mapa 2 [(Rio 3, [Tronco, Nenhum]),
                 (Rio 4, [Nenhum, Nenhum])]) --Mapa Invalido por rios contiguos na mesma direção

mapa6 = (Mapa 3 [(Rio 3, [Tronco, Nenhum,Tronco]),
                 (Rio (-5), [Nenhum, Nenhum,Tronco]),
                 (Rio 1, [Nenhum, Tronco,Nenhum]),
                 (Rio (-2), [Nenhum, Tronco,Nenhum]),
                 (Rio 7, [Nenhum, Tronco,Nenhum])]) --Mapa Invalido por ter 5 rios contiguos

mapa7 = (Mapa 3 [(Rio 3, [Tronco, Nenhum,Tronco]),
                 (Rio (-5), [Nenhum, Nenhum,Tronco]),
                 (Rio 1, [Nenhum, Tronco,Nenhum]),
                 (Rio (-2), [Nenhum, Tronco,Nenhum]),
                 (Estrada 7, [Nenhum, Carro,Nenhum])]) --Mapa Valido

mapa8 = (Mapa 3 [(Rio 3, [Tronco, Nenhum,Tronco]),
                 (Rio (-5), [Nenhum, Nenhum,Tronco]),
                 (Rio 1, [Nenhum, Tronco,Nenhum]),
                 (Rio (-2), [Nenhum, Tronco,Nenhum])]) --Mapa Valido

mapa9 = (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                 (Rio 1, [Nenhum,Tronco,Tronco]),
                 (Relva, [Nenhum, Arvore, Nenhum])]) --Mapa Valido

mapa10 = (Mapa 2 [(Rio 1, [Nenhum,Tronco]),
                  (Rio (-1), [Nenhum,Tronco]),
                  (Rio 1, [Nenhum,Tronco]),
                  (Rio (-3), [Nenhum,Tronco]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro])]) --Mapa Invalido por numero de estradas contiguas



testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1: mapa1 na função mapaValido" ~: True ~=? mapaValido(mapa1),
                                              "Teste 2: mapa2 na função mapaValido" ~: False ~=? mapaValido(mapa2),
                                              "Teste 3: mapa3 na função mapaValido" ~: False ~=? mapaValido(mapa3),
                                              "Teste 4: mapa4 na função mapaValido" ~: False ~=? mapaValido(mapa4),
                                              "Teste 5: mapa5 na função mapaValido" ~: False ~=? mapaValido(mapa5),
                                              "Teste 6: mapa6 na função mapaValido" ~: False ~=? mapaValido(mapa6),
                                              "Teste 7: mapa7 na função mapaValido" ~: True ~=? mapaValido(mapa7),
                                              "Teste 8: mapa8 na função mapaValido" ~: True ~=? mapaValido(mapa8),
                                              "Teste 9: mapa9 na função mapaValido" ~: True ~=? mapaValido(mapa9),
                                              "Teste 10: mapa10 na função mapaValido" ~: False ~=? mapaValido(mapa10),
                                              "Teste 11: mapa1 na função tamanhoValido" ~: True ~=? tamanhoValido(mapa1),
                                              "Teste 12: mapa2 na função tamanhoValido" ~: True ~=? tamanhoValido(mapa2),
                                              "Teste 13: mapa4 na função tamanhoValido" ~: False ~=? tamanhoValido(mapa4),
                                              "Teste 14: mapa2 na função verificaObstaculo" ~: False ~=? verificaObstaculo(mapa4),
                                              "Teste 15: mapa3 na função verificaObstaculo" ~: False ~=? verificaObstaculo(mapa3),
                                              "Teste 16: mapa4 na função verificaObstaculo" ~: False ~=? verificaObstaculo(mapa4),
                                              "Teste 17: mapa9 na função verificaObstaculo" ~: True ~=? verificaObstaculo(mapa9)]
