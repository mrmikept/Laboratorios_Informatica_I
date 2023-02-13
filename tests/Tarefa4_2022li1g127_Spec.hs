module Tarefa4_2022li1g127_Spec where

import LI12223
import Tarefa4_2022li1g127
import Tarefa3_2022li1g127
import Test.HUnit

jogo1 = (Jogo (Jogador (1,2)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum]),
                                       (Rio (-1), [Tronco, Nenhum, Tronco, Nenhum, Nenhum])]))

jogo2 = (Jogo (Jogador (0,3)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum]),
                                       (Rio (-1), [Tronco, Nenhum, Tronco, Nenhum, Nenhum])]))

jogo3 = (Jogo (Jogador ((-1),3)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum]),
                                       (Rio (-1), [Tronco, Nenhum, Tronco, Nenhum, Nenhum])]))

jogo4 = (Jogo (Jogador (1,3)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum]),
                                       (Rio (-1), [Tronco, Nenhum, Tronco, Nenhum, Nenhum])]))

jogo5 = (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo6 = (Jogo (Jogador (1,2)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo7 = (Jogo (Jogador (0,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                                       (Estrada 1, [Nenhum, Carro, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo8 = (Jogo (Jogador (0,3)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                                       (Estrada 1, [Nenhum, Carro, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1: jogo1 na função jogoTerminou" ~: True ~=? jogoTerminou(jogo1),
                                              "Teste 2: animaJogo de jogo1 na função jogoTerminou" ~: True ~=? jogoTerminou(animaJogo(jogo1)(Parado)),
                                              "Teste 3: jogo3 na função jogoTerminou" ~: True ~=? jogoTerminou(jogo3),
                                              "Teste 4: jogo4 na função jogoTerminou" ~: True ~=? jogoTerminou(jogo4),
                                              "Teste 5: jogo5 na função jogoTerminou" ~: False ~=? jogoTerminou(jogo5),
                                              "Teste 6: jogo6 na função jogoTerminou" ~: False ~=? jogoTerminou(jogo6),
                                              "Teste 7: jogo5 na função jogoTerminou" ~: False ~=? jogoTerminou(jogo5),
                                              "Teste 8: jogo7 na função jogoTerminou" ~: False ~=? jogoTerminou(jogo7),
                                              "Teste 8: jogo8 na função jogoTerminou" ~: True ~=? jogoTerminou(jogo8)]
