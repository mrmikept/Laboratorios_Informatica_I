module Tarefa3_2022li1g127_Spec where

import LI12223
import Tarefa3_2022li1g127
import Test.HUnit

jogo1 = (Jogo (Jogador (0,0)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Carro, Carro, Nenhum, Nenhum, Nenhum]),
                                       (Rio (-1), [Tronco, Nenhum, Tronco, Nenhum, Nenhum])]))

rParadoJogo1 = (Jogo (Jogador (0,0)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Nenhum, Carro, Carro, Nenhum, Nenhum]),
                                       (Rio (-1), [Nenhum, Tronco, Nenhum, Nenhum, Tronco])]))

rDireitaJogo1 = (Jogo (Jogador (1,0)) (Mapa 5 [(Relva, [Nenhum, Nenhum, Arvore, Nenhum, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Arvore, Arvore, Nenhum]),
                                       (Estrada 1, [Nenhum, Carro, Carro, Nenhum, Nenhum]),
                                       (Rio (-1), [Nenhum, Tronco, Nenhum, Nenhum, Tronco])]))

jogo2 = (Jogo (Jogador (1,2)) (Mapa 3 [(Rio (-1), [Nenhum, Tronco, Nenhum]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

rCimaJogo2 = (Jogo (Jogador (2,1)) (Mapa 3 [(Rio (-1), [Tronco, Nenhum, Nenhum]),
                                       (Rio 1, [Nenhum, Nenhum, Tronco]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo3 = (Jogo (Jogador (1,1)) (Mapa 3 [(Rio (-1), [Nenhum, Tronco, Nenhum]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

rCimaJogo3 = (Jogo (Jogador (0,0)) (Mapa 3 [(Rio (-1), [Tronco, Nenhum, Nenhum]),
                                            (Rio 1, [Nenhum, Nenhum, Tronco]),
                                            (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo4 = (Jogo (Jogador (0,0)) (Mapa 3 [(Rio (-1), [Tronco, Nenhum, Nenhum]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

rParadoJogo4 = (Jogo (Jogador ((-1),0)) (Mapa 3 [(Rio (-1), [Nenhum, Nenhum, Tronco]),
                                                 (Rio 1, [Nenhum, Nenhum, Tronco]),
                                                 (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo5 = (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

rCimaJogo5 = (Jogo (Jogador (2,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum]),
                                               (Rio 1, [Nenhum, Nenhum, Tronco]),
                                               (Relva, [Nenhum, Nenhum, Nenhum])]))

jogo6 = (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),
                                       (Rio 1, [Nenhum, Tronco, Nenhum]),
                                       (Relva, [Nenhum, Nenhum, Nenhum])]))

rCimaJogo6 = (Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),
                                            (Rio 1, [Nenhum, Nenhum, Tronco]),
                                            (Relva, [Nenhum, Nenhum, Nenhum])]))


testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1: jogo1 na função animaJogo Parado" ~: rParadoJogo1 ~=? animaJogo(jogo1)(Parado),
                                              "Teste 2: jogo1 na função animaJogo Move Cima" ~: rParadoJogo1 ~=? animaJogo(jogo1)(Move Cima),
                                              "Teste 3: jogo1 na função animaJogo Move Direita" ~: rDireitaJogo1 ~=? animaJogo(jogo1)(Move Direita),
                                              "Teste 4: jogo1 na função animaJogo Move Esquerda" ~: rParadoJogo1 ~=? animaJogo(jogo1)(Move Esquerda),
                                              "Teste 5: jogo2 na função animaJogo Move Cima" ~: rCimaJogo2 ~=? animaJogo(jogo2)(Move Cima),
                                              "Teste 6: jogo3 na função animaJogo Move Cima" ~: rCimaJogo3 ~=? animaJogo(jogo3)(Move Cima),
                                              "Teste 7: jogo4 na função animaJogo Parado" ~: rParadoJogo4 ~=? animaJogo(jogo4)(Parado),
                                              "Teste 8: jogo4 na função animaJogo Move Cima" ~: rParadoJogo4 ~=? animaJogo(jogo4)(Move Cima),
                                              "Teste 9: jogo5 na função animaJogo Move Cima" ~: rCimaJogo5 ~=? animaJogo(jogo5)(Move Cima),
                                              "Teste 10: jogo6 na função animaJogo Move Cima" ~: rCimaJogo6 ~=? animaJogo(jogo6)(Move Cima)]
