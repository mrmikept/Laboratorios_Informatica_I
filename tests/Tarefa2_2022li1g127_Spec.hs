module Tarefa2_2022li1g127_Spec where

import LI12223
import Tarefa2_2022li1g127
import Test.HUnit

mapa1 = (Mapa 5 [(Rio 5, [Tronco,Nenhum,Tronco,Nenhum,Nenhum]),
                 (Relva, [Arvore,Arvore,Arvore,Nenhum,Arvore]),
                 (Estrada 2, [Carro,Carro,Carro,Nenhum,Carro]),
                 (Estrada 1, [Carro, Nenhum, Carro, Nenhum, Nenhum]),
                 (Rio (-2), [Tronco, Nenhum, Tronco, Tronco, Nenhum])]) --Exemplo Mapa Valido

mapa2 = (Mapa 3 [(Rio 3, [Tronco, Nenhum,Tronco]),
                 (Rio (-5), [Nenhum, Nenhum,Tronco]),
                 (Rio 1, [Nenhum, Tronco,Nenhum]),
                 (Rio (-2), [Nenhum, Tronco,Nenhum]),
                 (Estrada 7, [Nenhum, Carro,Nenhum])]) --Mapa Valido

mapa3 = (Mapa 2 [ (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Estrada 1, [Nenhum, Carro]),
                  (Rio (-1), [Nenhum,Tronco]),
                  (Rio 1, [Nenhum,Tronco]),
                  (Rio (-3), [Nenhum,Tronco]),
                  (Rio 1, [Nenhum,Tronco])]) --Mapa Valido

mapa4 = (Mapa 3 [(Rio 3, [Tronco, Nenhum,Tronco]),
                 (Rio (-5), [Nenhum, Nenhum,Tronco]),
                 (Relva, [Nenhum, Nenhum, Arvore]),
                 (Rio 1, [Nenhum, Tronco,Nenhum]),
                 (Rio (-2), [Nenhum, Tronco,Nenhum])]) --Mapa Valido

mapa5 = (Mapa 5 [])

terreno1 = (Rio 3, [])
terreno2 = (Estrada 1, [])
terreno3 = (Relva, [])
terreno4 = (Rio 4, [Tronco, Tronco, Tronco, Tronco, Tronco])
terreno5 = (Estrada 2 ,[Carro, Carro, Carro])
terreno6 = (Relva, [Nenhum, Tronco, Nenhum])


testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1: mapa1 na função proximosTerrenosValidos" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos(mapa1),
                                              "Teste 2: mapa2 na função proximosTerrenosValidos" ~: [Relva, Estrada 0] ~=? proximosTerrenosValidos(mapa2),
                                              "Teste 3: mapa3 na função proximosTerrenosValidos" ~: [Rio 0, Relva] ~=? proximosTerrenosValidos(mapa3),
                                              "Teste 4: mapa4 na função proximosTerrenosValidos" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos(mapa4),
                                              "Teste 5: mapa5 na função proximosTerrenosValidos" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos(mapa5),
                                              "Teste 5: terreno1 na função proximosObstaculosValidos" ~: [Tronco,Nenhum] ~=? proximosObstaculosValidos(3)(terreno1),
                                              "Teste 6: terreno2 na função proximosObstaculosValidos" ~: [Carro,Nenhum] ~=? proximosObstaculosValidos(3)(terreno2),
                                              "Teste 7: terreno3 na função proximosOBstaculosValidos" ~: [Arvore, Nenhum] ~=? proximosObstaculosValidos(3)(terreno3),
                                              "Teste 8: terreno4 na função proximosOBstaculosValidos" ~: [Nenhum] ~=? proximosObstaculosValidos(6)(terreno4),
                                              "Teste 9: terreno5 na função proximosOBstaculosValidos" ~: [Nenhum] ~=? proximosObstaculosValidos(4)(terreno5),
                                              "Teste 10: terreno6 na função proximosOBstaculosValidos"~: [] ~=? proximosObstaculosValidos(3)(terreno5)]
