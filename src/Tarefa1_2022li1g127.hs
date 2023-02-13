{- |
Module      : Tarefa1_2022li1g127
Description : Validação de um mapa
Copyright   : Diego Alejandro Guzmán Rios <a98425@alunos.uminho.pt>
              Mike Stephane Melendes Pinto <a89292@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g127 where

import LI12223

{-|A função 'tamanhoValido' verifica se o 'Mapa' possui um tamanho válido e se a quantidade de 'Obstaculos' corresponde ao comprimento do 'Mapa'.

== Exemplos de utilização:
>>> tamanhoValido Mapa 2 [(Relva, [Nenhum, Nenhum]), (Relva, [Nenhum, Nenhum])]
True

>>> tamanho Valido Mapa 1 [(Relva, [Nenhum, Nenhum]), (Relva, [Nenhum, Nenhum])]
False
-}
tamanhoValido :: Mapa -> Bool
tamanhoValido (Mapa larg []) = True
tamanhoValido (Mapa larg ((_, obstaculos):t)) = (larg == length(obstaculos)) && tamanhoValido (Mapa larg t)

{-|A função 'relvaContiguos' confirma que se quantidade de 'Terrenos' 'Relva' seguidos está dentro dos limites estabelecidos(neste caso, 5).

== Exemplos de utilização:
>>> relvaContiguos _ (Mapa 8 [(Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _, [_]),])
True

>>> relvaContiguos _ (Mapa 8 [(Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva _, [_]), (Relva _, [_]), (Rio _, [_]),])
False
-}
relvaContiguos :: Int -> Mapa -> Bool
relvaContiguos _ (Mapa _ []) = True
relvaContiguos count (Mapa larg ((terreno, _):t)) | terreno /= Relva = True
                                                  | count > 4 = False
                                                  | otherwise = relvaContiguos (count+1) (Mapa larg t)

{-|A função 'estradaContiguos' confirma que a quantidade de 'Terrenos' 'Estrada' seguidos está dentro dos limites estabelecidos(neste caso, 4).

== Exemplos de utilização:
>>> estradaContiguos _ (Mapa 8 [(Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_]), (Relva, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _, [_]),])
True

>>> estradaContiguos _ (Mapa 8 [(Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_]), (Estrada _, [_]), (Estrada _ _, [_]), (Relva _, [_]), (Rio _, [_]),])
False
-}
estradaContiguos :: Int -> Mapa -> Bool
estradaContiguos _ (Mapa _ []) = True
estradaContiguos _ (Mapa _ ((Relva, _):t)) = True
estradaContiguos _ (Mapa _ ((Rio velocidade, _):t)) = True
estradaContiguos count (Mapa larg ((Estrada velocidade, _):t)) |count > 4 = False
                                                               |otherwise =estradaContiguos (count+1) (Mapa larg t)

{-|A função 'riosContiguos' confirma que a quantidade de 'Terrenos' 'Rio' seguidos está dentro dos limites estabelecidos(neste caso, 4).

== Exemplos de utilização:
>>> riosContiguos _ (Mapa 8 [(Rio _, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _, [_]), (Relva, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _, [_]),])
True

>>> riosContiguos _ (Mapa 8 [(Rio _, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _, [_]), (Rio _ _, [_]), (Relva _, [_]), (Rio _, [_]),])
False
-}
riosContiguos :: Int -> Mapa -> Bool
riosContiguos _ (Mapa _ []) = True
riosContiguos _ (Mapa _ ((Relva, _):t)) = True
riosContiguos _ (Mapa _ ((Estrada velocidade, _):t)) = True
riosContiguos count (Mapa larg ((Rio velocidade, _):t)) |count > 3 = False
                                                        |otherwise =riosContiguos (count+1) (Mapa larg t)
                                                               
{-|A função 'terrenosContiguos' utiliza as funções 'relvaContiguos', 'estradaContiguos' e 'riosContiguos' para verificar que a quantidade de 'Terrenos' seguidos estão dentro dos limites estabelecidos.

== Exemplos de utilização:
>>> terrenosContiguos Mapa 6 [(Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Rio _, [_])]
True

>>> terrenosContiguos Mapa 6 [(Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_]), (Relva, [_])]
False
-}
terrenosContiguos :: Mapa -> Bool
terrenosContiguos (Mapa _ []) = True
terrenosContiguos (Mapa larg ((Relva, _):t)) = relvaContiguos(1)(Mapa larg t) && terrenosContiguos(Mapa larg t)
terrenosContiguos (Mapa larg ((Estrada velocidade, _):t)) = estradaContiguos(1)(Mapa larg t) && terrenosContiguos(Mapa larg t)
terrenosContiguos (Mapa larg ((Rio velocidade, _):t)) = riosContiguos(1)(Mapa larg t) && terrenosContiguos(Mapa larg t)

{-|A função 'mapaValido' utiliza as funções 'tamanhoValido', 'verificaObstaculo' e 'terrenosContiguos' para verificar se um 'Mapa' é considerado válido.

== Exemplos de utilização:
>>> mapaValido Mapa 2 [(Relva, [Nenhum, Arvore]), (Rio 2, [Tronco, Nenhum])]
True

>>> mapaValido Mapa 2 [(Relva, [Carro, Tronco]), (Rio 3, [Nenhum, Nenhum, Arvore]), (Rio 5, [Nenhum, Nenhum])]
False
-}
mapaValido :: Mapa -> Bool
mapaValido mapa = tamanhoValido(mapa) && verificaObstaculo(mapa) && terrenosContiguos(mapa)

{-|A função 'riosDirecao' verifica que se dois 'Rio' seguidos têm direções opostas.

== Exemplos de utilização:
>>> riosDirecao _ (Mapa 2 [(Rio 2, [Nenhum, Tronco]), (Rio (-3), [Tronco, Nenhum])])
True

>>> riosDirecao _ (Mapa 2 [(Rio (-2), [Nenhum, Tronco]), (Rio (-3), [Tronco, Nenhum])])
False
-}
riosDirecao :: Int -> Mapa -> Bool
riosDirecao _ (Mapa _ []) = True
riosDirecao _ (Mapa _ ((Relva, _):t)) = True
riosDirecao _ (Mapa _ ((Estrada velocidade, _):t)) = True
riosDirecao velocidade1 (Mapa _ ((Rio velocidade2, _):t)) |velocidade1 > 0 && velocidade2 < 0 = True
                                                          |velocidade1 < 0 && velocidade2 > 0 = True 
                                                          |otherwise = False

{-| A função 'compTroncos' verifica se comprimento dos 'Obstaculo' 'Tronco' estão dentro do limite estabelecido (neste caso, 5).

== Exemplos de utilização:
>>> compTroncos _ (Mapa 6 [(Rio _, [Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum]), ...])
True

>>> compTroncos _ (Mapa 7 [(Rio _, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum]), ...])
False
-}
compTroncos :: Int -> [Obstaculo] -> Bool
compTroncos _ [] = True
compTroncos count (obstaculo:t) | count >= 5 && obstaculo == Tronco = False
                                | obstaculo == Nenhum = True && compTroncos (0) (t)
                                | otherwise = compTroncos (count+1) (t)

{-| A função 'compTroncos' verifica se comprimento dos 'Obstaculo' 'Carro' estão dentro do limite estabelecido (neste caso, 3).

== Exemplos de utilização:
>>> compCarros _ (Mapa 6 [(Estrada _, [Carro, Carro, Carro, Nenhum, Carro, Nenhum]), ...])
True

>>> compCarros _ (Mapa 7 [(Estrada _, [Carro, Carro, Carro, Carro, Nenhum, Carro, Nenhum]), ...])
False
-}
compCarros :: Int -> [Obstaculo] -> Bool
compCarros _ [] = True
compCarros count (obstaculo:t) | count >= 3 && obstaculo == Carro = False
                               | obstaculo == Nenhum = True && compCarros (0) (t)
                               | otherwise = compCarros (count+1) (t)

{-| A função 'verificaNenhum' verifica a existencia de pelo menos um 'Obstaculo' 'Nenhum' em cada 'Terreno'

== Exemplos de utilização:
>>> verificaNenhum [Arvore, Nenhum, Arvore]
True

>>> verificaNenhum [Arvore, Arvore]
False
-}
verificaNenhum :: [Obstaculo] -> Bool
verificaNenhum [] = False
verificaNenhum (obstaculo:t) |obstaculo == Nenhum = True
                             |otherwise = verificaNenhum(t)

{-|A função 'verificaObstaculo' utiliza as funções 'obstaculosRelva', 'obstaculosEstrada', 'obstaculosRio', 'verificaNenhum', 'compCarros', 'riosDirecao' e 'compTroncos' para verificar se os 'Obstaculo' são válidos, i.e. sejam do comprimento válido, estejam no terreno válido, tenham pelo menos um "Nenhum", entre outros...

== Exemplos de utilização:
>>> verificaObstaculo (Mapa 2 [(Relva, [Arvore, Nenhum]), (Rio 2, [Tronco, Nenhum])])
True

>>> verificaObstaculo (Mapa 2 [(Relva, [Arvore, Arvore]), (Rio 2, [Tronco, Carro])])
False
-}
verificaObstaculo :: Mapa -> Bool
verificaObstaculo (Mapa larg []) = True
verificaObstaculo (Mapa larg ((Relva, obstaculos):t)) = obstaculosRelva(obstaculos) && verificaNenhum(obstaculos) && verificaObstaculo (Mapa larg t)
verificaObstaculo (Mapa larg ((Estrada velocidade, obstaculos):t)) = obstaculosEstrada(obstaculos) && compCarros(0)(obstaculos) && verificaNenhum(obstaculos) && verificaObstaculo (Mapa larg t)
verificaObstaculo (Mapa larg ((Rio velocidade, obstaculos):t)) = obstaculosRio(obstaculos) && riosDirecao(velocidade)(Mapa larg t) && verificaNenhum(obstaculos) && compTroncos(0)(obstaculos) && verificaObstaculo (Mapa larg t)

{-|A função 'obstaculosRelva' verifica se os 'Obstaculo' atuais do 'Terreno' 'Relva' são obstaculos próprios ao 'Terreno'.

== Exemplos de utilização:
>>> obstaculosRelva [Arvore, Nenhum, Arvore]
True

>>> obstaculosRelva [Arvore, Nenhum, Carro]
False
-}
obstaculosRelva :: [Obstaculo] -> Bool
obstaculosRelva [] = True
obstaculosRelva (h:t) | h == Arvore = True && obstaculosRelva(t)
                      | h == Nenhum = True && obstaculosRelva(t)
                      | otherwise = False

{-|A função 'obstaculosEstrada' verifica se os 'Obstaculo' atuais do 'Terreno' 'Estrada' são obstaculos próprios ao 'Terreno'.

== Exemplos de utilização:
>>> obstaculosEstrada [Carro, Nenhum, Carro]
True

>>> obstaculosEstrada [Arvore, Nenhum, Carro]
False
-}
obstaculosEstrada :: [Obstaculo] -> Bool
obstaculosEstrada [] = True
obstaculosEstrada (h:t) | h == Carro = True && obstaculosEstrada(t)
                        | h == Nenhum = True && obstaculosEstrada(t)
                        | otherwise = False

{-|A função 'obstaculosRio' verifica se os 'Obstaculo' atuais do 'Terreno' 'Rio' são obstaculos próprios ao 'Terreno'

== Exemplos de utilização:
>>> obstaculosRio [Tronco, Nenhum, Tronco]
True

>>> obstaculosRio [Arvore, Nenhum, Tronco]
False
-}
obstaculosRio :: [Obstaculo] -> Bool
obstaculosRio [] = True
obstaculosRio (h:t) | h == Tronco = True && obstaculosRio(t)
                    | h == Nenhum = True && obstaculosRio(t)
                    | otherwise = False
