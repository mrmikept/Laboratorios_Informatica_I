{- |
Module      : Tarefa3_2022li1g127
Description : Movimentação do personagem e obstáculos
Copyright   : Diego Alejandro Guzmán Rios <a98425@alunos.uminho.pt>
              Mike Stephane Melendes Pinto <a89292@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}

module Tarefa3_2022li1g127 where

import LI12223

{-|A função 'animaJogo' utiliza as 'posicaoJogador' e 'animaMapa' para "animar" um 'Mapa'.

== Exemplos de utilização:
>>> animaJogo(Jogo(Jogador(1,1)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Tronco]), (Relva, [Arvore, Nenhum, Nenhum]), (Estrada (-1), [Carro, Nenhum, Nenhum])])) (Move Direita)
Jogo(Jogador(2,1)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Tronco]), (Relva, [Arvore, Nenhum, Nenhum]), (Estrada (-1), [Carro, Nenhum, Nenhum])])

>>> animaJogo(Jogo(Jogador(1,1)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Tronco]), (Relva, [Arvore, Nenhum, Nenhum]), (Estrada (-1), [Carro, Nenhum, Nenhum])])) (Move Cima)
Jogo(Jogador(1,0)) (Mapa 3 [(Rio 1, [Nenhum, Tronco, Tronco]), (Relva, [Arvore, Nenhum, Nenhum]), (Estrada (-1), [Carro, Nenhum, Nenhum])])
-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo jogador (Mapa larg l)) jogada | verificaMovValido(Mapa larg l)(jogador)(jogada) = (Jogo (posicaoJogador(Mapa larg l)(jogador)(jogada)) (Mapa larg (animaMapa(posicaoJogador(Mapa larg l)(jogador)(jogada))(l))))
                                              | otherwise = (Jogo (posicaoJogador(Mapa larg l)(jogador)(jogada)) (Mapa larg (animaMapa(jogador)(l))))

{-|A função 'moveJogador' move o Jogador uma unidade na direção pretendida.

== Exemplos de utilização:
>>> moveJogador (Jogador (1,1)) (Move Cima)
Jogador (1,0)

>>> moveJogador (Jogador (2,2)) (Move Esquerda)
Jogador (1,2)
-}
moveJogador :: Jogador -> Jogada -> Jogador
moveJogador jogador Parado = jogador
moveJogador (Jogador (x,y)) (Move dir) | dir == Cima = (Jogador (x,y-1))
                                       | dir == Baixo =  (Jogador (x,y+1))
                                       | dir == Esquerda = (Jogador (x-1, y))
                                       | dir == Direita = (Jogador (x+1, y))

{-|A função 'posicaoJogador' move o 'Jogador' no 'Mapa' tendo em conta as restrições de movimento, utilizando as funções auxiliares 'posicaoMapa', 'moveJogadorTronco', 'moveJogador', 'obstaculoDirecao' e 'verificaMovValido'.

== Exemplos de utilização:
>>> posicaoJogador (Mapa 2 [(Relva, [Arvore, Nenhum]), (Relva, [Nenhum, Arvore])] (Jogador (1,0)) (Parado))
Jogador (1,0)

>>> posicaoJogador (Mapa 2 [(Rio (-1), [Nenhum, Tronco]), (Relva, [Nenhum, Arvore])] (Jogador (1,0)) (Parado))
Jogador (0,0)
-}

posicaoJogador :: Mapa -> Jogador -> Jogada -> Jogador
posicaoJogador mapa (Jogador (x,y)) Parado | posicaoMapa(mapa)(x,y) /= Tronco = moveJogador(Jogador (x,y))(Parado) -- ^ Caso onde o Jogador não está num tronco
                                           | otherwise = moveJogadorTronco(mapa)(Jogador (x,y))(Parado) -- ^ Caso onde o Jogador esta em um tronco
posicaoJogador (Mapa larg l) (Jogador (x,y)) (Move dir) | verificaMovDentroMapa(larg)(Jogador (x,y))(Move dir) == False && obstaculo /= Tronco = (Jogador (x,y))
                                                        | verificaMovDentroMapa(larg)(Jogador (x,y))(Move dir) == False && obstaculo == Tronco = moveJogadorTronco(Mapa larg l)(Jogador (x,y))(Parado)
                                                        | obstaculo /= Tronco && obstaculoDir /= Tronco && movValido = moveJogador(Jogador (x,y))(Move dir) -- ^ Caso onde o jogador não esta num rio nem vai para um rio
                                                        | obstaculo /= Tronco && obstaculoDir == Tronco = moveJogadorTronco(Mapa larg l)(Jogador (x,y))(Move dir) -- ^ Caso onde o jogador não esta num rio mas vai para um rio
                                                        | obstaculo == Tronco && movValido = moveJogadorTronco(Mapa larg l)(Jogador (x,y))(Move dir) -- ^ Caso onde o jogador esta em um tronco
                                                        | obstaculo == Tronco && movValido == False = moveJogadorTronco(Mapa larg l)(Jogador(x,y))(Parado)
                                                        | movValido == False = (Jogador (x,y)) -- ^ Caso onde o jogador não esta em um tronco e não se pode mover
                                                      where obstaculo = posicaoMapa(Mapa larg l)(x,y)
                                                            obstaculoDir = obstaculoDirecao(Mapa larg l)(Jogador (x,y))(Move dir)
                                                            movValido = verificaMovValido(Mapa larg l)(Jogador (x,y))(Move dir)

{-|A função 'moveJogadorTronco' move o Jogador na velocidade e na direção do 'Obstaculo' 'Tronco', utilizando as funções auxiliares 'verificaTerrenoCoord', 'velocidadeRio', 'posicaoMapa', 'obstaculoDirecao' e 'verificaMovValido'.

== Exemplos de utilização:
>>> moveJogadorTronco (Mapa 2 [(Rio (-1), [Nenhum, Tronco]), (Relva, [Nenhum, Arvore])] (Jogador (1,0)) (Parado))
Jogador (0,0)

>>> posicaoJogador (Mapa 2 [(Rio (1), [Nenhum, Tronco]), (Relva, [Nenhum, Arvore])] (Jogador (0,0)) (Parado))
Jogador (1,0)
-}

moveJogadorTronco :: Mapa -> Jogador -> Jogada -> Jogador
moveJogadorTronco mapa (Jogador (x,y)) Parado = (Jogador (x+velocidade, y)) where terreno = verificaTerrenoCoord(mapa)(y); velocidade = velocidadeRio(terreno)
moveJogadorTronco mapa (Jogador (x,y)) (Move dir) | dir == Cima && obstaculo /= Tronco && obstaculoDir == Tronco = (Jogador (x,y-1)) -- ^ Caso onde o jogador não esta num tronco mas vai para um tronco
                                                  | dir == Baixo && obstaculo /= Tronco && obstaculoDir == Tronco = (Jogador (x,y+1))
                                                  | obstaculo == Tronco && obstaculoDir /= Tronco && movValido = moveJogador(Jogador (x,y))(Move dir) -- ^ Caso onde o jogador esta em um tronco mas não vai para um tronco
                                                  | obstaculo == Tronco && obstaculoDir /= Tronco && movValido == False = (Jogador (x,y))
                                                  | dir == Cima && obstaculo == Tronco && obstaculoDir == Tronco = (Jogador (x, y-1)) -- ^ Caso onde o jogador esta em um tronco e vai para um tronco
                                                  | dir == Baixo && obstaculo == Tronco && obstaculoDir == Tronco = (Jogador (x, y+1))
                                                  | dir == Cima && obstaculo == Tronco && movValido == False = (Jogador (x, y))
                                                  | dir == Esquerda && obstaculo == Tronco = moveJogador(Jogador(x+velocidade,y))(Move dir)
                                                  | dir == Direita && obstaculo == Tronco = moveJogador(Jogador(x+velocidade,y))(Move dir)
                                                where terreno = verificaTerrenoCoord(mapa)(y)
                                                      terrenoCima = verificaTerrenoCoord(mapa)(y-1)
                                                      terrenoBaixo = verificaTerrenoCoord(mapa)(y+1)
                                                      velocidade = velocidadeRio(terreno)
                                                      velocidadeCima = velocidadeRio(terrenoCima)
                                                      velocidadeBaixo = velocidadeRio(terrenoBaixo)
                                                      obstaculo = posicaoMapa(mapa)(x,y)
                                                      obstaculoDir = obstaculoDirecao(mapa)(Jogador (x,y))(Move dir)
                                                      movValido = verificaMovValido(mapa)(Jogador (x,y))(Move dir)

{-|A função 'obstaculoDirecao' devolve os 'Obstaculos' presentes na posição do Movimento do 'Jogador', utiliza a função auxiliar 'posicaoMapa'.

== Exemplos de utilização:
>>> obstaculoDirecao (Mapa 2 [(Relva, [Nenhum, Arvore]),(Relva, [Nenhum, Arvore])] (Jogador (0,0)) (Parado))
Nenhum

>>> obstaculoDirecao (Mapa 2 [(Relva, [Nenhum, Arvore]),(Relva, [Nenhum, Arvore])] (Jogador (0,0)) (Move Direita))
Arvore
-}
obstaculoDirecao :: Mapa -> Jogador -> Jogada -> Obstaculo
obstaculoDirecao mapa (Jogador coord) Parado = posicaoMapa(mapa)(coord)
obstaculoDirecao mapa (Jogador (x,y)) (Move dir) | dir == Cima = posicaoMapa(mapa)(x,y-1)
                                                 | dir == Baixo = posicaoMapa(mapa)(x,y+1)
                                                 | dir == Esquerda = posicaoMapa(mapa)(x-1,y)
                                                 | dir == Direita = posicaoMapa(mapa)(x+1,y)

{-|A função 'velocidadeRio' devolve a velocidade de um 'Terreno' 'Rio'.

== Exemplos de utilização:
>>> velocidadeRio (Rio 3, _)
3

>>> velocidadeRio (Rio (-2), _)
-2
-}
velocidadeRio :: (Terreno, [Obstaculo]) -> Int
velocidadeRio (Rio velocidade, _) = velocidade

{-|A função 'animaMapa' anima os 'Obstaculo' de um 'Mapa' conforme a sua velocidade de direção.

== Exemplos de utilização:
>>> animaMapa _ []
[]

>>> animaMapa _ [(Rio 1, [Tronco, Tronco, Nenhum])]
[Tronco, Nenhum, Tronco]

>>> animaMapa (Jogador (1,0)) [(Estrada 2, [Carro, Nenhum, Nenhum])]
[Nenhum, Carro, Nenhum]
-}
animaMapa :: Jogador -> [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
animaMapa _ [] = []
animaMapa (Jogador (x,y)) ((Relva, obstaculo):t) = (Relva, obstaculo):animaMapa(Jogador (x,y-1))(t)
animaMapa (Jogador (x,y)) ((Rio velocidade, l):t) = ((Rio velocidade, animaObstaculo(l)(velocidade)):animaMapa(Jogador (x,y-1))(t))
animaMapa (Jogador (x,y)) ((Estrada velocidade, l):t) | y == 0 = ((Estrada velocidade, animaEstrada(x)(velocidade)(l)):animaMapa(Jogador (x,y-1))(t))
                                                      | otherwise = ((Estrada velocidade, animaObstaculo(l)(velocidade)):animaMapa(Jogador (x,y-1))(t))

{-|A função 'animaEstrada' move os obstaculos do 'Terreno' 'Estrada' tendo em conta a posição do 'Jogador'.

== Exemplos de utilização:
>>> animaEstrada _ _ []
[]

>>> animaEstrada 1 2 [Carro, Nenhum, Nenhum]
[Nenhum, Carro, Nenhum]

>>>animaEstrada 0 1 [Nenhum, Carro, Nenhum]
[Nenhum, Nenhum, Carro]
-}
animaEstrada :: Int -> Int -> [Obstaculo] -> [Obstaculo]
animaEstrada posJogador vel [] = []
animaEstrada posJogador vel l | (l !! posJogador) == Carro = l
                              | vel == 0 = l
                              | vel < 0 = animaEstrada(posJogador)(vel+1)(tail(l)++[head l])
                              | vel > 0 = animaEstrada(posJogador)(vel-1)(last(l):init(l))

{-|A função 'animaObstaculo' move os 'Obstaculo' conforme a velocidade do 'Terreno'.

== Exemplos de utilização:
>>> animaObstaculo [Carro, Carro, Nenhum] 0
[Carro, Carro, Nenhum]

>>> animaObstaculo [Tronco, Tronco, Nenhum] 1
[Tronco, Nenhum, Tronco]
-}
animaObstaculo :: [Obstaculo] -> Int -> [Obstaculo]
animaObstaculo [] _ = []
animaObstaculo l vel | vel == 0 = l
                     | vel < 0 = animaObstaculo(tail(l)++[head l])(vel+1)
                     | vel > 0 = animaObstaculo(last(l):init(l))(vel-1)

{-|A função 'verificaMovValido' verifica se um dado movimento do jogador é valido, utiliza as funções auxiliares 'verificaMovDentroMapa', e 'verificaObsPorMov'.

== Exemplos de utilização:
>>> verificaMovValido (Mapa 2 [(Relva, [Nenhum, Arvore]),(Relva, [Nenhum, Arvore])] (Jogador (0,0)) (Move Baixo))
True

>>> verificaMovValido (Mapa 2 [(Relva, [Nenhum, Arvore]),(Relva, [Nenhum, Arvore])] (Jogador (0,0)) (Move Direita))
False
-}
verificaMovValido :: Mapa -> Jogador -> Jogada -> Bool
verificaMovValido (Mapa larg l) jogador jogada = verificaMovDentroMapa(larg)(jogador)(jogada) && verificaObsPorMov(Mapa larg l)(jogador)(jogada)

{-|A função 'verificaObsPorMov' verifica se o 'Jogador' não se movimenta para um 'Obstaculo' 'Arvore', utilizando a função auxiliar 'posicaoMapa'.

== Exemplos de utilização:
>>> verificaObsPorMov _ _ Parado
True

>>> verificaObsPorMov (Mapa 2 [(Relva, [Nenhum, Arvore]), (Relva, [Nenhum, Arvore])] (Jogador (0,0))) (Move Direita)
False
-}
verificaObsPorMov :: Mapa -> Jogador -> Jogada -> Bool
verificaObsPorMov _ _ Parado = True
verificaObsPorMov mapa (Jogador (x,y)) (Move dir) | dir == Cima && posicaoMapa(mapa)(x,y-1) /= Arvore = True
                                                  | dir == Baixo && posicaoMapa(mapa)(x,y+1) /= Arvore = True
                                                  | dir == Esquerda && posicaoMapa(mapa)(x-1,y) /= Arvore = True
                                                  | dir == Direita && posicaoMapa(mapa)(x+1,y) /= Arvore = True
                                                  | otherwise = False

{-|A função 'posicaoMapa' devolve um 'Obstaculo' conforme uma coordenada do mapa, utiliza as funções auxiliares 'verificaObstaculoCoord', e 'verificaTerrenoCoord'.

== Exemplos de utilização:
>>> posicaoMapa (Mapa 2 [(Relva, [Arvore, Nenhum]), (Relva, [Nenhum, Arvore])] (0,0))
Arvore

>>> posicaoMapa (Mapa 2 [(Relva, [Arvore, Nenhum]), (Rio _, [Nenhum, Tronco])] (1,1))
Tronco
-}
posicaoMapa :: Mapa -> Coordenadas -> Obstaculo
posicaoMapa mapa (x,y) = verificaObstaculoCoord(verificaTerrenoCoord(mapa)(y))(x)

{-|A função 'verificaTerrenoCoord' devolve um tuplo ('Terreno', ['Obstaculo']) conforme uma 'Coordenadas' do 'Mapa'.

== Exemplos de utilização:
>>> verificaTerrenoCoord (Mapa 2 [(Relva, [Nenhum, Arvore]), (Relva, [Nenhum, Arvore])]) 0
(Relva, [Nenhum, Arvore])

>>> verificaTerrenoCoord (Mapa 2 [(Relva, [Nenhum, Arvore]), (Relva, [Nenhum, Arvore])]) 1
(Relva, [Nenhum, Arvore])
-}
verificaTerrenoCoord :: Mapa -> Int -> (Terreno, [Obstaculo])
verificaTerrenoCoord (Mapa larg []) _ = (Relva, [Nenhum])
verificaTerrenoCoord (Mapa larg (h:t)) count |count > 0 = verificaTerrenoCoord(Mapa larg t)(count-1)
                                             |count <= 0 = h


{-|A função 'verificaObstaculoCoord' dado um tuplo ('Terreno', ['Obstaculo']) devolve um 'Obstaculo' conforme as 'Coordenadas' do 'Mapa'.

== Exemplos de utilização:
>>> verificaObstaculoCoord (Relva, [Nenhum, Arvore]) 0
Nenhum

>>> verificaObstaculoCoord (Relva, [Nenhum, Arvore]) 1
Arvore
-}
verificaObstaculoCoord :: (Terreno, [Obstaculo]) -> Int -> Obstaculo
verificaObstaculoCoord (_, []) _ = Nenhum
verificaObstaculoCoord (terreno, (h:t)) count | count > 0 = verificaObstaculoCoord(terreno, t)(count-1)
                                              | count <= 0 = h

{-|A função 'verificaMovDentroMapa' verifica se o 'Jogador' se movimenta dentro dos limites de um 'Mapa'.

== Exemplos de utilização:
>>> verificaMovDentroMapa 2 (Jogador (2,2)) (Move Direita)
False

>>> verificaMovDentroMapa 2 (Jogador (1,1)) Parado
True
-}
verificaMovDentroMapa :: Int -> Jogador -> Jogada -> Bool
verificaMovDentroMapa _ _ Parado = True
verificaMovDentroMapa larg (Jogador (x,y)) (Move dir) | dir == Cima && y > 0 = True
                                                      | dir == Baixo && y < (larg - 1) = True
                                                      | dir == Esquerda && x > 0 = True
                                                      | dir == Direita && x < (larg -1) = True
                                                      | otherwise = False