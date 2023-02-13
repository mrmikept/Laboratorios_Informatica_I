{- |
Module      : Tarefa5_2022li1g127
Description : Deslize do Mapa
Copyright   : Diego Alejandro Guzmán Rios <a98425@alunos.uminho.pt>
              Mike Stephane Melendes Pinto <a89292@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}

module Tarefa5_2022li1g127 where

import LI12223
import Tarefa2_2022li1g127

{-|A função 'deslizaJogo' recebe um "número aleatório" e um 'Jogo' e devolve um 'Jogo' com as coordenadas de posição do Jogador atualizadas e retira do 'Mapa' o ultimo elemento da lista de tuplos de [('Terreno', ['Obstaculo'])] e no fim com o auxílio da função 'estendeMapa' adiciona um novo terreno na cabeça da lista.

== Exemplos de utilizacao:
>>> deslizaJogo 35 (Jogo (Jogador (0,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Arvore]),(Estrada 3, [Carro, Nenhum, Nenhum]),(Relva, [Arvore, Arvore, Nenhum])]))
Jogo (Jogador (0,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum,Nenhum])])
-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo al (Jogo (Jogador (x,y)) (Mapa tam l)) = Jogo (Jogador (x,y+1)) (estendeMapa(Mapa tam (init(l)))(al))