{- |
Module      : Tarefa6_2022li1g127
Description : Interface Gráfica usando Gloss
Copyright   : Diego Alejandro Guzmán Rios <a98425@alunos.uminho.pt>
              Mike Stephane Melendes Pinto <a89292@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module Main where

import LI12223
import Tarefa1_2022li1g127
import Tarefa2_2022li1g127
import Tarefa3_2022li1g127
import Tarefa4_2022li1g127
import Tarefa5_2022li1g127

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import System.Random
import System.Exit
import System.Directory
import Data.Char

-- | Opções usadas nos diferentes menus(Menu Principal, Menu Pausa, Menu Perdeu e Menu Tipo de Jogo)
data Opcao = Jogar -- ^ Identifica se estamos na opção "Jogar"
           | Sair -- ^ Identifica se estamos na opção "Sair"
           | Menu -- ^ Identifica se estamos na opção "Menu Principal"
           | Guardar -- ^ Identifica se estamos na opção "Guardar Jogo"
           | GuardarS -- ^ Identifica se o Jogo foi guardado no Menu Pausa
           | Novo -- ^ Identifica se estamos na opção "Novo Jogo"
           | Carregar -- ^ Identifica se estamos na opção "Carregar Jogo"

-- | Diferentes Menus do Jogo
data Menu = MenuP Opcao -- ^ Identifica que estamos no Menu Principal e qual a "Opcao" atual
          | TipoJ Opcao -- ^ Identifica se estamos no Menu Tipo de Jogo e qual a "Opcao" atual
          | Pausa Opcao -- ^ Identifica se estamos no Menu de Pausa e qual a "Opcao" atual
          | Perdeu Opcao -- ^ Identifica se estamos no Menu de Perdeu o Jogo e qual a "Opcao" atual
          | Instrucoes -- ^ Identifica se estamos no Menu de Instruções do Jogo
          | ModoJogo -- ^ Identifica se estamos no Modo de Jogo

-- | Tempo de duração do Jogo
type Tempo = Float

-- | Pontuação do jogo atual
type Pontuacao = (PontuacaoAtual,NivelAtual)

-- | Pontuação do jogo atual
type PontuacaoAtual = Int

-- | Nível atual do jogador em relação ao mapa(cada terreno novo corresponde a um nível)
type NivelAtual = Int

-- | Melhor Pontuação obtida em todos os jogos.
type MelhorPontuacao = Int

-- | Estado de cada Jogo.
type EstadoJogo = (Tempo, Pontuacao, Jogo, Jogada)

-- | Estado usado no Gloss.
type EstadoGloss = (Menu, EstadoJogo, Int, MelhorPontuacao ,Imagens)

-- | As diferentes texturas usadas no Jogo separadas por tipo.
data Imagens = Texturas
              {menu :: [(String,Picture)],       -- ^ Texturas relativas aos diferentes Menus
               jogador :: [(String,Picture)],    -- ^ Texturas relativas ao Jogador
               relva :: [(Obstaculo,Picture)],   -- ^ Texturas relativas ao terreno Relva
               estrada :: [(Obstaculo,Picture)], -- ^ Texturas relativas ao terreno Estrada
               rio :: [(Obstaculo, Picture)]     -- ^ Texturas relativas ao terreno Rio
              }

-- | Função que devolve um mapa com a primeira linha do jogo.
mapaVazio :: Mapa
mapaVazio = Mapa 10 [(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]

{- | Função que devolve um EstadoJogo de um novo Jogo.
    
Usa as funções mapaInicial e mapaVazio como auxíliares para gerar um novo Jogo
-}
novoJogo :: Int -> EstadoJogo
novoJogo al = (0, (0,0), (Jogo (Jogador (5,4)) (mapaInicial(mapaVazio)(al)(4))), Move Cima)

-- | Função que verifica se existe um ficheiro com a Melhor Pontuação obtida
leMelhorPontuacao :: IO(Int)
leMelhorPontuacao = do
        fileExist <- doesFileExist "MelhorPontuacao.txt"
        saved <- if fileExist then readFile "MelhorPontuacao.txt"
                        else return "0"
        putStrLn $ "MelhorPontuacao" ++ (show saved)
        return (read saved)

-- | Função que gera um Mapa inicial aleatorio
mapaInicial :: Mapa -> Int -> Int -> Mapa
mapaInicial mapa _ 0 = mapa
mapaInicial (Mapa larg l) al count = mapaInicial(estendeMapa(Mapa larg l)(al))(al `div` 2)(count-1)

-- | Função que gera o estado inicial do Jogo
estadoInicial :: Imagens -> Int -> Int -> EstadoGloss
estadoInicial t al melhor = (MenuP Jogar, (0.0, (0,0), (Jogo (Jogador (5,4)) (mapaInicial(mapaVazio)(al)(3))), Move Cima), al, melhor, t)

-- | Função que através de um estado desenha os diferentes menus do Jogo
desenhaEstado :: EstadoGloss -> IO Picture
--Menu
desenhaEstado (MenuP Jogar, (tempo, pnt, _, _), _, _, imag) = return $ fromJust $ lookup "Jogar" $ menu imag
desenhaEstado (MenuP Sair, (tempo, pnt, _, _), _, _, imag) = return $ fromJust $ lookup "Sair" $ menu imag
--ModoJogo
desenhaEstado (ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag)
--Tipo Jogo
desenhaEstado (TipoJ Novo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ fromJust $ lookup "tipoN" $ menu imag
desenhaEstado (TipoJ Carregar, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ fromJust $ lookup "tipoC" $ menu imag
desenhaEstado (TipoJ Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ fromJust $ lookup "tipoM" $ menu imag
--Menu Pausa
desenhaEstado (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "PausaM" $ menu imag]
desenhaEstado (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "PausaG" $ menu imag]
desenhaEstado (Pausa GuardarS, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "PausaG" $ menu imag] ++
                                                                                     desenhaJogoGuardado(imag)                                                                                           
desenhaEstado (Pausa Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "PausaS" $ menu imag]
--Perdeu
desenhaEstado (Perdeu Novo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "perdeuN" $ menu imag] ++
                                                                                     [Translate (10) (95) $ Scale (0.2) (0.2) $ text (show $ fst(pnt))] ++
                                                                                     [Translate (55) (50) $ Scale (0.2) (0.2) $ text (show $ melhor)]
desenhaEstado (Perdeu Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "perdeuM" $ menu imag] ++
                                                                                     [Translate (10) (95) $ Scale (0.2) (0.2) $ text (show $ fst(pnt))] ++
                                                                                     [Translate (55) (50) $ Scale (0.2) (0.2) $ text (show $ melhor)]
desenhaEstado (Perdeu Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ Pictures $ desenhaMapa(ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)(-576,216) ++ 
                                                                                     desenhaBarraJogo(tempo)(pnt)(0,320)(imag) ++
                                                                                     desenhaJogador(jogo)(mov)(imag) ++
                                                                                     [Translate (0) (-35) $ fromJust $ lookup "perdeuS" $ menu imag] ++
                                                                                     [Translate (10) (95) $ Scale (0.2) (0.2) $ text (show $ fst(pnt))]  ++
                                                                                     [Translate (55) (50) $ Scale (0.2) (0.2) $ text (show $ melhor)]                                                                                   
--Instruções
desenhaEstado (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ fromJust $ lookup "inst" $ menu imag                                                                                                                                                           

-- | Função que desenha o Jogador no ecrã conforme a sua posição no Jogo.
desenhaJogador :: Jogo -> Jogada -> Imagens -> [Picture]
desenhaJogador (Jogo (Jogador (x,y)) _) mov imag | mov == (Move Cima) = [Translate ((fromIntegral x * 128)-576) ((-1) * ((fromIntegral y * 128)-216)) $ Scale (5) (4) $ (fromJust $ lookup "Cima" $ jogador imag)]
                                                 | mov == (Move Baixo) = [Translate ((fromIntegral x * 128)-576) ((-1) * ((fromIntegral y * 128)-216)) $ Scale (5) (4) $ (fromJust $ lookup "Baixo" $ jogador imag)]
                                                 | mov == (Move Direita) = [Translate ((fromIntegral x * 128)-576) ((-1) * ((fromIntegral y * 128)-216)) $ Scale (5) (4) $ (fromJust $ lookup "Dir" $ jogador imag)]
                                                 | mov == (Move Esquerda) = [Translate ((fromIntegral x * 128)-576) ((-1) * ((fromIntegral y * 128)-216)) $ Scale (5) (4) $ (fromJust $ lookup "Esq" $ jogador imag)]

-- | Função que desenha a barra de Tempo e Pontuação do jogo atual
desenhaBarraJogo :: Tempo -> Pontuacao -> Coordenadas -> Imagens -> [Picture]
desenhaBarraJogo tempo (pnt,_) (x,y) imag = [Translate (fromIntegral x) (fromIntegral y) $ (fromJust $ lookup "Barra" $ menu imag)] ++
                                              [Translate (-300) (310) $ Scale (0.35) (0.35) $ text (show pnt)] ++
                                              [Translate (175) (310) $ Scale (0.35) (0.35) $ text (show $ round tempo)]  

-- | Função que desenha um Mapa no ecrã
desenhaMapa :: EstadoGloss -> (Int,Int) -> [Picture] 
--Caso de Paragem
desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg [])), _), al, melhor, imag) _ = []
--Desenhar Terreno Relva
desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Relva, []):prox))), jog), al, melhor, imag) (x,y) = desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg prox)),jog), al, melhor, imag)(-576,y-128)
desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Relva, (h:t)):prox))), jog), al, melhor, imag) (x,y) | h == Nenhum = (Translate (fromIntegral x) (fromIntegral y) $ Scale 4 4 $ (fromJust $ lookup Nenhum $ relva imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Relva, (t)):prox))), jog), al, melhor, imag)(x+128,y)
                                                                                                            | h == Arvore = (Translate (fromIntegral x) (fromIntegral y) $ Scale 4 4 $ (fromJust $ lookup Arvore $ relva imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Relva, (t)):prox))), jog), al, melhor, imag)(x+128,y)
--Desenhar Terreno Rio
desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Rio vel, []):prox))), jog), al, melhor, imag) (x,y) = desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg prox)),jog), al, melhor, imag)(-576,y-128)

desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Rio vel, (h:t)):prox))), jog), al, melhor, imag) (x,y) | h == Nenhum = (Translate (fromIntegral x) (fromIntegral y) $ Scale 4 4 $ (fromJust $ lookup Nenhum $ rio imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Rio vel, t):prox))), jog), al, melhor, imag)(x+128,y)             
                                                                                                              | h == Tronco = (Translate (fromIntegral x) (fromIntegral y) $ Scale 4 4 $ (fromJust $ lookup Tronco $ rio imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Rio vel, t):prox))), jog), al, melhor, imag)(x+128,y)
--Desenhar Terreno Estrada
desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Estrada vel, []):prox))), jog), al, melhor, imag) (x,y) = desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg prox)),jog), al, melhor, imag)(-576,y-128)

desenhaMapa (ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Estrada vel, (h:t)):prox))), jog), al, melhor, imag) (x,y) | h == Nenhum = (Translate (fromIntegral x) (fromIntegral y) $ Scale 4 4 $ (fromJust $ lookup Nenhum $ estrada imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Estrada vel, t):prox))), jog), al, melhor, imag)(x+128,y)             
                                                                                                                  | h == Carro && vel > 0 = (Translate (fromIntegral x) (fromIntegral y) $ Rotate 90 $ Scale 2 2 $ (fromJust $ lookup Carro $ estrada imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Estrada vel, t):prox))), jog), al, melhor, imag)(x+128,y)
                                                                                                                  | h == Carro && vel < 0 = (Translate (fromIntegral x) (fromIntegral y) $ Rotate (-90) $ Scale 2 2 $ (fromJust $ lookup Carro $ estrada imag)):desenhaMapa(ModoJogo, (tempo, pnt, (Jogo jogador (Mapa larg ((Estrada vel, t):prox))), jog), al, melhor, imag)(x+128,y)

-- | Função que desenha no ecrã se o jogo foi guardado.
desenhaJogoGuardado :: Imagens -> [Picture]
desenhaJogoGuardado imag = [fromJust $ lookup "guardado" $ menu imag]

-- | Função que verifica se já existe algum jogo guardado para o carregar para o jogo.
verificaSave :: IO (Tempo, Pontuacao, Jogo, Jogada)
verificaSave = do
                  fileExist <- doesFileExist "saveJogo.txt"
                  saved <- if fileExist then readFile "saveJogo.txt"
                  else return "(0, (0,0), (Jogo (Jogador (5,4)) (Mapa 10 [(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva, [Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])), Move Cima)"
                  return (read saved)


-- | Função que cria uma janela de Jogo
window :: Display --Função que cria uma janela
window = InWindow "Crossy Road - LI1"
                    (1280,720) --Tamanho da Janela
                    (0,0) --Posição da Janela

{- | Função que devolve um Estado do Gloss conforme uma tecla é pressionada

Dependendo do Estado pode ainda guardar o jogo, carregar jogo guardados e sair do Jogo.
-}
reageEvento :: Event -> EstadoGloss -> IO EstadoGloss
--Eventos Menu
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuP Jogar, (tempo, pnt, j, jog), al, melhor, imag) = return $ (MenuP Sair, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuP Jogar, (tempo, pnt, j, jog), al, melhor, imag) = return $ (MenuP Sair, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuP Jogar, (tempo, pnt, (Jogo jogador mapa), jog), al, melhor, imag) = return $ (TipoJ Novo, (tempo, pnt, (Jogo jogador (estendeMapa(mapa)(al))), jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (MenuP Sair, (tempo, pnt, j, jog), al, melhor, imag) = return $ (MenuP Jogar, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (MenuP Sair, (tempo, pnt, j, jog), al, melhor, imag) = return $ (MenuP Jogar, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (MenuP Sair, (tempo, pnt, j, jog), al, melhor, imag) = do writeFile "MelhorPontuacao.txt" (show melhor)                                                                                                              
                                                                                                                putStrLn "Jogo Guardado!\nFim do Jogo."
                                                                                                                exitSuccess
--Menu Tipo Jogo
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (TipoJ Novo, (tempo, pnt, j, jog), al, melhor, imag) = return $ (TipoJ Menu, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (TipoJ Novo, (tempo, pnt, j, jog), al, melhor, imag) = return $ (TipoJ Carregar, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (TipoJ Carregar, (tempo, pnt, j, jog), al, melhor, imag) = return $ (TipoJ Novo, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (TipoJ Carregar, (tempo, pnt, j, jog), al, melhor, imag) = return $ (TipoJ Menu, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (TipoJ Menu, (tempo, pnt, j, jog), al, melhor, imag) = return $ (TipoJ Carregar, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (TipoJ Menu, (tempo, pnt, j, jog), al, melhor, imag) = return $ (TipoJ Novo, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (TipoJ Novo, (tempo, pnt, j, jog), al, melhor, imag) = return $ (ModoJogo, novoJogo(al), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (TipoJ Menu, (tempo, pnt, j, jog), al, melhor, imag) = return $ (MenuP Jogar, (tempo, pnt, j, jog), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (TipoJ Carregar, (tempo, pnt, j, jog), al, melhor, imag) = do estadoJogo <- verificaSave
                                                                                                                    return $ (ModoJogo, estadoJogo, al, melhor, imag)
--Evento de Mover o Jogado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (tempo, (pnt,nvl), (Jogo jogador mapa), mov), al, melhor, imag) = return $ (ModoJogo, (tempo, (max(pnt)(nvl+1),nvl+1), (Jogo (posicaoJogador(mapa)(jogador)(Move Cima)) mapa), Move Cima), al, (if max(pnt)(nvl+1) > melhor then max(pnt)(nvl+1) else melhor), imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (tempo, (pnt,nvl), (Jogo jogador mapa), mov), al, melhor, imag) = return $ (ModoJogo, (tempo, (max(pnt)(nvl-1),nvl-1), (Jogo (posicaoJogador(mapa)(jogador)(Move Baixo)) mapa), Move Baixo), al, (if max(pnt)(nvl+1) > melhor then max(pnt)(nvl+1) else melhor), imag)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (tempo, pnt, (Jogo jogador mapa), mov), al, melhor, imag) = return $ (ModoJogo, (tempo, pnt, (Jogo (posicaoJogador(mapa)(jogador)(Move Direita)) mapa), Move Direita), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (tempo, pnt, (Jogo jogador mapa), mov), al, melhor, imag)  = return $ (ModoJogo, (tempo, pnt, (Jogo (posicaoJogador(mapa)(jogador)(Move Esquerda)) mapa), Move Esquerda), al, melhor, imag)
--Evento menu de Pausa
reageEvento (EventKey (Char 'p') Down _ _) (ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (Char 'p') Down _ _) (Pausa _, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Sair, (tempo, pnt ,jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = do writeFile "saveJogo.txt" (show (tempo, pnt, jogo, mov))
                                                                                                                   writeFile "MelhorPontuacao.txt" (show melhor)                                                                                                              
                                                                                                                   putStrLn "Jogo Guardado!\n"
                                                                                                                   return $ (MenuP Jogar, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Sair, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag) = do writeFile "saveJogo.txt" (show (tempo, pnt, jogo, mov))
                                                                                                                      writeFile "MelhorPontuacao.txt" (show melhor)                                                                                                              
                                                                                                                      putStrLn "Jogo Guardado!\n"
                                                                                                                      return $ (Pausa GuardarS, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey _ Down _ _) (Pausa GuardarS, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Pausa Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Guardar, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Pausa Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Pausa Menu, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = do writeFile "saveJogo.txt" (show (tempo, pnt, jogo, mov))
                                                                                                                   writeFile "MelhorPontuacao.txt" (show melhor)                                                                                                              
                                                                                                                   putStrLn "Jogo Guardado!\nFim do Jogo."
                                                                                                                   exitSuccess
--Instruções
reageEvento (EventKey (Char 'i') Down _ _) (Pausa _, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (Char 'i') Down _ _) (ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (Char 'i') Down _ _) (MenuP _, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (Char 'i') Down _ _) (TipoJ _, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (Char 'm') Down _ _) (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (MenuP Jogar, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (Char 'j') Down _ _) (Instrucoes, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (ModoJogo, (tempo, pnt, jogo, mov), al, melhor, imag)
--Perdeu
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu Novo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Perdeu Sair, (tempo, pnt ,jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu Novo, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Perdeu Menu, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Novo, (tempo, (_,_), jogo, mov), al, melhor, imag) = return $ (ModoJogo, (0, (0,0), (Jogo (Jogador (5,4)) (mapaInicial(mapaVazio)(al)(4))), Move Cima), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Perdeu Novo, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Perdeu Sair, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Menu, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ estadoInicial(imag)(al)(melhor)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Perdeu Menu, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Perdeu Novo, (tempo, pnt, jogo, mov), al, melhor, imag)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu Sair, (tempo, pnt, jogo, mov), al, melhor, imag) = do exitSuccess
reageEvento (EventKey (Char 'i') Down _ _) (Perdeu _, (tempo, pnt, jogo, mov), al, melhor, imag) = return $ (Instrucoes, novoJogo(al), al, melhor, imag)
-- Evento para sair a qualquer altura do Jogo
reageEvento (EventKey (Char 'q') Down _ _) (_, (tempo, pnt, jogo, mov), _, melhor, _) = do writeFile "saveJogo.txt" (show (tempo, pnt, jogo, mov))
                                                                                           writeFile "MelhorPontuacao.txt" (show melhor)                                                                                                              
                                                                                           putStrLn "Jogo Guardado!\nFim do Jogo."
                                                                                           exitSuccess
-- Ignorar qualquer outro evento
reageEvento _ s = return $ s


{- | Função que altera o Estado do Gloss conforme o tempo

Passado 5 segundos de Jogo chama a função desliza mapa para dar continuade ao jogo.
Verifica ainda se o jogador perdeu o jogo.
-}
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
--Aumenta a variavel do tempo e atualiza o mapa
reageTempoGloss n (ModoJogo, (t, pnt, (Jogo jogador (Mapa larg l)), mov), al, melhor, imag)  | jogoTerminou(Jogo jogador (Mapa larg l)) = do putStrLn("Perdeu!")
                                                                                                                                             writeFile "MelhorPontuacao.txt" (show melhor)
                                                                                                                                             return $ (Perdeu Novo, (t, pnt, (Jogo jogador (Mapa larg l)), mov), al, melhor, imag)
                                                                                             | (round t) `mod` 5 == 0 && ((round t) /= 0) = do al <- randomRIO(1,100)
                                                                                                                                               let resultAl = valorAleatorio(al)
                                                                                                                                               return $ (ModoJogo, (t+n, pnt, deslizaJogo(resultAl)(animaJogo(Jogo jogador (Mapa larg l))(Parado)), mov), resultAl, melhor, imag)
                                                                                             | otherwise = do al <- randomRIO(1,100)
                                                                                                              let resultAl = valorAleatorio(al)
                                                                                                              return $ (ModoJogo, (t+n, pnt, (animaJogo(Jogo jogador (Mapa larg l))(Parado)), mov), al, melhor, imag)
reageTempoGloss _ s = return $ s

-- | Função de frame rate
fr :: Int
fr = 1

-- | Função para transformar um IO (Int) num Int
valorAleatorio :: Int -> Int
valorAleatorio i = i

-- | Função main do Jogo
main :: IO ()
main = do

  --Texturas usadas no Jogo
  --Menu Principal
  menuJ <- loadBMP "textures/MenuJogar.bmp" --MenuJogar
  menuS <- loadBMP "textures/MenuSair.bmp" --Menu Sair
  --Menu tipo jogo
  tipoN <- loadBMP "textures/tipoN.bmp"
  tipoC <- loadBMP "textures/tipoC.bmp"
  tipoM <- loadBMP "textures/tipoM.bmp"
  --Menu Pausa
  pausaM <- loadBMP "textures/PausaM.bmp" --Menu Pausa menu
  pausaG <- loadBMP "textures/PausaG.bmp" --Menu Pausa guardar jogo
  pausaS <- loadBMP "textures/PausaS.bmp" --Menu Pausa sair jogo
  --Menu Instruções
  inst <- loadBMP "textures/inst.bmp"
  --Mensagem Jogo guardado
  guardado <- loadBMP "textures/jogoGuardado.bmp"
  --Barra Pontuacao
  barraJ <- loadBMP "textures/barra.bmp" --Barra sup jogo
  --Menu Perdeu
  perdeuN <- loadBMP "textures/perdeuN.bmp"
  perdeuM <- loadBMP "textures/perdeuM.bmp"
  perdeuS <- loadBMP "textures/perdeuS.bmp"
  --Textura obstaculos/terreno
  relva <- loadBMP "textures/relva.bmp" --Relva
  arvore <- loadBMP "textures/arv.bmp" --Arvore
  agua <- loadBMP "textures/agua.bmp" --Agua
  tronco <- loadBMP "textures/tronco.bmp" --Tronco
  estrada <- loadBMP "textures/estrada.bmp" --Estrada
  carro <- loadBMP "textures/carro.bmp" --Carro
  --Texturas Personagem
  galcima <- loadBMP "textures/chickUp.bmp" --Galinha Cima
  galbaixo <- loadBMP "textures/chickDown.bmp" --Galinha Baixo
  galdir <- loadBMP "textures/chickRig.bmp" --Galinha Esquerda
  galesq <- loadBMP "textures/chickLeft.bmp" --Galinha Direita

  melhor <- leMelhorPontuacao
  --Carregar as imagens para uma estrutura
  let imag = Texturas [("Jogar",menuJ),("Sair",menuS),("Barra",barraJ),("PausaM",pausaM),("PausaG",pausaG),("PausaS",pausaS),("inst",inst),("perdeuN",perdeuN),("perdeuM",perdeuM),("perdeuS",perdeuS),("tipoN",tipoN),("tipoC",tipoC),("tipoM",tipoM),("guardado",guardado)]
                      [("Cima",galcima),("Baixo",galbaixo),("Dir",galdir),("Esq",galesq)]
                      [(Nenhum,relva),(Arvore,arvore)]
                      [(Nenhum,estrada),(Carro,carro)]
                      [(Nenhum,agua),(Tronco,tronco)] --Imagens Usadas no Jogo

  al <- randomRIO(1,100)
  let resultAl = valorAleatorio(al)
  playIO window (azure) fr (estadoInicial(imag)(resultAl)(melhor)) desenhaEstado reageEvento reageTempoGloss
