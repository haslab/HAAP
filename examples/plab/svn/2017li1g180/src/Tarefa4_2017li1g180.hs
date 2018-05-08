{-# LANGUAGE PatternGuards #-}

module Tarefa4_2017li1g180 where

import LI11718
import Tarefa3_2017li1g180
import Tarefa1_2017li1g180
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
--import Safe
--import Debug.Trace

testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [(0.2,jogoT { carros = ((head $ carros jogoT) { velocidade = (1,0)}) : tail (carros jogoT) },Acao True False True False (Just 0))
           ,(0.2,jogoT { carros = ((head $ carros jogoT) { velocidade = (3,-1)}) : tail (carros jogoT) },Acao True False False True (Just 0))
           ,(0.2,jogoT { carros = ((head $ carros jogoT) { velocidade = (0,0)}) : tail (carros jogoT) },Acao False True True False Nothing)
           ,(0.4,jogoT { carros = ((head $ carros jogoT) { velocidade = (1,0)}) : tail (carros jogoT) },Acao True False True False (Just 0))]

jogoT = jogoInicial $ constroi [Avanca,Avanca,CurvaDir,CurvaDir,Avanca,Avanca,CurvaDir,CurvaDir]


njogadores = 1
qntnitro = 5

jogoInicial :: Mapa -> Jogo
jogoInicial m@(Mapa p t) = Jogo { mapa      = m 
                                , pista     = standard
                                , carros    = map (carroInicial t (fst p)) [0..njogadores-1]
                                , nitros    = replicate njogadores qntnitro
                                , historico = replicate njogadores [(fst p)]
                                }

carroInicial :: Tabuleiro -> Posicao -> Int -> Carro
carroInicial t (a,b) i = Carro { posicao    = centroPeca tp (a,b)
                               , direcao    = 0
                               , velocidade = (0,0)
                               }
    where  (Peca tp _) = (t!!b!!a)


centroPeca :: Tipo -> Posicao -> Ponto
centroPeca (Curva Norte) (a,b) = (toEnum a+0.7,toEnum b+0.7)
centroPeca (Curva Este) (a,b) = (toEnum a+0.3,toEnum b+0.7)
centroPeca (Curva Sul) (a,b) = (toEnum a+0.3,toEnum b+0.3)
centroPeca (Curva Oeste) (a,b) = (toEnum a+0.7,toEnum b+0.3)
centroPeca _ (a,b) = (toEnum a+0.5,toEnum b+0.5)

standard = Propriedades (1.5) 1 4 2 15 90


atualiza :: Tempo -> Jogo -> Int -> Acao -> Jogo
atualiza t e j a = lancaNitros t j e e' a
  where e' = e { carros = cs', historico = hs' }
        Mapa _ m = (mapa e)
        c' = moveCarro t e (carros e!!j,a)
        h' = atualizaHistorico (carros e!!j) (historico e!!j)
        cs' = (take j (carros e))++(c':drop (j+1) (carros e))
        hs' = (take j (historico e))++(h':drop (j+1) (historico e))

moveCarro :: Tempo -> Jogo -> (Carro,Acao) -> Carro
moveCarro t e (c,a) = rodaCarro t e (andaCarro t e (c,a) ,a)

rodaCarro :: Tempo -> Jogo -> (Carro,Acao) -> Carro
rodaCarro t e (c,a) | direita a && not (esquerda a) = c { direcao = direcao c - (t*k_roda (pista e))}
                    | esquerda a && not (direita a) = c { direcao = direcao c + (t*k_roda (pista e))}
                    | otherwise = c

atualizaHistorico :: Carro -> [Posicao] -> [Posicao]
atualizaHistorico c [] = [(floor (fst (posicao c)), floor (snd (posicao c)))]
atualizaHistorico c (h:hs) = hs'
  where (i,j) = (floor (fst (posicao c)), floor (snd (posicao c)))
        hs' | h == (i,j) = (h:hs)
            | otherwise = (i,j):h:hs

lancaNitros :: Tempo -> Int -> Jogo -> Jogo -> Acao -> Jogo
lancaNitros _ _ e0 e (Acao _ _ _ _ Nothing) = e
lancaNitros t i e0 e (Acao _ _ _ _ (Just j)) | (nitros e)!!i == 0 = e
                                          | otherwise = e { carros = cs', nitros = ns' }
  where tNitro = min t (max 0 ((nitros e)!!i))
        ns' = (take i (nitros e))++(n':drop (i+1) (nitros e))
        cs' = (take j (carros e))++(c':drop (j+1) (carros e))
        n' = max 0 (((nitros e)!!i)-tNitro)
        c = (carros e)!!j
        c0 = (carros e0)!!j
        c' = c { velocidade = (velocidade c) .+. (tNitro .*. (nitroVec (pista e) c0)) }

andaCarro :: Tempo -> Jogo -> (Carro,Acao) -> Carro
andaCarro t e (c,a) = c { velocidade = v' }
  where v' = (velocidade c) .+. (t .*. ((accelVec ps (c,a)) .+. (dragVec ps c) .+. (driftVec ps c) .+. (gravityVec ps p c)))
        ps = pista e
        Mapa _ m = mapa e
        (i,j) = (floor (fst (posicao c)), floor (snd (posicao c)))
        p = m!!j!!i

accelVec :: Propriedades -> (Carro,Acao) -> Velocidade
accelVec ps (c,a) | acelerar a && not (travar a) = arrowToComponents (k_acel ps,direcao c)
                  | travar a && not (acelerar a) = arrowToComponents (k_acel ps,direcao c + 180)
                  | otherwise = (0,0)

dragVec :: Propriedades -> Carro -> Velocidade
dragVec ps c = arrowToComponents (v*k_atrito ps,a + 180) 
  where (v,a) = componentsToArrow (velocidade c)

driftVec :: Propriedades -> Carro -> Velocidade
driftVec ps c = arrowToComponents (driftCoef,driftAngle)
  where (v,a) = componentsToArrow $ velocidade c
        d = direcao c
        driftAngle = if (sin (radians (a-d))) > 0
                     then d-90 -- going right 
                     else d+90 -- going left
        driftCoef = v * k_pneus ps * abs (sin (radians (a-d)))
    
nitroVec :: Propriedades -> Carro -> Velocidade
nitroVec ps c = arrowToComponents (k_nitro ps,direcao c)

gravityVec :: Propriedades -> Peca -> Carro -> Velocidade
gravityVec ps (Peca (Rampa Sul) _)   c = arrowToComponents (k_peso ps, 90)
gravityVec ps (Peca (Rampa Norte) _) c = arrowToComponents (k_peso ps, 270)
gravityVec ps (Peca (Rampa Oeste) _) c = arrowToComponents (k_peso ps, 0)
gravityVec ps (Peca (Rampa Este) _)  c = arrowToComponents (k_peso ps, 180)
gravityVec ps _                      c = (0,0)
