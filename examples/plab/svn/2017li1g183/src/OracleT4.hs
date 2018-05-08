{-# LANGUAGE PatternGuards #-}

module OracleT4 where

import LI11718
import OracleT3
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe
--import Debug.Trace
import Data.Fixed

validaJogo :: Tempo -> Jogo -> Int -> Bool
validaJogo t j i = length (carros j) == length (nitros j)
                && length (carros j) == length (historico j)
                && 0 <= i && i < length (carros j)
                && 0 <= t

validaJogo' :: Tempo -> Jogo -> Bool
validaJogo' t j = length (carros j) == length (nitros j)
                && length (carros j) == length (historico j)
                && 0 <= t

comparaJogo :: Double -> Jogo -> Jogo -> Bool
comparaJogo delta j1 j2 = mapa j1 == mapa j2
                       && pista j1 == pista j2
                       && comparaCarros delta (carros j1) (carros j2)
                       && comparaDoubles delta (nitros j1) (nitros j2)
                       && comparaHistorico (historico j1) (historico j2)

comparaHistorico :: Eq a => [a] -> [a] -> Bool
comparaHistorico xs ys = xs == ys

comparaCarros :: Double -> [Carro] -> [Carro] -> Bool
comparaCarros delta xs ys = length xs == length ys && all (\(x,y) -> comparaCarro delta x y) (zip xs ys)

comparaCarro :: Double -> Carro -> Carro -> Bool
comparaCarro delta c1 c2 = comparaPonto delta (posicao c1) (posicao c2)
                        && comparaDirecao delta (direcao c1) (direcao c2) 
                        && comparaPonto delta (velocidade c1) (velocidade c2)

comparaDirecao :: Double -> Angulo -> Angulo -> Bool
comparaDirecao delta a1 a2 = comparaDouble delta (normalizaAngulo a1) (normalizaAngulo a2)

comparaPonto :: Double -> Ponto -> Ponto -> Bool
comparaPonto delta p1 p2 = dist p1 p2 <= delta

comparaDoubles :: Double -> [Double] -> [Double] -> Bool
comparaDoubles delta xs ys = length xs == length ys && all (\(x,y) -> comparaDouble delta x y) (zip xs ys)

comparaDouble :: Double -> Double -> Double -> Bool
comparaDouble delta x1 x2 = abs (x1 - x2) <= delta

normalizaAngulo :: Double -> Double
normalizaAngulo a = mod' a 360

genJogadores :: Jogo -> Gen Int
genJogadores jogo = elements (jogadores jogo)

jogadores :: Jogo -> [Int]
jogadores jogo = take (length $ carros jogo) [0..]

atualizaCarro :: Jogo -> Int -> Carro -> Jogo
atualizaCarro j p c' = j { carros = cs' }
    where
    cs' = (take p (carros j))++(c':drop (p+1) (carros j))

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
