{-# LANGUAGE PatternGuards #-}

module OracleT4 where

import LI11718
import OracleT3
import Mapas
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

atualizaTeste :: Int -> (Tempo,Jogo,Acao) -> Jogo
atualizaTeste p (t,j,a) = atualiza t j p a

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

testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [OracleT4.t0
           ,OracleT4.t1
           ,OracleT4.t2
           ,OracleT4.t3
           ,OracleT4.t4
           ,OracleT4.t5
           ,OracleT4.t6
           ,OracleT4.t7
           ,OracleT4.t8
           ,OracleT4.t9
           ,OracleT4.t10
           ,OracleT4.t11
           ,OracleT4.t12
           ,OracleT4.t13
           ,OracleT4.t14
           ,OracleT4.t15
           ,OracleT4.t16
           ,OracleT4.t17
           ,OracleT4.t18
           ,OracleT4.t19
           ,OracleT4.t20
           ,OracleT4.t21
           ,OracleT4.t22
           ,OracleT4.t23
           ,OracleT4.t24
           ,OracleT4.t25
           ,OracleT4.t26
           ,OracleT4.t27
           ,OracleT4.t28
           ,OracleT4.t29
           ,OracleT4.t30
           ,OracleT4.t31
           ,OracleT4.t32
           ,OracleT4.t33
           ,OracleT4.t34
           ,OracleT4.t35
           ,OracleT4.t36
           ,OracleT4.t37
           ,OracleT4.t38
           ,OracleT4.t39
           ,OracleT4.t40
           ,OracleT4.t41
           ,OracleT4.t42
           ,OracleT4.t43
           ,OracleT4.t44
           ,OracleT4.t45
           ,OracleT4.t46]


p0 = Propriedades 0 0 0 0 0 0 -- nada 
p1 = Propriedades 2 0 0 0 0 0 -- k_atrito
p2 = Propriedades 0 2 0 0 0 0 -- k_pneus
p3 = Propriedades 0 0 2 0 0 0 -- k_acel
p4 = Propriedades 0 0 0 2 0 0 -- k_peso
p5 = Propriedades 0 0 0 0 2 0 -- k_nitro
p6 = Propriedades 0 0 0 0 0 110 -- k_roda

a0 = Acao False False False False Nothing -- nao faz nada
a1 = Acao True  True  False True  Nothing -- vira direita
a2 = Acao False False True  False Nothing -- vira esquerda
a3 = Acao True  False True False  Nothing -- acelera, vira esquerda
a4 = Acao False True  False True  Nothing -- trava, vira direita
a5 = Acao True  False False False  (Just 0) -- acelera, da nitro
a6 = Acao True  False False False  (Just 1) -- acelera, da nitro

cr1 = Carro (2.3,1.4) (190) (2.3,1.1)
cr2 = Carro (3.3,1.6) (-45) (1.2,1.5)
cr3 = Carro (4.1,3.5) (145) (-2.3,1.1)
cr4 = Carro (1.3,2.6) (-50) (1.4,-0.5)

m1 = Mapa ((2,1),Este)
  [[Peca Lava 0,Peca Lava          0,Peca Lava  0,Peca Lava         0,Peca Lava         0,Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Rampa Este) 0,Peca (Curva Este) 1,Peca Lava 0]
  ,[Peca Lava 0,Peca (Rampa Sul)   0,Peca Lava  0,Peca Lava         0,Peca Recta        1,Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca Recta        1,Peca (Curva Sul)  1,Peca Lava 0]
  ,[Peca Lava 0,Peca Lava          0,Peca Lava  0,Peca Lava         0,Peca Lava         0,Peca Lava 0]]

-- Propriedades basicas
t0  = (0.11,Jogo m1 p0 [cr1] [1.2] [[(2,1)]], OracleT4.a0) -- faz nada
t1  = (0.11,Jogo m1 p1 [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_atrito
t2  = (0.11,Jogo m1 p2 [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_pneus
t3  = (0.11,Jogo m1 p3 [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_acel
t4  = (0.11,Jogo m1 p4 [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_peso
t5  = (0.11,Jogo m1 p5 [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_nitro
t6  = (0.11,Jogo m1 p6 [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_roda
t7  = (0.11,Jogo m1 p0 [cr1] [1.2] [[]], OracleT4.a0)      -- recta & actualiza historico lista vazia
t8  = (0.11,Jogo m1 p0 [cr2] [1.2] [[(2,1)]], OracleT4.a0) -- recta & actualiza historico lista nao vazia
t9  = (0.11,Jogo m1 p1 [cr2] [1.2] [[(3,1),(2,1)]], OracleT4.a3) -- rampa & acelera, vira esquerda & k_atrito
t10 = (0.11,Jogo m1 p5 [cr1] [1.2] [[(2,1)]], OracleT4.a5) -- recta & acelera & da nitro no proprio
t11 = (0.11,Jogo m1 p5 [cr1] [0]   [[(2,1)]], OracleT4.a5) -- recta & acelera & da nitro no proprio & nitro vazio
t12 = (0.21,Jogo m1 p5 [cr1] [0.1] [[(2,1)]], OracleT4.a5) -- recta & acelera & da nitro no proprio & nitro insuficiente
t13 = (0.11,Jogo m1 p5 [cr1,cr2] [1.2,0] [[(2,1)],[(3,1),(2,1)]], OracleT4.a6) -- recta & acelera & da nitro no proprio
t14 = (0.11,Jogo m1 p5 [cr1,cr2] [0,0]   [[(2,1)],[(3,1),(2,1)]], OracleT4.a6) -- recta & acelera & da nitro no noutro & nitro vazio
t15 = (0.21,Jogo m1 p5 [cr1,cr2] [0.1,0] [[(2,1)],[(3,1),(2,1)]], OracleT4.a6) -- recta & acelera & da nitro no noutro & nitro insuficiente

-- Propriedades normais
t16 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a0) -- recta
t17 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a1) -- recta
t18 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a2) -- recta
t19 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta
t20 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a4) -- recta
t21 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a5) -- recta
t22 = (0.11,Jogo m1 terra [cr1] [1.2] [[]], OracleT4.a0)      -- recta
t23 = (0.11,Jogo m1 terra [cr2] [1.2] [[(2,1)]], OracleT4.a1) -- rampa
t24 = (0.11,Jogo m1 terra [cr2] [1.2] [[(3,1),(2,1)]], OracleT4.a2) -- recta
t25 = (0.11,Jogo m1 terra [cr1] [1.2] [[(2,1)]], OracleT4.a3) -- recta
t26 = (0.11,Jogo m1 terra [cr1] [0]   [[(2,1)]], OracleT4.a4) -- recta
t27 = (0.21,Jogo m1 terra [cr1] [0.1] [[(2,1)]], OracleT4.a5) -- recta
t28 = (0.11,Jogo m1 terra [cr1,cr2] [1.2,0] [[(2,1)],[(3,1),(2,1)]], OracleT4.a6) -- recta & acelera & da nitro no proprio
t29 = (0.11,Jogo m1 terra [cr1,cr2] [0,0]   [[(2,1)],[(3,1),(2,1)]], OracleT4.a6) -- recta & acelera & da nitro no noutro & nitro vazio
t30 = (0.21,Jogo m1 terra [cr1,cr2] [0.1,0] [[(2,1)],[(3,1),(2,1)]], OracleT4.a6) -- recta & acelera & da nitro no noutro & nitro insuficiente

-- Propriedades basicas com 4 jogadores
t31 = (0.11,Jogo m1 p0 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a0) -- faz nada
t32 = (0.11,Jogo m1 p1 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_atrito
t33 = (0.11,Jogo m1 p2 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_pneus
t34 = (0.11,Jogo m1 p3 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_acel
t35 = (0.11,Jogo m1 p4 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_peso
t36 = (0.11,Jogo m1 p5 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_nitro
t37 = (0.11,Jogo m1 p6 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- recta & acelera, vira esquerda & k_roda
t38 = (0.11,Jogo m1 p0 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[]     ,[]     ,[]     ,[]     ], OracleT4.a0) -- recta & actualiza historico lista vazia
t39 = (0.11,Jogo m1 p0 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(2,1)],[(2,1)],[(2,1)]], OracleT4.a0) -- recta & actualiza historico lista nao vazia
t40 = (0.11,Jogo m1 p1 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a3) -- rampa & acelera, vira esquerda & k_atrito
t41 = (0.11,Jogo m1 p5 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a5) -- recta & acelera & da nitro no proprio
t42 = (0.11,Jogo m1 p5 [cr1,cr2,cr3,cr4] [0  ,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a5) -- recta & acelera & da nitro no proprio & nitro vazio
t43 = (0.21,Jogo m1 p5 [cr1,cr2,cr3,cr4] [0.1,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a5) -- recta & acelera & da nitro no proprio & nitro insuficiente
t44 = (0.11,Jogo m1 p5 [cr1,cr2,cr3,cr4] [1.2,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a6) -- recta & acelera & da nitro no proprio
t45 = (0.11,Jogo m1 p5 [cr1,cr2,cr3,cr4] [0  ,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a6) -- recta & acelera & da nitro no noutro & nitro vazio
t46 = (0.21,Jogo m1 p5 [cr1,cr2,cr3,cr4] [0.1,0,1,2] [[(2,1)],[(3,1)],[(4,3)],[(1,2)]], OracleT4.a6) -- recta & acelera & da nitro no noutro & nitro insuficiente
