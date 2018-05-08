{-# LANGUAGE PatternGuards #-}

module BotNuno where

import LI11718
import OracleT4
import OracleT3
import OracleT2
import OracleT1
--import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
--import Safe
--import Debug.Trace

bot :: Double -> Jogo -> Int -> Acao 
bot t e i | p' == Nothing = Acao False True (trg>0) (trg<0) nit
          | otherwise = Acao (v<vt) (v-vt>0.2) (trg>0) (trg<0) Nothing 
    where p = (carros e!!i) 
          vt = 1.6/(lookahead/3.5)
          lookahead = 1.2*(7 - k_atrito (pista e))
          p' = colide m (lookahead*t) (carros (e)!!i) 
          Mapa ((pi,pj),pe) m = mapa (e)
          Peca tp _ = (m!!pj!!pi)
          prc = percorre [] m (pi,pj) pe
          whereAmI = dropWhile (\(_,i,_) -> i /= ponto2Pos (posicao p)) (prc++prc)
          whereAmI' = dropWhile (\(_,i,_) -> i /= ponto2Pos (posicao p)) (tail whereAmI)
          dr0 = dir (posicao p) (whereAmI!!1)
          dr' = if length whereAmI' > 0 then dir (posicao p) (whereAmI'!!1) else dr0
          trg0 = distRad (round $ direcao p) (round dr0)
          trg1 = distRad (round $ direcao p) (round dr')
          trg = if abs trg0 < abs trg1 then trg0 else trg1
          (v,_) = componentsToArrow (velocidade p)
          ntc = (round (direcao p)) `mod` 4
          nit | ntc /= i = Just ntc
              | otherwise = Nothing

dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180