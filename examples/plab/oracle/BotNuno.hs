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
import Debug.Trace

bot :: Double -> Jogo -> Int -> Acao 
bot t e i | p' == Nothing = Acao False True (trg 1>0) (trg 1<0) ntc
          | p2' == Nothing = Acao (v<vt) (v-vt>0.2) (trg 1>0) (trg 1<0) (nt n') 
          | otherwise = Acao (v<vt) (v-vt>0.2) ((trg 4+trg 1+trg 1)`div`3>0) ((trg 4+trg 1+trg 1)`div`3<0) (nt n2') 
    where p = (carros e!!i) 
          p2 = p {direcao = fromIntegral (trg 4)}
          vt = 2/(lookahead/3)
          lookahead = 1.2*(7 - 1.5*k_atrito (pista e))
          p' = colide m (lookahead*t) p
          p2' = colide m (lookahead*t) p2
          n' = colide m (5*lookahead*t) p
          n2' = colide m (5*lookahead*t) p2
          Mapa ((pi,pj),pe) m = mapa (e)
          prc = percorre [] m (pi,pj) pe
          maybeAm = fst $ whereAmI e i 
          maybeWl k = map (\i -> (prc++prc)!!i) (map (+k) maybeAm)
          maybeDr k = map (distRad (round (direcao p)) . round . (dir (posicao p))) (maybeWl k)
          trg k = head $ sortOn abs (maybeDr k)
          (v,_) = componentsToArrow (velocidade p)
          ntc = Just $ (i+1) `mod` 4
          nt x | x == Nothing = Nothing
               | abs (trg 4) > 5 = Nothing
               | otherwise = Just i

whereAmI :: Jogo -> Int -> ([Int],Int)
whereAmI j n = (r,length prc)
  where r = sortOn (\v -> abs $ 0-v) f
        f = findIndices (\(_,i,_) -> i == head ((historico j)!!n)) prc
        Mapa p0 tab = mapa j
        prc = percorre [] tab (fst p0) (snd p0)

dir :: Ponto -> (Peca,Posicao,Orientacao) -> Double
dir p0 (Peca t _,p,_) = snd $ componentsToArrow (p'.-.p0)
  where p' = centroPeca t p

distRad :: Int -> Int -> Int
distRad r1 r2 = ((r2-r1) + 180) `mod` 360 - 180