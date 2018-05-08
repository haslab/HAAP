{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module BotHugo183 where

import OracleT1 hiding (roda)
import OracleT2
import OracleT3
import OracleT4

import LI11718

import Safe

import Data.Maybe

import Control.Monad
import Data.Fixed
import Data.List
import Data.Map (Map(..))
import qualified Data.Map as Map

--import Debug.Trace

-- factores
-- quão cedo começa a travar; em função da velocidade do carro
cagaco :: Propriedades -> Tempo
cagaco props
    | k_atrito props < 1 = 1
    | k_atrito props <= 2 = 0.6
    | otherwise = 0.3
-- quanto drift faz o carro; número de peças de lookahead; 4 será o ideal por causa dos atalhos
tuning :: Int
tuning = 4

apontaPeca :: Peca -> Orientacao -> Posicao -> Ponto
apontaPeca p o pos = ladoPonto (metaOri p o) pos

ladoPonto :: Orientacao -> Posicao -> Ponto
ladoPonto Norte (intToDouble -> x,intToDouble -> y) = (x+0.5,y+0.01)
ladoPonto Sul (intToDouble -> x,intToDouble -> y) = (x+0.5,y+0.99)
ladoPonto Este (intToDouble -> x,intToDouble -> y) = (x+0.99,y+0.5)
ladoPonto Oeste (intToDouble -> x,intToDouble -> y) = (x+0.01,y+0.5)

cantosCurva :: Peca -> Posicao -> [Ponto]
cantosCurva (Peca t _) (intToDouble -> x,intToDouble -> y) = cantosC t
    where
    cantosC (Curva Norte) = [(x+0.8,y+0.8)]
    cantosC (Curva Este) = [(x+0.2,y+0.8)]
    cantosC (Curva Sul) = [(x+0.2,y+0.2)]
    cantosC (Curva Oeste) = [(x+0.8,y+0.2)]
    cantosC _ = []

velocidadeJogador jogo p = v
    where
    carro = getCarro p jogo
    v = fst $ componentsToArrow $ velocidade carro

bot :: Tempo  -- ^ tempo decorrido desde a última decisão
    -> Jogo   -- ^ estado atual do jogo
    -> Int    -- ^ identificador do jogador dentro do estado
    -> Acao   -- ^ a decisão tomada pelo /bot/
--bot tick jogo p | vaiMorrer caganco tick jogo p = (travagem . rotacao) quieto
--                | otherwise = (aceleracao . rotacao) quieto
bot tick jogo p 
    | (dir-aponta) `menorAngulo` sens && temNitro && not (vaiColidir cmp True (caganco) tick jogoNitrado p) = (turbina p) quieto
    | vaiMorrer True caganco tick jogo p = travagem quieto
    | otherwise = aceleracao quieto
  where
  caganco = if v > 0 then cagaco (pista jogo) * v else cagaco (pista jogo)
  v = velocidadeJogador jogo p
  temNitro = atNote "nitros" (nitros jogo) p > tick
  jogoNitrado = lancaNitros tick p jogo jogo (turbina p quieto)
  
  rodatuning = if emRampa jogo p && maybe False (isCurva . fst3) (headMay proximas)
      then 1
      else tuning
  aponta = apontaPara (take rodatuning $ proximas) tuning tick jogo p
  contraria = contrariaPara (take rodatuning $ proximas) tuning tick jogo p
  cmp :: Ponto -> Ponto -> Bool
  cmp x y = dist x y <= 0.1
  travagem :: Acao -> Acao
  travagem = estabilizaVelocidade contraria jogo tick p
  aceleracao :: Acao -> Acao
  aceleracao = aumentaVelocidade aponta jogo tick p
  carro = getCarro p jogo
  dir = direcao carro
  sens = k_roda props * tick 
  props = pista jogo
  vel = snd $ componentsToArrow $ velocidade carro
  (_,proximas) = proximasPecas jogo p

aumentaVelocidade :: Double -> Jogo -> Tempo -> Int -> (Acao -> Acao)
aumentaVelocidade aponta jogo tick p = {-trace ("aumenta" ++show angA ++ " " ++ show dir' ++ "\n" ++ show vel ++ " " ++ show aceleraV ++ "\n" ++ show (fwd quieto) ++ "\n" ++ show ([aceleraV,travaV]))-} (fwd . roda)
    where
    props = pista jogo
    carro = getCarro p jogo
    dir = normalizaAngulo $ direcao carro
    sens = k_roda props * tick 
    angA = normalizaAngulo aponta
    vel = velocidade carro
    (angE) = if angA >= 180 then (angA-180) else (angA)
    (maybe id id -> roda,dir') = ajustaDirecaoCarro jogo carro tick angA
  
    aceleraV = velocidade $ andaCarro tick jogo (carro {direcao = dir'},acelera quieto) 
    travaV = velocidade $ andaCarro tick jogo (carro {direcao = dir'},trava quieto)
--    turbinaV = tick .*. arrowToComponents (min (nitros jogo!!p) (k_nitro props),dir')
    acoes1 = [(acelera, aceleraV),(trava,travaV)]
--    acoes2 = [(turbina p,turbinaV),(id,(0,0))]
--    tacoes = [ (a1 . a2,v1 .+. v2) | (a1,v1) <- acoes1, (a2,v2) <- acoes2 ]
    (go,_) = maximumBy (compareSndDistort angA) acoes1
    fwd = if (normalizaAngulo (angA - dir') `menorAngulo` normalizaAngulo sens) then go else id
    

estabilizaVelocidade :: Double -> Jogo -> Tempo -> Int -> (Acao -> Acao)
estabilizaVelocidade aponta jogo tick p = {-trace ("estabiliza" ++show angA ++" "++ show angE ++ "\n"++show (go quieto) ++"\n"++show vel ++ " " ++ show sens ++ "\n" ++ show ([aceleraV,travaV]))-} (go . roda)
    where
    Mapa _ tab = mapa jogo
    pos = pontoToPosicao $ posicao carro
    props = pista jogo
    carro = getCarro p jogo
    dir = normalizaAngulo $ direcao carro
    vel = velocidade carro
    sens = k_roda props * tick 
    (v,normalizaAngulo -> angV) = componentsToArrow vel
    angA = normalizaAngulo aponta
    --(angE) = if normalizaAngulo (angV - dir) `menorAngulo` normalizaAngulo (angV-180-dir) then angV else angV-180      
    angE = angA --if normalizaAngulo (dir - angA) `menorAngulo` normalizaAngulo (dir-angA-180) then angA else angA-180      
--        angV >= 180 then (angV-180) else (angV)
    (maybe id id -> roda,dir') = ajustaDirecaoCarro jogo carro tick angE
--    (angD) = if normalizaAngulo (dir' - angA) `menorAngulo` normalizaAngulo (dir'-180-angA) then dir' else dir'-180   
    
    aceleraV = velocidade $ andaCarro tick jogo (carro {direcao = dir'},acelera quieto) 
    travaV = velocidade $ andaCarro tick jogo (carro {direcao = dir'},trava quieto) 
--    turbinaV = tick .*. arrowToComponents (min (nitros jogo!!p) (k_nitro props),dir')
    acoes1 = [(acelera, aceleraV),(trava,travaV),(id,vel)]
--    acoes2 = [(turbina p,turbinaV),(id,(0,0))]
--    tacoes = [ (a1 . a2,v1 .+. v2) | (a1,v1) <- acoes1, (a2,v2) <- acoes2 ]
--    (go2,_) = maximumBy (compareMorteLenta (angV - 180) tick jogo p roda) acoes1
    (go2,_) = maximumBy (compareSndDistort (angV - 180)) acoes1
    go = go2

--compareMorteLenta :: Double -> Tempo -> Jogo -> Int -> (Acao -> Acao) -> (Acao -> Acao,Ponto) -> (Acao -> Acao,Ponto) -> Ordering
--compareMorteLenta ang tick jogo p a x1@(a1,p1) x2@(a2,p2)
--    | morte1 == morte2 = compareSndDistort ang x1 x2
--    | morte1 < morte2 = LT
--    | morte2 < morte1 = GT
--  where
--    Mapa _ tab = mapa jogo
--    morte1 = vaiMorrer True (2*tick) tick (atualiza tick jogo p $ (a1 . a) quieto) p
--    morte2 = vaiMorrer True (2*tick) tick (atualiza tick jogo p $ (a2 . a) quieto) p
--    carro = getCarro p jogo
--    peca = getPeca (pontoToPosicao $ posicao carro) tab
--    pos = pontoToPosicao $ posicao carro
--
--compareSndNorma :: (a,Ponto) -> (a,Ponto) -> Ordering
--compareSndNorma (x1,y1) (x2,y2) = compare (normaVetor y1) (normaVetor y2)
--
--compareSndNormaDist :: Tabuleiro -> (a,Ponto) -> (a,Ponto) -> Ordering
--compareSndNormaDist tab (_,p1) (_,p2) = compare (normaVetor p1 - distanceParedes peca1 pos1 p1) (normaVetor p2 - distanceParedes peca2 pos2 p2)
--    where
--    pos1 = normalizaPosicao tab $ pontoToPosicao p1
--    pos2 = normalizaPosicao tab $ pontoToPosicao p2
--    peca1 = getPeca pos1 tab
--    peca2 = getPeca pos2 tab
--
--compareSndNormaDistPeca :: Peca -> Posicao -> (a,Ponto) -> (a,Ponto) -> Ordering
--compareSndNormaDistPeca peca pos (_,p1) (_,p2) = compare (normaVetor p1 - distanceParedes peca pos p1) (normaVetor p2 - distanceParedes peca pos p2)
--
--compareSndDistortStable :: Double -> (a,Ponto) -> (a,Ponto) -> Ordering
--compareSndDistortStable th (_,p1) (_,p2) = compare (abs $ 0.2 - distortVectorToAngle th p2) (abs $ 0.2 - distortVectorToAngle th p1)
--
emRampa :: Jogo -> Int -> Bool
emRampa jogo p = case getPecaAtual jogo p of
    Peca (Rampa _) _ -> True
    otherwise -> False

compareSndDistort :: Double -> (a,Ponto) -> (a,Ponto) -> Ordering
compareSndDistort th (_,p1) (_,p2) = compare (distortVectorToAngle th p1) (distortVectorToAngle th p2)

neg x = -x

distortVectorToAngle :: Double -> Ponto -> Double
distortVectorToAngle 0 (x,neg -> y) = x - y
distortVectorToAngle th (x,neg -> y)
    | hypy > hypx = hypy - extray
    | otherwise = hypx - extrax
  where
    hypy = x / cosDegrees th
    y' = sinDegrees th * hypy
    extray = abs (y - y')
    
    hypx = y / sinDegrees th
    x' = cosDegrees th * hypx
    extrax = abs (x - x')
      

--colideT4Batota :: Jogo -> Tempo -> Tempo -> Int -> Maybe Carro
--colideT4Batota jogo tempo tick p = do
--    jogo' <- colideT4 jogo tempo tick p
--    let score = getScore jogo p
--    let score' = getScore jogo' p
--    trace ("old score " ++ show (posicao $ getCarro p jogo) ++ " " ++ show score ++ "\n" ++ "new score " ++ show (posicao $ getCarro p jogo') ++ " " ++ show score') $ if score' >= score
--        then return $ carros jogo' !! p
--        else mzero

colideT4Batota :: Bool -> Jogo -> Tempo -> Tempo -> Int -> Maybe Carro
colideT4Batota doAtualiza jogo tempo tick p = fmap (getCarro p) $ colideT4 doAtualiza Nothing jogo tempo tick p
        
colideT4 :: Bool -> Maybe Int -> Jogo -> Tempo -> Tempo -> Int -> Maybe Jogo
colideT4 doAtualiza score jogo tempo tick p | tempo < tick = return jogo
colideT4 doAtualiza mbscore jogo tempo tick p = do
    let score = maybe (getScore  jogo p) id mbscore
    let jogo' = if doAtualiza then atualiza tick jogo p quieto else jogo
    let Mapa _ tab' = mapa jogo'
    let carro' = carros jogo !! p
    carro'' <- colide tab' tick carro'
    let jogo'' = atualizaCarro jogo' p carro''
    let score'' = getScore  jogo'' p
    if score'' >= score
        then colideT4 doAtualiza (Just score'') jogo'' (tempo-tick) tick p
        else mzero

vaiMorrer :: Bool -> Tempo -> Tempo -> Jogo -> Int -> Bool
vaiMorrer doAtualiza tempo tick jogo p = isNothing $ colideT4Batota doAtualiza jogo tempo tick p
    where
    Mapa _ tabuleiro = mapa jogo
    carro = getCarro p jogo

vaiColidir :: (Ponto -> Ponto -> Bool) -> Bool -> Tempo -> Tempo -> Jogo -> Int -> Bool
vaiColidir cmp doAtualiza tempo tick jogo p = not $ fmap (posicao) (colideT4Batota doAtualiza jogo tempo tick p) `mbcmp` (Just (tgt))
    where
    mbcmp (Just x) (Just y) = cmp x y
    mbcmp _ _ = False
    Mapa _ tabuleiro = mapa jogo
    carro = getCarro p jogo
    src = posicao carro
    tgt = src .+. (tempo .*. velocidade carro)

menorAngulo a1 a2 = absAngulo (normalizaAngulo a1) < absAngulo (normalizaAngulo a2)
absAngulo x = if x >= 180 then abs (360-x) else x

apontaPara :: [(Peca,Posicao,Orientacao)] -> Int -> Tempo -> Jogo -> Int -> Double
apontaPara pecas 0 tick jogo p = {-error ("aponta " ++ show (direcao $ getCarro p jogo)) -- $ -} direcao $ getCarro p jogo
apontaPara pecas la tick jogo p = case atMay pecas (pred la) of
    Just (peca@(Peca t _),tgt,orientacao) -> do
        let centro = centroPeca t tgt
        let (centrodist,normalizaAngulo -> angulo) = componentsToArrow (centro .-. pos)
        --let centroR = centroPeca t tgt
        --let centroROffset = componentsToArrow (centroR .-. pos)
        if tgt /= pontoToPosicao pos && (la==1 || linhaRecta jogo tick p (centrodist,angulo))
            then {-trace ("from " ++ show pos ++ " to " ++ show tgt ++" "++ show angulo) $ -} angulo
            else {-trace ("nofrom " ++ show pos ++ " to " ++ show tgt ++ "\n" ++ show (take 10 pecas)) $-} apontaPara pecas (pred la) tick jogo p
    Nothing -> apontaPara pecas (pred la) tick jogo p
  where
    carro = getCarro p jogo
    pos@(posx,posy) = posicao carro

contrariaPara :: [(Peca,Posicao,Orientacao)] -> Int -> Tempo -> Jogo -> Int -> Double
contrariaPara pecas la tick jogo p = case headMay tgts of
    Just (tgt',angulo) -> {-trace ("from " ++ show pos ++ " to " ++ show tgt' ++" "++ show angulo ++ " " ++ show invAngV) $-} angulo
    Nothing -> direcao $ getCarro p jogo --error ("contraria " ++ show (direcao $ getCarro p jogo))
  where
    props = pista jogo
    tgts = sortBy maisEstavel $ contrariaPara' la
    carro = getCarro p jogo
    pos@(posx,posy) = posicao carro
    (v,normalizaAngulo -> angV) = componentsToArrow $ velocidade carro
    invAngV = normalizaAngulo $ angV-180
    Mapa _ tab = mapa jogo
    estaPeca = getPeca (pontoToPosicao pos) tab

    contrariaPara' :: Int -> [(Ponto,Double)]
    contrariaPara' 0 = descobre 0 estaPeca (pontoToPosicao pos)
    contrariaPara' la = case atMay pecas (pred la) of
        Just (peca,tgt,orientacao) -> let tgts1 = descobre la peca tgt
                                          tgts2 = contrariaPara' (pred la)
                                      in tgts1 ++ tgts2
        Nothing -> contrariaPara' (pred la)
    
    descobre :: Int -> Peca -> Posicao -> [(Ponto,Double)]
    descobre la peca@(Peca t _) tgt = {-trace ("descobre " ++ show (angulos)) -} angulos
        where
        angulos = map (mapSnd (normalizaAngulo . snd)) $ filter valida $ map calcula pontos
        pontos :: [Ponto]
        pontos = centroPeca t tgt : map (flip ladoPonto tgt) lados ++ (if (k_atrito props < 1 && la==0) then cantosCurva peca tgt else [])
        lados = ladosPeca peca
        calcula :: Ponto -> (Ponto,((Double,Double)))
        calcula p = (p,componentsToArrow $ p .-. pos)
        valida :: (Ponto,((Double,Double))) -> Bool
        valida (_,s) = la == 1 || linhaRecta jogo tick p s
    maisEstavel :: (Ponto,Double) -> (Ponto,Double) -> Ordering 
    maisEstavel (_,th1) (_,th2) = if normalizaAngulo (th1-invAngV) `menorAngulo` normalizaAngulo (th2-invAngV) then LT else GT

ajustaDirecaoCarro :: Jogo -> Carro -> Tempo -> Double -> (Maybe (Acao -> Acao),Double)
ajustaDirecaoCarro jogo carro tick angulo = {-trace ("roda " ++ show ((maybe id id rotacao) quieto) ++ " " ++ show dir ++ " " ++ show dir') $-} (rotacao,dir')
  where
    dir = normalizaAngulo $ direcao carro
    sens = k_roda (pista jogo) * tick 
    dif = normalizaAngulo $ dir-angulo
    difE = normalizaAngulo $ dir+sens-angulo
    difD = normalizaAngulo $ dir-sens-angulo
    (rotacao,dir') = fst $ minimumBy maisCurto [((Nothing,dir),dif),((Just roda_esq,normalizaAngulo $ dir+sens),difE),((Just roda_dir,normalizaAngulo $ dir-sens),difD)]

maisCurto (_,a1) (_,a2) = if a1 `menorAngulo` a2 then LT else if a1==a2 then EQ else GT

linhaRecta :: Jogo -> Tempo -> Int -> (Double,Double) -> Bool
linhaRecta jogo tick p (dist,angulo) = not (vaiColidir cmp False 1 tick jogo' p)
    where
    cmp x y = {-trace ("linhaRecta " ++ show x ++" "++ show y) $-} pontoToPosicao x == pontoToPosicao y
    oldcarro = getCarro p jogo
    carro' = oldcarro { direcao = angulo , velocidade = arrowToComponents (dist,angulo) }
    oldcarros = carros jogo
    carros' = take p oldcarros ++ carro' : drop (succ p) oldcarros
    jogo' = jogo { carros = carros' }
    --v = fst $ componentsToArrow (velocidade oldcarro)

getScore :: Jogo -> Int -> Int
getScore jogo p = score
    where
    (score,pecas) = proximasPecas jogo p

proximasPecas :: Jogo -> Int -> (Int,[(Peca,Posicao,Orientacao)])
proximasPecas jogo p = {-trace ("percurso" ++ show hist ++ "\n" ++ (show $ take 10 $ snd $ cpecas)) $ -} cpecas
    where
    hist = historico jogo !! p
    hist' = if headMay hist == Just (pontoToPosicao pos) then hist else (pontoToPosicao pos):hist
    carro = getCarro p jogo
    pos@(posx,posy) = posicao carro
    cpecas = percursoRestante (reverse hist') jogo
    --cpecas = retiraJaPercorrido (if null hist then [(floor posx,floor posy)] else reverse hist) pecas
    --cpecas = lookupPosicaoPecas (floor posx,floor posy) pecas

percursoRestante :: [Posicao] -> Jogo -> (Int,[(Peca,Posicao,Orientacao)])
percursoRestante ps jogo = (score,pecas')
  where
    pecas = init $ pecasMapa (mapa jogo)
    ori = head pecas
    oris = tail pecas
    allpecas = concat $ repeat pecas
    (score,pecas') = percorreHistorico 1 ori oris ps (Map.singleton (snd3 ori) (1,oris)) allpecas

pecasMap :: [(Peca,Posicao,Orientacao)] -> Map Posicao [(Peca,Posicao,Orientacao)]
pecasMap xs = pecasMap' xs Map.empty
    where
    pecasMap' [] m = m
    pecasMap' (x:xs) m = pecasMap' xs (Map.insert (snd3 x) xs m)

percorreHistorico :: Int -> (Peca,Posicao,Orientacao) -> [(Peca,Posicao,Orientacao)] -> [Posicao] -> Map Posicao (Int,[(Peca,Posicao,Orientacao)]) -> [(Peca,Posicao,Orientacao)] -> (Int,[(Peca,Posicao,Orientacao)])
percorreHistorico score ori oripecas [] m ys = (score,ys)
percorreHistorico score ori oripecas (x:xs) m ys
    | isJust fwd = percorreHistorico (score+1+fromJust fwd) ori oripecas xs mfwd (drop (1+fromJust fwd) ys)
    | null xs = case Map.lookup x m of
        Just ps -> ps
        Nothing -> {-trace ("percorreHistorico "++show x ++ "\n" ++ show (take 10 ys) ++"\n" ++ show (Map.keys m)) -}
            (0,lookupPosicaoPecas x (ori:oripecas))
    | otherwise = percorreHistorico score ori oripecas xs m ys
  where
    fwd = elemIndex x (map snd3 $ take (1+tuning) ys)
--    m' = m --if x == snd3 ori then Map.singleton (snd3 ori) oripecas else m
    mfwd = addToMap (score+1) (score+1+fromJust fwd) m ys

    addToMap :: Int -> Int -> Map Posicao (Int,[(Peca,Posicao,Orientacao)]) -> [(Peca,Posicao,Orientacao)] -> Map Posicao (Int,[(Peca,Posicao,Orientacao)])
    addToMap from to m ys | from > to = m
    addToMap from to m (y:ys) = addToMap (from+1) to (Map.insert (snd3 y) (from,ys) m) ys

retiraJaPercorrido :: [Posicao] -> [(Peca,Posicao,Orientacao)] -> [(Peca,Posicao,Orientacao)]
retiraJaPercorrido [] ys = ys
retiraJaPercorrido xs [] = []
retiraJaPercorrido (x:xs) (y@(_,p,_):ys)
    | x == p = retiraJaPercorrido xs ys
    | isJust idx  = retiraJaPercorrido xs (drop (fromJust idx) ys)
    | otherwise = retiraJaPercorrido xs (y:ys)
  where
    idx = elemIndex x (map snd3 $ take tuning ys)

fst3 (x,_,_) = x
snd3 (_,y,_) = y

lookupPosicaoPecas :: Posicao -> [(Peca,Posicao,Orientacao)] -> [(Peca,Posicao,Orientacao)]
lookupPosicaoPecas p [] = []
lookupPosicaoPecas p (x@(_,p',_):xs) = if p == p' then xs else lookupPosicaoPecas p xs

-- * Ações

trava a = a { travar = True }
turbina p a = a { nitro = Just p }
acelera a = a { acelerar = True }
roda_esq a = a { esquerda = True }
roda_dir a = a { direita = True }
quieto = Acao False False False False Nothing

compareSnd :: Ord b => (a,b) -> (a,b) -> Ordering
compareSnd (x1,y1) (x2,y2) = compare y1 y2

cosDegrees th = cos (radians th)
sinDegrees th = sin (radians th)

mapSnd f (x,y) = (x,f y)
