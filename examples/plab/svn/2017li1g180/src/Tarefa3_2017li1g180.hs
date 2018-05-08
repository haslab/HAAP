{-# LANGUAGE PatternGuards #-}

module Tarefa3_2017li1g180 where

import LI11718
import Tarefa2_2017li1g180
import Test.QuickCheck.Gen
import Data.List
import Data.Maybe
import Safe

testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = unitTests

randomizeCarro :: Tabuleiro -> Double -> Carro -> Gen Carro
randomizeCarro tab delta c = do
    suchThat genC (validaCarro tab)
  where
    (x,y) = posicao c
    genC = do
        dx <- choose (0,delta)
        dy <- choose (0,delta)
        let x' = x + dx
        let y' = y + dy
        return c { posicao = (x',y') }

ranksT3 :: [(Tabuleiro,Tempo,Carro)]
ranksT3 = unitTests

-- Nuno/Hugo's simpler version
-- ranksT3Nuno :: [(Tabuleiro,Tempo,Carro)]
-- ranksT3Nuno = [(tab1,5,carro1)]
--     where
--     tab1 = [[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
--                           ,[Peca Lava altLava, Peca (Curva Norte) 2,Peca Recta 2,Peca (Curva Este) 2, Peca Lava altLava]
--                           ,[Peca Lava altLava, Peca Recta 2,Peca Lava altLava,Peca Recta 2, Peca Lava altLava]
--                           ,[Peca Lava altLava, Peca (Curva Oeste) 2,Peca Recta 2,Peca (Curva Sul) 2, Peca Lava altLava]
--                           ,[Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava, Peca Lava altLava]
--                           ]
--     carro1 = Carro (3,3) 30 (2,1.5)

solutionsT3 :: [((Tabuleiro,Tempo,Carro),Maybe Carro)]
solutionsT3 = zip ranksT3 (map aux ranksT3)
    where
    aux x@(tab,tempo,carro) = movimenta tab tempo carro

compareT3Solutions :: Double -> Maybe Carro -> Maybe Carro -> Double
compareT3Solutions distance Nothing Nothing = 100
compareT3Solutions distance Nothing (Just y) = 0
compareT3Solutions distance (Just x) Nothing = 0
compareT3Solutions distance (Just x) (Just y) = (pos+dir+vel) / 3
    where
    pos = if distance == 0 then 100 else (1 - dist (posicao x) (posicao y) / distance) * 100
    dir = (1 - (normAngulo (abs (direcao x - direcao y))) / 360) * 100
    vel = if distance == 0 then 100 else (1 - dist (velocidade x) (velocidade y) / distance) * 100

normAngulo x = if x >= 360 then normAngulo (x - 360) else x

genTempo :: Gen Tempo
genTempo = choose (0,30)

validaCarro :: Tabuleiro -> Carro -> Bool
validaCarro tab carro = validaPonto (posicao carro) tab && (not $ derrete tab (posicao carro))

derrete :: Tabuleiro -> Ponto -> Bool
derrete t p = derrete' tp a
  where
  (i,j) = ponto2Pos p
  a = denorm p (i,j)
  Peca tp _ = (atNote "derrete" (atNote "derrete" t j) i)
  derrete' :: Tipo -> Ponto -> Bool 
  derrete' Lava _ = True
  -- hpacheco: mudei para incluir parede
  derrete' (Curva Norte) (x,y) = x < (1-y)
  derrete' (Curva Este) (x,y) = x > y
  derrete' (Curva Sul) (x,y) = x > (1-y)
  derrete' (Curva Oeste) (x,y) = x < y
  derrete' _ _  = False

movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta m t c = case (colideLocal m v (a,b) i) of
                  Nothing -> Nothing
                  Just (p',v') -> Just $ c { posicao = p', velocidade = v' }
  where a = posicao c
        v = velocidade c
        b = a .+. (t .*. v)
        i = ponto2Pos a

bounce :: (Double,Double) -> (Double,Double) -> (Double,Double)
bounce (d,a1) (1,a2) = componentsToArrow $ (arrowToComponents (d,a1)) .+. (-x,-y)
    where dp = 2 * (arrowToComponents (d,a1) .$. arrowToComponents (1,a2))
          (x,y) = dp .*. (arrowToComponents (1,a2))

colideLocal :: Tabuleiro -> Velocidade -> (Ponto,Ponto) -> Posicao -> Maybe (Ponto,Velocidade)
colideLocal e v ab ij = case colideLocalAcc [] e v ab ij of
                          (l,Just v) -> Just (last l,v)
                          (_,Nothing) -> Nothing

colideLocalAcc :: [Ponto] -> Tabuleiro -> Velocidade -> (Ponto,Ponto) -> Posicao -> ([Ponto],Maybe Velocidade)
colideLocalAcc ps m v (a,b) (i,j) | morto = (ps++[a,b],Nothing)
                                  | colideInterno = colideLocalAcc (ps++[a]) m iVel iPos (i,j)  -- se houverem colisões internas (diagonais), da prioridade e volta a aplicar novo vector
                                  | dentroPeca (a,b) (i,j) g = (ps++[a,b],Just v)                    -- caso contrario, se estiver dentro da peça pode parar
                                  | colideExterno = colideLocalAcc (ps++[a]) m eVel ePos (i,j)  -- se passar para fora da peça mas houver parede, colide na parede e volta a aplicar novo vector
                                  | otherwise = colideLocalAcc (ps++[a]) m v (aPos,b) ij'       -- se passar para fora da peça mas não houver parede, chama na peça seguinte
  where (_,g) = componentsToArrow (b.-.a)

        morto = (isJust int && isNothing (fromJust int)) || (colideExterno && not (fromJust $ temParede m (i,j) (fromJust ext)))

        int = colisaoInterna (atNote "colideLocalAcc" (atNote "colideLocalAcc" m j) i) (i,j) (a,b)                            
        colideInterno = isJust int && isJust (fromJust int) && colisaoRelevante (a,b) (fromJust (fromJust int))
        (iPos,iVel) = inv v (a,b) (fromJust int) 

        ext = atravessaOnde (i,j) (a,b)    
        colideExterno = isJust ext && (isJust . temParede m (i,j)) (fromJust ext)
        (ePos,eVel) = inv v (a,b) (Just (norm (fst (fromJust ext)) (i,j),snd (fromJust ext)))

        (aPos,aVel) = (norm (fst (fromJust ext)) (i,j), snd (fromJust ext))
        ij' = atravessa aVel (i,j)  


norm (x,y) (i,j) = (x+toEnum i,y+toEnum j)
denorm (x,y) (i,j) = (x-toEnum i,y-toEnum j)

-- dada uma velocidade e um deslocamento, inverte ambos dada uma colisão
inv :: Velocidade -> (Ponto,Ponto) -> Maybe (Ponto,Double) -> ((Ponto,Ponto),Velocidade)
inv v ab Nothing = (ab,v)
inv v ab (Just (c,g)) = ((c,d),arrowToComponents (vd,v'))
  where (_,d,v') = inverte ab c g
        (vd,_) = componentsToArrow v

inverte :: (Ponto,Ponto) -> Ponto -> Double -> (Ponto,Ponto,Double)
inverte (a,b) c g = (c,d,v')
  where (z,v') = bounce (componentsToArrow (b .-. c)) (1,g)
        d = c .+. arrowToComponents (z,v')

-- dada uma peça e um segmento (relativo à peça), detecta colisoes e devolve angulo da normal
-- colisões internas apenas acontecem em diagonais e no maximo uma
colisaoInterna :: Peca -> Posicao -> (Ponto,Ponto) -> Maybe (Maybe (Ponto,Double))
colisaoInterna (Peca (Curva d) al) ij (a,b) | d == Norte || d == Sul = f (intersecta ab ((1,0),(0,1))) (135 + dlt)
                                            | otherwise = f (intersecta ab ((0,0),(1,1))) (45 + dlt)
  where f :: Maybe Ponto -> Double -> Maybe (Maybe (Ponto,Double))
        f Nothing _ = Nothing
        f (Just a) b | al < altLava = Just $ Just (norm a ij,b)
                     | otherwise = Just Nothing
        dlt = if d == Norte || d == Este then 180 else 0
        ab = (denorm a ij, denorm b ij)
colisaoInterna _ _ _ = Nothing

-- se o movimento mantem-se dentro da peça
dentroPeca :: (Ponto,Ponto) -> Posicao -> Double -> Bool
dentroPeca (a,b) (i,j) g = belongs b
  where belongs (x,y) = (x == toEnum i+1 || floor x == i) && (floor y == j || y == toEnum j+1)

-- dado um segmento (relativo à peça), calcula pontos de interceção e angulos das normais
-- entre 0 e 2 no caso extremo (segmento que começa em cima da linha e atravessa outra)
atravessaOnde :: Posicao -> (Ponto,Ponto) -> Maybe (Ponto, Double)
atravessaOnde ij (x,y) | null rel = Nothing
                       | length rel == 1 = Just (head rel)
                       | otherwise = Just (last rel) --error $ "Duas travessias: "++(show rel)
  where rel = filter (colisaoRelevante (x,y)) $ normalize $ zip is [0,270,90,180]
        is = [intersecta xy ((i,i),(toEnum j,toEnum $ (j+1)`mod`2)) | i <- [0,1], j <- [0,1]]
        normalize = catMaybes . map (\(a,b) -> if (isJust a) then Just (fromJust a,b) else Nothing)
        xy = (denorm x ij, denorm y ij)

-- para uma peça e uma aresta, testa se vai haver colisão
temParede :: Tabuleiro -> Posicao -> (Ponto,Double) -> Maybe Bool
temParede m (i,j) c | t == Lava = Just (a1 < altLava)
                    | snd c == 270 && (t == Curva Sul || t == Curva Oeste) = Just (a1 < altLava)
                    | snd c == 180 && (t == Curva Norte || t == Curva Oeste) = Just (a1 < altLava)
                    | snd c == 90 && (t == Curva Norte || t == Curva Este) = Just (a1 < altLava)
                    | snd c == 0 && (t == Curva Sul || t == Curva Este) = Just (a1 < altLava)
                    | middle && mwall t0 (fst c) && aa > aa' = Just True
                    | middle && mfall t0 (fst c) && aa < aa' = Just False                    
                    | a2 > a1 = Just True
                    | a2 < a1 = Just False
                    | otherwise = Nothing
  where (i',j') = atravessa (snd c) (i,j)
        Peca t aa = atNote "temParede" (atNote "temParede" m j') i'
        Peca t0 aa' = atNote "temParede" (atNote "temParede" m j) i
        isR (Rampa _) = True
        isR _ = False
        (a1,a2) = normAlturas (atNote "temParede" (atNote "temParede" m j) i) (atNote "temParede" (atNote "temParede" m j') i')
        middle = case t of (Rampa x) -> t0 == (Rampa (toEnum (((fromEnum x) + 2) `mod` 4))) && (abs $ aa-aa') == 1
                           otherwise -> False
        mwall (Rampa Oeste) (x,y) = x >= 0.5
        mwall (Rampa Este) (x,y) = x <= 0.5
        mwall (Rampa Norte) (x,y) = y >= 0.5
        mwall (Rampa Sul) (x,y) = y <= 0.5
        mfall r p = not $ mwall r p
        
normAlturas :: Peca -> Peca -> (Altura,Altura)
normAlturas (Peca (Rampa _) a1) p@(Peca _ a2) | a2 > a1 = normAlturas (Peca Recta (a1+1)) p
                                              | otherwise = normAlturas (Peca Recta a1) p
normAlturas p@(Peca _ a1) (Peca (Rampa _) a2) | a2 < a1 = normAlturas p (Peca Recta (a2+1))
                                              | otherwise = normAlturas p (Peca Recta a2)
normAlturas (Peca _ a1) (Peca _ a2) = (a1,a2)
 
-- dado o angulo de travessia, da a nova posiçao
atravessa :: Double -> Posicao -> Posicao
atravessa 270 (i,j) = (i,j-1)
atravessa 90  (i,j) = (i,j+1)
atravessa 0   (i,j) = (i-1,j)
atravessa 180 (i,j) = (i+1,j)

-- dado o deslocamento e o ponto de colisão com o angulo da normal, testa de é relevante
-- é relevante se a diferença de angulos > 90º
colisaoRelevante :: (Ponto,Ponto) -> (Ponto,Double) -> Bool
colisaoRelevante ((a1,a2),(b1,b2)) ((x,y),a) = diff > 90 && diff < 270 
  where (_,a') = componentsToArrow (b1-a1,b2-a2)
        diff = f (a-a')
        f x | x < 0 = f (x+360)
            | x > 360 = f (x-360)
            | otherwise = x


-- geometry

-- copiado do Gloss para Double
intersecta :: (Ponto,Ponto) -> (Ponto,Ponto) -> Maybe Ponto
intersecta (p1,p2) (p3,p4) | Just p0 <- intersectaL p1 p2 p3 p4
                           , t12   <- closestPontoOnL p1 p2 p0
                           , t23   <- closestPontoOnL p3 p4 p0
                           , t12 >= 0 && t12 <= 1
                           , t23 >= 0 && t23 <= 1
                           = Just p0
                           | otherwise
                           = Nothing

-- copiado do Gloss para Double
intersectaL :: Ponto -> Ponto -> Ponto -> Ponto -> Maybe Ponto
intersectaL (x1, y1) (x2, y2) (x3, y3) (x4, y4)
 = let  dx12  = x1 - x2
        dx34  = x3 - x4
        dy12  = y1 - y2
        dy34  = y3 - y4
  
        den = dx12 * dy34  - dy12 * dx34

    in if den == 0
       then Nothing
       else let det12 = x1*y2 - y1*x2 
                det34 = x3*y4 - y3*x4 
                numx  = det12 * dx34 - dx12 * det34
                numy  = det12 * dy34 - dy12 * det34
            in Just (numx / den, numy / den)

-- copiado do Gloss para Double
closestPontoOnL :: Ponto -> Ponto -> Ponto -> Double
closestPontoOnL p1 p2 p3 = (p3 .-. p1) .$. (p2 .-. p1) / (p2 .-. p1) .$. (p2 .-. p1)

(.*.) :: Double -> (Double,Double) -> (Double,Double)
(.*.) x (a,b) = ((x*a),(x*b))
       
(.+.) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(.+.) (x,y) (a,b) = ((x+a),(y+b))

(.-.) :: (Double,Double) -> (Double,Double) -> (Double,Double)
(.-.) (x,y) (a,b) = ((x-a),(y-b))

-- the dot product between two (Double,Double)s
(.$.) :: (Double,Double) -> (Double,Double) -> Double
(.$.) (d1,a1) (d2,a2) = (x1*x2) + (y1*y2)
    where (x1,y1) = (d1,a1)
          (x2,y2) = (d2,a2)

radians th = th * (pi/180)
degrees th = th * (180/pi)

arrowToComponents :: (Double,Double) -> Ponto
arrowToComponents (v,th) = (getX v th,getY v th)
    where getX v th = v * cos (radians (th))
          getY v th = v * sin (radians (-th))

componentsToArrow :: Ponto -> (Double,Double)
componentsToArrow (x,0) | x >= 0 = (x,0)
componentsToArrow (x,0) | x < 0 = (abs x,180)
componentsToArrow (0,y) | y >= 0 = (y,-90)
componentsToArrow (0,y) | y < 0 = (abs y,90)
componentsToArrow (x,y) = (hyp,dir angle)
    where
    dir o = case (x >= 0, y >= 0) of
                (True,True) -> -o
                (True,False) -> o
                (False,False) -> 180 - o
                (False,True) -> 180 + o
    hyp = sqrt ((abs x)^2 + (abs y)^2)
    angle = degrees $ atan (abs y / abs x)

dist :: Ponto -> Ponto -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2+(y2-y1)^2)


--------------

testeMapa =
  [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0
               ,Peca Lava 0,Peca Lava 0,Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1)
               ,Peca (Rampa Este) (-1),Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0,Peca (Rampa Oeste) 0,Peca (Rampa Este) 0,Peca Recta 1,Peca (Rampa Oeste) 0,Peca (Rampa Oeste) (-1)
               ,Peca Recta (-1),Peca (Rampa Oeste) (-2),Peca (Rampa Oeste) (-3),Peca (Rampa Este) (-3),Peca (Rampa Este) (-2),Peca (Curva Sul) (-1),Peca Lava 0]
  ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0
               ,Peca Lava 0,Peca Lava 0,Peca Lava 0]]
--   00011110000009999001100999  
-- ..//>>[]<<[][]<<[]>>>><<<<\\..
-- ..\\>><<>>[]<<<<[]<<<<>>>>//..
--   00011001111009999887788999

testeCurvas =
  [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Norte) 0,Peca (Rampa Oeste) (-1),Peca (Curva Este) (-1),Peca Lava 0,Peca (Curva Norte) 0,Peca (Curva Este) 0
               ,Peca Lava 0,Peca (Curva Norte) 1,Peca (Curva Este) 1,Peca Lava 0]
  ,[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Curva Oeste) (-1),Peca (Rampa Este) (-1),Peca (Curva Sul) 0,Peca (Curva Oeste) 0
               ,Peca (Rampa Este) 0,Peca (Curva Sul) 1,Peca Recta 1,Peca Lava 0]
  ,[Peca Lava 0,Peca (Curva Oeste) 1,Peca (Rampa Oeste) 0,Peca Recta 0,Peca (Rampa Oeste) (-1),Peca Recta (-1),Peca (Rampa Este) (-1)
               ,Peca (Rampa Este) 0,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0]
  ,[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- ..//<<\\..//\\..//\\..
-- ..\/..\\>>//\\>>//[]..
-- ..\\<<[]<<[]>>>>[]//..

unitTests :: [(Tabuleiro,Tempo,Carro)]
unitTests = [
  --- apenas um passo recursivo
  -- rectas: passa percurso
    (testeMapa , 1.2, Carro (5.5,1.5) 0 (0.9,0.1))
  -- rectas: cai adjacente
  , (testeMapa , 1.2, Carro (5.5,2.5) 0 (0.1,-0.9))
  -- rectas: passa adjacente
  , (testeMapa , 1.2, Carro (8.5,1.5) 0 (0.1,0.9))
  -- rectas: choca adjacente
  , (testeMapa , 1.2, Carro (5.5,1.5) 0 (0.1,0.9))
  -- rectas: cai lava
  , (testeMapa , 1.2, Carro (3.5,1.5) 0 (0.1,-0.9))
  -- rectas: passa lava (mesma altura)
  , (testeMapa , 1.2, Carro (5.5,1.5) 0 (0.1,-0.9))
  -- rectas: choca lava
  , (testeMapa , 1.2, Carro (8.5,1.5) 0 (0.1,-0.9))
  -- curva: choca lava
  , (testeMapa , 1.2, Carro (13.4,1.5) 0 (0.3,-0.1))
  -- curva: cai lava (mesma altura)
  , (testeMapa , 1.2, Carro (1.6,1.5) 0 (-0.3,-0.1))

  -- recta->curva: cai
  , (testeCurvas , 1.2, Carro (8.5,3.5) 0 (0.1,-0.9))
  -- recta->curva: lava
  , (testeCurvas , 1.2, Carro (5.5,3.5) 0 (0.1,-0.9))
  -- recta->curva: choca
  , (testeCurvas , 1.2, Carro (3.5,3.5) 0 (0.1,-0.9))

  --- dois+ passos recursivos
  -- rectas: passa percurso 2
  , (testeMapa , 1.2, Carro (3.6,1.5) 0 (-1.6,0.1))  
  -- rectas: passa percurso 3
  , (testeMapa , 1.2, Carro (4.6,1.5) 0 (-2.6,0.1))  
  -- curvas: cai lava (mesma altura) 3
  , (testeMapa , 1.2, Carro (3.6,1.5) 0 (-2.6,0.1))  
  -- rectas: adjacente + cai lava (mesma altura) 10 
  , (testeMapa , 5.5, Carro (12.5,1.2) 0 (-2.1,0.1))
  -- rectas: adjacente + choca lava 2 
  , (testeMapa , 1.2, Carro (8.5,1.5) 0 (0.1,1.9))
  -- rectas: choca lava + adjacente 2 
  , (testeMapa , 1.2, Carro (8.5,2.5) 0 (0.1,1.9))
  -- rectas: choca lava + adjacente + choca lava 3 
  , (testeMapa , 1.2, Carro (8.5,2.5) 0 (0.1,2.3))
  -- rectas: choca lava + adjacente + choca lava + adjacente 4 
  , (testeMapa , 1.2, Carro (8.5,2.5) 0 (0.1,3.3))
  -- rectas: choca adjacente + cai lava (mesma altura) 2
  , (testeMapa , 1.2, Carro (5.5,1.5) 0 (0.1,1.9))
  -- curvas: adjacente + choca lava 3
  , (testeMapa , 1.2, Carro (11.6,1.5) 0 (2.1,0.1))
  -- curva: adjacente + cai lava 2
  , (testeMapa , 1.2, Carro (1.6,2.5) 0 (-0.1,-1.1))
  -- curva: choca lava 2
  , (testeMapa , 1.2, Carro (12.5,1.5) 0 (2.8,-0.1))
  -- curva: cai 2 (passa a lava) -- @nmm nao percebo a particularidade deste teste!
  , (testeMapa , 1.2, Carro (1.6,2.5) 0 (-0.3,-0.1))

  ----- PLANO NOVO (cai se queda >= 1, choca se degrau >= 1, passa se diff. entre 1 e -1)
  -- rampas: cai 
  , (testeMapa , 1.2, Carro (10.5,1.5) 0 (-0.1,0.9))
  -- rampas: choca 
  , (testeMapa , 1.2, Carro (10.5,2.5) 0 (0.1,-0.9))
  -- rampas: passa (igual)
  , (testeMapa , 1.2, Carro (2.5,1.5) 0 (0.1,0.9))
  -- rampas x2: passa (um pouco maior)
  , (testeMapa , 1.2, Carro (4.5,1.5) 0 (0.1,0.9))
  -- rampas x2: passa (um pouco menor)
  , (testeMapa , 1.2, Carro (4.5,1.5) 0 (-0.1,0.9))
  ---- casos difíceis (sobe/desce Bom Jesus)!
  -- rampas: choca (tricky)
  , (testeMapa , 1.2, Carro (9.5,2.5) 0 (0.1,-0.9))
  -- rampas: não choca (tricky)
  , (testeMapa , 1.2, Carro (9.5,2.5) 0 (-0.1,-0.9))
  -- rampas: passa (tricky)
  , (testeMapa , 1.2, Carro (9.5,1.5) 0 (0.1,0.9))
  -- rampas: não passa (tricky)
  , (testeMapa , 1.2, Carro (9.5,1.5) 0 (-0.1,0.9))
  -- rampas: choca rés-vés (tricky x2)
  , (testeMapa , 1.2, Carro (9.5,2.5) 0 (0,-0.9))
  -- rampas: choca lava
  , (testeMapa , 1.2, Carro (9.5,2.5) 0 (0.1,0.9))
  -- rampas: cai lava
  , (testeMapa , 1.2, Carro (9.5,1.5) 0 (0.1,-0.9))
  ------
  
  ----- PLANO ANTIGO (cai se altura maior, choca se menor, passa se igual)
  -- -- rampas: cai 
  -- , (testeMapa , 1.2, Carro (6.5,2.5) 0 (0.1,-0.9)) -- oracle: (6.6,1.4), got Noth
  -- -- rampas: passa
  -- , (testeMapa , 1.2, Carro (2.5,1.5) 0 (0.1,0.9))
  -- -- rampas: choca
  -- , (testeMapa , 1.2, Carro (3.5,2.5) 0 (0.1,-0.9)) -- oracle: (_.1.5), got (_,2.6)
  -- -- rampas x2: choca
  -- , (testeMapa , 1.2, Carro (4.5,1.5) 0 (0.1,0.9))  -- oracle (_.2.5), got (_,1.4)
  -- -- rampas x2: cai
  -- , (testeMapa , 1.2, Carro (4.5,1.5) 0 (-0.1,0.9)) -- oracle (4.4,2.6), got Noth
  -- -- rampas x2: bónus - passa?
  -- , (testeMapa , 1.2, Carro (4.5,1.5) 0 (0,0.9))
  -------------
  ]



