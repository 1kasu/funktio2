module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Data.Monoid
import Data.Maybe

instance Monoid Bool where
    mempty = True
    mappend a b = a && b

data AsteroidWorld = Play [Rock] Ship [Bullet] UFO Float Float [Action] Int
                   | GameOver 
                   deriving (Eq,Show)

type Velocity     = (Float, Float)
type Size         = Float
type Age          = Float
type H            = Float

data UfoPaikka = UP PointInSpace (Maybe PointInSpace) H
    deriving (Eq, Show)


data Ship   = Ship   PointInSpace Velocity      
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age  
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity 
    deriving (Eq,Show)
data UFO    = UFO    UfoPaikka Velocity (Maybe (PointInSpace, Float))
    deriving (Eq, Show)
    
data Action = EtsiPaikka | SiirryPaikkaan | Pakene | Tyhja | EtsiLahin
    deriving (Eq, Show)

initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)    
                   ,Rock (-45,201)  45 (13,-8) 
                   ,Rock (45,22)    25 (-2,8)  
                   ,Rock (-210,-15) 30 (-2,-8) 
                   ,Rock (-45,-201) 25 (8,2)   
                   ] -- The default rocks
                   (Ship (0,0) (0,5)) -- The initial ship
                   [] -- The initial bullets (none)
                   (UFO (UP (-80,50) Nothing 0.0) (0,3) Nothing)
                   0 0
                   [Tyhja, EtsiPaikka, EtsiLahin, Pakene, EtsiLahin, Pakene, Pakene, SiirryPaikkaan] 0


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)
simulateWorld _        GameOver          = GameOver  
simulateWorld timeStep (Play rocks (Ship shipPos shipV) bullets (UFO (UP u1 u2 h) ufoV et) aika taika actions n)
  | any (collidesWith shipPos) rocks = GameOver
  | otherwise = Play (concatMap updateRock rocks) 
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
                              uusiUFO (aika + timeStep) uusiaika actions (uusiN)
  where
      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
     
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v) 
       | collidesWithBullet r && s < 7 
            = []
       | collidesWithBullet r && s > 7 
            = splitRock r
       | otherwise                     
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
 
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collidesWith p) rocks 
             = [] 
        | otherwise                  
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a + timeStep)] 

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)
      newUFOPos :: UfoPaikka
      newUFOPos = UP (restoreToScreen (u1 .+ timeStep .* ufoV)) u2 h
      
      uusiUFO = case actions!!(n `rem` (length actions)) of
            EtsiPaikka -> case u2 of 
                Nothing ->  case etsiTurvaPaikka ((round aika) `rem` 150) 30 (Ship shipPos shipV) bullets (UP u1 u2 h) of
                                        Nothing -> UFO newUFOPos ufoV et
                                        Just p -> UFO (aloitaSiirto p newUFOPos) ufoV et
                Just x -> UFO newUFOPos ufoV et
            SiirryPaikkaan -> siirryUfo timeStep (UFO (newUFOPos) ufoV et)
            Pakene -> case et of 
                Nothing -> UFO newUFOPos ufoV et
                Just (paikka_l, etaisyys_l) -> UFO newUFOPos (20 .* norm (u1 .- paikka_l)) et
            Tyhja -> UFO newUFOPos ufoV et
            EtsiLahin -> UFO newUFOPos ufoV (Just (annaLahin (Ship shipPos shipV) bullets u1))
            
      
      
      uusiaika = if aika - taika >= 1 then aika else taika
      uusiN = if uusiaika == aika then n + 1 `rem` (length actions) else n
      

splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]

restoreToScreen :: PointInSpace -> PointInSpace
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)



annaLahin :: Ship -> [Bullet] -> PointInSpace -> (PointInSpace, Float)
annaLahin ship bullets p = foldl (\(px,fl) (yx, yl) -> if fl < yl then (px, fl) else (yx,yl)) shipp yl
        where
            bulletp = map (\(Bullet rp _ _)  -> (rp , (etaisyys rp p) - 5)) bullets
            shipp = (\(Ship rp _ )  -> (rp , (etaisyys rp p) - 10)) ship
            yl = bulletp


cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
    | x < (-400) = 800+x
    | x > 400    = x-800
    | otherwise  = x

drawWorld :: AsteroidWorld -> Picture
drawWorld GameOver 
   = scale 0.3 0.3 
     . translate (-400) 0 
     . color red 
     . text 
     $ "Game Over!"

drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets (UFO (UP (u1x,u1y) u2p h) _ _) _ _ _ _)
  = pictures [ship, asteroids,shots, ufo1, ufo2]
   where 
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [translate x y (color orange (circle s)) 
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color red (circle 2)) 
                         | Bullet (x,y) _ _ <- bullets]
    ufo1       = color blue (pictures [translate u1x u1y (circle (10*(1-h)))])
    ufo2       = case u2p of
                Just (u2x, u2y) -> color blue (pictures [translate u2x u2y (circle (10*h))])
                Nothing -> color black (pictures [translate 0 0 (circle (10*h))])
    --paikat     = pictures [translate x y (color green (circle 2)) 
    --                     | (x,y) <- map annaPaikka [50..200]]
    --keskus     = pictures [translate 0 0 (color yellow (circle 2))]
                
kelpaakoPaikka :: Ship -> [Bullet] -> UfoPaikka -> PointInSpace -> Maybe PointInSpace
kelpaakoPaikka (Ship sp _) bullets (UP u1 _ _) paikka = if shipOk <> bulletOk <> ufoOk
                then Just paikka
                else Nothing
    where
        shipOk = onkoEtainen sp 10 paikka ufoR
        bulletOk = foldMap (\(Bullet p _ _) -> onkoEtainen p 8 paikka ufoR) bullets
        ufoOk = onkoEtainen u1 40 paikka ufoR
        ufoR = 20
        

aloitaSiirto :: PointInSpace -> UfoPaikka -> UfoPaikka
aloitaSiirto p (UP p1 Nothing h) = UP p1 (Just p) h
aloitaSiirto p u = u
        
siirryUfo :: Float -> UFO -> UFO
siirryUfo timeStep (UFO (UP p1 p2 h) v et) = case p2 of
        Nothing -> UFO (UP p1 p2 h) v et
        Just x -> if perilla 
            then UFO (UP x Nothing 0.0) v et
            else UFO (UP p1 p2 uusiH) v et
    where
        perilla = h >= 1
        uusiH = 5.0 * timeStep + h

etaisyys :: PointInSpace -> PointInSpace -> Float
etaisyys (ax,ay) (bx,by) = sqrt (a * a + b * b)
    where
    a = ax - bx
    b = ay - by --Pikkuvirheenä tässä oli yhteenlasku...
  
onkoEtainen :: PointInSpace -> Size -> PointInSpace -> Size -> Bool
onkoEtainen a s b bs = (etaisyys a b) - s - bs > 0
  
annaPaikka :: Int -> PointInSpace--Ihania virheitä tuli kääntäjältä, kun oli vääriä lukutyyppejä jne.
annaPaikka i = case sektori of
        0 -> restoreToScreen (vektorA .+ vektorAB)
        1 -> restoreToScreen (vektorB .+ vektorBC)
        2 -> restoreToScreen (vektorC .+ vektorCD)
        _ -> restoreToScreen (vektorD .+ vektorDA)
    where
        kehan_pituus = 20 + 5 * 2^kieroksia :: Float --oikeammin säde
        vektorA = ( 0.0, kehan_pituus) :: PointInSpace
        vektorB = ( kehan_pituus, 0.0) :: PointInSpace
        vektorC =( 0.0, -kehan_pituus) :: PointInSpace
        vektorD = ( -kehan_pituus, 0.0) :: PointInSpace
        vektorAB = suhdeluku .* (vektorB .- vektorA)
        vektorBC = suhdeluku .* (vektorC .- vektorB)
        vektorCD = suhdeluku .* (vektorD .- vektorC)
        vektorDA = suhdeluku .* (vektorA .- vektorD)
        suhdeluku = (fromIntegral sektorissa_liikutaan) / (fromIntegral(sektorin_tilojenmaara)) :: Float
        
        ro = fromIntegral (kieroksia + 1) :: Float      
        kieroksia = i `div` kierros :: Int
        kieroksista_Ylijaavat = i `rem` kierros :: Int
        sektori = (kieroksista_Ylijaavat `div` jakaja) :: Int
        sektorissa_liikutaan = kieroksista_Ylijaavat `rem` jakaja :: Int
        suuntia = 4 :: Int
        jakaja = 4 :: Int
        kierros = suuntia * jakaja :: Int
        sektorin_tilojenmaara = kierros `div` suuntia :: Int
        

etsiTurvaPaikka :: Int -> Int -> Ship -> [Bullet] -> UfoPaikka -> Maybe PointInSpace
etsiTurvaPaikka _ 0 _ _ _ = Nothing
etsiTurvaPaikka monesta monta ship bullets ufo = case kelpaako monesta of
        Just p -> Just p
        Nothing -> etsiTurvaPaikka (monesta + 1) monta ship bullets ufo
    where
        kelpaako i = kelpaakoPaikka ship bullets ufo (annaPaikka i)
        
 
handleEvents :: Event -> AsteroidWorld -> AsteroidWorld
handleEvents _ GameOver = GameOver
handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) bullets ufo t t2 a b)
             = Play rocks (Ship shipPos newVel) 
                          (newBullet : bullets)
                          ufo t t2 a b
 where 
     newBullet = Bullet shipPos 
                        (-150 .* norm (shipPos .- clickPos)) 
                        0
     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))

handleEvents _ w = w

type PointInSpace = (Float, Float)
(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> PointInSpace -> PointInSpace
s .* (u,v) = (s*u,s*v)


infixl 6 .- , .+
infixl 7 .*

norm :: PointInSpace -> PointInSpace
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: PointInSpace -> Float
magV (x,y) = sqrt (x**2 + y**2) 

limitMag :: Float -> PointInSpace -> PointInSpace
limitMag n pt = if (magV pt > n) 
                  then n .* (norm pt)
                  else pt

rotateV :: Float -> PointInSpace -> PointInSpace
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)


main = play 
         (InWindow "Asteroids!" (550,550) (20,20)) 
         black 
         24 
         initialWorld 
         drawWorld 
         handleEvents
         simulateWorld
