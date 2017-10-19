module Misc where
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game
  import Graphics.Gloss.Interface.Pure.Simulate
  import Graphics.Gloss.Interface.Pure.Display
  import Flow
  import Entity

  getVel :: Direction -> Velocity
  getVel U = (0, 300)
  getVel D = (0, (-300))
  getVel L = ((-300, 0))
  getVel R = (300, 0)
  getVel N = (0, 0)
      
  f b rs = concat $ map (\r -> if g b r then [] else [r]) rs

  g (Bullet (x, y) _) r = checkCollision r
    where
      checkCollision (Rock (rx, ry) _ _) = (x+45) >= rx && (x+45) <= (rx+90) && (y+45) >= ry && (y+45) <= (ry+90)

  h bullets rocks = concat $ map (flip f rocks) bullets

  collide :: Ship -> [Rock] -> Bool
  collide (Ship (x, y) _ _) rocks = any (==True) (map checkCollision rocks)
    where
      checkCollision (Rock (rx, ry) _ _) = (x+45) >= rx && (x+45) <= (rx+90) && (y+45) >= ry && (y+45) <= (ry+90)

  collideWithRock :: Bullet -> [Rock] -> Bool
  collideWithRock (Bullet (x, y) _) rocks = any (==True) (map checkCollision rocks)
    where
      checkCollision (Rock (rx, ry) _ _) = (x+45) >= rx && (x+45) <= (rx+90) && (y+45) >= ry && (y+45) <= (ry+90)

  hitRock :: Bullet -> [Rock] -> [Rock]
  hitRock b@(Bullet (x, y) _) rocks
    | rocks == [] = []
    | checkCollision (head rocks) = [Rock (0, 0) (0, 0) 1] ++ hitRock b (tail rocks)
    | otherwise = [head rocks] ++ hitRock b (tail rocks)
    where
      checkCollision (Rock (rx, ry) _ _) = x >= rx && x <= (rx+45) && y >= ry && y <= (ry+45)

  updateShip :: Float -> Ship -> Ship
  updateShip _ s = s |> move |> checkBounds
    where
      move = (\(Ship (x, y) (xV, yV) d) -> Ship (x+xV, y+yV) (xV, yV) d)

  updateRocks :: Float -> [Rock] -> [Bullet] -> [Rock]
  updateRocks t rs bs = rs |> move |> check
    where
      check = map checkBounds
      move = map (\(Rock (x, y) (xV, yV) age) -> Rock (x+(xV*t), y+(yV*t)) (xV, yV) age)

  updateBullets :: Float -> [Bullet] -> [Bullet]
  updateBullets t b = b |> move |> checkLocX |> checkLocY
    where
      checkLocX = filter (\(Bullet (x, y) _) -> x < width && x > (negate width))
      checkLocY = filter (\(Bullet (x, y) _) -> y < height && y > (negate height))
      move = map (\(Bullet (x, y) (xV, yV)) -> Bullet (x+(xV*t), y+(yV*t)) (xV, yV))



  testBullet :: Bullet
  testBullet = Bullet (0, 0) (0, 0)

  testRock :: [Rock]
  testRock = [Rock (1, 1) (0, 0) 0, Rock (3000000, 1) (0, 0) 0]