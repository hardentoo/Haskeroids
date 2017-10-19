module Entity where
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game
  import Graphics.Gloss.Interface.Pure.Simulate
  import Graphics.Gloss.Interface.Pure.Display

  data World = Game Ship [Rock] [Bullet]
             | GameOver
             deriving (Eq, Show)

  data Rock = Rock Position Velocity Age deriving (Eq, Show)
  data Ship = Ship Position Velocity Direction deriving (Eq, Show)
  data Bullet = Bullet Position Velocity deriving (Eq, Show)

  data Direction = U | D | L | R | N deriving (Eq, Show)

  type Velocity = (Float, Float)
  type Position = (Float, Float)
  type Age = Float

  width :: Float
  width = 960

  height :: Float
  height = 540

  class Entity a where
    checkBounds :: a -> a

  instance Entity Rock where
    checkBounds r@(Rock (x, y) (xV, yV) age)
      | x > width || x < (-width) = Rock (negate x, y) (xV, yV) age
      | y > height || y < (-height) = Rock (x, negate y) (xV, yV) age
      | otherwise = r

  instance Entity Ship where
    checkBounds s@(Ship (x, y) (xV, yV) d)
      | x > width || x < (-width) = Ship (negate x, y) (xV, yV) d
      | y > height || y < (-height) = Ship (x, negate y) (xV, yV) d
      | otherwise = s