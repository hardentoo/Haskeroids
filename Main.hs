module Main where
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game
  import Graphics.Gloss.Interface.Pure.Simulate
  import Graphics.Gloss.Interface.Pure.Display
  import Flow
  import Entity
  import Misc

  initialWorld :: World
  initialWorld = Game (Ship (0, 0) (0, 0) N) [Rock (80, 80) (240, 240) 0, Rock (300, 3) (240, 240) 0, Rock ((-300), 47) ((-240), 140) 0, Rock (0, (-100)) (240, (-240)) 0] []

  renderWorld :: World -> Picture
  renderWorld GameOver = translate 0 0 $ scale 0.3 0.3 $ color red $ text "Gama Ovar"
  renderWorld (Game (Ship (x, y) _ _) rocks bullets) = Pictures ([translate x y $ color white $ circle 20] ++ ast ++ bul)
    where
      ast = map (\(Rock (x, y) _ _) -> translate x y $ color white $ rectangleWire 90 90) rocks
      bul = map (\(Bullet (x, y) _) -> translate x y $ color white $ circle 10) bullets
      t = translate 0 0 $ scale 0.3 0.3 $ color white $ text (show (length bullets))

  handleKeys :: Event -> World -> World
  handleKeys (EventKey (Char 'w') Down _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (0, 5) U) rocks bullets
  handleKeys (EventKey (Char 'w') Up _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (0, 0) U) rocks bullets
  handleKeys (EventKey (Char 'd') Down _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (5, 0) R) rocks bullets
  handleKeys (EventKey (Char 'd') Up _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (0, 0) R) rocks bullets
  handleKeys (EventKey (Char 's') Down _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (0, (-5)) D) rocks bullets
  handleKeys (EventKey (Char 's') Up _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (0, 0) D) rocks bullets
  handleKeys (EventKey (Char 'a') Down _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos ((-5), 0) L) rocks bullets
  handleKeys (EventKey (Char 'a') Up _ _) (Game (Ship pos _ _) rocks bullets) = Game (Ship pos (0, 0) L) rocks bullets
  handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (Game s@(Ship pos vel d) rocks bullets) = Game s rocks (bullets ++ [Bullet pos (getVel d)])
  handleKeys _ w = w

  updateWorld :: Float -> World -> World
  updateWorld t world
    | endGame world = GameOver
    | hits world = checkBulletHits world
    | otherwise = update world
      where
        endGame (Game ship rocks bullets) = collide ship rocks
        endGame _ = False
        checkBulletHits (Game s rocks bullets) = (Game s (h bullets rocks) bullets)
        checkBulletHits GameOver = GameOver
        hits (Game _ rocks bullets) = if null bullets then False else collideWithRock (head bullets) rocks
        hits _ = False
        update (Game ship rocks bullets) = Game (updateShip t ship) (updateRocks t rocks bullets) (updateBullets t bullets)
        update GameOver = GameOver
  
  main :: IO ()
  main = play FullScreen black 60 initialWorld renderWorld handleKeys updateWorld