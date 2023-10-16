
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Function (fix)
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC

bombNum :: Num a => a
bombNum = 5

wWidth, wHeight :: Num a => a
wWidth  = 640
wHeight = 480

window :: Display
window = InWindow "Minesweeper" (wWidth, wHeight) (100, 100)

main :: IO ()
main = do
    world <- generateNewWorld
    playIO window white 10 world drawWorld eventHandler stepWorld

cSize, cWidth, cHeight :: Num a => a
cSize   = 30
cWidth  = fromIntegral $ wWidth  `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize

type Position = (Int, Int)

randomPosition :: GenIO -> IO Position
randomPosition gen = (,) <$> uniformR (0, cWidth - 1) gen <*> uniformR (0, cHeight - 1) gen

data GameState = InGame | GameOver

data SnakeAction = SAStop | SAUp | SADown | SALeft | SARight deriving Eq

moveSnake :: SnakeAction -> Position -> Position
moveSnake SAStop  (x, y) = (x, y)
moveSnake SAUp    (x, y) = (x, y + 1)
moveSnake SADown  (x, y) = (x, y - 1)
moveSnake SALeft  (x, y) = (x - 1, y)
moveSnake SARight (x, y) = (x + 1, y)

data World = World
    { _state  :: GameState
    , _target :: [Position]
    , _cursor  :: Position
    , _action :: SnakeAction
    , _score  :: Int
    }

getRandomList :: Variate a => Int -> (GenIO -> IO a) -> GenIO -> IO [a]
getRandomList 0 _ _ = pure []
getRandomList n sample gen = (:) <$> sample gen <*> getRandomList (n-1) sample gen

generateNewWorld :: IO World
generateNewWorld = do
	gen <- createSystemRandom
	targetH <- getRandomList 5 randomPosition gen
	return $ World InGame targetH (0, 0) SAStop 0

drawWorld :: World -> IO Picture
drawWorld World{..} = case _state of
    InGame -> pure $ pictures
        [ pictures $ map (drawCell red) _target
        , drawCell (greyN 0.3) _cursor
        , translate (-wWidth/2+10) (-wHeight/2+10)  . scale 0.2 0.2 $ text ("SCORE: " ++ show _score)
        ]
        where
            cell = translate (-wWidth/2) (-wHeight/2) $ polygon [(0, 0), (0, cSize), (cSize, cSize), (cSize, 0)]
            drawCell c (x, y) = translate (fromIntegral x * cSize) (fromIntegral y * cSize) $ color c cell
    GameOver -> pure $ pictures
        [ translate (-270) 20     . scale 0.7 0.7 $ text "GAME OVER"
        , translate (-100) (-50)  . scale 0.3 0.3 $ text ("SCORE: " ++ show _score)
        , translate (-200) (-120) . scale 0.3 0.3 $ text "Press Enter to Retry"
        ]

eventHandler :: Event -> World -> IO World
eventHandler e w@World{..} = case _state of
    InGame -> case e of
        EventKey (SpecialKey KeyUp)    Down _ _ -> pure $ if _action == SADown  then w else w { _action = SAUp }
        EventKey (SpecialKey KeyDown)  Down _ _ -> pure $ if _action == SAUp    then w else w { _action = SADown }
        EventKey (SpecialKey KeyLeft)  Down _ _ -> pure $ if _action == SARight then w else w { _action = SALeft }
        EventKey (SpecialKey KeyRight) Down _ _ -> pure $ if _action == SALeft  then w else w { _action = SARight }
        _ -> pure w
    GameOver -> case e of
        EventKey (SpecialKey KeyEnter) Down _ _ -> generateNewWorld
        _ -> pure w

stepWorld :: Float -> World -> IO World
stepWorld _ w@World{..} = case _state of
    InGame -> do
        let (x, y) = moveSnake _action _cursor
            cursor = (x, y)
        if (x, y) `elem` _target
            then pure $ w { _state = GameOver }
        else
        	return $ w { _cursor = cursor, _score = _score + 1}
    GameOver -> pure w



