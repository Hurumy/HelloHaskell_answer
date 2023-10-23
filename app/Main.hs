
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Float
import Graphics.Gloss.Interface.IO.Game
import System.Random.MWC

bombNum :: Num a => a
bombNum = 10

wWidth, wHeight :: Num a => a
wWidth  = 640
wHeight = 480

cSize, cWidth, cHeight :: Num a => a
cSize = 40
cWidth  = fromIntegral $ wWidth  `div` cSize
cHeight = fromIntegral $ wHeight `div` cSize

window :: Display
window = InWindow "Minesweeper" (wWidth, wHeight) (100, 100)

main :: IO ()
main = do
    world <- generateNewWorld
    playIO window white 10 world drawWorld eventHandler stepWorld

type Position = (Int, Int)

type CellState = (Position, Int)

data GameState = InGame | GameOver

data CursorAction = MStop | MUp | MDown | MLeft | MRight | MEnter deriving Eq

moveSnake :: CursorAction -> Position -> Position
moveSnake MStop  (x, y) = (x, y)
moveSnake MUp    (x, y) = if y + 1 >= cHeight then (x, y) else (x, y + 1)
moveSnake MDown  (x, y) = if y <= 0 then (x, y) else (x, y - 1)
moveSnake MLeft  (x, y) = if x <= 0 then (x, y) else (x - 1, y)
moveSnake MRight (x, y) = if x + 1 >= cWidth then (x, y) else (x + 1, y)
moveSnake MEnter (x, y) = (x, y)

data World = World
    { _state  :: GameState
    , _target :: [Position]
    , _cursor  :: Position
	, _openedcell :: [CellState]
	, _action :: CursorAction
    , _score  :: Int
    }

randomPosition :: GenIO -> IO Position
randomPosition gen = (,) <$> uniformR (0, cWidth - 1) gen <*> uniformR (0, cHeight - 1) gen

getRandomList :: Variate a => Int -> (GenIO -> IO a) -> GenIO -> IO [a]
getRandomList 0 _ _ = pure []
getRandomList n sample gen = (:) <$> sample gen <*> getRandomList (n-1) sample gen

generateNewWorld :: IO World
generateNewWorld = do
	gen <- createSystemRandom
	targetH <- getRandomList bombNum randomPosition gen
	return $ World InGame targetH (0, 0) [] MStop 0

drawNumberToEachCell :: CellState -> Picture
drawNumberToEachCell cell = let
	(x, y) = fst cell
	_x = int2Float x
	_y = int2Float y
	num = show $ snd cell in
	translate ((-wWidth/2) + (cSize*_x) + (cSize/2 - 5)) ((-wHeight/2) + (cSize*_y) + (cSize/2 - 5)) . scale 0.1 0.1 $ text num

drawNumber :: [CellState] -> [Picture]
drawNumber openedcell = map drawNumberToEachCell openedcell	

drawWorld :: World -> IO Picture
drawWorld World{..} = case _state of
    InGame ->　pure $ pictures
        [ pictures $ map (drawCell red) _target
        , pictures $ map (drawCell cyan) $ map fst _openedcell
		, if length _openedcell /= 0 then pictures $ drawNumber _openedcell else translate 0 0 . scale 0.1 0.1 $ text ("")
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
        EventKey (SpecialKey KeyUp)    Down _ _ -> pure $ w { _action = MUp }
        EventKey (SpecialKey KeyDown)  Down _ _ -> pure $ w { _action = MDown }
        EventKey (SpecialKey KeyLeft)  Down _ _ -> pure $ w { _action = MLeft }
        EventKey (SpecialKey KeyRight) Down _ _ -> pure $ w { _action = MRight }
        EventKey (SpecialKey KeyEnter) Down _ _ -> pure $ w { _action = MEnter }
        EventKey (SpecialKey KeyUp)    Up _ _ -> pure $ w { _action = MStop }
        EventKey (SpecialKey KeyDown)  Up _ _ -> pure $ w { _action = MStop }
        EventKey (SpecialKey KeyLeft)  Up _ _ -> pure $ w { _action = MStop }
        EventKey (SpecialKey KeyRight) Up _ _ -> pure $ w { _action = MStop }
        _ -> pure w
    GameOver -> case e of
        EventKey (SpecialKey KeyEnter) Down _ _ -> generateNewWorld
        _ -> pure w

isBombCell :: [Position] -> Position -> Int
isBombCell p (x, y) = if (x, y) `elem` p then 1 else 0

searchBombCell :: [Position] -> Position -> Int
searchBombCell bom (x, y) = let
	lst = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)] in
	foldr (+) 0 (map (isBombCell bom) lst)

stepWorld :: Float -> World -> IO World
stepWorld _ w@World{..} = case _state of
    InGame -> do
        let (x, y) = moveSnake _action _cursor
        if (x, y) `elem` _target && _action == MEnter
            then pure $ w { _state = GameOver }
		else if _action == MEnter && (x, y) `notElem` map fst _openedcell
			then pure $ w { _openedcell = ((x, y), searchBombCell _target (x, y)) : _openedcell, _score = _score + 1 }
        else
        	return $ w { _cursor = (x, y) }
    GameOver -> pure w


