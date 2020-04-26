module Main where

import           Labyrinth.Prelude

import           System.Environment         (getArgs)
import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import qualified Hydra.Domain               as D
import qualified Hydra.Language             as L
import qualified Hydra.Runtime              as R
import qualified Hydra.Interpreters         as R

import           Labyrinth.App (app)
import           Labyrinth.Types
import           Labyrinth.Domain

loggerCfg :: D.LoggerConfig
loggerCfg = D.LoggerConfig
  { D._format       = "$prio $loggername: $msg"
  , D._level        = D.Debug
  , D._logFilePath  = ""
  , D._logToConsole = False
  , D._logToFile    = False
  }

-- l r u d
testLabyrinth2 = Map.fromList
  [ ((0, 0), (Cell (Monolith False) Wall Wall Wall, NoContent))
  , ((1, 0), (Cell (Monolith False) Wall NoWall Wall, Treasure))
  , ((2, 0), (Cell (Monolith False) Wall Wall Wall, NoContent))

  , ((0, 1), (Cell NoWall Wall Wall Wall, NoContent))
  , ((1, 1), (Cell NoWall NoWall NoWall NoWall, NoContent))
  , ((2, 1), (Cell Wall NoWall Wall Wall, NoContent))

  , ((0, 2), (Cell Wall (Monolith False) Wall Wall, NoContent))
  , ((1, 2), (Cell Wall (Monolith False) Wall NoWall, NoContent))
  , ((2, 2), (Cell Wall (Monolith False) Wall Wall, NoContent))
  ]

testLabyrinth1 :: Labyrinth
testLabyrinth1 = Map.fromList
  [ ((0, 0), (Cell (Monolith False) (Monolith False) (Monolith False) (Monolith True), Treasure))
  ]


renderLabyrinthSkeleton :: Int -> Int -> LabRender
renderLabyrinthSkeleton maxX maxY = skeleton
  where
    p x y = (x, y)
    rendMaxX = maxX * 2
    rendMaxY = maxY * 2

    genLeftMonolithCross   = [(p 0 y,        "┠") | y <- [0, 2..rendMaxY ], y > 1, y < rendMaxY - 1 ]
    genRightMonolithCross  = [(p rendMaxX y, "┨") | y <- [0, 2..rendMaxY ], y > 1, y < rendMaxY - 1 ]
    genTopMonolithCross    = [(p x 0,        "┯") | x <- [0, 2..rendMaxX ], x > 1, x < rendMaxX - 1 ]
    genBottomMonolithCross = [(p x rendMaxY, "┷") | x <- [0, 2..rendMaxX ], x > 1, x < rendMaxX - 1 ]
    genInternalCross       = [(p x y, "┼") | x <- [0, 2..rendMaxX - 1]
                                           , y <- [0, 2..rendMaxY - 1]
                                           , y > 1, y < rendMaxY - 1
                                           , x > 1, x < rendMaxX - 1
                                           ]
    fullTemplate           = [(p x y, " ") | x <- [0..rendMaxX], y <- [0..rendMaxY]]

    intersections =
      [ (p 0 0,               "┏")
      , (p rendMaxX 0,        "┓")
      , (p 0 rendMaxY,        "┗")
      , (p rendMaxX rendMaxY, "┛")
      ]
      ++ genLeftMonolithCross
      ++ genRightMonolithCross
      ++ genTopMonolithCross
      ++ genBottomMonolithCross
      ++ genInternalCross

    skeleton = Map.union (Map.fromList intersections) (Map.fromList fullTemplate)

printSkeleton :: Int -> Int -> LabRender -> AppL ()
printSkeleton mx my skeleton = do
  scenario $ putStrLn $ show skeleton

  let rendMaxX = mx * 2
  let rendMaxY = my * 2

  let printAndMergeCells y row x = case Map.lookup (x, y) skeleton of
        Nothing -> row <> "!"
        Just c  -> row <> c

  let printAndMergeRows y rows =
        let row = foldl' (printAndMergeCells y) "" [0..rendMaxX]
        in row : rows

  let printedRows = foldr printAndMergeRows [] [0..rendMaxY]

  let outputRows row = putStrLn row
  scenario $ mapM_ outputRows printedRows


-- ┓ ┯ ┯ ┯ ┏
--
-- ┨ ┼ ┼ ┼ ┠
--
-- ┨ ┼ ┼ ┼ ┠
--
-- ┨ ┼ ┼ ┼ ┠
--
-- ┛ ┷ ┷ ┷ ┗



initGameState :: AppL GameState
initGameState = do
  printSkeleton 1 1 $ renderLabyrinthSkeleton 1 1
  printSkeleton 2 2 $ renderLabyrinthSkeleton 2 2
  printSkeleton 3 3 $ renderLabyrinthSkeleton 3 3
  printSkeleton 4 4 $ renderLabyrinthSkeleton 4 4

  let maxX = 1
  let maxY = 1

  labRenderVar <- newVarIO $ Map.fromList $ do
    x <- [0..maxX*2]
    y <- [0..maxY*2]
    pure ((x, y), T.pack "")

  let wormholes = Map.empty

  labVar           <- newVarIO testLabyrinth1
  labSizeVar       <- newVarIO (maxX, maxY)
  posVar           <- newVarIO (0, 0)
  inv              <- Inventory <$> newVarIO False
  aboutLeaving     <- newVarIO Nothing
  finished         <- newVarIO False

  pure $ GameState labVar labSizeVar labRenderVar wormholes posVar inv aboutLeaving finished



startApp :: AppL ()
startApp = initGameState >>= app

main :: IO ()
main = R.withAppRuntime (Just loggerCfg) (\rt -> R.runAppL rt startApp)
