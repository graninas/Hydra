module Labyrinth.Render where

import           Labyrinth.Prelude

import qualified Data.Text                  as T
import qualified Data.Map                   as Map

import           Labyrinth.Types
import           Labyrinth.Domain

lu, ru, ld, rd :: Text
lu = "┏"
ru = "┓"
ld = "┗"
rd = "┛"

lCross, rCross, uCross, dCross :: Text
lCross = "┠"
rCross = "┨"
uCross = "┯"
dCross = "┷"

cross :: Text
cross = "┼"

vWall, hWall :: Text
vWall = "│"
hWall = "─"

vMonolithWall, hMonolithWall :: Text
vMonolithWall = "┃"
hMonolithWall = "━"

fullSpace :: Text
fullSpace = "    "

vExit, hExit :: Text
vExit = " "
hExit = "    "

fullHWall :: Text
fullHWall = hWall <> hWall <> hWall <> hWall

fullHMonolithWall :: Text
fullHMonolithWall = hMonolithWall <> hMonolithWall <> hMonolithWall <> hMonolithWall

mergeCellContent :: Content -> Maybe Text -> Text
mergeCellContent content Nothing = "!" <> show content
mergeCellContent NoContent _ = fullSpace
mergeCellContent Treasure _ = "T   "
mergeCellContent (Wormhole n) _ | n < 10 = " W" <> show n <> " "
mergeCellContent (Wormhole n) _ | n >= 10 = " W? "

mergeCell :: Direction -> Wall -> Text -> Text
mergeCell dir NoWall curW |
  (dir == DirRight || dir == DirLeft)
  && (curW == " ") = " "

mergeCell dir NoWall curW |
  (dir == DirUp || dir == DirDown)
  && (curW == fullSpace || curW == " ") = fullSpace

mergeCell dir Wall curW |
  (dir == DirLeft || dir == DirRight)
  && (curW == vWall || curW == " ")
  = vWall

mergeCell dir Wall curW |
  (dir == DirUp || dir == DirDown)
  && (curW == fullHWall || curW == " ") = fullHWall


mergeCell dir (Monolith False) curW |
  (dir == DirLeft || dir == DirRight)
  && (curW == vMonolithWall || curW == " ")
  = vMonolithWall

mergeCell dir (Monolith True) curW |
  (dir == DirLeft || dir == DirRight)
  && (curW == vExit || curW == " ")
  = vExit


mergeCell dir (Monolith False) curW |
  (dir == DirUp || dir == DirDown)
  && (curW == fullHMonolithWall || curW == " ")
  = fullHMonolithWall

mergeCell dir (Monolith True) curW |
  (dir == DirUp || dir == DirDown)
  && (curW == hExit || curW == " ")
  = hExit


mergeCell dir w curW   = "!" <> show dir <> show w <> curW


renderSkeleton :: Bounds -> LabRender
renderSkeleton (maxX, maxY) = skeleton
  where
    p x y = (x, y)
    rendMaxX = maxX * 2
    rendMaxY = maxY * 2

    genLeftMonolithCross   = [(p 0 y,        lCross) | y <- [0, 2..rendMaxY ], y > 1, y < rendMaxY - 1 ]
    genRightMonolithCross  = [(p rendMaxX y, rCross) | y <- [0, 2..rendMaxY ], y > 1, y < rendMaxY - 1 ]
    genTopMonolithCross    = [(p x 0,        uCross) | x <- [0, 2..rendMaxX ], x > 1, x < rendMaxX - 1 ]
    genBottomMonolithCross = [(p x rendMaxY, dCross) | x <- [0, 2..rendMaxX ], x > 1, x < rendMaxX - 1 ]
    genInternalCross       = [(p x y,        cross)  | x <- [0, 2..rendMaxX - 1]
                                                     , y <- [0, 2..rendMaxY - 1]
                                                     , y > 1, y < rendMaxY - 1
                                                     , x > 1, x < rendMaxX - 1
                                                     ]
    fullTemplate           = [(p x y, " ") | x <- [0..rendMaxX], y <- [0..rendMaxY]]

    intersections =
      [ (p 0 0,               lu)
      , (p rendMaxX 0,        ru)
      , (p 0 rendMaxY,        ld)
      , (p rendMaxX rendMaxY, rd)
      ]
      ++ genLeftMonolithCross
      ++ genRightMonolithCross
      ++ genTopMonolithCross
      ++ genBottomMonolithCross
      ++ genInternalCross

    skeleton = Map.union (Map.fromList intersections) (Map.fromList fullTemplate)

-- foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
cellRender :: Pos -> (Cell, Content) -> LabRender -> LabRender
cellRender (x0, y0) (cell, content) labRender = let
  (x, y) = (x0 * 2 + 1, y0 * 2 + 1)
  l = (x-1, y)
  r = (x+1, y)
  u = (x, y-1)
  d = (x, y+1)
  g (a, b) = Map.lookup (a, b) labRender
  mbCellR = g (x, y)
  accessedRenderedCells = (g l, g r, g u, g d)
  in case accessedRenderedCells of
      (Nothing, _, _, _) -> Map.insert l ("!l" <> show l) labRender
      (_, Nothing, _, _) -> Map.insert r ("!r" <> show r) labRender
      (_, _, Nothing, _) -> Map.insert u ("!u" <> show u) labRender
      (_, _, _, Nothing) -> Map.insert d ("!d" <> show d) labRender
      (Just lCr, Just rCr, Just uCr, Just dCr) ->
            Map.insert l (mergeCell DirLeft  (leftWall  cell) lCr)
          $ Map.insert r (mergeCell DirRight (rightWall cell) rCr)
          $ Map.insert u (mergeCell DirUp    (upWall    cell) uCr)
          $ Map.insert d (mergeCell DirDown  (downWall  cell) dCr)
          $ Map.insert (x, y) (mergeCellContent content mbCellR)
          labRender

renderPlayer :: Pos -> LabRender -> LabRender
renderPlayer (x0, y0) lab = let
  (x, y) = (x0 * 2 + 1, y0 * 2 + 1)
  in case Map.lookup (x, y) lab of
    Nothing   -> Map.insert (0, 0) ("!Player:" <> show (x0, y0)) lab
    Just curW -> Map.insert (x, y) ((T.take 3 curW) <> "@") lab


renderLabyrinth :: LabRender -> Labyrinth -> Pos -> LabRender
renderLabyrinth template lab plPos =
  renderPlayer plPos
    $ Map.foldrWithKey cellRender template lab

printLabRender :: Bounds -> LabRender -> LangL ()
printLabRender (maxX, maxY) labRender = do

  let rendMaxX = maxX * 2
  let rendMaxY = maxY * 2

  let printAndMergeCells y row x = case Map.lookup (x, y) labRender of
        Nothing -> row <> "!" <> show (x, y)
        Just c  -> row <> c

  let printAndMergeRows y rows =
        let row = foldl' (printAndMergeCells y) "" [0..rendMaxX]
        in row : rows

  let printedRows = foldr printAndMergeRows [] [0..rendMaxY]

  let outputRows row = putStrLn row
  mapM_ outputRows printedRows