module Labyrinth.Render where

import           Labyrinth.Prelude

import qualified Data.Map                   as Map


import           Labyrinth.Domain
import           Labyrinth.Algorithms

lu, ru, ld, rd :: String
lu = "┏"
ru = "┓"
ld = "┗"
rd = "┛"

lCross, rCross, uCross, dCross :: String
lCross = "┠"
rCross = "┨"
uCross = "┯"
dCross = "┷"

cross :: String
cross = "┼"

vWall, hWall :: String
vWall = "│"
hWall = "─"

vMonolithWall, hMonolithWall :: String
vMonolithWall = "┃"
hMonolithWall = "━"

fullSpace :: String
fullSpace = "    "

vExit, hExit :: String
vExit = " "
hExit = "    "

fullHWall :: String
fullHWall = hWall <> hWall <> hWall <> hWall

fullHMonolithWall :: String
fullHMonolithWall = hMonolithWall <> hMonolithWall <> hMonolithWall <> hMonolithWall

mergeCellContent :: Content -> Maybe String -> String
mergeCellContent content Nothing = "!" <> show content
mergeCellContent NoContent _ = fullSpace
mergeCellContent Treasure _ = "T   "
mergeCellContent (Wormhole n) _ | n < 10 = "  W" <> show n
mergeCellContent (Wormhole n) _ | n >= 10 = "  W?"
mergeCellContent content renderedContent = error $ "mergeCellContent: unexpected arguments: " <> show content <> ", " <> show renderedContent

mergeCell :: Direction -> Wall -> String -> String
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
renderSkeleton (maxX, maxY) = ((rendMaxX, rendMaxY), skeleton)
  where
    p x y = (x, y)
    rendMaxX = maxX * 2
    rendMaxY = maxY * 2

    genLeftMonolithCross   = [(p 0 y,        lCross) | y <- [0, 2..rendMaxY - 1], y > 1]
    genRightMonolithCross  = [(p rendMaxX y, rCross) | y <- [0, 2..rendMaxY - 1], y > 1]
    genTopMonolithCross    = [(p x 0,        uCross) | x <- [0, 2..rendMaxX - 1], x > 1]
    genBottomMonolithCross = [(p x rendMaxY, dCross) | x <- [0, 2..rendMaxX - 1], x > 1]
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
cellRender (x0, y0) (cell, content) (bounds, labRender) = let
  (x, y) = (x0 * 2 + 1, y0 * 2 + 1)
  l = (x-1, y)
  r = (x+1, y)
  u = (x, y-1)
  d = (x, y+1)
  g (a, b) = Map.lookup (a, b) labRender
  mbCellR = g (x, y)
  accessedRenderedCells = (g l, g r, g u, g d)
  labWithCell = case accessedRenderedCells of
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
  in (bounds, labWithCell)

renderPlayer :: Pos -> LabRender -> LabRender
renderPlayer (x0, y0) (bounds, lab) = (bounds, lab')
  where
    (x, y) = (x0 * 2 + 1, y0 * 2 + 1)
    lab' = case Map.lookup (x, y) lab of
              Nothing   -> Map.insert (0, 0) ("!Player:" <> show (x0, y0)) lab
              -- TODO: check for a bear
              Just curW -> Map.insert (x, y) (take 1 curW <> "@" <> (take 2 $ drop 2 curW)) lab

renderBear :: Pos -> LabRender -> LabRender
renderBear (x0, y0) (bounds, lab) = (bounds, lab')
  where
    (x, y) = (x0 * 2 + 1, y0 * 2 + 1)
    lab' = case Map.lookup (x, y) lab of
              Nothing   -> Map.insert (0, 0) ("!Bear:" <> show (x0, y0)) lab
              Just curW -> Map.insert (x, y) (take 1 curW <> "B" <> (take 2 $ drop 2 curW)) lab

renderLabyrinth' :: Skeleton -> Labyrinth -> Pos -> Pos -> LabRender
renderLabyrinth' skeleton lab plPos bearPos =
  renderPlayer plPos
    $ renderBear bearPos
    $ Map.foldrWithKey cellRender skeleton lab

renderLabyrinth :: Labyrinth -> Pos -> Pos -> LabRender
renderLabyrinth lab plPos bearPos = renderLabyrinth' skeleton lab plPos bearPos
  where
    LabyrinthInfo {..} = analyzeLabyrinth lab
    skeleton = renderSkeleton _bounds

printLabRender' :: LabRender -> LangL ()
printLabRender' ((rendMaxX, rendMaxY), labRender) = do

  let printAndMergeCells y row x = case Map.lookup (x, y) labRender of
        Nothing -> row <> "!" <> show (x, y)
        Just c  -> row <> c

  let printAndMergeRows y rows =
        let row = foldl' (printAndMergeCells y) "" [0..rendMaxX]
        in row : rows

  let printedRows = foldr printAndMergeRows [] [0..rendMaxY]

  let outputRows row = putStrLn row
  mapM_ outputRows printedRows

printLabyrinth :: Labyrinth -> LangL ()
printLabyrinth lab = printLabRender' $ renderLabyrinth lab (0, 0) (0, 0)
