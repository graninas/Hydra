{-|
Set of test-labyrinths to ensure functionality of code.
-}


module Labyrinth.Labyrinths where

import qualified Data.Map  as Map

import           Labyrinth.Prelude
import           Labyrinth.Domain

testLabyrinth1 :: Labyrinth
testLabyrinth1 = Map.fromList
  [ ((0, 0), (Cell (Monolith False) (Monolith False) (Monolith False) (Monolith True), Treasure))
  , ((0, 1), (Cell (Monolith False) (Monolith False) (Monolith False) (Monolith True), TheMap))
  ]

-- l r u d
testLabyrinth2 :: Labyrinth
testLabyrinth2 = Map.fromList
  [ ((0, 0), (Cell (Monolith False) Wall (Monolith False) NoWall, NoContent))
  , ((1, 0), (Cell Wall NoWall (Monolith False) NoWall, NoContent))
  , ((2, 0), (Cell NoWall (Monolith False) (Monolith False) NoWall, (Wormhole 0)))

  , ((0, 1), (Cell (Monolith False) NoWall NoWall NoWall, NoContent))
  , ((1, 1), (Cell NoWall Wall NoWall Wall, Treasure))
  , ((2, 1), (Cell Wall (Monolith True) NoWall NoWall, NoContent))

  , ((0, 2), (Cell (Monolith False) NoWall NoWall (Monolith False), (Wormhole 1)))
  , ((1, 2), (Cell NoWall NoWall Wall (Monolith False), NoContent))
  , ((2, 2), (Cell NoWall (Monolith False) NoWall (Monolith False), TheMap))
  ]

-- x ->    0    1    2
--      ┏━━━━┯━━━━┯━━━━┓
--  0   ┃ @  │       W0┃
--      ┠    ┼    ┼    ┨
--  1   ┃     T   │
--      ┠    ┼────┼    ┨
--  2   ┃  W1      M   ┃
--      ┗━━━━┷━━━━┷━━━━┛

testTrail :: Trailpoints
testTrail = Map.fromList
  [ ((0, 0), (Cell (Monolith False) Wall (Monolith False) NoWall, (Trailpoint 1)))
  , ((1, 0), (Cell Wall NoWall (Monolith False) NoWall, NoContent))
  , ((2, 0), (Cell NoWall (Monolith False) (Monolith False) NoWall, (Trailpoint 0)))

  , ((0, 1), (Cell (Monolith False) NoWall NoWall NoWall, (Trailpoint 1)))
  , ((1, 1), (Cell NoWall Wall NoWall Wall, NoContent))
  , ((2, 1), (Cell Wall (Monolith True) NoWall NoWall, (Trailpoint 1)))

  , ((0, 2), (Cell (Monolith False) NoWall NoWall (Monolith False), (Trailpoint 1)))
  , ((1, 2), (Cell NoWall NoWall Wall (Monolith False), NoContent))
  , ((2, 2), (Cell NoWall (Monolith False) NoWall (Monolith False), NoContent))
  ]

--  x ->    0    1    2
--       ┏━━━━┯━━━━┯━━━━┓
--   0   ┃ @  │       W0┃
--       ┠    ┼    ┼    ┨
--   1   ┃     T   │
--       ┠    ┼────┼    ┨
--   2   ┃  W1      M   ┃
--       ┗━━━━┷━━━━┷━━━━┛
--
--  x ->    0    1    2
--       ┏━━━━┯━━━━┯━━━━┓
--   0   ┃ *1 │      *1 ┃
--       ┠    ┼    ┼    ┨
--   1   ┃ *1      │ *1
--       ┠    ┼────┼    ┨
--   2   ┃ *0        @  ┃
--       ┗━━━━┷━━━━┷━━━━┛
--
--
--
--  x ->    0    1    2
--       ┏━━━━┯━━━━┯━━━━┓
--   0   ┃    │         ┃
--       ┠    ┼    ┼    ┨
--   1   ┃         │
--       ┠    ┼────┼    ┨
--   2   ┃              ┃
--       ┗━━━━┷━━━━┷━━━━┛
--
--
-- (0, 0)
-- up
-- down
-- left
-- right
-- right
-- up
-- right
--
-- "A blind player senses a room around"
--
--  x ->    0    1    2
--         ━━━━
--   0|
--    |
--   1|    ┃
--    |
--   2|
--
-- "Breadcrubs"
--
-- x ->      0    1    2
--
--   0|      1    4    5w0
--    |
--   1|      2    3T
--    |
--   2|      5w1
--
--
-- "Player observes a room around"
--
--    x ->    0    1    2
--          ┏━━━━┯
--     0|   ┃ 1  │
--      |   ┠    ┼
--     1|   ┃ 2
--      |   ┠    ┼
--     2|
