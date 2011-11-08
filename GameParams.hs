module GameParams where


data GameParams = GameParams
  { loadtime :: Int
  , turntime :: Int
  , rows :: Int
  , cols :: Int
  , turns :: Int
  , playerSeed :: Int
  , viewradius2 :: Int
  , attackradius2 :: Int
  , spawnradius2 :: Int
  --, viewCircle :: [Point]
  --, attackCircle :: [Point]
  --, spawnCircle :: [Point]
  } deriving (Show)

