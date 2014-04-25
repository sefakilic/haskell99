-- Extract a given number of randomly selected elements from a list.

-- Example in Haskell:

-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- eda

-- You may need to install random package using cabal package manager

import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- newStdGen
  return (map (\i -> xs!!(i-1)) (take n $ randomRs (1, length xs) gen))

