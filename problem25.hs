-- Generate a random permutation of the elements of a list.

-- Example in Haskell:

-- Prelude System.Random>rnd_permu "abcdef"
-- Prelude System.Random>"badcef"

import System.Random
import Data.List

rnd_select :: Eq a => [a] -> IO [a]
rnd_select xs = do
  gen <- newStdGen
  return $ (take l $ nub $ map (\x -> xs !! (x-1)) (randomRs (1,l) gen))
  where l = length xs