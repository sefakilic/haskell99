-- Lotto: Draw N different random numbers from the set 1..M.

-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]

import Control.Monad
import System.Random
import Data.List

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
  gen <- newStdGen
  return $ take n $ nub $ randomRs (1,m) gen