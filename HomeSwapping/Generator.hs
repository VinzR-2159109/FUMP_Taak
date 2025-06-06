module Generator( generateYaml, writeYamlFile) where

import System.Random (newStdGen, randomRs)
import Data.List     (nub)

generateYaml :: Int -> Int -> Int -> IO [Char]
generateYaml n min_c max_c = do
  let header = unlines
        [ "# aantal woningen"
        , "homes: " ++ show n
        , ""
        , "# compatibiliteiten per woning"
        , "compatibilities:"
        ]

  bodyLines <- mapM makeLine [1..n]
  let body = unlines (map ("  " ++) bodyLines)
  return (header ++ body)
  where
    makeLine :: Int -> IO String
    makeLine i = do
      g1 <- newStdGen
      let k = head (randomRs (min_c, max_c) g1)      
      g2 <- newStdGen
      let candidates = randomRs (1, n) g2
          vs         = take (k-1) . nub . filter (/= i) $ candidates
      return $ show i ++ ": " ++ show (i : vs)


writeYamlFile :: FilePath -> Int -> Int -> Int -> IO ()
writeYamlFile fp n lb ub = do
  txt <- generateYaml n lb ub
  writeFile fp txt
