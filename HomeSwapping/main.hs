module Main where

import System.Environment (getArgs)
import Generator          (writeYamlFile)
import Reader             (readInput)
import Solver             (maxMatching)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sn, smin_c, smax_c, inputFile] -> do
      let n  = read sn  :: Int
          min_c = read smin_c :: Int
          max_c = read smax_c :: Int

      writeYamlFile inputFile n min_c max_c
      adj <- readInput inputFile
      (count, plan) <- maxMatching adj

      putStrLn $ "Max aantal echte verhuizingen: " ++ show count
      putStrLn "Verhuisplan: "
      mapM_ (\(o,d) -> putStrLn $ "  " ++ show o ++ " -> " ++ show d) plan

    _ -> putStrLn "Usage: homeswapping-gen <n> <min_c> <max_c> <output.yaml>"
