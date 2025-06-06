module Reader (readInput) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseList :: String -> [Int]
parseList s =
  let inner = init (tail s) -- drop the '[' and ']'
      parts = split inner   -- split on commas
  in map (read . trim) parts
  where
    split "" = []
    split xs = let (a,b) = break (==',') xs
               in a : case b of { [] -> []; (_:r) -> split r }

readInput :: FilePath -> IO [(Int, [Int])]
readInput filePath = do
  content <- readFile filePath
  let rawLines   = lines content
      cleaned    = [trim l | l <- rawLines, not (null (trim l)), head (trim l) /= '#']

      homesLine  = head [ l | l <- cleaned, "homes:" `isPrefixOf` l ]
      n          = read (trim (drop 6 homesLine)) :: Int

      afterComp  = dropWhile (not . ("compatibilities:" `isPrefixOf`)) cleaned
      compLines  = take n (tail afterComp)

      parseLine line =
        let (numStr, rest) = span (/= ':') line
            i              = read (trim numStr) :: Int
            vs             = parseList (trim (drop 1 rest))
        in (i, filter (/= i) vs)

  return (map parseLine compLines)