module Lib
    ( mergeMacros
    ) where

import Data.Map.Ordered
import Data.List.Split

newtype Macros = Macros { commands :: OMap String String } deriving (Eq, Show)

mergeMacros :: String -> String -> String -> IO ()
mergeMacros out file1 file2 = do
  str1 <- readFile file1
  str2 <- readFile file2
  let merged = getMacros str1 <^> getMacros str2
  writeFile out $ unlines merged

(<^>) :: Macros -> Macros -> [String]
(<^>) macros1 macros2 = snd <$> merged 
  where merged = assocs $ commands macros1 <>| commands macros2
  
getMacros :: String -> Macros
getMacros str = foldr parseW (Macros empty) (lines str)

parseW :: String -> Macros -> Macros
parseW ln ms = case wordsBy (== '{') ln of
              (_ : name : _) -> Macros $ (name, ln) |< commands ms
              _ -> ms
