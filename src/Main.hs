{-# LANGUAGE OverloadedStrings #-}
module Main where

import            Prelude       hiding  ( takeWhile, readFile, writeFile, unlines, unwords, append )
import            Data.Attoparsec.Text  ( Parser, parseOnly, eitherResult, string, skipSpace, endOfLine, takeWhile1, many1 )
import            Data.Char             ( isSpace )
import            Data.Text             ( Text, append, unlines, unwords )
import            Data.Text.IO          ( readFile, writeFile )
import            Data.Map              ( Map )
import qualified  Data.Map      as Map  ( empty, fromList, toList, unionWith )
import            System.Environment    ( getArgs )
import            Control.Monad         ( guard, foldM )

main :: IO ()
main = do
  args <- getArgs
  guard (length args > 2)
  writeFile (head args) =<< fmap prettyFile (foldM merge emptyCoverFile $ tail args)

merge :: CoverFile -> String -> IO CoverFile
merge cf fn = either fail (return . mergeCoverFiles cf) =<< fmap (parseOnly coverfileP) (readFile fn)

data CoverLine = CoverLine
  { stmt  :: Text
  , no1   :: Text
  , no2   :: Text
  }

data CoverFile = CoverFile
  { mode        :: Text
  , coverlines  :: Map Text CoverLine
  }

emptyCoverFile :: CoverFile
emptyCoverFile = CoverFile
  { mode        = "set"
  , coverlines  = Map.empty
  }

modeP :: Parser Text
modeP = do
  string "mode: set"
  endOfLine
  return "set"

coverlineP :: Parser CoverLine
coverlineP = do
  stmt  <- takeWhile1 (not . isSpace)
  skipSpace
  no1   <- takeWhile1 (not . isSpace)
  skipSpace
  no2   <- takeWhile1 (not . isSpace)
  endOfLine
  return $ CoverLine stmt no1 no2

coverfileP :: Parser CoverFile
coverfileP = do
  mo  <- modeP
  cs  <- many1 coverlineP
  return $ CoverFile mo $ Map.fromList $ map (\c -> (stmt c, c)) cs

mergeCoverFiles :: CoverFile -> CoverFile -> CoverFile
mergeCoverFiles a b = CoverFile (mode a) $ Map.unionWith updateCovers (coverlines a) (coverlines b)

updateCovers :: CoverLine -> CoverLine -> CoverLine
updateCovers a b
  | no2 a == "1"  = a
  | no2 b == "1"  = b
  | otherwise     = a

prettyFile :: CoverFile -> Text
prettyFile a = unlines (append "mode: " (mode a) : (map (\(k,v) -> prettyLine v) $ Map.toList (coverlines a)))

prettyLine :: CoverLine -> Text
prettyLine a = unwords [stmt a, no1 a, no2 a]
