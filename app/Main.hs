module Main where
import Text.Megaparsec
import System.Directory
import Data.Decimal
import System.IO.Strict as S

import Lib
import Options

main :: IO ()
main = do args <- parseArgs defaultOptions
          let inputPath = targetPath args
          let mult = multiplier args
          let verb = optVerbose args
          df <- doesFileExist inputPath
          dd <- doesDirectoryExist inputPath
          if df
            then do new <- fileReplace inputPath mult verb
                    copyFile inputPath (inputPath ++ ".bak")
                    writeFile inputPath new
          else if dd
            then do fs <- (listDirectory inputPath)
                    let fs2 = map (\f -> inputPath ++ "/" ++ f) fs
                    news <- mapM (\f -> fileReplace f mult verb) fs2
                    mapM_ (\f -> copyFile f (f ++ ".bak")) fs2
                    let mapped = zip fs2 news
                    mapM_ (\(f, n) -> writeFile f n) mapped
          else error "Not valid path"

fileReplace :: FilePath -> Decimal -> Bool -> IO String
fileReplace inputPath mult verb = do f  <- S.readFile inputPath
                                     let ls = lines f
                                     return $ unlines (map (\l -> case parse (lineP mult verb) "" l of Left  err -> error (errorBundlePretty err)
                                                                                                       Right str -> str) ls)
