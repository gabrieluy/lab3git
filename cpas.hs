import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsPascal
import LexPascal
import ParPascal
import ErrM
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath
import Control.Monad.State
 
import TypeChecker
import Compiler

check :: String -> String -> IO ()
check fileName s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok tree -> do
      case typeCheckProgram tree of
        Bad err -> do
          putStrLn "TYPE ERROR"
          putStrLn err
          exitFailure
        Ok tree -> do
          -- putStrLn $ show tree
          writeFile (joinPath [takeDirectory fileName, takeBaseName fileName ++ ".j"])
                    (unlines $ compile tree)
          putStrLn "OK"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check file
    _      -> do
      putStrLn "Usage: cpas <SourceFilePath>"
      exitFailure
