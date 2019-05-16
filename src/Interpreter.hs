module Main where

import AbsReBabel
import Semantics
import System.Environment
import System.IO
import System.IO.Error hiding (try)

import ErrM
import LexReBabel
import LexReBabel
import ParReBabel
import System.IO

main = do
  args <- getArgs
  input <-
    case args of
      [] -> getContents
      (file:_) -> readFile file
  case pProgram (myLexer input) of
    Ok program -> putStrLn (show (evProgram (program)))
    Bad msg -> putStrLn msg
--  case pProgram (myLexer input) of
--    (Ok s) -> do
--      analysis <- runExceptT $ check s
--      case analysis of
--        (Left e) -> hPutStrLn stderr $ "Type error: " ++ e
--        _ -> do
--          res <- runExceptT $ interpret s
--          case res of
--            (Left e) -> hPutStrLn stderr $ "Runtime error: " ++ e
--            _ -> return ()
--    (Bad s) -> hPutStrLn stderr s -- syntax error