
> {-# OPTIONS -Wall #-}

> module Main where

> import Control.Monad
> import qualified Data.Map as Map
> import System
> import System.IO

> import Grammar
> import ParserCombinators
> import JALRParser
> import Substitution

> type Error = String

> repl :: Context -> IO ()
> repl c = do putStr "jalr> "
>             hFlush stdout
>             line <- getLine
>             case parse lexer line of
>               Left err -> graceful c err
>               Right toks -> case parseG parser toks of
>                 Left err -> graceful c err
>                 Right [] -> graceful c "No expressions"
>                 Right [e] -> case evaluate e c of
>                   Left err -> graceful c err
>                   Right (e', c') -> do putStrLn $ (show e')
>                                        repl c'
>                 Right _ -> graceful c "Multiple expressions"

> graceful :: Context -> Error -> IO ()
> graceful c err = putStrLn err >> repl c

> loadExpressions :: String -> IO [Expression]
> loadExpressions file = do
>      toks <- parseFromFile lexer file
>      case toks of 
>        Left err -> do
>          putStrLn $ "Error lexing " ++ file
>          putStrLn err
>          return []
>        Right toks' -> do
>          case parseG parser toks' of
>             Left err -> do
>               putStrLn $ "Error parsing " ++ file
>               putStrLn err
>               return []
>             Right es -> return es

> tryLoadExpression :: Context -> Expression -> IO Context
> tryLoadExpression c e = 
>   case evaluate e c of
>     Left err -> do putStrLn $ "Error loading " ++ show e
>                    putStrLn err
>                    return c
>     Right (_e',c') -> return c'

> main :: IO ()
> main = do loadFiles <- getArgs
>           es <- (liftM concat) . (mapM loadExpressions) $ loadFiles
>           context <- foldM tryLoadExpression Map.empty es
>           repl context

