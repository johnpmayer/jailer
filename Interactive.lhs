
> {-# OPTIONS -Wall #-}

> module Interactive where

> import ParserCombinators
> import JALRParser
> import Substitution

> import qualified Data.Map as Map

> type Error = String

> repl :: Context -> IO ()
> repl c = do putStr "jalr> "
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

> main :: IO ()
> main = repl Map.empty

