
> {-# OPTIONS -Wall #-}

> module JALRParser where

> import Control.Monad
> import Grammar
> import Parser
> import ParserCombinators

First, the lexer takes a stream of characters (presumeably from a file, but
not neccesarily), and computes a seqences of tokens. Tokens are either some
reserved character or an atom.

> data Token = ReservedT Char
>            | AtomT String
>   deriving Eq

> instance Show Token where
>   show (ReservedT c) = [c]
>   show (AtomT     s) = s

> lexer :: Parser [Token]
> lexer = many $ (wsP reservedTP <|> wsP atomTP)

Certain characters are whitespace, and here we define a parser that, given
a parser, computes a new parser which comsumes any trailing whitespace.

> whitespace :: [Char]
> whitespace = [' ', '\t', '\n']

> wsP :: Parser a -> Parser a
> wsP p = do a <- p
>            _ <- many . choice . fmap char $ whitespace
>            return a

There are a small set of reserverd tokens, which follow roughly the standard
lisp syntax. Parenthesis denote combinations, and single-quotes denote symbol
expressions (S-expressions?). Whitespace is also included.

> reservedTokens :: [Char]
> reservedTokens = ['(', ')', '\'']

> reservedTP :: Parser Token
> reservedTP = liftM ReservedT . choice . fmap char $ 
>                  reservedTokens

Everything else can be names for atoms.

> atomTP :: Parser Token
> atomTP = liftM AtomT . many1 . satisfy $
>         (\c -> not $ elem c (reservedTokens ++ whitespace))

Here are some tests for the lexer only.

> testLexer :: IO (Either ParseError [Token])
> testLexer = parseFromFile lexer "prelude.jalr"

Second, the parser takes a stream of tokens and computes of sequence of jalr
expressions or a parse error.

> parser :: ParserG Token [Expression]
> parser = manyG expressionP

An expression is either an atom, a symbol, or a combination

> expressionP :: ParserG Token Expression
> expressionP = atomP <||> symbolP <||> combinationP

Atoms are easy, since we already set them aside during lexing

> isAtomT :: Token -> Bool
> isAtomT (AtomT _) = True
> isAtomT _         = False

> atomP :: ParserG Token Expression
> atomP = do (AtomT s) <- satisfyG isAtomT
>            return $ Atom s

Symbols are expressions preceded by the tick mark

> symbolP :: ParserG Token Expression
> symbolP = do _ <- specificG $ ReservedT '\''
>              liftM Symbol expressionP

Combinations are sequences of subexpressions surrounded by parenthesis

> combinationP :: ParserG Token Expression
> combinationP = do _ <- specificG $ ReservedT '('
>                   es <- many1G $ expressionP
>                   _ <- specificG $ ReservedT ')'
>                   return $ Combination es

> testParser :: IO ()
> testParser = 
>   do tokens <- testLexer
>      case tokens of
>        Left err   -> putStrLn $ "Error:\n\t" ++ err
>        Right toks -> 
>          case parseG parser toks of
>            Left err   -> putStrLn $ "Error:\n\t" ++ err
>            Right exps -> foldl (\a b -> a >> b) (return ()) $
>                          fmap (putStrLn.show) exps
