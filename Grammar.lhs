
> module Grammar where

> import Data.List

JALR is a just another lisp runtime, so it has a simple syntax

> type Name = String

> data Expression = Atom Name
>                 | Symbol Expression
>                 | Combination [Expression]
>   deriving Eq

> instance Show Expression where
>   show (Atom   s)       = s
>   show (Symbol e)       = '\'' : show e
>   show (Combination es) = 
>     "(" ++
>     (concat $ intersperse " " (fmap show es)) ++
>     ")"
