
> {-# OPTIONS -Wall #-}

> {-# LANGUAGE FlexibleContexts #-}

, Rank2Types, ImpredicativeTypes #-}

This will be the first model of computation using substitution.

> module Substitution where

> import Control.Monad.State
> import Control.Monad.Error
> import Data.Map (Map)
> import qualified Data.Map as Map

> import Grammar

We need to define a function eval, which takes an expression and computes
a new expression, given some context. This context will be a mapping of
names to definitions.

> type Context = Map Name Expression
> type EvalError = String

All evalutation operations occur in a monad capable of throwing evalutaion
erros, as well as mutating some context state containing definitions.

> evaluate :: Expression -> Context -> Either EvalError (Expression, Context)
> evaluate e c = runStateT (evalExp e) c

> type Evaluator a = StateT Context (Either EvalError) a

> evalExp :: Expression -> Evaluator Expression

Evalutaion of atoms, symbols, and empty combinations is trivial; in fact
nothing is done and the atom, symbol, or empty combination is itself the
result of evalutaion.

> evalExp e@(Atom         _) = return e
> evalExp e@(Symbol       _) = return e
> evalExp e@(Combination []) = return e

> evalExp (Combination (operator:operands)) = case operator of

>   (Symbol _) -> 
>     throwError "No rule to apply a symbol"

>   (Combination ((Atom "lambda") : (Combination fArgs) : body : [])) ->
>     do operands' <- mapM evalExp operands
>        substitute fArgs operands' body >>= evalExp

>   (Combination _) ->
>     throwError "No rule to apply a combination"

>   (Atom s) -> case Map.lookup s specialForms of
>     Just rule -> rule operands
>     Nothing ->
>       do operands' <- mapM evalExp operands
>          case Map.lookup s primitives of
>            Just rule -> rule operands'
>            Nothing ->
>              do context <- get
>                 case Map.lookup s context of
>                   Just definition -> evalExp.Combination $ 
>                                      definition : operands'
>                   Nothing -> throwError $ 
>                              "No rule or definition for '" ++ s ++ "'"

Here's a table for all rules associated with each special form and 
primitive operation. The input to each computation is the cdr of the
list of expressions in the combination.

> type Rule = [Expression] -> Evaluator Expression
> type RuleTable = Map String Rule

> specialForms :: RuleTable
> specialForms = Map.fromList [ ("lambda", evalLambda),
>                               ("define", evalDefine),
>                               ("if",     evalIf    ) ]

> primitives :: RuleTable
> primitives = Map.fromList [ ("inc", evalInc),
>                             ("dec", evalDec),
>                             ("=", evalIntEq) ]

Note that evaluating a lambda doesn't create anything special in
the substitution model, lambda is only special when, after evaluation,
it is the first expression in another combination (see above).

> evalLambda :: Rule
> evalLambda ops = return $ Combination $ Atom "lambda" : ops

"define" augments the context with a new definition (only once per name)

> evalDefine :: Rule
> evalDefine ((Atom s):e:[]) = do
>   context <- get
>   if Map.member s context
>   then throwError $ "Redefinition of '" ++ s ++ "'"
>   else do put $ Map.insert s e context
>           return.Atom $ "*defined " ++ s ++ "*"
> evalDefine _ = throwError $ 
>   "Define expressions must be of form (define <atom> <expression>)"

"if" is a simple test if <text> is the Atom "true"

> evalIf :: Rule
> evalIf (test:consequent:alternate:[]) = do
>   testValue <- evalExp test
>   case testValue of
>     (Atom "true") -> evalExp consequent
>     _             -> evalExp alternate
> evalIf _ = throwError 
>   "conditional clause not of form (if <test> <consequent> <alternate>)"

"inc" and "dec" is valid only when applied to a single integer

> evalInc :: Rule
> evalInc ((Atom s):[]) =
>   case reads s :: [(Int, String)] of
>     [(x,"")] -> return . Atom . show $ (x + 1)
>     _        -> throwError $ "Cannot increment '" ++ s ++ "'"
> evalInc ops = throwError $ "invalid operands to inc: \"" ++ 
>                            show ops ++ "\""

> evalDec :: Rule
> evalDec ((Atom s):[]) =
>   case reads s :: [(Int, String)] of
>     [(x,"")] -> return . Atom . show $ (x - 1)
>     _        -> throwError $ "Cannot increment '" ++ s ++ "'"
> evalDec ops = throwError $ "invalid operands to inc: \"" ++ 
>                            show ops ++ "\""

"=" is valid only when applied to two integers

> evalIntEq :: Rule
> evalIntEq ((Atom s1):(Atom s2):[]) = 
>   case reads s1 :: [(Int, String)] of
>     [(x,"")] -> case reads s2 :: [(Int,String)] of
>       [(y,"")] -> if x == y 
>                   then return (Atom "true") 
>                   else return (Atom "false")
>       _ -> throwError $ "Not an integer \"" ++ s2 ++ "\""
>     _ -> throwError $ "Not aan integer \"" ++ s1 ++ "\""
> evalIntEq ops = throwError $ "invalid operands to =: \"" ++ 
>                              show ops ++ "\""

Substitution replaces all occurances of a set of formal arguments with the
respective supplied operands in the body of the expression.

> substitute :: [Expression] -> [Expression] -> Expression -> Evaluator Expression

First check the number of arguments, if we're done, or if we failed to supply
enough operands to the application.

> substitute []            _  body = return body
> substitute ((Atom s):_) [] _    = 
>   throwError $ "The formal argument " ++ s ++ " was not supplied an operand"

Symbols are never substituted

> substitute _ _ s@(Symbol _) = return s

Combinations are recursively substituted

> substitute as ops (Combination es) = 
>   do
>     es' <- mapM (substitute as ops) es
>     return $ Combination es'

In these cases, we ensure that the formal arguments to the lambda
expression are, in fact, atoms.

> substitute ((Symbol _):_) _ _ = 
>   throwError "Symbol as formal arguement of lambda expression"

> substitute ((Combination _):_) _ _ =
>   throwError "Combination as formal argument of lambda expression"

Finally, an atomic body with name identical to an atomic argument is 
replaced with the operand, and no further substitution is done to
this expression!

> substitute ((Atom s):as) (operand:ops) body@(Atom s') =
>   if s == s'
>   then return operand
>   else substitute as ops body

