
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

> evalExp (Combination (operator:operands)) =
>   do operands' <- mapM evalExp operands
>      operator' <- evalExp operator
>      evalCombination operator' operands'

> evalCombination :: Expression -> [Expression] -> Evaluator Expression

> evalCombination (Atom s) operands = 
>   case Map.lookup s specialForms of
>     Just rule -> rule operands
>     Nothing ->
>       do context <- get
>          case Map.lookup s context of
>            Just definition -> evalExp.Combination $ definition : operands
>            Nothing -> throwError $ 
>                       "No rule or definition for '" ++ s ++ "'"

> evalCombination (Symbol _) _ = throwError "No rule to apply a symbol"

> evalCombination (Combination 
>                    ((Atom "lambda") : (Combination formalArgs) : body : []))
>                 operands = do
>   exp' <- substitute formalArgs operands body

TODO! catch error and augment with more information

>   evalExp exp'

> evalCombination (Combination _) _ = throwError "No rule to apply a combination"

Here's a table for all rules associated with each special form

> type Rule = [Expression] -> Evaluator Expression

> specialForms :: Map String Rule
> specialForms = Map.fromList [ ("lambda", evalLambda),
>                               ("define", evalDefine) ]

> evalLambda :: Rule
> evalLambda ops = return $ Combination $ Atom "lambda" : ops

> evalDefine :: Rule
> evalDefine ((Atom s):e:[]) = do
>   context <- get
>   if Map.member s context
>   then throwError $ "Redefinition of '" ++ s ++ "'"
>   else do put $ Map.insert s e context
>           return.Atom $ "*defined '" ++ s ++ "'*"

> evalDefine _ = throwError $ 
>   "Define expressions must be of form (define <atom> <expression>)"

Substitution

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

