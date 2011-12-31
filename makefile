
all: runtime

runtime: Grammar.lhs Interactive.lhs JALRParser.lhs Parser.hs ParserCombinators.hs Substitution.lhs
	ghc --make -o runtime Interactive.lhs

