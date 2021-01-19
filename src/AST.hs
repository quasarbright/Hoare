module AST where

infixl 6 :+
infixl 6 :-
infixl 7 :*
infixr 3 :&&
infixr 2 :||
infix 4 :==,:/=,:<,:<=,:>,:>=
infixl 1 :=, :=>, :@
infixr 0 :.


data Expr = Var String
           | Int Integer
           | T
           | F
           | Expr :+ Expr
           | Expr :- Expr
           | Expr :* Expr
           | Not Expr
           | Expr :&& Expr
           | Expr :|| Expr
           | Expr :=> Expr
           | Expr :== Expr
           | Expr :/= Expr
           | Expr :< Expr
           | Expr :<= Expr
           | Expr :> Expr
           | Expr :>= Expr
           deriving(Eq, Ord)

instance Show Expr where
    show = \case
        Var x -> x
        Int n -> show n
        T -> "#T"
        F -> "#F"
        Not e -> "(~"++show e++")"
        l :+ r -> "("++show l++"+"++show r++")"
        l :- r -> "("++show l++"-"++show r++")"
        l :* r -> "("++show l++"*"++show r++")"
        l :&& r -> "("++show l++"&&"++show r++")"
        l :|| r -> "("++show l++"||"++show r++")"
        l :=> r -> "("++show l++"=>"++show r++")"
        l :== r -> "("++show l++"=="++show r++")"
        l :/= r -> "("++show l++"/="++show r++")"
        l :< r -> "("++show l++"<"++show r++")"
        l :<= r -> "("++show l++"<="++show r++")"
        l :> r -> "("++show l++">"++show r++")"
        l :>= r -> "("++show l++">="++show r++")"

data Statement = String := Expr
               | Skip
               | If Expr Statement Statement
               | While Expr Expr Statement
               | Statement :. Statement
               | Statement :@ Expr -- assertion
               deriving(Eq, Ord, Show)

subst :: String -> Expr -> Expr -> Expr
subst target replacement =
    let go = subst target replacement
    in \case
    Var x
        | x == target -> replacement
        | otherwise -> Var x
    Int n -> Int n
    Not b -> Not (go b)
    T -> T
    F -> F
    l :&& r -> go l :&& go r
    l :|| r -> go l :|| go r
    l :=> r -> go l :=> go r
    l :== r -> go l :== go r
    l :/= r -> go l :/= go r
    l :< r -> go l :< go r
    l :<= r -> go l :<= go r
    l :> r -> go l :> go r
    l :>= r -> go l :>= go r
    l :* r -> go l :* go r
    l :+ r -> go l :+ go r
    l :- r -> go l :- go r
