module VC where

import AST

genVCs :: Expr -> Statement -> Expr -> [Expr]
genVCs p s q = case s of
    x := rhs ->
        [p :=> subst x rhs q]
    If cnd thn els ->
        genVCs (p :&& cnd) thn q
        ++ genVCs (p :&& Not cnd) els q
    s' :. (x := rhs) ->
        genVCs p s' (subst x rhs q)
    (s1 :@ r) :. s2 ->
        genVCs p s1 r
        ++ genVCs r s2 q
    _ :. _ ->
        error ("unannotated sequence "++show s)
    _ :@ _ ->
        error "bad annotation"
    Skip ->
        [p :=> q]
    While cnd invariant body ->
        [ p :=> invariant ] -- invariant is initially true
        ++ genVCs (invariant :&& cnd) body invariant -- loop maintains invariant
        ++ [ invariant :&& Not cnd :=> q ] -- leaving loop guarantees post condition

division :: Statement
division =
    ("R" := Var "X" :@ (Var "R" :== Var "X")):.
    -- TODO shouldn't need this @
    -- statements should be of the form x = e; y = e; {b} while ...
    -- group those assignments and run their substitutions on b?
    
    -- or maybe just require annotations between assignments sometimes
    (("Q" := Int 0) :@ (Var "R" :== Var "X" :&& Var "Q" :== Int 0)):.
    While (Var "Y" :<= Var "R") (Var "X" :== Var "R" :+ Var "Y" :* Var "Q")
        ("R" := Var "R" :- Var "Y":. "Q" := Var "Q" :+ Int 1)

divisionVCs :: [Expr]
divisionVCs = genVCs T division (Var "X" :== Var "R" :+ Var "Y" :* Var "Q" :&& Var "R" :< Var "Y")

