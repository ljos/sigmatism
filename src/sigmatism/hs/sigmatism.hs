module Sigmatism
(   eval
  , read
) where

data Expr = Symbol String
          | Cons {car :: Expr, cdr :: Expr}
          | Nil
          | T
          deriving(Show, Eq)

eq :: Expr -> Expr -> Expr
eq Nil Nil = T
eq (Symbol a) (Symbol b) = if (a == b) then T else Nil
eq _ _ = Nil

assoc :: Expr -> Expr -> Expr
assoc a (Cons f s) =
  if T == eq a (car f) then
    cdr f
  else
    assoc a s

atom (Symbol _) = T
atom Nil = T
atom _ = Nil

evcon :: Expr -> Expr -> Expr
evcon (Cons f s) a =
  if T == eval (car f) a then
    eval ((car.cdr) f) a
  else
    evcon s a

pair :: Expr -> Expr -> Expr
pair Nil Nil = Nil
pair (Cons f r) (Cons h t) =
  Cons (Cons f (Cons h Nil)) (pair r t)

evlis :: Expr -> Expr -> Expr
evlis Nil _ = Nil
evlis (Cons f r) a = Cons (eval f a) (evlis r a)

append :: Expr -> Expr -> Expr
append Nil y = y
append (Cons f r) y = Cons f (append r y)

eval :: Expr -> Expr -> Expr
eval (Symbol s) ns = assoc (Symbol s) ns
eval (Cons (Symbol s) snd) a
  | s == "quote" = car snd
  | s == "atom"  = atom $ eval (car snd) a
  | s == "eq"    = eq (eval (car snd) a) (eval ((car.cdr) snd) a)
  | s == "car"   = car $ eval (car snd) a
  | s == "cdr"   = cdr $ eval (car snd) a
  | s == "cons"  = (Cons (eval (car snd) a) (eval ((car.cdr) snd) a))
  | s == "cond"  = evcon snd a
  | otherwise = eval (Cons (assoc (Symbol s) a) snd) a
eval (Cons (Cons (Symbol "label") rest) snd) a =
  let label = (Cons (car rest) (Cons (Cons (Symbol "label") rest) Nil))
      ns    = (Cons label a)
      expr  = ((car.cdr) rest)
  in
   eval expr ns
eval (Cons (Cons (Symbol "lambda") rest) snd) a =
  let expr  = ((car.cdr) rest)
      pairs = pair (car rest) $ evlis snd a
      ns    = append pairs a
  in
   eval expr ns
