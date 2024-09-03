data Expr = Val Int | Add Expr Expr | Mult Expr Expr

data Op = EVAL_ADD Expr | ADD Int | EVAL_MULT Expr | MULT Int

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val x) c = exec c x
eval (Add x y) c = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL_ADD y : c) n = eval y (ADD n : c)
exec (ADD m : c) n = exec c (m + n)
exec (EVAL_MULT y : c) n = eval y (MULT n : c)
exec (MULT n : c) m = exec c (m * n)