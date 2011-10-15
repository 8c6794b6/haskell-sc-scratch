module Un where

data Un
  = Var Un
  | Lam (Un -> Un)
  | App Un Un

instance Show Un where
  show (Var v) = "v"
  show (Lam f) = "lam"
  show (App a b) = show a ++ " " ++ show b

eval (Var v) = Var v
eval (Lam b) = Lam b
eval (App m n) = eval m `op` n where
  Lam b `op` n = eval (b n)
  m `op` n     = App m n
