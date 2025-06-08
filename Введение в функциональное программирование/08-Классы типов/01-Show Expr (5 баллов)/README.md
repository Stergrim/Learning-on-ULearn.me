# Show Expr

Добавьте тип `Expr` в класс `Show`.

```hs
data Expr
  = Number Integer
  | Plus Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | UnaryMinus Expr
  | Abs Expr
  deriving Eq

-- Number 0 ~> 0
-- Plus (Number 0) (Number 3) ~> 0 + 3
-- Abs (Plus (Number 0) (Number 3)) ~> |0 + 3|
-- UnaryMinus (Plus (Mult (Number 2) (Number 2)) (Number 2)) ~> -((2 * 2) + 2)
```


Все тесты пройдены, задача сдана:
```hs
instance Show Expr where
  show expr = case expr of
    Number n      -> show n
    Plus a b      -> showArg a ++ " + " ++ showArg b
    Minus a b     -> showArg a ++ " - " ++ showArg b
    Mult a b      -> showArg a ++ " * " ++ showArg b
    UnaryMinus a  -> "-" ++ wrapUnary a
    Abs a         -> "|" ++ show a ++ "|"
    where
      showArg e = case e of
        Number _     -> show e
        UnaryMinus _ -> show e
        Abs _        -> show e
        _            -> "(" ++ show e ++ ")"
      
      wrapUnary e = case e of
        Number _     -> show e
        UnaryMinus _ -> show e
        Abs _        -> show e
        _            -> "(" ++ show e ++ ")"
```
