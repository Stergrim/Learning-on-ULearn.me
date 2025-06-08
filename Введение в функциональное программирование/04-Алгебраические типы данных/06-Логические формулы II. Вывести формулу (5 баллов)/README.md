# Логические формулы II. Вывести формулу

Продолжим работу с типом `LogExpr` из предыдущей задачи.

Напишите функцию `showExpr :: LogExpr -> String`, которая будет вычислять строковое представление логической формулы. Считаем, что в строковом представлении:
- операции `&` и `|` отделены от своих операндов ровно одним пробелом и заключены в скобки (даже там, где их можно было бы и опустить)
- отрицание не отделяется пробелами от операнда и имеет приоритет более высокий, чем конъюнкция и дизъюнкция, поэтому ее не нужно заключать в скобки.


```hs
-- showExpr(f1) ~ "^((false & b) | (a & true))"
f1 = Not (Or (And FConst (Var "b")) (And (Var "a") TConst))

-- showExpr(f2) ~ "^^false"
f2 = Not (Not FConst)
```


Все тесты пройдены, задача сдана:
```hs
module LFShow where
import LFBasic

instance Show LogExpr where
    show = showExpr

showExpr :: LogExpr -> String
showExpr TConst        = "true"
showExpr FConst        = "false"
showExpr (Var name)    = name
showExpr (Not expr)    = "^" ++ showExpr expr
showExpr (And e1 e2)   = "(" ++ showExpr e1 ++ " & " ++ showExpr e2 ++ ")"
showExpr (Or e1 e2)    = "(" ++ showExpr e1 ++ " | " ++ showExpr e2 ++ ")"
```

Вы можете скачать [тесты для локального запуска](LFShow.zip).

Для создания значения `LogExpr` по строковому представлению формулы можно использовать функцию `read :: String -> LogExpr` из модуля `LFBasic`. Там же объявлен тип `LogExpr`.
