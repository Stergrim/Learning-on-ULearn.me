# Логические формулы III. Тавтология [review]

Продолжим работу с типом `LogExpr`.

[Тавтологией](https://ru.wikipedia.org/wiki/%D0%A2%D0%B0%D0%B2%D1%82%D0%BE%D0%BB%D0%BE%D0%B3%D0%B8%D1%8F_(%D0%BB%D0%BE%D0%B3%D0%B8%D0%BA%D0%B0)) называется логическая формула, которая истинна при любых значениях входящих в нее переменных. Напишите функцию `isTautology :: LogExpr -> Bool`, проверяющую, что формула является тавтологией.

Не забывайте объявлять вспомогательные функции для декомпозиции решения этой задачи.


Все тесты пройдены, решение ожидает код-ревью:
```hs
module LFTautology where
import LFBasic

-- Объединение списков без дубликатов
union :: [String] -> [String] -> [String]
union xs ys = xs ++ filter (`notElem` xs) ys

-- Получить список всех переменных в выражении
getVariables :: LogExpr -> [String]
getVariables expr = case expr of
    Var x         -> [x]
    Not e         -> getVariables e
    And e1 e2     -> union (getVariables e1) (getVariables e2)
    Or e1 e2      -> union (getVariables e1) (getVariables e2)
    _             -> []

-- Генерация всех возможных комбинаций значений переменных
generateAllSubstitutions :: [String] -> [[(String, Bool)]]
generateAllSubstitutions [] = [[]]
generateAllSubstitutions (x:xs) = 
    let rest = generateAllSubstitutions xs
    in [(x, True):sub | sub <- rest] ++ [(x, False):sub | sub <- rest]

-- Подстановка значений переменных в выражение
substitute :: LogExpr -> [(String, Bool)] -> LogExpr
substitute expr env = case expr of
    Var x      -> case lookup x env of
                    Just True  -> TConst
                    Just False -> FConst
                    Nothing    -> Var x
    Not e      -> Not (substitute e env)
    And e1 e2  -> And (substitute e1 env) (substitute e2 env)
    Or e1 e2   -> Or (substitute e1 env) (substitute e2 env)
    e          -> e

-- Вычисление значения выражения
eval :: LogExpr -> Bool
eval expr = case expr of
    TConst      -> True
    FConst      -> False
    Not e       -> not (eval e)
    And e1 e2   -> eval e1 && eval e2
    Or e1 e2    -> eval e1 || eval e2
    _           -> error "Expression contains free variables"

-- Проверка, является ли выражение тавтологией
isTautology :: LogExpr -> Bool
isTautology expr = 
    let vars = getVariables expr
        substitutions = generateAllSubstitutions vars
    in all (\sub -> eval (substitute expr sub)) substitutions
```

Вы можете скачать [тесты для локального запуска](LFTautology.zip).

Для создания значения `LogExpr` по строковому представлению формулы можно использовать функцию `read :: String -> LogExpr` из модуля `LFBasic`. Там же объявлен тип `LogExpr`.
