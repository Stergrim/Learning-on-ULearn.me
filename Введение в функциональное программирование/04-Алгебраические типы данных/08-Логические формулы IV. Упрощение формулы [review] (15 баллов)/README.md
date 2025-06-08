# Логические формулы IV. Упрощение формулы [review]

Продолжим работу с типом `LogExpr`.

*Тавтологией* называется логическая формула, которая истинна при любых значениях входящих в нее переменных. **Противоречие** (*тождественно ложная формула*) — формула, которая имеет ложное значение при любых значениях входящих в неё переменных.

Например, `true`, `(x | ^x)` — тавтология, а `false`, `(x & ^x)` — противоречие.

Напишите функцию `simplify :: LogExpr -> LogExpr`, которая упрощает переданную ей логическую формулу. Подразумевается тривиальное упрощение, связанное с тавтологичностью и противоречивостью операндов логических операций:
- `(x & y) ~~~> y`, если `x` — тавтология
- `(x & y) ~~~> false`, если `x` — противоречие
- `(x | y) ~~~> true`, если `x` — тавтология
- `(x | y) ~~~> y`, если `x` — противоречие
- `^x ~~~> false`, если `x` — тавтология
- `^x ~~~> true`, если `x` — противоречие

Если какая-то подформула не является ни тавтологией, ни противоречием, то после упрощения она остается без изменений.

**Замечания**

Скорее всего, надо будет написать много вспомогательных функций. Кроме того, почти наверняка вам понадобятся функции, которые вы писали при решении предыдущих задач.

Подумайте над оптимальностью решения: почти каждую подформулу заданного выражения надо проверить и на тавтологичность, и на противоречие. Но каждая из этих проверок связана с вычислением формулы на большом наборе значений переменных. Можно ли как-то оптимизировать процесс проверки типа формулы?


Все тесты пройдены, решение ожидает код-ревью:
```hs
module LFSimplify where
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

-- Вычисление значения выражения
compute :: [(String, Bool)] -> LogExpr -> Bool
compute env expr = case expr of
    FConst           -> False
    TConst           -> True
    (Var name)       -> case lookup name env of
                          Just value  -> value
                          Nothing     -> error "Variable not found in environment"
    (Not e)          -> not (compute env e)
    (And e1 e2)      -> compute env e1 && compute env e2
    (Or e1 e2)       -> compute env e1 || compute env e2

-- Проверка, является ли выражение тавтологией
isTautology :: LogExpr -> Bool
isTautology expr = 
    let vars = getVariables expr
        substitutions = generateAllSubstitutions vars
    in all (\sub -> compute sub expr) substitutions

-- Проверка, является ли выражение противоречием
isContradiction :: LogExpr -> Bool
isContradiction expr = 
    let vars = getVariables expr
        substitutions = generateAllSubstitutions vars
    in all (\sub -> not (compute sub expr)) substitutions

-- Основная функция
simplify :: LogExpr -> LogExpr
simplify expr =
    case simplifyStep expr of
        simplified 
            | isTautology simplified -> TConst
            | isContradiction simplified -> FConst
            | otherwise -> simplified
    where
    simplifyStep e = case e of
        TConst -> TConst
        FConst -> FConst
        Var x  -> Var x
        Not e' -> 
            case simplify e' of
                se | isTautology se -> FConst
                   | isContradiction se -> TConst
                   | otherwise -> Not se
        And e1 e2 -> 
            case (simplify e1, simplify e2) of
                (s1, s2) 
                    | isTautology s1 -> s2
                    | isContradiction s1 -> FConst
                    | isTautology s2 -> s1
                    | isContradiction s2 -> FConst
                    | otherwise -> And s1 s2
        Or e1 e2 -> 
            case (simplify e1, simplify e2) of
                (s1, s2)
                    | isTautology s1 -> TConst
                    | isContradiction s1 -> s2
                    | isTautology s2 -> TConst
                    | isContradiction s2 -> s1
                    | otherwise -> Or s1 s2
        _ -> e
```

Вы можете скачать [тесты для локального запуска](LFSimplify.zip).

Для создания значения `LogExpr` по строковому представлению формулы можно использовать функцию `read :: String -> LogExpr` из модуля `LFBasic`. Там же объявлен тип `LogExpr`.
