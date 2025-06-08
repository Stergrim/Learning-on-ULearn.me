# Генератор спама 2.0 [review]

В этом задании нужно адаптировать ваше решение задачи "[Генератор спама](https://ulearn.me/course/fpintroduction/Generator_spama_review__ed85c7f1-b03d-4bbe-97b4-db1140e7e5f5)" к новым требованиям при помощи трансформеров.

*Если вы не решили задачу "Генератор спама", то можете вернуться к ней.*

Для решения этой задачи, вам нужно создать тип `SpamT`, который будет комбинировать монады `Reader`, `Writer` и `State` с помощью соответствующих трансформеров. Монады `Writer` и `State` нужны для того, чтобы помимо генерации писем:
- В функции `findById` с помощью монады `Writer` логировать `id` персоны, по которой ведётся поиск, выводя сообщение `Looking for a person {id}`
- В функции `processPerson`:
  - логировать была ли найдена персона по `id` с помощью сообщений <br> `Person '{id}' is not found` или `Person '{id}' is '{surname}'`
  - cобирать статистику `PersonSearchStats` с помощью монады `State`
- Реализовать несколько вспомогательных функций

Вы можете переиспользовать любые функции из вашего решения "Генератора спама". Скачать задачу вы можете [здесь](SpamT.zip).


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module SpamT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Person (Person (..), PersonId, Sex(..), persons)

-- Тип для сбора статистики
data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Eq, Show)

-- Тип возвращаемого значения функций для обработки персон
newtype SpamT a = SpamT
  { runSpamT :: StateT PersonSearchStats (WriterT [String] (Reader [Person])) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader [Person]
    , MonadWriter [String]
    , MonadState PersonSearchStats
    )

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

-- Функция, извлекающая из SpamT результат вычислений, состояние и лог
runSpam :: SpamT a -> ((a, PersonSearchStats), [String])
runSpam p = runReader (runWriterT (runStateT (runSpamT p) emptyStats)) persons

-- Поиск персоны по id с логгированием
findById :: PersonId -> SpamT (Maybe Person)
findById pId = do
  tell ["Looking for a person '" ++ show pId ++ "'"]
  persons <- ask
  return $ find ((== pId) . id) persons

-- Формирует письмо для неженатого человека
processSingle :: Person -> String
processSingle p =
  (if sex p == Male then "Уважаемый " else "Уважаемая ") ++ firstName p ++ " " ++ middleName p ++ "!\n" ++
  "Разрешите предложить Вам наши услуги."

-- Формирует письмо для семейной пары
processPair :: Person -> Person -> String
processPair husband wife = concat
  [ "Уважаемые ", firstName husband, " ", middleName husband, " и "
  , firstName wife, " ", middleName wife, "!\n"
  , "Разрешите предложить вам наши услуги."
  ]

-- Функция, генерирующая письмо по id с подсчетом статистики и логгированием
processPerson :: PersonId -> SpamT (Maybe String)
processPerson pId = findById pId >>= \case
  Nothing -> tell ["Person '" ++ show pId ++ "' is not found"] >> return Nothing
  Just p -> do
    tell ["Person '" ++ show pId ++ "' is '" ++ surname p ++ "'"]
    case marriedBy p of
      Nothing -> modify (\s -> s { singlePersonsCount = succ (singlePersonsCount s) }) 
                >> return (Just $ processSingle p)
      Just mId -> findById mId >>= \case
        Nothing -> return Nothing
        Just spouse -> modify (\s -> s { marriedPersonsCount = succ (marriedPersonsCount s) })
                      >> return (Just $ processPair p spouse)

-- Функция, которая обрабатывает все id из списка, выкидывая результат по ненайденным id
processPersons :: [PersonId] -> SpamT [String]
processPersons personIds = do
  results <- mapM processPerson personIds
  return $ catMaybes results

-- Функция для вывода статистики
showStat :: SpamT a -> IO ()
showStat spamT = do
  let ((result, stats), logs) = runSpam spamT
  putStrLn $ "marriedPersonsCount: " ++ show (marriedPersonsCount stats)
  putStrLn $ "singlePersonsCount: " ++ show (singlePersonsCount stats)

-- Функция для вывода лога
showLog :: SpamT a -> IO ()
showLog spamT = do
  let ((result, stats), logs) = runSpam spamT
  mapM_ putStrLn logs
```
