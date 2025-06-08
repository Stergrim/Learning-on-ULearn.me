# Угадай-ка [review]

Напишите игру для угадывания случайного числа.

При старте, необходимо случайным образом выбрать число в отрезке `[0..100]` и предоставить пользователю возможность его угадать. Функцию для генерации случайного числа можно найти в `System.Random`.

Игру необходимо реализовать следующим образом:
- Для запроса числа от пользователя надо вывести `Your number:`.
- Если введенное число больше, чем загаданное, то надо вывести строку `Too big`.
- Если введенное число меньше, чем загаданное, то надо вывести строку `Too small`.
- Если число равно загаданному, то надо вывести строку `Yep, that's the number!` и выйти из программы.

Обрабатывать некорректные числа не обязательно, можно предполагать, что ввод всегда корректный.

Пример игровой сессии:

```hs
> playGuessGame
Your number: 50
> Too big
Your number: 25
> Too small
Your number: 37
> Yep, that's the number!
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
module GuessWhat where

import System.Random (randomRIO)

playGuessGame :: IO ()
playGuessGame = do
    secretNumber <- randomRIO (0, 100) :: IO Int
    play secretNumber

play :: Int -> IO ()
play secretNumber = do
    putStr "Your number: "
    guess <- readLn
    case compare guess secretNumber of
        GT -> do
            putStrLn "Too big"
            play secretNumber
        LT -> do
            putStrLn "Too small"
            play secretNumber
        EQ -> putStrLn "Yep, that's the number!"
```

Вы можете скачать [тесты для локального запуска](GuessWhat.zip).

Вам так же понадобятся функции `compare :: Int -> Int -> Ordering` и `read :: String -> Int`:

```hs
> compare 2 3
LT
> compare 5 1
GT
> compare 1 1
EQ
> read "12" + 2
14
```
