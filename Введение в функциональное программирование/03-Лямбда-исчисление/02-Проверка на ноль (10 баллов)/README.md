# Проверка на ноль

При написании функций для нумеров Чёрча важно понимать, как эти нумералы устроены. Мы уже видели, как можно написать функцию `succ`, используя их внутреннее устройство.

В качестве ещё одного такого примера рассмотрим то, как устроена функция `plus`, которая складывает два нумерала.

Фактически такую функцию можно прочитать как "примени f к m n раз". Так и напишем:

```hs
#      аргументы функции сложения — два нумерала
#      |  |   возвращаемое значение — нумерал, т.е. функция двух аргументов
#      v  v   v  v
plus = \n.\m. \f.\x. n f (m f x)
#                    ^^^^^^^^^^^
# конструируем новый нумерал со вторым аргументом в качестве начального значения
```

Используя устройство нумералов Чёрча, напишите функцию проверки нумерала Чёрча на ноль.

```hs
  isZero 0 ->> true
  isZero n ->> false
```

В этом упражнении, помимо самих нумералов, вам доступны только два именованных терма:

```hs
true = \a.\b.a
false = \a.\b.b
```

Они необходимы для запуска тестов, но функцию `isZero` можно написать и без них.

Вы можете скачать [тесты для локального запуска](IsZero.zip).

Все тесты пройдены, задача сдана:
```hs
isZero := \x.x (\f. false) true
```

**Error: undefined terms on the lambda expression**

Если вы видите это ошибку, скорее всего:
- в своём решении вы использовали имя необъявленного терма
- вы запустили тесты для терма `имя_терма`, не написав его определение.
