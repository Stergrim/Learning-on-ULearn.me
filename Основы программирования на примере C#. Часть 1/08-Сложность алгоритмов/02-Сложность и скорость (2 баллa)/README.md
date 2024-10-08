# Сложность и скорость

1. Пусть некоторые алгоритмы F и G решают одну и ту же задачу, но сложность алгоритма F строго меньше сложности алгоритма G на всех размерах входа. Вам ничего больше не известно про F и G. Найдите верный вывод, который можно сделать о F и G. (1 из 1 балла)
   * ✅ **Не исключено, что G может выполняться быстрее F на некоторых входах.** (Правильно!)
   * ❌ **Начиная с некоторого размера входа, F выполняется быстрее G на любом входе.**
   * ❌ **Алгоритм F на любом входе выполняется быстрее (то есть за меньшее количество элементарных операций) алгоритма G.**


2. Почему значение функции сложности алгоритма не определяет однозначно время работы алгоритма на компьютере? (1 из 1 балла)
   * ✅ **Некоторые "элементарные" операции могут выполняться процессором дольше, чем другие.** (Правильно!)
   * ✅ **В многозадачных операционных системах выполнение алгоритма может приостанавливаться на время выполнение других важных для ОС задач.** (Правильно!)
   * ✅ **Кеш процессора может ускорять работу с памятью, однако алгоритмы не могут напрямую управлять работой кеша.** (Правильно!)
   * ✅ **Компиляция одной и той же программы разными компиляторами или с разными настройками компилятора может давать разный набор "элементарных" инструкций.** (Правильно!)
