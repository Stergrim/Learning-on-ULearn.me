# Создание частотного словаря

Частотным словарём текста называют список пар: для каждого слова количество раз, которое оно встретилось в тексте.

В этой задаче нужно по тексту вернуть не весь частотный словарь, а только `count` слов, встретившихся в тексте больше всего раз. Среди слов, встречающихся одинаково часто, при выводе отдавать предпочтение лексикографически меньшим словам. Например, если все слова в тексте встретились только по одному разу, то вывести нужно `count` лексикографически первых слов.

При этом слова сравнивайте без учёта регистра, а возвращайте в нижнем регистре.

Напомним сигнатуры некоторых `LINQ`-методов, которые могут понадобиться в этом упражнении:

```cs
IEnumerable<IGrouping<K, T>>    GroupBy(this IEnumerable<T> items, Func<T, K> keySelector)
IOrderedEnumerable<T>           OrderBy(this IEnumerable<T> items, Func<T, K> keySelector)
IOrderedEnumerable<T> OrderByDescending(this IEnumerable<T> items, Func<T, K> keySelector)
IOrderedEnumerable<T>            ThenBy(this IOrderedEnumerable<T> items, Func<T, K> keySelector)
IOrderedEnumerable<T>  ThenByDescending(this IOrderedEnumerable<T> items, Func<T, K> keySelector)
IEnumerable<T>                     Take(this IEnumerable<T> items, int count)
```


Все тесты пройдены, задача сдана:
```cs
public static (string, int)[] GetMostFrequentWords(string text, int count)
{
    return Regex.Split(text, @"\W+")
           .Where(word => word != "")
           .GroupBy(x => x.ToLower())
           .OrderBy(x => (-x.Count(), x.Key))
           .Take(count)
           .Select(x => (x.Key, x.Count()))
           .ToArray();
}
```

Вывод программы:
```cs
GetMostFrequentWords("A box of biscuits, a box of mixed biscuits, and a biscuit mixer.", 2)
  a 3
  biscuits 2

GetMostFrequentWords("", 100)


GetMostFrequentWords("Each Easter Eddie eats eighty Easter eggs.", 3)
  easter 2
  each 1
  eats 1
```
