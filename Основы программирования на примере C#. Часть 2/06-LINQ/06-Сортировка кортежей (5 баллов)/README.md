# Сортировка кортежей

Еще одно полезное свойство кортежей — они реализуют интерфейс `IComparable`, сравнивающий кортежи по компонентам. То есть `Tuple.Create(1, 2)` будет меньше `Tuple.Create(2, 1)`. Этот интерфейс по умолчанию используется в методах сортировки и поиска минимума и максимума.

Используя этот факт, решите следующую задачу.

Дан текст, нужно составить список всех встречающихся в тексте слов, упорядоченный сначала по возрастанию длины слова, а потом лексикографически.

Запрещено использовать `ThenBy` и `ThenByDescending`.


Все тесты пройдены, задача сдана:
```cs
public static List<string>  GetSortedWords(string text)
{
    return (Regex.Split(string.Join(" ", text).ToLower(), @"\W+"))
           .Where(str => str != "")
           .Distinct()
           .OrderBy(str => Tuple.Create(str.Length, str))
           .ToList();
}
```

Вывод программы:
```cs
GetSortedWords("A box of biscuits, a box of mixed biscuits, and a biscuit mixer.")
'a' 'of' 'and' 'box' 'mixed' 'mixer' 'biscuit' 'biscuits'

GetSortedWords("")


GetSortedWords("Each Easter Eddie eats eighty Easter eggs.")
'each' 'eats' 'eggs' 'eddie' 'easter' 'eighty'
```
