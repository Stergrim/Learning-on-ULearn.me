# ZipSum

Необходимо реализовать функцию ZipSum с использованием yield return, которая принимает на вход две последовательности целых чисел и возвращает последовательность, состоящую из попарных сумм их элементов. Можете считать, что входные последовательности одинаковой длины.

```cs
public static void Main()
{
    Console.WriteLine(string.Join(" ", ZipSum(new[] { 1 }, new[] { 0 })));
    Console.WriteLine(string.Join(" ", ZipSum(new[] { 1, 2 }, new[] { 1, 2 })));
    Console.WriteLine(string.Join(" ", ZipSum(new int[0], new int[0])));
    Console.WriteLine(string.Join(" ", ZipSum(new[] { 1, 3, 5 }, new[] { 5, 3, -1 })));
    CheckYieldReturn();
}
```


Все тесты пройдены, задача сдана:
```cs
private static IEnumerable<int> ZipSum(IEnumerable<int> first, IEnumerable<int> second)
{
    var e1 = first.GetEnumerator();
    var e2 = second.GetEnumerator();
    while(e1.MoveNext() && e2.MoveNext())
        yield return e1.Current + e2.Current;
}
```

Вывод программы:
```cs
1
2 4

6 6 4
Проверка на ленивость пройдена!
```
