# Максимум в массиве

Помните задачу нахождения максимума в массиве? Пришло время повторить ее для общего случая!

```cs
public static void Main()
{
    Console.WriteLine(Max(new int[0]));
    Console.WriteLine(Max(new[] { 3 }));
    Console.WriteLine(Max(new[] { 3, 1, 2 }));
    Console.WriteLine(Max(new[] { "A", "B", "C" }));
}
```


Все тесты пройдены, задача сдана:
```cs
public static T Max<T>(T[] source) where T:IComparable
{
    return source.Length == 0 ? default(T) : source.Max();
}
```

Вывод программы:
```cs
0
3
3
C
```
