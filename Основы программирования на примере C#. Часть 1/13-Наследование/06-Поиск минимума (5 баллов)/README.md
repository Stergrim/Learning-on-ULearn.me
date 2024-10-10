# Поиск минимума

Напишите метод Min, который бы вычислял минимум из элементов массива.

```cs
public static void Main()
{
    Console.WriteLine(Min(new[] { 3, 6, 2, 4 }));
    Console.WriteLine(Min(new[] { "B", "A", "C", "D" }));
    Console.WriteLine(Min(new[] { '4', '2', '7' }));
}
```

Все тесты пройдены, задача сдана:
```cs
static IComparable Min(Array array)
{
    Array.Sort(array);
    return (IComparable)array.GetValue(0);
}
```

Вывод программы:
```cs
2
A
2
```
