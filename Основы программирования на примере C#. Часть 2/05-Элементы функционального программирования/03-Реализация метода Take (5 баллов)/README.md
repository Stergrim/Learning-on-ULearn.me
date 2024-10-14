# Реализация метода Take

Реализуйте метод Take с использованием yield return, который принимает на вход последовательность source и число count, а возвращает последовательность только из первых count элементов source.

Если source содержит меньше count элементов, то вернуть нужно все элементы source.

```cs
public static void Main()
{
    Func<int[], int, string> take = (source, count) => string.Join(" ", Take(source, count));
    
    Assert.AreEqual("1 2", take(new[] { 1, 2, 3, 4 }, 2));
    Assert.AreEqual("4", take(new[] { 4 }, 1));
    Assert.AreEqual("", take(new[] { 5 }, 0));
    
    var num = new Random().Next(0, 1000);
    Assert.AreEqual(num.ToString(), take(new[] { num }, 100500));
    
    CheckLazyness();
    Console.WriteLine("OK");
}
```

Все тесты пройдены, задача сдана:
```cs
private static IEnumerable<T> Take<T>(IEnumerable<T> source, int count)
{
    if(count == 0) yield break;
    foreach(var e in source)
    { yield return e; if(--count <= 0) yield break; }
}
```

Вывод программы:
```cs
OK
```
