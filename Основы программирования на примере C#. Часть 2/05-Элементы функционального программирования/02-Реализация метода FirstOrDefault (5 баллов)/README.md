# Реализация метода FirstOrDefault

Реализуйте метод FirstOrDefault, который принимает на вход последовательность и предикат, а возвращает первый элемент, который удовлетворяет предикату. Если последовательность пуста или не удалось найти ни один элемент, удовлетворяющий предикату, то необходимо вернуть default(T).

```cs
public static void Main()
{
    Assert.AreEqual(0, FirstOrDefault(new int[0], x => true)); // default(int) == 0
    Assert.AreEqual(null, FirstOrDefault(new string[0], x => true)); // default(string) == null
    Assert.AreEqual(3, FirstOrDefault(new[] { 1, 2, 3 }, x => x > 2));
    Assert.AreEqual(3, FirstOrDefault(new[] { 3, 2, 1 }, x => x > 2));
    Assert.AreEqual(3, FirstOrDefault(new[] { 2, 3, 1 }, x => x > 2));
    CheckYieldReturn();
    
    Console.WriteLine("OK");
}
```

Все тесты пройдены, задача сдана:
```cs
private static T FirstOrDefault<T>(IEnumerable<T> source, Func<T, bool> filter)
{
    foreach(var e in source)
        if(filter(e)) return e;
    return default(T);
}
```

Вывод программы:
```cs
OK
```
