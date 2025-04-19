# Сделать Fluent

Сделайте интерфейс работы с List чуть более Fluent. Создайте extension-метод AddItems, чтобы следующий код работал!

```cs
public static void Main()
{
    var helloWorldList = new List<string>().AddItems("hello").AddItems("world");
    Console.WriteLine(helloWorldList.Count);
    
    var oneToFourList = new List<int>().AddItems(1, 2, 3, 4);
    Console.WriteLine(oneToFourList.Count);
    
    SecretTests();
}
```

Все тесты пройдены, задача сдана:
```cs
public static List<T> AddItems<T>(this List<T> items, params T[] elements)
{
    foreach(var e in elements)
        items.Add(e);
    return items;
}
```

Вывод программы:
```cs
2
4
```
