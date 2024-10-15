# Поиск самого длинного слова

Дан список слов, нужно найти самое длинное слово из этого списка, а из всех самых длинных — лексикографически первое слово.

Решите эту задачу в одно выражение.

Не используйте методы сортировки — сложность сортировки `O(N * log(N))`, однако эту задачу можно решить за `O(N)`.

```cs
public static void Main()
{
    Console.WriteLine(GetLongest(new[] {"azaz", "as", "sdsd"}));
    Console.WriteLine(GetLongest(new[] {"zzzz", "as", "sdsd"}));
    Console.WriteLine(GetLongest(new[] {"as", "12345", "as", "sds"}));
}
```


Все тесты пройдены, задача сдана:
```cs
public static string GetLongest(IEnumerable<string> words)
{
    return words
           .Where(word => word.Length == words.Max(n => n.Length))
           .Min();
}
```

Вывод программы:
```cs
azaz
sdsd
12345
```
