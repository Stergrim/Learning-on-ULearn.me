# Чтение списка точек

Теперь у вас есть список строк, в каждой из которой написаны две координаты точки (X, Y), разделенные пробелом. Кто-то уже вызвал метод `File.ReadLines(filename)` и теперь у вас есть массив всех строк файла.

```cs
public static void Main()
{
    // Функция тестирования ParsePoints
    
    foreach (var point in ParsePoints(new[] { "1 -2", "-3 4", "0 2" }))
        Console.WriteLine(point.X + " " + point.Y);
    foreach (var point in ParsePoints(new List<string> { "+01 -0042" }))
        Console.WriteLine(point.X + " " + point.Y);
}

public class Point
{
    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }
    public int X, Y;
}
```

Реализуйте метод `ParsePoints` в одно `LINQ`-выражение.

Постарайтесь не использовать функцию преобразования строки в число более одного раза.

Все тесты пройдены, задача сдана:
```cs
public static List<Point> ParsePoints(IEnumerable<string> lines)
{
    return lines
           .Select(line => line.Split())
           .Select(arrayLine => arrayLine.Select(int.Parse).ToArray())
           .Select(point => new Point(point[0], point[1]))
           .ToList();
}
```

Вывод программы:
```cs
1 -2
-3 4
0 2
1 -42
```
