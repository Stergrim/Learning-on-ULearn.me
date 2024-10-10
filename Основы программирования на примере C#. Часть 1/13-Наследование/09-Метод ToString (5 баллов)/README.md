# Метод ToString

Метод `ToString` в реальной практике переопределяется довольно часто, поэтому давайте научимся это делать.

Создайте класс `Triangle` и переопределите в нем метод `ToString`.

```cs
public static void Main()
{
    var triangle = new Triangle
    {
        A = new Point { X = 0, Y = 0 },
        B = new Point { X = 1, Y = 2 },
        C = new Point { X = 3, Y = 2 }
    };
    Console.WriteLine(triangle.ToString());
}

public class Point
{
    public double X;
    public double Y;
    
    public override string ToString()
    {
        return $"{X} {Y}";
    }
}
```

Все тесты пройдены, задача сдана:
```cs
class Triangle
{
    public Point A; public Point B; public Point C;
    
    public override string ToString() { return string.Format("({0}) ({1}) ({2})", A, B, C); }
}
```

Вывод программы:
```cs
(0 0) (1 2) (3 2)
```
