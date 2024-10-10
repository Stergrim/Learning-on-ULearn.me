# Против часовой стрелки

А как насчет того, чтобы отсортировать точки в порядке следования против часовой стрелки, считая первой ту, что находится на 3:00?

```cs
public static void Main()
{
    var array = new[]
    {
        new Point { X = 1, Y = 0 },
        new Point { X = -1, Y = 0 },
        new Point { X = 0, Y = 1 },
        new Point { X = 0, Y = -1 },
        new Point { X = 0.01, Y = 1 }
    };
    Array.Sort(array, new ClockwiseComparer());
    foreach (Point e in array)
        Console.WriteLine("{0} {1}", e.X, e.Y);
}

public class Point
{
    public double X;
    public double Y;
}
```

Все тесты пройдены, задача сдана:
```cs
public class ClockwiseComparer : IComparer
{
    public int Compare(object x, object y)
    {
        var point1 = (Point)x;
        var point2 = (Point)y;
        
        double alphaX = Math.Atan2(point1.Y,point1.X);
        double alphaY = Math.Atan2(point2.Y,point2.X);
        
        if (alphaX < 0) alphaX = 360 + alphaX;
        if (alphaY < 0) alphaY = 360 + alphaY;
        
        return alphaX.CompareTo(alphaY);
    }
}
```

Вывод программы:
```cs
1 0
0.01 1
0 1
-1 0
0 -1
```
