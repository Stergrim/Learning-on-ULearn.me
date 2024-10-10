# Вектор

Добавьте конструктор в класс Vector.

Сделайте так, чтобы:
- поля этого класса инициализировались в конструкторе.
- поле Length (длина вектора), стало вычисляемым свойством.


```cs
public static void Check()
{
    Vector vector = new Vector(3, 4);
    Console.WriteLine(vector.ToString());
    
    vector.X = 0;
    vector.Y = -1;
    Console.WriteLine(vector.ToString());
    
    vector = new Vector(9, 40);
    Console.WriteLine(vector.ToString());
    
    Console.WriteLine(new Vector(0, 0).ToString());
}
```

Все тесты пройдены, задача сдана:
```cs
public class Vector
{
    public double X { get; set; }
    public double Y { get; set; }
    public double Length => Math.Sqrt(X*X + Y*Y);
    public Vector() : this(0, 0) {}
    public Vector(double x, double y) { X = x; Y = y; }
    
    public override string ToString()
    { return string.Format("({0}, {1}) with length: {2}", X, Y, Length); }
}
```

Вывод программы:
```cs
(3, 4) with length: 5
(0, -1) with length: 1
(9, 40) with length: 41
(0, 0) with length: 0
```
