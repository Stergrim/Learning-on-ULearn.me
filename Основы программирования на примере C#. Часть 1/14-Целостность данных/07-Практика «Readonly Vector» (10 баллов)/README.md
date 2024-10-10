# Практика «Readonly Vector»

Помните класс Vector из позапрошлой практики? Скорее всего, он был написан ужасно, с открытыми полями и всем прочим.

Как правило, такие структуры данных делают read-only.

В пространстве имен ReadOnlyVector сделайте класс ReadOnlyVector с двумя публичными readonly-полями X и Y, которые устанавливаются в конструкторе.

ReadOnlyVector должен содержать метод Add(ReadOnlyVector other), который возвращает сумму векторов.

При работе с readonly классами часто хочется изготовить вектор "такой же, но с другим значением поля X или Y". Обеспечьте такую функциональность с помощью методов WithX(double) и WithY(double)


Все тесты пройдены, задача сдана:
```cs
namespace ReadOnlyVector
{
    public class ReadOnlyVector
    {
        public readonly double X;
        public readonly double Y;
    
        public ReadOnlyVector(double x, double y) { X = x; Y = y; }
    
        public ReadOnlyVector Add(ReadOnlyVector other)
        { return new ReadOnlyVector( X + other.X, Y + other.Y ); }
    
        public ReadOnlyVector WithX(double x) { return new ReadOnlyVector( x, Y ); }
    
        public ReadOnlyVector WithY(double y) { return new ReadOnlyVector( X, y ); }
    }
}
```
