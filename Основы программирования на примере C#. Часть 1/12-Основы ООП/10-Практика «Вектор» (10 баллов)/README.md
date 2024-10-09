# Практика «Вектор»

Создайте новый проект в Visual Studio. Выберите в качестве типа проекта Class Library.

В этом проекте создайте два класса, Vector и Geometry, в пространстве имен Geometry.

В классе Vector должно быть два публичных поля, X и Y, типа double.

В классе Geometry должно быть два статических метода: GetLength, который возвращает длину переданного вектора, и Add, который возвращает сумму двух переданных векторов.

Оба класса разместите в одном файле. Вообще-то так обычно делать не стоит, но так удобнее для нашей автоматической проверки выполнения задания.


Все тесты пройдены, задача сдана:
```cs
using System;

namespace Geometry
{
    public class Vector { public double X; public double Y; }

    public class Geometry
    {
        public static double GetLength(Vector vec)
        { return Math.Sqrt(vec.X*vec.X + vec.Y*vec.Y); }
    
        public static Vector Add(Vector vecFirst, Vector vecSecond)
        { 
            var vecSum = new Vector();
            vecSum.X = vecFirst.X + vecSecond.X;
            vecSum.Y = vecFirst.Y + vecSecond.Y;
            return vecSum;
        }
    }
}
```
