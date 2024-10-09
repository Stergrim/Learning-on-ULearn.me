# Практика «Отрезок»

Продолжаем разработку геометрической библиотеки.

Создайте класс `Segment`, представляющий отрезок прямой. Концы его отрезков должны задаваться двумя публичными полями: Begin и End типа Vector.

Добавьте метод `Geometry.GetLength`, вычисляющий длину сегмента, и метод `Geometry.IsVectorInSegment(Vector, Segment)`, проверяющий, что задаваемая вектором точка лежит в отрезке.

Сохраните функциональность предыдущего этапа.


Все тесты пройдены, задача сдана:
```cs
using System;

namespace Geometry
{
    public class Vector { public double X; public double Y; }
    
    public class Segment { public Vector Begin; public Vector End;}
    
    public class Geometry
    {
        public static double GetLength(Vector vec)
        { return Math.Sqrt(vec.X*vec.X + vec.Y*vec.Y); }
    
        public static double GetLength(Segment segment)
        {
            return Math.Sqrt((segment.End.X - segment.Begin.X) *
                             (segment.End.X - segment.Begin.X) +
                             (segment.End.Y - segment.Begin.Y) *
                             (segment.End.Y - segment.Begin.Y));
        }
    
        public static Vector Add(Vector vecFirst, Vector vecSecond)
        {
            var vecSum = new Vector();
            vecSum.X = vecFirst.X + vecSecond.X;
            vecSum.Y = vecFirst.Y + vecSecond.Y;
            return vecSum;
        }
    
        public static bool IsVectorInSegment (Vector vec, Segment segment)
        {
            var sBegin = new Segment { Begin = segment.Begin, End = vec };
            var sEnd = new Segment { Begin = segment.End, End = vec };
    
            double sLenBegin = Geometry.GetLength(sBegin);
            double sLenEnd = Geometry.GetLength(sEnd);
            double sLen = Geometry.GetLength(segment);
    
            return sLen == (sLenBegin+sLenEnd);
        }
    }
}
```
