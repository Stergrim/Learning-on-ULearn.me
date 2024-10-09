# Практика «Нестатические методы»

Вы вдруг поняли, что не очень-то удобно писать имя класса Geometry при выполнении любой операции с векторами и сегментами. Однако, отказаться от этого класса вы не можете, потому что за те несколько минут, пока вы сдавали предыдущую задачу, вашу библиотеку скачали и начали использовать в своих проектах тысячи человек.

Поэтому вы решили сохранить этот класс, но добавить методы Vector.GetLength(), Segment.GetLength(), Vector.Add(Vector), Vector.Belongs(Segment) и Segment.Contains(Vector) не вместо, а вместе с соответствующими методами класса Geometry.

Сделайте это! Каждый из этих методов должен вызывать уже существующий метод класса Geometry, чтобы не дублировать код.

Вся функциональность предыдущего этапа должна остаться!


Все тесты пройдены, задача сдана:
```cs
using System;

namespace Geometry
{
    public class Vector
    {
        public double X; public double Y;
    
        public double GetLength() { return Geometry.GetLength(this); }
    
        public Vector Add(Vector vecSecond) { return Geometry.Add(this, vecSecond); }
    
        public bool Belongs(Segment segment)
        { return Geometry.IsVectorInSegment(this, segment); }
    }
    
    public class Segment
    {
        public Vector Begin; public Vector End;
    
        public double GetLength() { return Geometry.GetLength(this); }
    
        public bool Contains(Vector vector) { return Geometry.IsVectorInSegment(vector, this); }
    }
    
    public class Geometry
    {
        public static double GetLength(Vector vec)
        { return Math.Sqrt(vec.X*vec.X + vec.Y*vec.Y); }
    
        public static double GetLength(Segment segment)
        {
            return Math.Sqrt((segment.End.X - segment.Begin.X)*
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
