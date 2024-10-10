# Практика «Поиск угла»

В [в том же проекте](manipulator.zip) решите вспомогательную задачу на геометрию в классе TriangleTask.cs.

Реализуйте метод `double GetABAngle(a, b, c)`. Он должен возвращать угол в радианах между сторонами a и b в треугольнике со сторонами a, b, c.

Естественно, для практических целей треугольник может быть вырожденным, то есть некоторые стороны могут иметь длину 0. При недопустимых аргументах или при невозможности определить угол в вырожденном треугольнике метод должен возвращать `double.NaN`.

В том же файле напишите модульные тесты, покрывающие все случаи. В том числе и особые, граничные случаи.


Все тесты пройдены, задача сдана:
```cs
using System;
using NUnit.Framework;

namespace Manipulation
{
    public class TriangleTask
    {
        public static double GetABAngle(double a, double b, double c)
        {
            double alpha;
            if (a <= 0 || b <= 0 || c < 0) alpha = double.NaN;
            else if (c == 0) alpha = 0;
            else if (c == a + b) alpha = Math.PI;
            else alpha = Math.Acos((a*a + b*b - c*c) / (2 * a * b));
            return alpha;
        }
    }
    
    [TestFixture]
    public class TriangleTask_Tests
    {
        [TestCase(3, 4, 5, Math.PI / 2)]
        [TestCase(1, 1, 1, Math.PI / 3)]
        [TestCase(3, 4, 5, Math.PI / 2)]
        [TestCase(1, 1, 1, Math.PI / 3)]
        
        public void TestGetABAngle(double a, double b, double c, double expectedAngle)
        { Assert.AreEqual(expectedAngle, TriangleTask.GetABAngle(a, b, c), 1e-5); }
    }
}
```
