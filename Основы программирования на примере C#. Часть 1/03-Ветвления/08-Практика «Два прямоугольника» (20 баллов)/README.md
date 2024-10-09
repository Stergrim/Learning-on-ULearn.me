# Практика «Два прямоугольника»

[Скачайте проект Rectangles](Rectangles.zip)

Вам даны два прямоугольника на плоскости, со сторонами параллельными осям координат с целочисленными координатами.

Реализуйте в классе RectanglesTask.cs три метода для работы с прямоугольниками:
- определение, есть ли у двух прямоугольников хотя бы одна общая точка (и граница и внутренность считаются частью прямоугольника);
- вычисление площади пересечения;
- определение, вложен ли один в другой.

Решите задание без использования библиотечных методов, кроме Min и Max.

Обратите внимание, что ваше решение должно корректно работать с вырожденными прямоугольниками: у которых длина или ширина равны 0.

Для проверки своего решения запустите скачанный проект.

В мире компьютерной графики принято, что верхний левый угол экрана имеет координаты (0, 0), а ось Y направлена вниз, а не вверх, как принято в математике. Поэтому в этой задаче нижний край прямоугольника имеет большую координату, чем верхний. Учитывайте это при решении задачи!

Все тесты пройдены, задача сдана:
```cs
using System;

namespace Rectangles
{
    public static class RectanglesTask
    {
        public static bool AreIntersected(Rectangle r1, Rectangle r2)
        {
            if (AreIntersectedLeftTop(r1, r2)) return true;
            else if (AreIntersectedLeftDown(r1, r2)) return true;
            else if (AreIntersectedRightTop(r1, r2)) return true;
            return AreIntersectedRightDown(r1, r2);
        }
    
        public static int IntersectionSquare(Rectangle r1, Rectangle r2)
        {
            if (AreIntersectedLeftTop(r1, r2))
                return Math.Min(r1.Top + r1.Height - r2.Top, r2.Height)*
                       Math.Min(r1.Left + r1.Width - r2.Left, r2.Width);
            else if (AreIntersectedLeftDown(r1, r2))
                return Math.Min(r2.Top + r2.Height - r1.Top, r1.Height)*
                       Math.Min(r2.Left + r2.Width - r1.Left, r1.Width);
            else if (AreIntersectedRightTop(r1, r2))
                return Math.Min(r1.Top + r1.Height - r2.Top, r2.Height)*
                       Math.Min(r2.Left + r2.Width - r1.Left, r1.Width);
            else if (AreIntersectedRightDown(r1, r2))
                return Math.Min(r2.Top + r2.Height - r1.Top, r1.Height)*
                       Math.Min(r1.Left + r1.Width - r2.Left, r2.Width);
            else return 0;
        }
        
        public static bool AreIntersectedLeftTop(Rectangle r1, Rectangle r2)
        { 
            return (((r1.Top + r1.Height) >= r2.Top) &&
                     (r1.Top <= r2.Top)) &&
                   (((r1.Left + r1.Width) >= r2.Left) &&
                     (r1.Left <= r2.Left));
        }
        
        public static bool AreIntersectedLeftDown(Rectangle r1, Rectangle r2)
        { 
            return (((r2.Top + r2.Height) >= r1.Top) &&
                     (r2.Top <= r1.Top)) &&
                   (((r2.Left + r2.Width) >= r1.Left) &&
                     (r2.Left <= r1.Left));
        }
        
        public static bool AreIntersectedRightTop(Rectangle r1, Rectangle r2)
        { 
            return (((r1.Top + r1.Height) >= r2.Top) &&
                      (r1.Top <= r2.Top)) &&
                    (((r2.Left + r2.Width) >= r1.Left) &&
                      (r1.Left >= r2.Left));
        }
        
        public static bool AreIntersectedRightDown(Rectangle r1, Rectangle r2)
        { 
            return (((r2.Top + r2.Height) >= r1.Top) &&
                     (r1.Top >= r2.Top)) &&
                   (((r1.Left + r1.Width) >= r2.Left) &&
                     (r1.Left <= r2.Left));
        }
        
        public static int IndexOfInnerRectangle(Rectangle r1, Rectangle r2)
        {
            if ((((r1.Top + r1.Height) >= (r2.Top + r2.Height)) &&
                  (r1.Top <= r2.Top)) &&
                (((r1.Left + r1.Width) >= (r2.Left + r2.Width)) &&
                  (r1.Left <= r2.Left))) return 1;
            else if ((((r2.Top + r2.Height) >= (r1.Top + r1.Height)) &&
                       (r2.Top <= r1.Top)) &&
                     (((r2.Left + r2.Width) >= (r1.Left + r1.Width)) &&
                       (r2.Left <= r1.Left))) return 0;
            else return -1;
        }
    }
}
```
