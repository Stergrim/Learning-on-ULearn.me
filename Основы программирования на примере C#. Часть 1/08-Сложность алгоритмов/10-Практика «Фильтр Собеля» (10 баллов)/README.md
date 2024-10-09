# Практика «Фильтр Собеля»

Продолжайте [в том же проекте](image.zip).

Перед преобразованием в черно-белое, хорошо бы каким-то образом выделить границы объектов, чтобы только они стали белыми, а всё остальное черным.

Оказывается, это не сложно сделать с помощью так называемого фильтра Собеля. Он уже реализован в файле SobelFilterTask.cs. Ваша задача — обобщить этот код. Подробности — в комментариях!

Выполните эту задачу в файле SobelFilterTask.cs


Все тесты пройдены, задача сдана:
```cs
using System;

namespace Recognizer
{
    internal static class SobelFilterTask
    {
        public static double[,] SobelFilter(double[,] g, double[,] sx)
        {
            var width = g.GetLength(0);
            var height = g.GetLength(1);
            var lsy = sx.GetLength(0);
            var lsx = sx.GetLength(1);
            var result = new double[width, height];
            
            var sy = GetSy(sx, lsx, lsy);
    
            for (int x = (int)(lsy / 2); x < width - (int)(lsy / 2); x++)
                for (int y = (int)(lsx / 2); y < height - (int)(lsx / 2); y++)
                    result[x, y] = GetGxAndGy(g, sx, sy, x, y, lsx, lsy);
            return result;
        }
        
        public static double[,] GetSy(double[,] sx, int lsx, int lsy)
        {
            double[,] sy = new double[lsy, lsx];
            
            if ((lsy == 1) || (lsx == 1)) sy = sx;
    
            for (int i = 0; i < lsy; i++)
                for (int j = 0; j < lsx; j++)
                    sy[i, j] = sx[i, j];
    
            double tmp = 0.0;
            for (int i = 0; i < lsy; i++)
                for (int j = 0; j < i; j++)
                {
                    tmp = sy[i, j];
                    sy[i, j] = sy[j, i];
                    sy[j, i] = tmp;
                }
            return sy;
        }
        
        public static double GetGxAndGy(double[,] g, double[,] sx, double[,] sy,
                                        int x, int y, int lsx, int lsy)
        {
            double gx = 0.0;
            double gy = 0.0;
            
            for (int i = 0; i < lsy; i++)
                for (int j = 0; j < lsx; j++)
                {
                    gx = gx + g[x + i - (int)(lsy / 2), y + j - (int)(lsx / 2)] * sx[i, j];
                    gy = gy + g[x + i - (int)(lsy / 2), y + j - (int)(lsx / 2)] * sy[i, j];
                }
            return Math.Sqrt(gx * gx + gy * gy);
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[image Edit](image_Edit.zip)
