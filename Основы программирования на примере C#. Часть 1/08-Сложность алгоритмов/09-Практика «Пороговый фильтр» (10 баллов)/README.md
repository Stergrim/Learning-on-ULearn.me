# Практика «Пороговый фильтр»

Продолжайте [в том же проекте](image.zip).

Пора превратить изображение в черно-белое.

Сделать это можно с помощью порогового преобразования. Реализуйте его в методе

`public static double[,] ThresholdFilter(double[,] original, double whitePixelsFraction)`

Метод должен заменять пиксели со значением больше либо равному порогу `T` на белый (1.0), а остальные на черный (0.0).

Пороговое значение `T` найдите так, чтобы:
- если N — общее количество пикселей изображения, то как минимум (int)(whitePixelsFraction*N) пикселей стали белыми;
- при этом белыми стало как можно меньше пикселей.

Выполните эту задачу в файле ThresholdFilterTask.cs


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Linq;

namespace Recognizer
{
    public static class ThresholdFilterTask
    {
        public static double[] GetVector(double[,] original, int lenY, int lenX)
        {
            int N = lenX * lenY;
            double[] vector = new double[N];
            int k = 0;
            for (int i = 0; i < lenY; i++)
                for (int j = 0; j < lenX; j++)
                {
                    vector[k] = original[i, j];
                    k++;
                }
            return vector;
        }
    
        public static double[,] GetMatrix(double[] vector, int lenY, int lenX)
        {
            double[,] image = new double[lenY, lenX];
            int k = 0;
            for (int i = 0; i < lenY; i++)
                for (int j = 0; j < lenX; j++)
                {
                    image[i,j] = vector[k];
                    k++;
                }
            return image;
        }
    
        public static double[,] ThresholdFilter(double[,] original, double whitePixelsFraction)
        {
            int lenY = original.GetLength(0);
            int lenX = original.GetLength(1);
            int length = lenY * lenX;
            double[] temp = GetVector(original, lenY, lenX);
            double[] vector = GetVector(original, lenY, lenX);
            Array.Sort(temp);
            int numbersWhite = (int)Math.Floor(whitePixelsFraction * length);
            double threshold = 0.0;
            if (numbersWhite == 0) threshold = temp[length - 1] + 1;
            else threshold = temp[length - numbersWhite];
            for (int i = 0; i < length; i++)
                if (vector[i] < threshold) vector[i] = 0.0;
                else vector[i] = 1.0;
            original = GetMatrix(vector, lenY, lenX);
            return original;
        }
    }
}
```
