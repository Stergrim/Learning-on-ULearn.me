# Практика «Медианный фильтр»

Продолжайте [в том же проекте](image.zip).

Перед преобразованием в черно-белое, с изображения лучше бы удалить шум.

Для этого обработайте его так называемым медианным фильтром. Каждый пиксель изображения нужно заменить медианой всех пикселей в 1-окрестности этого пикселя. То есть для внутреннего пикселя, это будет медиана 9 значений. А для углового — медиана 4 значений.

Медианой массива называется значение элемента, который окажется точно посередине после сортировки массива по возрастанию. Медианой четного количества значений для определённости считайте среднее арифметическое двух значений посередине отсортированного массива.

Выполните эту задачу в файле MedianFilterTask.cs


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;

namespace Recognizer
{
    internal static class MedianFilterTask
    {
        public static double[,] MedianFilter(double[,] original)
        {
            int lenX = original.GetLength(0);
            int lenY = original.GetLength(1);
            double[,] temp = new double[lenX, lenY];
            List<double> window = new List<double>();
            int i = 0;
            int j = 0;
            while(i < lenX)
            {
                while(j < lenY)
                {
                    AddWindow(original, window, lenX, lenY, ref i, ref j);
                    window.Sort();
                    if (window.Count() % 2 == 0)
                        temp[i, j] = (window[window.Count()/2] + window[(window.Count() / 2)-1])/2.0;
                    else temp[i, j] = window[(window.Count()-1)/2];
                    window.Clear();
                    j++;
                }
                j = 0;
                i++;
            }
            return temp;
        }
		
        public static void AddWindow(double[,] original, List<double> window, int lenX, int lenY, ref int i, ref int j)
        {
            window.Add(original[i, j]);
            j++;
            if (j < lenY) window.Add(original[i, j]);
            i++;
            if ((i < lenX)&&(j < lenY)) window.Add(original[i, j]);
            j--;
            if ((j >= 0) && (i < lenX)) window.Add(original[i, j]);
            j--;
            if ((j >= 0) && (i < lenX)) window.Add(original[i, j]);
            i--;
            if ((i >= 0) && (j >= 0)) window.Add(original[i, j]);
            i--;
            if ((i >= 0) && (j >= 0)) window.Add(original[i, j]);
            j++;
            if ((j >= 0)&&(i >= 0)) window.Add(original[i, j]);
            j++;
            if ((j < lenY) && (i >= 0)) window.Add(original[i, j]);
            i++;
            j--;
        }
    }
}
```
