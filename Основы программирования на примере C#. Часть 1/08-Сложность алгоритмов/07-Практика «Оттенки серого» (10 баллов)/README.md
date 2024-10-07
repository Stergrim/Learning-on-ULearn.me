# Практика «Поля в кавычках»

[Скачайте проект](image.zip).

Представьте себе робота-уборщика на кухне, которого только что случайно пнула хозяйка. Ему нужно сориентироваться, где он теперь находится и куда повёрнут. К счастью у робота есть камера, а пол на кухне выложен квадратной кафельной плиткой. Осталось немного обработать изображение с видеокамеры, выделить границы объектов и по ним сориентироваться.

Первым шагом нужно перевести цветное изображение в оттенки серого. Его будет проще анализировать.

Выполните эту задачу в файле GrayscaleTask.cs


Все тесты пройдены, задача сдана:
```cs
namespace Recognizer
{
    public static class GrayscaleTask
    {
        public static double[,] ToGrayscale(Pixel[,] original)
        {
            int lenX = original.GetLength(0);
            int lenY = original.GetLength(1);
            var grayscale = new double[lenX, lenY];
        
            for (int i = 0; i < lenX; i++)
                for (int j = 0; j < lenY; j++)
                    grayscale[i, j] = (0.299 * original[i, j].R + 0.587 * original[i, j].G + 0.114 * original[i, j].B) / 255;
            return grayscale;
        }
    }
}
```
