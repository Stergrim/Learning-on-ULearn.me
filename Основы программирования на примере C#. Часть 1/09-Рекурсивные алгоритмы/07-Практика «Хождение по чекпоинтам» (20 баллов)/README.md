# Практика «Хождение по чекпоинтам»

[Скачайте проект](route-planning.zip).

Роботу нужно проехать через указанные точки, посетив каждую хотя бы один раз. Нужно спланировать маршрут так, чтобы суммарный путь был минимален.

<p float="left">
<img src="travelling_salesman_problem.png" width="600" />
</p>

В файле PathFinderTask допишите код функции `int[] FindBestCheckpointsOrder(Point[] checkpoints)`.

Функция принимает массив чекпоинтов. Робот изначально находится в точке `checkpoints[0]`. Вернуть нужно порядок посещения чекпоинтов. Например, если функция возвращает массив {0,2,1}, это означает, что робот сначала поедет в чекпоинт с индексом 2, а из него в чекпоинт с индексом 1 и на этом закончит свой путь.

Действуйте как на лекциях, можете адаптировать код с лекций. Функция должна быть рекурсивной.

Реализуйте следующую оптимизацию (отсечение перебора): прекращайте перебор, если текущая длина пути уже больше, чем минимальный путь, найденный ранее.

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Drawing;

namespace RoutePlanning
{
    public static class PathFinderTask
    {
        static double price = 0.0;
        static int[] temper;
    
        public static int[] FindBestCheckpointsOrder(Point[] checkpoints)
        {
            var bestOrder = MakeTrivialPermutation(checkpoints.Length);
            temper = MakeTrivialPermutation(checkpoints.Length);
            price = Evaluate(checkpoints, bestOrder, bestOrder.Length);
            FindBestCheckpointsOrder(checkpoints, bestOrder, 1);
            return temper;
        }
    
        public static void FindBestCheckpointsOrder(Point[] checkpoints, int[] bestOrder, int position)
        {
            if (position == bestOrder.Length)
            {
                if (PointExtensions.GetPathLength(checkpoints, bestOrder) < price)
                {
                    price = PointExtensions.GetPathLength(checkpoints, bestOrder);
                    bestOrder.CopyTo(temper,0);
                }
                return;
            }
        
            for (int i = 0; i < bestOrder.Length; i++)
            {
                var index = Array.IndexOf(bestOrder, i, 0, position);
                if (index != -1) continue;
                bestOrder[position] = i;
                if (PointExtensions.GetPathLength(checkpoints, SubArray(bestOrder, position)) > price) 
                    return;
                else FindBestCheckpointsOrder(checkpoints, bestOrder, position + 1);
            }
        }
    
        public static int[] SubArray(int[] bestOrder, int position)
        {
            int[] subArray = new int[position];
            for (int i = 1; i < position; i++)
                subArray[i] = bestOrder[i];
            return subArray;
        }
    
        public static double Evaluate(Point[] checkpoints, int[] bestOrder, int position)
        {
            double temp = 0;
            for (int i = 1; i < position; i++)
            {
                var dx = checkpoints[bestOrder[i]].X - checkpoints[bestOrder[i - 1]].X;
                var dy = checkpoints[bestOrder[i]].Y - checkpoints[bestOrder[i - 1]].Y;
                temp = temp + Math.Sqrt(dx * dx + dy * dy);
            }
            return temp;
        }
    
        private static int[] MakeTrivialPermutation(int size)
        {
            var bestOrder = new int[size];
            for (int i = 0; i < bestOrder.Length; i++)
                bestOrder[i] = i;
            return bestOrder;
        }
    }
}
```
