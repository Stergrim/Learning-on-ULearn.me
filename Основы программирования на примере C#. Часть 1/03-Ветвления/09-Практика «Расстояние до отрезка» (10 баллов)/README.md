# Практика «Расстояние до отрезка»

[Скачайте проект Distance](DistanceTask.zip)

Напишите метод вычисления расстояния от отрезка до точки.

Для проверки своего решения запустите скачанный проект.

Расстоянием от отрезка до точки называется расстояние от ближайшей точки отрезка до точки. Это либо расстояние до точки от прямой, содержащей отрезок, либо расстояние до точки от одного из концов отрезка.

Все тесты пройдены, задача сдана:
```cs
using System;

namespace DistanceTask
{
	public static class DistanceTask
	{
		public static double GetDistanceToSegment(double ax, double ay,
												  double bx, double by,
												  double x,  double y)
		{
            double x0 = 0;
			double y0 = 0;

            if (ax == bx) return GetDistanceAxEqualBx(ax, ay, bx, by, x, y, x0, y0);
            if (ay == by) return GetDistanceAyEqualBy(ax, ay, bx, by, x, y, x0, y0);
			
			double b1 = ay - ((ay - by) / (ax - bx)) * ax;
			double b2 = y - (-1.0 / ((ay - by) / (ax - bx))) * x;
			x0 = -(b1 - b2) / (((ay - by) / (ax - bx)) - (-1.0 / ((ay - by) / (ax - bx))));
			y0 = ((ay - by) / (ax - bx)) * x0 + b1;
			if (x0 <= Math.Min(ax, bx)) x0 = Math.Min(ax, bx);
			else if (x0 >= Math.Max(ax, bx)) x0 = Math.Max(ax, bx);
			if (y0 <= Math.Min(ay, by)) y0 = Math.Min(ay, by);
			else if (y0 >= Math.Max(ay, by)) y0 = Math.Max(ay, by);
			return Math.Abs(Math.Sqrt((x - x0)*(x - x0) + (y - y0)*(y - y0)));
		}
		
		public static double GetDistanceAxEqualBx(double ax, double ay,
												  double bx, double by,
												  double x,  double y,
												  double x0, double y0)
		{
			x0 = ax;
			if (y <= Math.Min(ay, by)) y0 = Math.Min(ay, by);
			else if (y >= Math.Max(ay, by)) y0 = Math.Max(ay, by);
			else y0 = y;
			return Math.Abs(Math.Sqrt((x - x0)*(x - x0) + (y - y0)*(y - y0)));
		}

		public static double GetDistanceAyEqualBy(double ax, double ay,
												  double bx, double by,
												  double x,  double y,
												  double x0, double y0)
		{
			y0 = ay;
			if (x <= Math.Min(ax, bx)) x0 = Math.Min(ax, bx);
			else if (x >= Math.Max(ax, bx)) x0 = Math.Max(ax, bx);
			else x0 = x;
			return Math.Abs(Math.Sqrt((x - x0)*(x - x0) + (y - y0)*(y - y0)));
		}
	}
}
```