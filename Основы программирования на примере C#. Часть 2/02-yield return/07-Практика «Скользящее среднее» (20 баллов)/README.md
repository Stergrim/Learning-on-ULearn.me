# Практика «Скользящее среднее»

Продолжайте работу в [том же проекте Smooth](Smooth.zip)

В классе MovingAverageTask реализуйте функцию скользящего среднего.

При усреднении с окном размера W, первые W-1 точки результата в действительности должны усредняться по окнам меньшего размера. Так, первая точка должна попасть в результат без изменения. Отладьте реализацию с помощью приложенных модульных тестов.

Запустите тестирующее приложение и объясните наблюдаемый результат.

[Скользящее среднее в википедии](https://en.wikipedia.org/wiki/Moving_average#Simple_moving_average)

**Queue**

Для этой задачи вам пригодится структура данных Очередь. Не нужно создавать её самостоятельно, воспользуйтесь готовым классом Queue в пространстве имён System.Collections.Generic.


Все тесты пройдены, задача сдана:
```cs
using System.Collections;
using System.Collections.Generic;

namespace yield
{
    public static class MovingAverageTask
    {
        public static IEnumerable<DataPoint> MovingAverage(this IEnumerable<DataPoint> data,
                                                           int windowWidth)
        {
            var x = data.GetEnumerator();
            if (!x.MoveNext()) yield break;
            Queue myQ = new Queue();
            myQ.Enqueue(x.Current.OriginalY);
            double sum = x.Current.OriginalY;
            yield return x.Current.WithAvgSmoothedY(sum);
            while (x.MoveNext())
            {
                myQ.Enqueue(x.Current.OriginalY);
                sum += x.Current.OriginalY;
                if (myQ.Count > windowWidth) sum -= (double)myQ.Dequeue();
                yield return x.Current.WithAvgSmoothedY(sum/myQ.Count);
            }
        }
    }
}
```
