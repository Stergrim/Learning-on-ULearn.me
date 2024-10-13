# Практика «Экспоненциальное сглаживание»

[Скачайте проект Smooth](Smooth.zip)

В классе ExpSmoothingTask реализуйте функцию экспоненциального сглаживания данных.

Отладьте реализацию с помощью приложенных модульных тестов. Запустите тестирующее приложение и объясните наблюдаемый результат.

[Экспоненциальное сглаживание в википедии](https://en.wikipedia.org/wiki/Exponential_smoothing#The_exponential_moving_average)

**Использование методов IEnumerator**

В этом модуле вы изучили интерфейс IEnumerator. Однако использовать напрямую методы этого интерфейса в своём коде как правило не нужно. Для перечисления элементов есть оператор foreach и методы LINQ. В отличие от интерфейса IEnumerator, они просты в использовании и легко читаются. Используйте работу через методы IEnumerator только, если задача не решается с помощью foreach или уже готовых методов LINQ.

В частности, вся серия задач данного модуля решается без использования методов IEnumerator.


Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;

namespace yield
{
    public static class ExpSmoothingTask
    {
        public static IEnumerable<DataPoint> SmoothExponentialy(
            this IEnumerable<DataPoint> data, double alpha)
        {
            var x = data.GetEnumerator();
            if (!x.MoveNext()) yield break;
            var s = x.Current.OriginalY;
            yield return x.Current.WithExpSmoothedY(s);
            while (x.MoveNext())
            {
                s += (x.Current.OriginalY - s)*alpha;
                yield return x.Current.WithExpSmoothedY(s);
            }
        }
    }
}
```
