# Практика «Статистика»

Продолжайте в том же проекте [linq-slideviews](linq-slideviews.zip).

В файле StatisticsTask реализуйте метод GetMedianTimePerSlide. Он должен работать так.

Обозначим T(U, S) время между посещением пользователем U слайда S и ближайшим следующим посещением тем же пользователем U какого-то другого слайда S2 != S.

T(U, S) можно считать примерной оценкой того, сколько времени пользователь U провел на слайде S.

Метод должен для указанного типа слайда, считать медиану значений T(U, S) по всем пользователям и всем слайдам этого типа.

Нужно игнорировать значения меньшие 1 минуты и большие 2 часов при расчете медианы.

Гарантируется, что в тестах и реальных данных отсутствуют записи, когда определенный пользователь заходит на один и тот же слайд более одного раза.

Время нужно возвращать в минутах.

Воспользуйтесь реализованными ранее методами Bigrams и Median.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;

namespace linq_slideviews
{
    public class StatisticsTask
    {
        public static double GetMedianTimePerSlide(List<VisitRecord> visits,
                                                   SlideType slideType)
        {
            return visits
                   .OrderBy(x => x.DateTime)
                   .GroupBy(x => x.UserId)
                   .Select(x => x.Bigrams()
                                .Where(user => user.First.UserId.Equals(user.Second.UserId)))
                   .SelectMany(x => x)
                   .Where(x => x.First.SlideType == slideType)
                   .Select(x => x.Second.DateTime.Subtract(x.First.DateTime).TotalMinutes)
                   .Where(x => !((x < 1.0) || (x > 120)))
                   .DefaultIfEmpty()
                   .Median();
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[linq-slideviews Edit](linq-slideviews_Edit.zip)
