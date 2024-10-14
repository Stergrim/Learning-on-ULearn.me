# Практика «Чтение файла»

Продолжайте в том же проекте [linq-slideviews](linq-slideviews.zip).

В этой серии задач вам нужно будет проанализировать статистику посещения слайдов этого курса студентами.

Исходные данные содержатся в двух файлах:
1. slide.txt содержит информацию про каждый из слайдов — идентификатор, тип слайда (теория, задача или тест), и тема соответствующей недели. Пример файла slides.txt:
```
SlideId;SlideType;UnitTitle
0;theory;Первое знакомство с C#
1;quiz;Первое знакомство с C#
2;theory;Первое знакомство с C#
3;exercise;Первое знакомство с C#
```
2. visits.txt содержит по одной записи на первое посещение слайда каждым пользователем. Запись состоит из идентификатора пользователя, идентификатора слайда, даты и времени посещения этим пользователем этого слайда. Пример файла visits.txt:
```
UserId;SlideId;Date;Time
0;5;2014-09-03;12:20:28
1;6;2014-09-03;12:25:09
1;4;2014-09-03;12:25:24
```

В этой задаче в классе ParsingTask нужно реализовать методы чтения этих файлов.

Не используйте циклы в решении. Вместо этого используйте LINQ.

Обратите внимание, что в разных методах предлагается реализовать разную реакцию на некорректные строки файлов: в одном случае — игнорировать их, а в другом — выбрасывать исключение на первой же ошибочной строке. Это сделано исключительно в учебных целях — в реальных проектах стоит, конечно, придерживаться какой-то одной выбранной стратегии.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;

namespace linq_slideviews
{
    public class ParsingTask
    {
        public static IDictionary<int, SlideRecord> ParseSlideRecords(IEnumerable<string> lines)
        {
            var dictionary = new Dictionary<int, SlideRecord>();
            string[] parseLine;
            int id = 0;
            SlideType type;
            foreach (var line in lines.Skip(1))
            {
                parseLine = line.Split(';', StringSplitOptions.None);
                if ((parseLine.GetLength(0) == 3) &&
                    int.TryParse(parseLine[0], out id) &&
                    SlideType.TryParse(parseLine[1], true, out type))
                    dictionary.Add(id, new SlideRecord(id, type, parseLine[2]));
            }
            return dictionary;
        }
    
        public static IEnumerable<VisitRecord> ParseVisitRecords(
                      IEnumerable<string> lines, IDictionary<int, SlideRecord> slides)
        {
            string[] parseLine;
            int userId;
            int slideId;
            DateTime dateTime;
            foreach (var line in lines.Skip(1))
            {
                parseLine = line.Split(';', StringSplitOptions.None);
    
                if ((parseLine.GetLength(0) == 4) &&
                    int.TryParse(parseLine[0], out userId) &&
                    int.TryParse(parseLine[1], out slideId) &&
                    DateTime.TryParse(parseLine[2] + " " + parseLine[3], out dateTime) &&
                    slides.ContainsKey(slideId))
                    yield return new VisitRecord(userId,
                                                 slideId,
                                                 dateTime,
                                                 slides[slideId].SlideType);
                else throw new FormatException("Wrong line [" + line + "]");
            }
        }
    }
}
```
