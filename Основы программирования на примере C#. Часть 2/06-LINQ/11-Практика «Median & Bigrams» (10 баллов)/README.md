# Практика «Median & Bigrams»

Скачайте проект [linq-slideviews](linq-slideviews.zip).

В файле ExtensionsTask реализуйте два метода расширения: для вычисления медианы и для вычисления списка биграмм.

Эти методы пригодятся в будущем. Вы сможете их использовать на ряду и в перемешку с остальными методами LINQ.

Есть важное замечание по деталям реализации.

Создавая методы, работающие с `IEnumerable` стоит придерживаться следующих рекомендаций:
- Если это возможно, не перечисляйте входной IEnumerable до конца. Потому что IEnumerable может теоретически быть бесконечным.
- Не перечисляйте больше элементов, чем нужно для работы IEnumerable. Возможно, при перечислении лишнего элемента случится ошибка или другой нежелательный побочный эффект.
- Не полагайтесь на то, что `IEnumerable` можно будет перечислить дважды. Этого никто не гарантирует. Кстати, некоторые IDE, автоматически находят нарушение этого пункта. Например, подобные предупреждения умеют показывать JetBrains Rider и Visual Studio с установленным Resharper.
- Лучше использовать foreach, а не явные вызовы `MoveNext` и `Current`. Это лучше читается и сложнее допустить ошибку.




Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;

namespace linq_slideviews
{
    public static class ExtensionsTask
    {
        public static double Median(this IEnumerable<double> items)
        {
            int count;			
            var sources = items.OrderBy(x => x);
            sources.TryGetNonEnumeratedCount(out count);
            double mediana = 0;
            int i = 0;
            foreach (var source in sources)
            {
                i++;
                if (i == count / 2 + 1)
                    if (count % 2 == 0) return (mediana + source) / 2;
                    else return source;
                mediana = source;
            }
            throw new InvalidOperationException();
        }
    
        public static IEnumerable<(T First, T Second)> Bigrams<T>(this IEnumerable<T> items)
        {
            var q = new Queue<T> ();
            foreach (var source in items)
            {
                q.Enqueue(source);
                if(q.Count > 1) yield return new(q.Dequeue(), q.Peek());
            }
        }
    }
}
```
