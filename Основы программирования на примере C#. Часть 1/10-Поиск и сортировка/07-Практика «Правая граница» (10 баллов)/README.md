# Практика «Правая граница»

Продолжайте [в том же проекте](autocomplete.zip)

По аналогии с предыдущим заданием, в файле RightBorderTask.cs реализуйте бинарный поиск правой границы в упорядоченном множестве фраз. Подробности в комментариях в файле RightBorderTask.cs!


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;

namespace Autocomplete
{
    public class RightBorderTask
    {
        public static int GetRightBorderIndex(IReadOnlyList<string> phrases, string prefix,
                                              int left, int right)
        {
            int m = (right - left) / 2 + left;
            while (left + 1 < right)
            {
                if (string.Compare(prefix, phrases[m], 
                                   StringComparison.InvariantCultureIgnoreCase) >= 0 || 
                    phrases[m].StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase))
                    left = m;
                else right = m;
                m = (right - left) / 2 + left;
            }
            return right;
        }
    }
}
```
