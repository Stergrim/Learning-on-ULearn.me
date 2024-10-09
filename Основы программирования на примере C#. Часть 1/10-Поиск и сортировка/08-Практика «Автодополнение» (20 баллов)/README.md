# Практика «Автодополнение»

Продолжайте [в том же проекте](autocomplete.zip)

В файле AutocompleteTask.cs реализуйте методы GetTopByPrefix и GetCountByPrefix. Проверить корректность можно запустив проект autocomplete. Теперь отображаться должен не один вариант, а 10. А в строке статуса отображаться общее количество подходящих фраз.

На эти два метода нужно написать модульные тесты с использованием библиотеки NUnit. Они должны быть в том же файле AutocoompleteTask.cs отдельным классом AutocompleteTests.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace Autocomplete
{
    internal class AutocompleteTask
    {
        public static string FindFirstByPrefix(IReadOnlyList<string> phrases, string prefix)
        {
            var index = LeftBorderTask.GetLeftBorderIndex(phrases, prefix, -1, phrases.Count) + 1;
            if (index < phrases.Count &&
                phrases[index].StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase))
                return phrases[index];
            return null;
        }

        public static string[] GetTopByPrefix(IReadOnlyList<string> phrases, string prefix, int count)
        {
            var listPrefix = new List<String>();
            int left = LeftBorderTask.GetLeftBorderIndex(phrases, prefix, -1, phrases.Count);
        
            for (int i = 1; i < count + 1; i++)
                if((left + i) < phrases.Count)
                    if (phrases[left + i].StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase))
                        listPrefix.Add(phrases[left + i]);
        
            string[] topPrefix = new string[listPrefix.Count];
            for (int i = 0; i < listPrefix.Count; i++)
                topPrefix[i] = listPrefix[i];
            return topPrefix;
        }

        public static int GetCountByPrefix(IReadOnlyList<string> phrases, string prefix)
        {
            int left = LeftBorderTask.GetLeftBorderIndex(phrases, prefix, -1, phrases.Count);
            int right = RightBorderTask.GetRightBorderIndex(phrases, prefix, -1, phrases.Count);
            int count = right - left - 1;
            return count;
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[autocomplete Edit](autocomplete_Edit.zip)
