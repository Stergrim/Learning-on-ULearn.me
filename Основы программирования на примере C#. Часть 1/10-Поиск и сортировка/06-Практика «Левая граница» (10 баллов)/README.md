# Практика «Левая граница»

[Скачайте проект](autocomplete.zip)

Во многих программах в разных контекстах можно увидеть функцию автодополнения вводимого текста. Обычно это работает так: есть словарь всех допустимых значений, и когда пользователь вводит начало некоторого слова, ему показывают несколько подходящих слов из словаря, начинающихся с букв, уже введенных пользователем.

Такую функцию очень просто реализовать "в лоб", если словарь небольшой. Если же словарь большой, то необходимо задумываться об эффективности алгоритма.

Запустите проект autocomplete и поизучайте программу. В частности попробуйте набрать префиксы a, ab, zzz. На zzz поиск будет заканчиваться таймаутом.

В следующих трех заданиях нужно будет внедрить в эту программу бинарный поиск и ускорить её!

Начать нужно с простого. В файле LeftBorderTask.cs реализуйте бинарный поиск левой границы в упорядоченном множестве фраз. Подробности в комментариях в файле LeftBorderTask.cs!


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;

namespace Autocomplete
{
    public class LeftBorderTask
    {
        public static int GetLeftBorderIndex(IReadOnlyList<string> phrases, string prefix,
                                             int left, int right)
        {
            if (left + 1 >= right) return left;
            int m = (right - left) / 2 + left;
    
            if (string.Compare(prefix, phrases[m],
                               StringComparison.InvariantCultureIgnoreCase) < 0 || 
                phrases[m].StartsWith(prefix, StringComparison.InvariantCultureIgnoreCase))
                return GetLeftBorderIndex(phrases, prefix, left, m);
            else return GetLeftBorderIndex(phrases, prefix, m, right);
        }
    }
}
```
