# Уровень 2

Придерживайтесь общепринятого стиля именования для текущего языка программирования.

Исходный код:
```cs
List<string> GetBigrams(IList<string> words)
{
    var colBigrams = words.Count - 1;
    var bigrams_list = new string[colBigrams];
    for (var I = 0; I < colBigrams; I++)
        bigrams_list[I] = words[I] + " " + words[I + 1];
    return bigrams_list.Distinct().ToList();
}
```

Исправленный код:
```cs
List<string> GetBigrams(IList<string> words)
{
    var bigramsCount = words.Count - 1;
    var bigrams = new string[bigramsCount];
    for (var i = 0; i < bigramsCount; i++)
        bigrams[i] = words[i] + " " + words[i + 1];
    return bigrams.Distinct().ToList();
}
```

Объяснения:
- Не используйте русские слова в именах (если только вы не программируете на 1C). Читая код, программисты ожидают видеть английские имена, поэтому написанные транслитом русские слова могут быть восприняты неправильно. Например, в данном случае col (количество) легко спутать с сокращением от слова column.
- В C# для составных имен принято использовать стиль CamelCase, и не использовать snake_case.
- В C# имена локальных переменных принято начинать с маленькой буквы. Нарушение таких, казалось бы, несущественных правил часто сильно раздражает опытных программистов.
