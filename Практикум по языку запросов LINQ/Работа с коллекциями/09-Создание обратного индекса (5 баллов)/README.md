# Создание обратного индекса

Обратный индекс — это структура данных, часто использующаяся в задачах полнотекстового поиска нужного документа в большой базе документов.

По своей сути обратный индекс напоминает индекс в конце бумажных энциклопедий, где для каждого ключевого слова указан список страниц, где оно встречается.

Вам требуется по списку документов построить обратный индекс.

Документ определен так:

```cs
public class Document
{
    public int Id;
    public string Text;
}
```

Обратный индекс в нашем случае — это словарь `ILookup<string, int>`, ключом в котором является слово, а значениями — идентификаторы всех документов, содержащих это слово.

```cs
public static void Main()
{
    Document[] documents =
    {
        new Document {Id = 1, Text = "Hello world!"},
        new Document {Id = 2, Text = "World, world, world... Just words..."},
        new Document {Id = 3, Text = "Words — power"},
        new Document {Id = 4, Text = ""}
    };
    var index = BuildInvertedIndex(documents);
    SearchQuery("world", index);
    SearchQuery("words", index);
    SearchQuery("power", index);
    SearchQuery("cthulhu", index);
    SearchQuery("", index);
}
```


Все тесты пройдены, задача сдана:
```cs
public static ILookup<string, int> BuildInvertedIndex(Document[] documents)
{
    return documents
           .SelectMany( x => Regex.Split(x.Text.ToLower(), @"\W+")
                             .Where(n => !string.IsNullOrEmpty(n))
                             .Distinct()
                             .Select(n => new Document { Id = x.Id, Text = n }))
           .ToLookup(x => x.Text, x => x.Id);
}
```

Вывод программы:
```cs
SearchQuery('world') found documents: 1, 2
SearchQuery('words') found documents: 2, 3
SearchQuery('power') found documents: 3
SearchQuery('cthulhu') found documents:
SearchQuery('') found documents:
```
