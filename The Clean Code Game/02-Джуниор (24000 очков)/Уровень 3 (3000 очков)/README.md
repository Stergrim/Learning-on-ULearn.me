# Уровень 3

В имени нужно отражать полезный для понимания смысл и стараться избегать слов заместителей с неопределенным смыслом.

Исходный код:
```cs
static void Main(string[] args)
{
    var arg = args.Length > 0 ? args[0] : defaultFilename;
    DateTime lastWriteTime = new FileInfo(arg).LastWriteTime;
    bool check = lastWriteTime > DateTime.Now - TimeSpan.FromSeconds(1);
    Handle(arg, check);
    Console.WriteLine(lastWriteTime);
}
```

Исправленный код:
```cs
static void Main(string[] args)
{
    var inputFile = args.Length > 0 ? args[0] : defaultFilename;
    DateTime lastWriteTime = new FileInfo(inputFile).LastWriteTime;
    bool recentlyModified = lastWriteTime > DateTime.Now - TimeSpan.FromSeconds(1);
    ConvertFileToJson(inputFile, recentlyModified);
    Console.WriteLine(lastWriteTime);
}
```

Объяснения:
- Отражайте в имени то, что важно при дальнейшем использовании. В данном случае то, что это имя входного файла важнее того, что оно получено из аргументов командной строки.
- Handle — слово заместитель, не добавляющее смысла. В чем именно заключается "обработка"? Отразите это в имени вместо слова Handle.
- Имя 'check' почти всегда можно улучшить. Сообщите в имени, что именно проверяется.
