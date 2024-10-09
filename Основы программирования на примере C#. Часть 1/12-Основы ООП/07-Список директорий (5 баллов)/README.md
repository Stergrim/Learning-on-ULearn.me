# Список директорий

Так получилось, что вы пишете духовную операционную систему "Оконце, версия 0.99", и модуль к этой системе, отвечающий за составление медиа-альбомов пользователя.

По данному вам списку файлов составьте список всех директорий, которые содержат файлы с расширением .mp3 и .wav.

Каждую директорию нужно вернуть только один раз, порядок значения не имеет.


Все тесты пройдены, задача сдана:
```cs
public static List<DirectoryInfo> GetAlbums(List<FileInfo> files)
{
    return (from file in files where file.Extension is ".mp3" or ".wav"
            group file by file.Directory.FullName into dictionary
            select dictionary.First().Directory).ToList();
}
```

Вывод программы:
```cs
Любо!
```
