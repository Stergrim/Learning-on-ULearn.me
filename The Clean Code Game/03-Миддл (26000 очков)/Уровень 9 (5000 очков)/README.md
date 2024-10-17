# Уровень 9

Последний уровень! По законам жанра тут должно быть много кода!

Исходный код:
```cs
///<param name="path">
/// Path to file or directory with map description. 
/// If path is a path to directory, file default.map is used.
///</param>
GameMap LoadMap(string path)
{
    var Filename = Directory.Exists(path) 
        ? Path.Combine(path, "default.map") 
        : path;
    var lines = File.ReadAllLines(Filename);
    var height = lines.Length;
    var width = lines[0].Length;
    //Initialize map
    var map = new GameMap(width, height) 
                {
                    Score = 0,
                    HeroLifesCount = 3,
                    Time = 0,
                }; 
    for(var y=0; y<height; y++)
        for(var x=0; x<lines[0].Length; x++)
        {
            //============Select object to put in (x, y) cell;
            GameObject obj = null;
            switch (lines[y][x])
            {
                case 'H': 
                    obj = new Hero();
                    break;
                case '#':
                    obj = new Wall();
                    break;
                case 'M':
                    obj = new Monster();
                    break;
            }
            //============Put created object on map
            map.Put(x, y, obj);
        }
    return map;
}
```

Исправленный код:
```cs
///<param name="path">
/// Path to file or directory with map description. 
/// If path is a path to directory, file default.map is used.
///</param>
GameMap LoadMap(string path)
{
    var filename = Directory.Exists(path) 
        ? Path.Combine(path, "default.map") 
        : path;
    var lines = File.ReadAllLines(filename);
    var height = lines.Length;
    var width = lines[0].Length;
    var map = new GameMap(width, height);
    for(var y=0; y<height; y++)
        for(var x=0; x<width; x++)
        {
            var obj = CreateGameObjectFromChar(lines[y][x]);
            map.Put(x, y, obj);
        }
    return map;
}
```

Объяснения:
- Логику инициализации полей карты лучше переместить в конструктор класса карты.
- В C# локальные переменные принято называть с маленькой буквы.
- При виде комментария, разделяющего метод на смысловые части, стоит вынести эти смысловые части в отдельные методы.
- Устраняйте дублирование. Это делает код понятнее и надежнее.
- Разделительные комментарии вроде такого часто показывают, что программист поленился выделить вспомогательный метод.
