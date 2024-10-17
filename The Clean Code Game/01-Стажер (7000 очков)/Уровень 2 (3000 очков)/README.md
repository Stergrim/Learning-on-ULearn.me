# Уровень 2

Качественно выбранные имена — то что делает ваш код понятнее!

Исходный код:
```cs
List<Position> GetThem(List<Cell> theBigList)
{
    var list1 = new List<Position>();
    foreach (var cell in theBigList)
    {
        if (cell.IsEmpty) 
            list1.Add(cell.Position);
    }
    return list1;
}
```

Исправленный код:
```cs
List<Position> GetEmptyPositions(List<Cell> allCells)
{
    var emptyPositions = new List<Position>();
    foreach (var cell in allCells)
    {
        if (cell.IsEmpty) 
            emptyPositions.Add(cell.Position);
    }
    return emptyPositions;
}
```

Объяснения:
- Не используйте местоимения в именах. Это совсем не добавляет понятности.
- Имя должно отражать семантику, а не тип переменной. Имена вида s, list, array не дают полезной информации читателю.
- Имя должно отражать семантику, а не тип переменной. Имена вида s, list, array не дают полезной информации читателю.
