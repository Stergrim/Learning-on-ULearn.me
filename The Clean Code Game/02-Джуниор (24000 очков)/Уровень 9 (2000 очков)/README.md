# Уровень 9

Переменные и классы — это сущности, а методы — действия. Имейте это в виду!

Исходный код:
```cs
public void Initialization(int boardSize)
{
    Log("Board initialization...");
    this.piecesCount = 0;
    this.board = Board(boardSize, boardSize);
    Log("Board initialization finished");
}
```

Исправленный код:
```cs
public void InitializeBoard(int boardSize)
{
    Log("Board initialization...");
    this.piecesCount = 0;
    this.board = CreateBoard(boardSize, boardSize);
    Log("Board initialization finished");
}
```

Объяснения:
- Методы — это действия, называйте их глаголами или глагольными фразами.
- Методы — это действия, называйте их глаголами или глагольными фразами.
