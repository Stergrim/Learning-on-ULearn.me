# Уровень 8

Время закрепить все полученные на предыдущих уровнях знания!

Исходный код:
```cs
enum ColorCell
{ 
    Black, 
    White
}

class Chessboard 
{
    private ColorCell[,] array;
    private int m_brdSz;
    
    public Chessboard(int n) 
    {
        this.m_brdSz = n;
        this.array = new ColorCell[m_brdSz,m_brdSz];
        //Fill board with black and white colors
        for(var a = 0; a < m_brdSz; a++)
            for(var b = 0; b < m_brdSz; b++) 
            {
                bool isBlack = (a + b) % 2 == 0;
                array[a, b] = isBlack ? ColorCell.Black : ColorCell.White;
            }
    }
}
```

Исправленный код:
```cs
enum CellColor
{ 
    Black,
    White
}

class Chessboard 
{
    private CellColor[,] cellColors;
    private int size;
    
    public Chessboard(int size) 
    {
        this.size = size;
        this.cellColors = new CellColor[size,size];
    
        for(var y = 0; y < size; y++)
            for(var x = 0; x < size; x++) 
            {
                bool isBlack = (y + x) % 2 == 0;
                cellColors[y, x] = isBlack ? CellColor.Black : CellColor.White;
            }
    }
}
```

Объяснения:
- Нарушение правильного для английского языка порядка слов в составных именах — частая ошибка программистов со слабым знанием английского. ColorCell — это цветная клетка, а цвет клетки — CellColor. Не путайте — не вводите в замешательство читающих.
- Имя должно отражать семантику, а не тип переменной. Имена вида s, list, array не дают полезной информации читателю.
- Не используйте закодированные или труднопроизносимые имена. Вам их придется произносить или хотя бы мысленно проговаривать!
- В данном контексте, n может обозначать как размер шахматной доски, так и количество фигур на доске или номер доски. Имя size устранит эту неоднозначность.
- Комментарии повторяющие код не имеют смысла.
- Для обозначения координат вместо i, j и a, b лучше использовать предсказуемые и понятные x, y.
- Избегайте необходимости мысленного декодирования при чтении кода.
