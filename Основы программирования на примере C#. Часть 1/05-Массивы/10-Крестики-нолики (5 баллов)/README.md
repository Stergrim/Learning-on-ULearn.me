# Крестики-нолики

Вам с Васей наконец-то надоело тренироваться на маленьких программках и вы взялись за настоящее дело! Вы решили написать игру [крестики-нолики](https://ru.wikipedia.org/wiki/%D0%9A%D1%80%D0%B5%D1%81%D1%82%D0%B8%D0%BA%D0%B8-%D0%BD%D0%BE%D0%BB%D0%B8%D0%BA%D0%B8)!

Начать было решено с подпрограммы, определяющей не закончилась ли уже игра, а если закончилась, то кто выиграл.

Методу `GetGameResult` передается поле, представленное массивом 3х3 из `enum Markers`. Вам надо вернуть победителя `CrossWin` или `CircleWin`, если таковой имеется или `Draw`, если выигрышной последовательности нет ни у одного, либо есть у обоих.

Постарайтесь придумать красивое, понятное решение.

Подумайте, как разбить задачу на более простые подзадачи. Попытайтесь выделить один или два вспомогательных метода.

Если вы в затруднении, воспользуйтесь подсказками (кнопка `Get hint`)

```cs
public enum Mark
{
    Empty,
    Cross,
    Circle
}

public enum GameResult
{
    CrossWin,
    CircleWin,
    Draw
}

public static void Main()
{
    Run("XXX OO. ...");
    Run("OXO XO. .XO");
    Run("OXO XOX OX.");
    Run("XOX OXO OXO");
    Run("... ... ...");
    Run("XXX OOO ...");
    Run("XOO XOO XX.");
    Run(".O. XO. XOX");
}

private static void Run(string description)
{
    Console.WriteLine(description.Replace(" ", Environment.NewLine));
    Console.WriteLine(GetGameResult(CreateFromString(description)));
    Console.WriteLine();
}

private static Mark[,] CreateFromString(string str)
{
    var field = str.Split(' ');
    var ans = new Mark[3, 3];
    for (int x = 0; x < field.Length; x++)
        for (var y = 0; y < field.Length; y++)
            ans[x, y] = field[x][y] == 'X' ? Mark.Cross : (field[x][y] == 'O' ? Mark.Circle : Mark.Empty);
    return ans;
}
```

Все тесты пройдены, задача сдана:
```cs
public static GameResult GetGameResult(Mark[,] field)
{
    bool a = false;
    bool b = false;
    
    if ((CircleCrossHorizont(field, 0)) ||
        (CircleCrossVetical(field, 0)) ||
        (CircleCrossDiagonal(field, 0))) a = true;
    
    if ((CircleCrossHorizont(field, 1)) ||
        (CircleCrossVetical(field, 1)) ||
        (CircleCrossDiagonal(field, 1))) b = true;
    
    if ((a && b)||(!a && !b)) return GameResult.Draw;
    else if (a) return GameResult.CircleWin;
    else return GameResult.CrossWin;
}

public static bool CircleCrossHorizont (Mark[,] field, int a)
{
    int[] subArray;
    
    if (a == 0) subArray = new int[3] {2,2,2};
    else subArray = new int[3] {1,1,1};
    
    int b = 0;
    bool c = false;
    
    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 3; j++)
            if (subArray[i] == (int)field[i,j]) b++;
    
        if (b == 3)
        {
            c = true;
            break;
        }
        else b = 0;
    }
    return c;
}

public static bool CircleCrossVetical (Mark[,] field, int a)
{
    int[] subArray;
    
    if (a == 0) subArray = new int[3] {2,2,2};
    else subArray = new int[3] {1,1,1};
    
    int b = 0;
    bool c = false;
    
    for (int i = 0; i < 3; i++)
    {
        for (int j = 0; j < 3; j++)
            if (subArray[i] == (int)field[j,i]) b++;
    
        if (b == 3)
        {
            c = true;
            break;
        }
        else b = 0;
    }
    return c;
}

public static bool CircleCrossDiagonal (Mark[,] field, int a)
{
    int[] subArray;
    
    if (a == 0) subArray = new int[3] {2,2,2};
    else subArray = new int[3] {1,1,1};
    
    int b = 0;
    bool c = false;
    
    for (int i = 0; i < 3; i++)
        if (subArray[i] == (int)field[i,i]) b++;
    if (b == 3) c = true;
    else b = 0;
    
    for (int i = 0; i < 3; i++)
        if (subArray[i] == (int)field[i,2-i])b++;
    if (b == 3) c = true;
    
    return c;
}
```

Вывод программы:
```cs
XXX
OO.
...
CrossWin

OXO
XO.
.XO
CircleWin

OXO
XOX
OX.
CircleWin

XOX
OXO
OXO
Draw

...
...
...
Draw

XXX
OOO
...
Draw

XOO
XOO
XX.
CrossWin

.O.
XO.
XOX
CircleWin
```