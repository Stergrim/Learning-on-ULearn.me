# Практика «Таблицы»

В анализе данных бывают очень полезны таблицы. Например, строки могут соответствовать датам, столбцы — департаментам, а в ячейках может хранится выручка департамента за контракты на соответствующую дату.

Сделайте такую таблицу, которая бы:
1. Индексировалась величинами типов, указанных при создании таблицы.
2. Имела бы две возможности для индексирования:
3. С автоматическим созданием нужных строк и столбцов «на лету» при обращении к таблице по соответствующим индексам;
4. Которая бы требовала создания столбцов и строк заранее и выбрасывала исключение при доступе к несуществующим столбцам или строкам.

Скачайте [проект Generics.Tables](Generics.Tables.zip) и изучите тесты, которые должна проходить ваша таблица, чтобы понять детали задания.

Дополнительно подумайте над тем, как не хранить лишней информации в таблице: если в данную дату в данном департаменте не было заключено контрактов, то значение будет 0, и хранить сотни нулей, очевидно, не нужно.

Все тесты пройдены, задача сдана:
```cs
namespace Generics.Tables;

public class Table<TRow, TCol, TVal>
{
    public Dictionary<TRow, Dictionary<TCol, TVal>> Data { get; private set; }
    public HashSet<TRow> Rows { get; private set; }
    public HashSet<TCol> Columns { get; private set; }
    
    public Table()
    {
        Data = new Dictionary<TRow, Dictionary<TCol, TVal>>();
        Rows = new HashSet<TRow>();
        Columns = new HashSet<TCol>();
    }
    
    public Table<TRow, TCol, TVal> Open => this;
    public TableExist<TRow, TCol, TVal> Existed => new TableExist<TRow, TCol, TVal>(this);
    public bool Exists(TRow row, TCol col) => Rows.Contains(row) && Columns.Contains(col);
    
    public TVal this[TRow row, TCol col]
    {
        get
        {
            if (!Data.ContainsKey(row) || !Data[row].ContainsKey(col)) return default(TVal);
            return Data[row][col];
        }
        set
        {
            if (value.GetType() != typeof(TVal)) throw new ArgumentException();
            if (!Rows.Contains(row)) AddRow(row);
            if (!Columns.Contains(col)) AddColumn(col);
            Data[row].Add(col, value);
        }
    }
    
    public void AddRow(TRow row)
    {
        Rows.Add(row);
        if (!Data.ContainsKey(row)) Data.Add(row, new Dictionary<TCol, TVal>());
    }
    
    public void AddColumn(TCol col) => Columns.Add(col);
}

public class TableExist<TRow, TCol, TValue>
{
    private Table<TRow, TCol, TValue> table;
    public TableExist(Table<TRow, TCol, TValue> table) => this.table = table;
    
    public TValue this[TRow row, TCol col]
    {
        get
        {
            if (!table.Exists(row, col)) throw new ArgumentException();
            return table[row, col];
        }
        set
        {
            if (!table.Exists(row, col) || value.GetType() != typeof(TValue)) throw new ArgumentException();
            table[row, col] = value;
        }
    }
}
```
