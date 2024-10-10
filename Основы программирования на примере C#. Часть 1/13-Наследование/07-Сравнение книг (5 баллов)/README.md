# Сравнение книг

Вы пишете учетную систему для книжного магазина, и вам необходимо научиться сравнивать между собой записи о книгах, чтобы продавцам было удобно искать нужную.

Каждая книга имеет название `Title` и номер тематического раздела `Theme`.

Реализуйте интерфейс IComparable у класса Book. Логику сравнения восстановите по тестам, которые вы увидите при первом запуске.


Все тесты пройдены, задача сдана:
```cs
class Book : IComparable
{
    public string Title;
    public int Theme;
    
    public int CompareTo(object obj)
    {
        var book = (Book)obj;
        if (book.Theme == Theme) return Title.CompareTo(book.Title);
        else return Theme.CompareTo(book.Theme);
    }
}
```

Вывод программы:
```cs
5 B = 5 B
1 A < 1 C
3 B > 2 B
1 B < 2 A
9 A > 1 Z
```
