# Практика «Структура данных»

Простейший сценарий, когда вам нужна перегрузка методов и реализация интерфейсов - написание небольших структур данных, которые должны быть совместимы с листами, словарями и т.д.

Допустим, вы разрабатываете систему для анализа сообщений в техподдержку, и хотите классифицировать их по имени продукту, типу сообщения и его теме. Соответственно, вам необходим класс, обозначающий категорию сообщений с указанными полями.

Скачайте [проект Inheritance.DataStucture](Inheritance.DataStructure.zip) и создайте класс Category.cs. В этом классе переопределите методы Equals и GetHashCode, реализуйте интерфейс IComparable, упорядочивающий категории сначала по продукту, затем по типу и затем - по теме, а также реализуйте все операторы сравнения. Изучите тесты для того, чтобы понять детали реализации.

Все тесты пройдены, задача сдана:
```cs
namespace Inheritance.DataStructure;

public class Category : IComparable
{
    private string Message { get; }
    private MessageType Type { get; }
    private MessageTopic Topic { get; }
    
    public Category(string message, MessageType type, MessageTopic topic)
    {
        Message = message ?? "";
        Type = type;
        Topic = topic;
    }
    
    public int CompareTo(object? obj)
    {
        if (ReferenceEquals(obj, this)) return 0;
        if (obj is not Category other) return -1;
    
        if (Message != other.Message) return Message.CompareTo(other.Message);
        if (Type != other.Type) return Type.CompareTo(other.Type);
    
        return Topic != other.Topic ? Topic.CompareTo(other.Topic) : 0;
    }
    
    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(obj, this))
            return true;
        if (obj is not Category other)
            return false;
        return Message == other.Message && Type == other.Type && Topic == other.Topic;
    }
    
    public override int GetHashCode()
    {
        Random rnd = new Random();
        return Message.GetHashCode() * rnd.Next(100)
                + Type.GetHashCode() * rnd.Next(100)
                + Topic.GetHashCode() * rnd.Next(100);
    }
    
    public override string ToString() => $"{Message}.{Type}.{Topic}";
    public static bool operator <(Category left, Category right) => left.CompareTo(right) < 0;
    public static bool operator >(Category left, Category right) => left.CompareTo(right) > 0;
    public static bool operator >=(Category left, Category right) => left.CompareTo(right) >= 0;
    public static bool operator <=(Category left, Category right) => left.CompareTo(right) <= 0;
}
```
