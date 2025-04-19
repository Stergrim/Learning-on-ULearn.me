# Практика «HoMM»

В компьютерной игре, персонаж игрока взаимодействует с различными объектами на карте. Есть всего три способа взаимодействовать:
- Сражаться с армией.
- Собирать сокровища.
- Присваивать объект себе.

А вот различных видов объектов на карте уже 5, а будет ещё больше. Скачайте [проект Inheritance.MapObjects](Inheritance.MapObjects.zip), откройте файл Task.cs и изучите, как это реализовано сейчас.

Проблема в том, что метод Interaction.Make содержит много почти повторяющегося кода, нарушая принцип Dont Repeat Yourself. Кроме того, он будет расти с появлением новых объектов в игре.

Выделите все поля, необходимую для каждого взаимодействия, в свой интерфейс. Отрефакторьте программу, избавившись от повторяющихся участков кода в Interaction.Make.

В итоговом решении Interaction.Make должен работать только с интерфейсами, и не должен содержать упоминаний конкретных классов.

Все тесты пройдены, задача сдана:
```cs
namespace Inheritance.MapObjects;

public interface IOwner
{
    public int Owner { get; set; }
}

public interface IArmy
{
    public Army Army { get; set; }
}

public interface ITreasure
{
    public Treasure Treasure { get; set; }
}

public class Dwelling : IOwner
{
    public int Owner { get; set; }
}

public class Mine : IOwner, IArmy, ITreasure
{
    public int Owner { get; set; }
    public Army Army { get; set; }
    public Treasure Treasure { get; set; }
}

public class Creeps : IArmy, ITreasure
{
    public Army Army { get; set; }
    public Treasure Treasure { get; set; }
}

public class Wolves : IArmy
{
    public Army Army { get; set; }
}

public class ResourcePile : ITreasure
{
    public Treasure Treasure { get; set; }
}

public static class Interaction
{
    public static void Make(Player player, object mapObject)
    {
        if (mapObject is IArmy armyObj && !player.CanBeat(armyObj.Army))
        {
            player.Die();
            return;
        }
        if (mapObject is IOwner ownerObj) ownerObj.Owner = player.Id;
        if (mapObject is ITreasure treasureObj) player.Consume(treasureObj.Treasure);
    }
}
```
