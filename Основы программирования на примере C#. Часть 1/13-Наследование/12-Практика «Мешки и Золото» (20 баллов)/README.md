# Практика «Мешки и Золото»

Продолжайте [в том же проекте](Digger.zip).

Пора добавить мешки с золотом и само золото!

**Sack**

Сделайте класс Sack, реализовав ICreature. Это будет мешок с золотом.
- Мешок может лежать на любой другой сущности (диггер, земля, мешок, золото, край карты).
- Если под мешком находится пустое место, он начинает падать.
- Если мешок падает на диггера, диггер умирает, а мешок продолжает падать, пока не приземлится на землю, другой мешок, золото или край карты.
- Диггер не может подобрать мешок, толкнуть его или пройти по нему.

Если мешок падает, а диггер находится непосредственно под ним и идет вверх, они могут "разминуться", и диггер окажется над мешком. Это поведение непросто исправить в существующей упрощенной архитектуре, поэтому считайте его нормальным.

**Gold**

Сделайте класс Gold, реализовав ICreature.
- Мешок превращается в золото, если он падал дольше одной клетки игрового поля и приземлился на землю, на другой мешок, на золото или на край карты.
- Мешок не превращается в золото, а остаётся мешком, если он падал ровно одну клетку.
- Золото никогда не падает.
- Когда диггер собирает золото, ему начисляется 10 очков (через Game.Scores).


Все тесты пройдены, задача сдана:
```cs
using System;
using Avalonia.Input;
using Digger.Architecture;

namespace Digger
{
    public class Terrain : ICreature
    {
        public CreatureCommand Act(int x, int y) { return new CreatureCommand(); }
    
        public bool DeadInConflict(ICreature conflictedObject)
        { return "Digger.Player" == conflictedObject.ToString(); }
    
        public int GetDrawingPriority() { return 1; }
    
        public string GetImageFileName() { return "Terrain.png"; }
    }
    
    public class Player : ICreature
    {
        public CreatureCommand Act(int x, int y)
        {
            var creatureCommand = new CreatureCommand
            {
                DeltaX = x,
                DeltaY = y
            };
    
            if (Game.KeyPressed == Key.Right) creatureCommand.DeltaX++;
            if (Game.KeyPressed == Key.Left) creatureCommand.DeltaX--;
            if (Game.KeyPressed == Key.Up) creatureCommand.DeltaY--;
            if (Game.KeyPressed == Key.Down) creatureCommand.DeltaY++;
    
            if ((creatureCommand.DeltaX < 0) ||
                (creatureCommand.DeltaX >= Game.MapWidth)) creatureCommand.DeltaX = x;
            if ((creatureCommand.DeltaY < 0) ||
                (creatureCommand.DeltaY >= Game.MapHeight)) creatureCommand.DeltaY = y;
    
            if (Game.Map[creatureCommand.DeltaX,
                creatureCommand.DeltaY]?.ToString() == "Digger.Sack")
                { creatureCommand.DeltaX = x; creatureCommand.DeltaY = y; }
            creatureCommand.DeltaX -= x;
            creatureCommand.DeltaY -= y;
    
            return creatureCommand;
        }
    
        public bool DeadInConflict(ICreature conflictedObject)
        {
            if ("Digger.Terrain" == conflictedObject.ToString()) return false;
            if ("Digger.Gold" == conflictedObject.ToString()) return false;
            return "Digger.Sack" == conflictedObject.ToString();
        }
    
        public int GetDrawingPriority() { return 1; }
    
        public string GetImageFileName() { return "Digger.png"; }
    }
    
    public class Sack : ICreature
    {
        public int Counter = 0;
    
        public CreatureCommand Act(int x, int y)
        {
            if (y + 1 < Game.MapHeight && Game.Map[x, y + 1]?.ToString() == null)
            {
                Counter++;
                return new CreatureCommand() { DeltaX = 0, DeltaY = 1 };
            }
    
            if (Counter == 1)
                if (y + 1 < Game.MapHeight && Game.Map[x, y + 1]?.ToString() == "Digger.Player")
                {
                    Counter++;
                    return new CreatureCommand() { DeltaX = 0, DeltaY = 1 };
                }
    
            if (Counter > 1)
                if (y + 1 < Game.MapHeight && Game.Map[x, y + 1]?.ToString() == "Digger.Player")
                {
                    Counter++;
                    return new CreatureCommand() { DeltaX = 0, DeltaY = 1 };
                }
                else
                {
                    Counter = 0;
                    return new CreatureCommand()
                    { DeltaX = 0, DeltaY = 0, TransformTo = new Gold() };
                }
    
            Counter = 0;
            return new CreatureCommand() { DeltaX = 0, DeltaY = 0 };
        }
    
        public bool DeadInConflict(ICreature conflictedObject)
        { return !conflictedObject.ToString().Equals("Digger.Player"); }
    
        public int GetDrawingPriority() { return 1; }
    
        public string GetImageFileName() { return "Sack.png"; }
    }
    
    public class Gold : ICreature
    {
        public CreatureCommand Act(int x, int y) { return new CreatureCommand(); }
    
        public bool DeadInConflict(ICreature conflictedObject)
        {
            if ("Digger.Player" == conflictedObject.ToString())
            { Game.Scores += 10; return true; }
            else return false;
        }
    
        public int GetDrawingPriority() { return 1; }
    
        public string GetImageFileName() { return "Gold.png"; }
    }
}
```
