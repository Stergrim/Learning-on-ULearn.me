# Практика «Монстры»

Продолжайте [в том же проекте](Digger.zip).

**Monster**

Сделайте класс Monster, реализовав ICreature. Его поведение должно быть таким:
- Если на карте нет диггера, монстр стоит на месте.
- Если на карте есть диггер, монстр двигается в его сторону по горизонтали или вертикали. Можете написать поиск кратчайшего пути к диггеру, но это не обязательно.
- Монстр не может ходить сквозь землю или мешки.
- Если после хода монстр и диггер оказались в одной клетке, диггер умирает.
- Если монстр оказывается в клетке с золотом, золото исчезает.
- Мешок может лежать на монстре.
- Падающий на монстра мешок убивает монстра.
- Монстр не должен начинать ходить в клетку, где уже есть другой монстр.
- Если два или более монстров сходили в одну и ту же клетку, они все умирают. Если в этой клетке был диггер — он тоже умирает.


**Место для творчества!**

После того, как вы сдадите все задачи, можете попробовать придумать и добавить ещё какую-нибудь возможность в игру.


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
            else if ("Digger.Gold" == conflictedObject.ToString()) return false;
            else if ("Digger.Sack" == conflictedObject.ToString())
                 { Game.IsOver = true; return true;}
            else if ("Digger.Monster" == conflictedObject.ToString())
                 { Game.IsOver = true; return true; }
            else { Game.IsOver = true; return true; }
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
            { Counter++; return new CreatureCommand() { DeltaX = 0, DeltaY = 1 }; }
            if (Counter == 1)
                if (y + 1 < Game.MapHeight && 
                   (Game.Map[x, y + 1]?.ToString() == "Digger.Player" ||
                    Game.Map[x, y + 1]?.ToString() == "Digger.Monster"))
                    { Counter++; return new CreatureCommand() { DeltaX = 0, DeltaY = 1 }; }
            if (Counter > 1)
                if (y + 1 < Game.MapHeight &&
                   (Game.Map[x, y + 1]?.ToString() == "Digger.Player" ||
                    Game.Map[x, y + 1]?.ToString() == "Digger.Monster"))
                { Counter++; return new CreatureCommand() { DeltaX = 0, DeltaY = 1 }; }
                else
                {
                    Counter = 0; return new CreatureCommand()
                    { DeltaX = 0, DeltaY = 0, TransformTo = new Gold() };
                }
            Counter = 0;
            return new CreatureCommand() { DeltaX = 0, DeltaY = 0 };
        }
    
        public bool DeadInConflict(ICreature conflictedObject)
        {
            if ("Digger.Player" == conflictedObject.ToString()) return false;
            return "Digger.Monster" != conflictedObject.ToString();
        }
    
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
            return "Digger.Monster" == conflictedObject.ToString();
        }
    
        public int GetDrawingPriority() { return 1; }
    
        public string GetImageFileName() { return "Gold.png"; }
    }
    
    public class Monster : ICreature
    {
        static bool IsPlayer = false;
        static int PlayerX = 0;
        static int PlayerY = 0;
    
        public void PlayerIsMap()
        {
            for (var i = 0; i < Game.MapWidth; i++)
                for (var j = 0; j < Game.MapHeight; j++)
                    if (Game.Map[i, j]?.ToString() == "Digger.Player")
                    {
                        PlayerX = i;
                        PlayerY = j;
                        IsPlayer = true;
                    }
        }
    
        public CreatureCommand SearchWay(int x, int y)
        {
            if (x + 1 < Game.MapWidth)
                if (Game.Map[x + 1, y]?.ToString() != "Digger.Monster" &&
                    Game.Map[x + 1, y]?.ToString() != "Digger.Terrain" &&
                    Game.Map[x + 1, y]?.ToString() != "Digger.Sack")
                    if (Math.Abs(PlayerX - x) >= Math.Abs(PlayerX - (x + 1)))
                        return new CreatureCommand() { DeltaX = 1, DeltaY = 0 };
            if (x - 1 >= 0)
                if (Game.Map[x - 1, y]?.ToString() != "Digger.Monster" &&
                    Game.Map[x - 1, y]?.ToString() != "Digger.Terrain" &&
                    Game.Map[x - 1, y]?.ToString() != "Digger.Sack")
                    if (Math.Abs(PlayerX - x) >= Math.Abs(PlayerX - (x - 1)))
                        return new CreatureCommand() { DeltaX = -1, DeltaY = 0 };
            if (y + 1 < Game.MapHeight)
                if (Game.Map[x, y + 1]?.ToString() != "Digger.Monster" &&
                    Game.Map[x, y + 1]?.ToString() != "Digger.Terrain" &&
                    Game.Map[x, y + 1]?.ToString() != "Digger.Sack")
                    if (Math.Abs(PlayerY - y) >= Math.Abs(PlayerY - (y + 1)))
                        return new CreatureCommand() { DeltaX = 0, DeltaY = 1 };
            if (y - 1 >= 0)
                if (Game.Map[x, y - 1]?.ToString() != "Digger.Monster" &&
                    Game.Map[x, y - 1]?.ToString() != "Digger.Terrain" &&
                    Game.Map[x, y - 1]?.ToString() != "Digger.Sack")
                    if (Math.Abs(PlayerY - y) >= Math.Abs(PlayerY - (y - 1)))
                        return new CreatureCommand() { DeltaX = 0, DeltaY = -1 };
            return new CreatureCommand() { DeltaX = 0, DeltaY = 0 };
        }
    
        public CreatureCommand Act(int x, int y)
        {
            PlayerIsMap();
            if (!IsPlayer) { return new CreatureCommand() { DeltaX = 0, DeltaY = 0 }; }
            var creatureCommand = SearchWay(x, y);
            return creatureCommand;
        }
    
        public bool DeadInConflict(ICreature conflictedObject)
        {
            if ("Digger.Gold" == conflictedObject.ToString()) { return false; }
            else if ("Digger.Sack" == conflictedObject.ToString()) { return true; }
            else if ("Digger.Player" == conflictedObject.ToString()) { return false; }
            return "Digger.Monster" == conflictedObject.ToString();
        }
    
        public int GetDrawingPriority() { return 1; }
    
        public string GetImageFileName() { return "Monster.png";}
    
        public class Point { public int X; public int Y; }
    }
}
```

**Проект со всеми внесенными решениями.**
[Digger Edit](Digger_Edit.zip)
