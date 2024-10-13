# Практика «Ghosts»

Скачайте проект [ghost](ghost.zip).

Неаккуратная реализация Equals и GetHashCode может приводить к тому, что добавленный в Dictionary или HashSet ключ внезапно исчезает. Чтобы не попадаться на подобные ошибки в будущем, в этом задании предлагается поизучать всевозможные подобные ошибки.

В проекте вам даны несколько классов с уже реализованными GetHashCode и Equals. Вам нужно придумать, как их использовать, чтобы HashSet стал вести себя некорректно.

Изучите тест GhostsTest.cs и в файле GhostsTask.cs создайте класс GhostsTask так, чтобы этот тест проходил.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Text;

namespace hashes
{
    public class GhostsTask : IFactory<Document>, IFactory<Vector>, IFactory<Segment>, 	
                              IFactory<Cat>, IFactory<Robot>, IMagic
    {
        byte[] byteDoc = new byte[] { 1, 2, 3 };
        public Document Create() { return new Document("Title", Encoding.UTF8, byteDoc); }
    
        Vector newVec = new Vector(3, 4);
        Vector IFactory<Vector>.Create() { return newVec; }
    
        Cat catOne = new Cat("Pizic", "Siams", DateTime.Now);
        Cat IFactory<Cat>.Create() { return catOne; }
    
        static Vector start = new Vector(0.2, 0.1);
        static Vector end = new Vector(-1.2, 4.0);
        Segment pathSegmant = new Segment(start, end);
        Segment IFactory<Segment>.Create() { return pathSegmant; }
    
        Robot newRobot = new Robot("42");
        Robot IFactory<Robot>.Create() { Robot.BatteryCapacity = 100; return newRobot; }
    
        public void DoMagic()
        {
            Robot.BatteryCapacity = 50;
            start.Add(start);
            catOne.Rename("Fufil");
            newVec.Add(newVec);
            for (int i = 0; i < byteDoc.Length; i++)
                byteDoc[i] = 4;
        }
    }
}
```
