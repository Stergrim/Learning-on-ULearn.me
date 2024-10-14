# Практика «Уровни»

Продолжайте в том же проекте [rocket](rocket.zip).

В этой задаче в классе LevelsTask нужно добавить в игру ещё несколько уровней.

В результате должны быть следующие уровни:
- Zero. Нулевая гравитация. Начальное положение ракеты и положение цели см. в коде. Если не указано иное, на других уровнях начальное положение цели и ракеты такое же.
- Heavy. Постоянная гравитация 0.9, направленная вниз.
- Up. Гравитация направлена вверх и значение её модуля вычисляется по формуле 300 / (d + 300.0), где d — это расстояние от нижнего края пространства. Цель должна иметь координаты (X:700, Y:500)
- WhiteHole. Гравитация направлена от цели. Модуль вектора гравитации вычисляется по формуле 140*d / (d²+1), где d — расстояние до цели.
- BlackHole. В середине отрезка, соединяющего начальное положение ракеты и цель, находится аномалия. Гравитация направлена к аномалии. Модуль вектора гравитации равен 300*d / (d²+1), где d — расстояние до аномалии.
- BlackAndWhite. Гравитация равна среднему арифметическому гравитаций на уровнях WhiteHole и BlackHole.

Все уровни должны удовлетворять таким дополнительным условиям:
- Расстояние от начального положения ракеты до цели должно быть в пределах от 450 до 550.
- Угол между направлением на цель и начальным направлением ракеты должен быть не менее PI/4.

Постарайтесь избежать дублирования кода в этой задаче.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;

namespace func_rocket
{
    public class LevelsTask
    {
        static readonly Physics standardPhysics = new Physics();
    
        public static IEnumerable<Level> CreateLevels()
        {
            yield return new Level(
                "Zero", new Rocket(new Vector(200, 500), Vector.Zero, -0.5 * Math.PI),
                new Vector(600, 200), (size, v) => Vector.Zero, standardPhysics
            );
            yield return new Level(
                "Heavy", new Rocket(new Vector(200, 500), Vector.Zero, -0.5 * Math.PI),
                new Vector(600, 200), (size, v) => new Vector(0,0.9), standardPhysics
            );
            yield return new Level(
                "Up", new Rocket(new Vector(200, 500), Vector.Zero, -0.5 * Math.PI),
                new Vector(700, 500), (size, v) => new Vector(0, -300/(size.Y - v.Y + 300.0)), 
                standardPhysics
            );
            var levelWhiteHole = CreateLevelWhiteHole();
            yield return levelWhiteHole;
            var levelBlackHole = CreateLevelBlackHole();
            yield return levelBlackHole;
            yield return new Level(
                "BlackAndWhite", new Rocket(new Vector(200, 500), Vector.Zero, -0.5 * Math.PI),
                new Vector(600, 200), (size, v) => (levelBlackHole.Gravity(size, v) +
                                                    levelWhiteHole.Gravity(size, v))/2,
                standardPhysics);
        }
        
        public static Level CreateLevelBlackHole()
        {
            var targetBlackHole = new Vector(600, 200);
            var vectorLocationStart = new Vector(200, 500);
            var anomaly = (vectorLocationStart + targetBlackHole) / 2;
            var levelBlackHole = new Level(
                "BlackHole", new Rocket(vectorLocationStart, Vector.Zero, -0.5 * Math.PI),
                targetBlackHole,
                (size, v) =>
                {
                    double d = Math.Sqrt((v.X - anomaly.X) * (v.X - anomaly.X) +
                                         (v.Y - anomaly.Y) * (v.Y - anomaly.Y));
                    double k = 300 * d / (d * d + 1);
                    return new Vector(anomaly.X - v.X, anomaly.Y - v.Y).Normalize() * k;
                },
                standardPhysics);
            return levelBlackHole;
        }
    
        public static Level CreateLevelWhiteHole()
        {
            var targetWhiteHole = new Vector(600, 200);
            var levelWhiteHole = new Level(
                "WhiteHole", new Rocket(new Vector(200, 500), Vector.Zero, -0.5 * Math.PI),
                targetWhiteHole,
                (size, v) =>
                {
                    double d = Math.Sqrt((v.X - targetWhiteHole.X) *
                                         (v.X - targetWhiteHole.X) +
                                         (v.Y - targetWhiteHole.Y) * (v.Y - targetWhiteHole.Y));
                    double k = 140 * d / (d * d + 1);
                    return new Vector(v.X - targetWhiteHole.X, v.Y - 
                                      targetWhiteHole.Y).Normalize() * k;
                },
                standardPhysics);
            return levelWhiteHole;
        }
    }
}
```
