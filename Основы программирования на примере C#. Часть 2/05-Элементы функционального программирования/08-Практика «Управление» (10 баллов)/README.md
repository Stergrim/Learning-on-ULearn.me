# Практика «Управление»

Продолжайте в том же проекте [rocket](rocket.zip).

В этой задаче в классе ControlTask нужно реализовать метод управления ракетой.

В результате ракета должна достигать цели в уровнях Zero, Heavy, Up и WhiteHole.

Все тесты пройдены, задача сдана:
```cs
using System;

namespace func_rocket
{
    public class ControlTask
    {
        public static Turn ControlRocket(Rocket rocket, Vector target)
        {
            double targetAngle = Math.Atan2((target - rocket.Location).Y,
                                            (target - rocket.Location).X);
            double rocketDirectionAngle = Math.Atan2(Math.Sin(rocket.Direction),
                                                     Math.Cos(rocket.Direction));
            double velocityAngle = rocket.Velocity.Angle;
            double difBetweenTargetAndVelocity = DifferenceBetweenAngles(targetAngle,
                                                                         velocityAngle);
            double idealRocketDirectionAngle;
            if (Math.Abs(difBetweenTargetAndVelocity) < Math.PI / 4)
                idealRocketDirectionAngle = targetAngle + difBetweenTargetAndVelocity;
            else
                idealRocketDirectionAngle = targetAngle + Math.PI / 8;
            idealRocketDirectionAngle = Math.Atan2(Math.Sin(idealRocketDirectionAngle),
                                                   Math.Cos(idealRocketDirectionAngle));
            double difBetweenIdealAndRealRocketDir =
                DifferenceBetweenAngles(idealRocketDirectionAngle, rocketDirectionAngle);
            return difBetweenIdealAndRealRocketDir > 0 ? Turn.Right : 
                   difBetweenIdealAndRealRocketDir < 0 ? Turn.Left : Turn.None;
        }
    
        static double DifferenceBetweenAngles(double first, double second) =>
            (((first - second) + Math.PI) % (2 * Math.PI)) - Math.PI;
    }
}
```

**Проект со всеми внесенными решениями.**
[rocket Edit](rocket_Edit.zip)
