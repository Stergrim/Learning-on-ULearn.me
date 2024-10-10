# Практика «Решение манипулятора»

**MoveManipulatorTo**

В [в том же проекте](manipulator.zip) реализуйте метод MoveManipulatorTo в классе ManipulatorTask.

Он должен возвращать массив углов `new[] {shoulder, elbow, wrist}`, необходимых для приведения эффектора манипулятора в точку `(x, y)` относительно крепления манипулятора к столу, и с углом между последним суставом и горизонталью равному `alpha` в радианах.

Если это невозможно, то возвращайте массив из трех `double.NaN`.

Сверяйтесь с чертежом! Не перепутайте углы и их направления!

<p float="left">
<img src="manipulator.png" width="600" />
</p>

**Рандомизированный тест**

В том же файле напишите вместо нескольких тестов один рандомизированный.

Он должен в цикле генерировать случайные значения X, Y, Angle, вызывать MoveManipulatorTo, по углам вычислять позицию эффектора и проверять, что она совпадает с требуемой.

Среди сгенерированных X, Y, Angle иногда будут попадаться нерешаемые. Например, если требуемая точка (X, Y) расположена слишком далеко, то метод вернет NaN-ы. Однако несложно определить, для каких точек задача имеет решение, а для каких нет. Подсказку ищите в коде задачи о визуализации. Ваш рандомизированный тест должен проверять, что NaN возвращается только тогда, когда решения действительно нет.

**Подсказка про геометрию**

Для решения этой задачи достаточно школьных знаний геометрии, однако поскольку наш курс посвящён программированию, и учитывая нелюбовь студентов к этой прекрасной области знаний, ниже дана схема геометрического решения этой задачи.
1. Проще всего найти угол wrist. Сделать это можно по формуле wrist = –alpha – shoulder – elbow.
2. Найдите координаты сустава Wrist по координатам (x, y) и углу alpha — они понадобятся в следующих шагах.
3. Найдите угол elbow с помощью метода GetABAngle в треугольнике (shoulder, elbow, wrist), в котором все три стороны теперь известны.
4. Найдите угол shoulder, как сумму двух углов:
   - угла (wrist, shoulder, elbow), который опять же можно найти с помощью метода GetABAngle.
   - и угла между лучом (shoulder, wrist) и осью OX. Его можно найти с помощью функции Math.Atan2 и координат сустава wrist.

Стоит заметить, что это не единственный способ решить данную задачу. Вы можете изобрести свой собственный.

**Обратно к визуализации**

После решения этой задачи, если вы правильно реализовали визуализацию во второй практике, кончик манипулятора должен начать следовать за курсором мыши.

Пока вы ведете мышь внутри зеленой области, кончик манипулятора касается мыши. Если передвинуть мышь на красную зону, манипулятор не может достать до мыши и замирает в последнем положении.

Ближайший к мыши отрезок манипулятора при движении за мышью всегда смотри в одну сторону. Его направление меняется при прокрутке колесика мыши.

Ulearn не тестирует визуализацию. Запустите программу, проверьте, что визуализация ведет себя правильно, и поправьте ее, если что-то не так.


Все тесты пройдены, задача сдана:
```cs
using System;
using NUnit.Framework;
using static Manipulation.Manipulator;

namespace Manipulation
{
    public static class ManipulatorTask
    {
        public static double[] MoveManipulatorTo(double x, double y, double alpha)
        {
            double xPalm = x - Palm * (double)Math.Cos(alpha);
            double yPalm = y + Palm * (double)Math.Sin(alpha);
            double c = Math.Sqrt(xPalm * xPalm + yPalm * yPalm);
            double elbow = TriangleTask.GetABAngle(UpperArm, Forearm, c);
            double shoulderPath1 = TriangleTask.GetABAngle(UpperArm, c, Forearm);
            double shoulderPath2 = Math.Atan2(yPalm, xPalm);
            double shoulder = shoulderPath1 + shoulderPath2;
            double wrist = -alpha - shoulder - elbow;
            if (shoulder == double.NaN || elbow == double.NaN)
                return new[] { double.NaN, double.NaN, double.NaN };
            return new[] { shoulder, elbow, wrist };
        }
    }
   
    [TestFixture]
    public class ManipulatorTask_Tests
    {
        [Test]
        public void TestMoveManipulatorTo()
        {
            var trialsCount = 100;
            var angles = ManipulatorTask.MoveManipulatorTo(0, 0, 0);
            var positions = AnglesToCoordinatesTask.GetJointPositions(0, 0, 0);
            Random rnd = new Random();
            for (int i = 0; i < trialsCount; i++)
            {
                var x = rnd.Next(-1000, 1000);
                var y = rnd.Next(-1000, 1000);
                var alpha = rnd.NextDouble() * Math.PI * 2;
        
                angles = ManipulatorTask.MoveManipulatorTo(x, y, alpha);
                positions = AnglesToCoordinatesTask.GetJointPositions(angles[0], angles[1], angles[2]);
                if (angles[0].Equals(double.NaN))
                {
                    Assert.AreEqual(double.NaN, positions[2].X);
                    Assert.AreEqual(double.NaN, positions[2].Y);
                }
                else
                {
                    Assert.AreEqual(x, positions[2].X, 1e-3);
                    Assert.AreEqual(y, positions[2].Y, 1e-3);
                }
            }
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[manipulator Edit](manipulator_Edit.zip)
