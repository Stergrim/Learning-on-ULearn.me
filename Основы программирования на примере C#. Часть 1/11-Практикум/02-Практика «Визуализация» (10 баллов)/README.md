# Практика «Визуализация»

В [в том же проекте](manipulator.zip) доработайте файл VisualizerTask.cs, чтобы заработала визуализация.

1. В методе KeyDown сделайте, чтобы манипулятор реагировал на клавиши QASW таким образом:
   - по Q увеличивает угол Shoulder на небольшую величину, а по A — уменьшает;
   - по W увеличивает угол Elbow на небольшую величину, а по S — уменьшает;
   - при любых изменениях пересчитывает Wrist по формуле Wrist = - Alpha - Shoulder - Elbow;


2. В методе MouseMove менял бы X и Y в соответствии со значением из события — координатами мыши относительно окна. Имейте в виду, что ось Y в окне направлена вниз, а в математике — вверх. Поэтому координаты мыши нужно преобразовывать из оконной системы координат в логическую, а при отрисовке наоборот. X и Y в итоге должны хранить логические координаты указателя мыши, относительно shoulderPos — координат единственного неподвижного сустава. Преобразовать оконные координаты в логические и наоборот можно с помощью пары готовых функций ConvertMathToWindow и ConvertWindowToMath.


3. В методе MouseWheel добавить обработку прокрутки колеса мыши. Оно должно менять Alpha.


4. В методе UpdateManipulator вызвать ManipulatorTask.MoveManipulatorTo и обновить значения Shoulder, Elbow и Wrist (это понадобится в последней задаче). UpdateManipulator нужно вызывать после каждого изменения X, Y или Alpha, то есть в методах MouseMove и MouseWheel.


5. В методе DrawManipulator допишите рисование манипулятора. Нарисуйте каждый сегмент манипулятора отрезком прямой, а каждый сустав — окружностью. Координаты сустава получите методом AnglesToCoordinatesTask.GetJointPositions. Не забудьте преобразовать логические координаты в оконные.

При запуске проекта в результате вы сможете управлять углами сгиба суставов манипулятора с помощью клавиатуры. А после выполнения следующих заданий, манипулятор должен будет следовать за курсором мыши.


Все тесты пройдены, задача сдана:
```cs
using System;
using Avalonia;
using Avalonia.Input;
using Avalonia.Media;

namespace Manipulation
{
    public static class VisualizerTask
    {
        public const double DELTA = Math.PI / 180;
        public const float JOINT_RADIUS = 5;
        
        public static double X = 220;
        public static double Y = -100;
        public static double Alpha = 0.05;
        public static double Wrist = 2 * Math.PI / 3;
        public static double Elbow = 3 * Math.PI / 4;
        public static double Shoulder = Math.PI / 2;
        
        public static Brush UnreachableAreaBrush = new SolidColorBrush(
                                                   Color.FromArgb(255, 255, 230, 230));
        public static Brush ReachableAreaBrush = new SolidColorBrush(
                                                 Color.FromArgb(255, 230, 255, 230));
        public static Pen ManipulatorPen = new Pen(Brushes.Black, 3);
        public static Brush JointBrush = new SolidColorBrush(Colors.Gray);
   
        public static void KeyDown(Visual display, KeyEventArgs key)
        {
            switch (key.Key)
            {
                case Key.Q:
                    Shoulder += DELTA;
                    break;
                case Key.A:
                    Shoulder -= DELTA;
                    break;
                case Key.W:
                    Elbow += DELTA;
                    break;
                case Key.S:
                    Elbow -= DELTA;
                    break;
                default:
                    return;
            }
            Wrist = -Alpha - Shoulder - Elbow;
            display.InvalidateVisual();
        }
        
        public static void MouseMove(Visual display, PointerEventArgs e)
        {
            var mathPoint = ConvertWindowToMath(new Point(e.GetPosition(display).X, 
                                                          e.GetPosition(display).Y),
                                                GetShoulderPos(display));
            X = mathPoint.X;
            Y = mathPoint.Y;
            UpdateManipulator();
            display.InvalidateVisual();
        }
        
        public static void MouseWheel(Visual display, PointerWheelEventArgs e)
        {
            Alpha += DELTA * e.Delta.Y;
            UpdateManipulator();
            display.InvalidateVisual();
        }
        
        public static void UpdateManipulator()
        {
            var angles = ManipulatorTask.MoveManipulatorTo(X, Y, Alpha);
            foreach (var angle in angles)
                if (Double.IsNaN(angle)) return;
            Shoulder = angles[0];
            Elbow = angles[1];
            Wrist = angles[2];
        }
        
        public static void DrawManipulator(DrawingContext context, Point shoulderPos)
        {
            var joints = AnglesToCoordinatesTask.GetJointPositions(Shoulder, Elbow, Wrist);
            DrawReachableZone(context, ReachableAreaBrush, UnreachableAreaBrush, shoulderPos, joints);
        
            var formattedText = new FormattedText($"X={X:0}, Y={Y:0}, Alpha={Alpha:0.00}",
                System.Globalization.CultureInfo.InvariantCulture,
                FlowDirection.RightToLeft,
                Typeface.Default,
                18,
                Brushes.Black);
            context.DrawText(formattedText, new Point(10, 10));
        
            var points = new Point[joints.Length + 1];
            points[0] = ConvertMathToWindow(new Point(0, 0), shoulderPos);
            for (int i = 0; i < joints.Length; i++)
            {
                points[i + 1] = ConvertMathToWindow(joints[i], shoulderPos);
                context.DrawLine(ManipulatorPen, points[i], points[i + 1]);
            }
            for (int i = 0; i <= joints.Length; i++)
                context.DrawEllipse(JointBrush, null,
                    points[i], 2 * JOINT_RADIUS, 2 * JOINT_RADIUS);
        }
        
        private static void DrawReachableZone(
            DrawingContext context,
            Brush reachableBrush,
            Brush unreachableBrush,
            Point shoulderPos,
            Point[] joints)
        {
            var rmin = Math.Abs(Manipulator.UpperArm - Manipulator.Forearm);
            var rmax = Manipulator.UpperArm + Manipulator.Forearm;
            var mathCenter = new Point(joints[2].X - joints[1].X, joints[2].Y - joints[1].Y);
            var windowCenter = ConvertMathToWindow(mathCenter, shoulderPos);
            context.DrawEllipse(reachableBrush,
                null,
                new Point(windowCenter.X, windowCenter.Y),
                rmax, rmax);
            context.DrawEllipse(unreachableBrush,
                null,
                new Point(windowCenter.X, windowCenter.Y),
                rmin, rmin);
        }
        
        public static Point GetShoulderPos(Visual display)
        { return new Point(display.Bounds.Width / 2, display.Bounds.Height / 2); }
        
        public static Point ConvertMathToWindow(Point mathPoint, Point shoulderPos)
        { return new Point(mathPoint.X + shoulderPos.X, shoulderPos.Y - mathPoint.Y); }
   
        public static Point ConvertWindowToMath(Point windowPoint, Point shoulderPos)
        { return new Point(windowPoint.X - shoulderPos.X, shoulderPos.Y - windowPoint.Y); }
    }
}
```
