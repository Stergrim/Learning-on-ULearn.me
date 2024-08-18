# Практика «Лабиринт змейка»

В том же проекте выведите робота из лабиринта вида «змейка».

Запустите проект и самостоятельно изучите этот тип лабиринтов.

Дополнительные ограничения:
- Запрещено использовать более одного цикла в одном методе.
- Запрещено иметь методы длиннее 12 строк кода.
- Запрещено использовать ключевое слово `catch`
- Разрешено создавать вспомогательные методы, но только понятными именами, в том числе именами аргументов.

Обратите внимание, чтобы в вашем коде не было дублирующихся почти одинаковых методов

Все тесты пройдены, задача сдана:
```cs
namespace Mazes
{
	public static class SnakeMazeTask
	{
        	public static void MoveRightAndLeft(Robot robot, int width, bool a)
       	 	{
            		for (int i = 0; i < width - 3; i++)
                		if (a) robot.MoveTo(Direction.Right);
                		else robot.MoveTo(Direction.Left);
		}

		public static void MoveOut(Robot robot, int width, int height)
		{
			int k = 1;
			int c = (height - 1) / 2;
			while (k <= c)
                		if (k % 2 != 0) k = MoveUnEven(robot, width, k);
                		else k = MoveEven(robot, width, k, c);
		}
		
		public static void MoveDownTwice(Robot robot)
        	{
			robot.MoveTo(Direction.Down);
			robot.MoveTo(Direction.Down);
		}
		
		public static int MoveUnEven(Robot robot, int width, int k)
        	{
			MoveRightAndLeft(robot, width, true);
			MoveDownTwice(robot);
			return ++k;
		}

		public static int MoveEven(Robot robot, int width, int k, int c)
		{
			MoveRightAndLeft(robot, width, false);
			if (k != c) MoveDownTwice(robot);
			return ++k;
		}
	}
}
```
