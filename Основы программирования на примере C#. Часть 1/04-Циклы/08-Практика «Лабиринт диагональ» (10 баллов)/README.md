# Практика «Лабиринт диагональ»

В том же проекте выведите робота из лабиринта вида «диагональ».

Запустите проект и самостоятельно изучите этот тип лабиринтов.

Дополнительные ограничения:
- Запрещено использовать более одного цикла в одном методе.
- Запрещено иметь методы длиннее 12 строк кода.
- Запрещено использовать ключевое слово `catch`
- Разрешено создавать вспомогательные методы, но только понятными именами, в том числе именами аргументов.

Все тесты пройдены, задача сдана:
```cs
namespace Mazes
{
	public static class DiagonalMazeTask
	{
	        public static void MoveWidth(Robot robot, int width, int height, int c)
	        {
	            for (int i = 1; i < width-2; i++)
	            {
	                robot.MoveTo(Direction.Right);
	                if ((i % c == 0)&&(i < width - 3)) robot.MoveTo(Direction.Down);
	            }
	        }
	
	        public static void MoveHeight(Robot robot, int width, int height, int c)
	        {
	            for (int i = 1; i < height-2; i++)
	            {
	                robot.MoveTo(Direction.Down);
	                if ((i % c == 0)&&(i < height - 3)) robot.MoveTo(Direction.Right);
	            }
		}
	
	        public static void MoveOut(Robot robot, int width, int height)
		{
			if (width >= height) MoveWidth(robot, width, height, (int)((width - 2) / (height - 2)));
			else MoveHeight(robot, width, height, (int)((height - 2) / (width - 2)));
	        }
	}
}
```

**Проект со всеми внесенными решениями.**
[Mazes Edit](Mazes_Edit.zip)
