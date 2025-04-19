# Практика «Роботы»

Не так-то просто сделать упражнение на ковариацию и контравариацию, но нам удалось.

Скачайте [проект Generics.Robots](Generics.Robots.zip) и изучите класс Architecture.cs. Он описывает некий проект архитектуры робота. В нем есть AI, вырабатывающий команды, и Device, команды исполняющий.

При этом, AI уже готовы для двух типов роботов (Builder и Shooter), а Device есть только для подвижной части.

Сейчас все работает, но вам не нравится. Что это за object-ы повсюду, где строгая типизация? Конечно, RobotAI и Mover должны стать дженерик-классами, типизируемыми классом команды. Однако, когда вы это сделаете, вы обнаружите, что эта архитектура не компилируется. Здесь нужно применить ковариацию для того, чтобы исправить эту проблему.

Все тесты пройдены, задача сдана:
```cs
namespace Generics.Robots;

public interface IRobotAI<out T> where T : IMoveCommand
{
    T GetCommand();
}

public abstract class RobotAI<T> : IRobotAI<T> where T : IMoveCommand
{
    public abstract T GetCommand();
}

public class ShooterAI : RobotAI<ShooterCommand>
{
    int counter = 1;
    public override ShooterCommand GetCommand() => ShooterCommand.ForCounter(counter++);
}

public class BuilderAI : RobotAI<BuilderCommand>
{
    int counter = 1;
    public override BuilderCommand GetCommand() => BuilderCommand.ForCounter(counter++);
}

public interface IDevice<in T> where T : IMoveCommand
{
    string ExecuteCommand(T command);
}

public abstract class Device<T> : IDevice<T> where T : IMoveCommand
{
    public abstract string ExecuteCommand(T command);
}

public class Mover : Device<IMoveCommand>
{
    public override string ExecuteCommand(IMoveCommand command)
    {
        if (command == null)
            throw new ArgumentException();
        return $"MOV {command.Destination.X}, {command.Destination.Y}";
    }
}

public class ShooterMover : Device<IMoveCommand>
{
    public override string ExecuteCommand(IMoveCommand _command)
    {
        var command = _command as IShooterMoveCommand;
        if (command == null)
            throw new ArgumentException();
        var hide = command.ShouldHide ? "YES" : "NO";
        return $"MOV {command.Destination.X}, {command.Destination.Y}, USE COVER {hide}";
    }
}

public class Robot<T> where T : IMoveCommand
{
    private readonly IRobotAI<T> ai;
    private readonly IDevice<T> device;
    
    public Robot(IRobotAI<T> ai, IDevice<T> executor)
    {
        this.ai = ai;
        this.device = executor;
    }
    
    public IEnumerable<string> Start(int steps)
    {
        for (int i = 0; i < steps; i++)
        {
            var command = ai.GetCommand();
            if (command == null)
                break;
            yield return device.ExecuteCommand(command);
        }
    }
}

public static class Robot
{
    public static Robot<IMoveCommand> Create<T>(IRobotAI<IMoveCommand> ai, IDevice<IMoveCommand> executor)
    {
        return new Robot<IMoveCommand>(ai, executor);
    }
}
```
