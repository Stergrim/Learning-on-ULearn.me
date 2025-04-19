# Практика «Геометрия-1»

Какое же наследование без геометрии!

Скачайте [проект Inheritance.Geometry](Inheritance.Geometry.zip) и изучите файл `Virtual\VirtualTask.cs`. Проблема этого подхода в том, что каждый раз при добавлении нового типа тела придется менять метод в базовом классе, из-за чего код в последствии будет разрастаться и становиться все более сложным, вдобавок компилятор не способен определить, добавили ли мы необходимые вычисления в метод, и помочь нам отловить ошибку на этапе компиляции.

Предположим вы знаете, что в планах добавить ещё много новых геометрических примитивов. В этом случае разумно сделать методы `ContainsPoint` и `GetBoundingBox` абстрактными и переопределить их в классах `Ball`, `RectangularCuboid`, `Cylinder` и `CompoundBody`.

Сделайте это! Метод `ContainsPoint` уже реализован внутри класса `Body`, вам остается лишь переместить необходимые участки кода в конкретные классы фигур. Метод `GetBoundingBox` нужно будет реализовать с нуля. Он должен возвращать минимальный ограничивающий прямоугольный параллелепипед (`RectangularCuboid`), то есть такой, который полностью содержит в себе фигуру.

После такого рефакторинга код должен стать проще. В частности в финальном решении не должно остаться ни одного if-а.

Все тесты пройдены, задача сдана:
```cs
namespace Inheritance.Geometry.Virtual;

public abstract class Body
{
    public Vector3 Position { get; }
    
    protected Body(Vector3 position)
    {
        Position = position;
    }
    
    public abstract bool ContainsPoint(Vector3 point);
    
    public abstract RectangularCuboid GetBoundingBox();
}

public class Ball : Body
{
    public double Radius { get; }
    
    public Ball(Vector3 position, double radius) : base(position)
    {
        Radius = radius;
    }
    
    public override bool ContainsPoint(Vector3 point)
    {
        var vector = point - Position;
        var length2 = vector.GetLength2();
        return length2 <= this.Radius * this.Radius;
    }
    
    public override RectangularCuboid GetBoundingBox() => new(Position, 2*Radius, 2*Radius, 2*Radius);
}

public class RectangularCuboid : Body
{
    public double SizeX { get; }
    public double SizeY { get; }
    public double SizeZ { get; }
    
    public RectangularCuboid(Vector3 position, double sizeX, double sizeY, double sizeZ) : base(position)
    {
        SizeX = sizeX;
        SizeY = sizeY;
        SizeZ = sizeZ;
    }
    
    public override bool ContainsPoint(Vector3 point)
    {
        var minPoint = new Vector3(
            Position.X - this.SizeX / 2,
            Position.Y - this.SizeY / 2,
            Position.Z - this.SizeZ / 2);
        var maxPoint = new Vector3(
            Position.X + this.SizeX / 2,
            Position.Y + this.SizeY / 2,
            Position.Z + this.SizeZ / 2);
    
        return point >= minPoint && point <= maxPoint;
    }
    
    public override RectangularCuboid GetBoundingBox() => this;
}

public class Cylinder : Body
{
    public double SizeZ { get; }
    
    public double Radius { get; }
    
    public Cylinder(Vector3 position, double sizeZ, double radius) : base(position)
    {
        SizeZ = sizeZ;
        Radius = radius;
    }
    
    public override bool ContainsPoint(Vector3 point)
    {
        var vectorX = point.X - Position.X;
        var vectorY = point.Y - Position.Y;
        var length2 = vectorX * vectorX + vectorY * vectorY;
        var minZ = Position.Z - this.SizeZ / 2;
        var maxZ = minZ + this.SizeZ;
    
        return length2 <= this.Radius * this.Radius && point.Z >= minZ && point.Z <= maxZ;
    }
    
    public override RectangularCuboid GetBoundingBox() => new(Position, 2*Radius, 2*Radius, SizeZ);
}

public class CompoundBody : Body
{
    public IReadOnlyList<Body> Parts { get; }
    
    public CompoundBody(IReadOnlyList<Body> parts) : base(parts[0].Position)
    {
        Parts = parts;
    }
    
    public override bool ContainsPoint(Vector3 point) => Parts.Any(body => body.ContainsPoint(point));
    
    public override RectangularCuboid GetBoundingBox()
    {
        var minX = double.MaxValue; var minY = double.MaxValue; var minZ = double.MaxValue;
        var maxX = double.MinValue; var maxY = double.MinValue; var maxZ = double.MinValue;
    
        foreach (var part in Parts)
        {
            var cuboid = part.GetBoundingBox();
    
            minX = Math.Min(minX, cuboid.Position.X - cuboid.SizeX / 2);
            minY = Math.Min(minY, cuboid.Position.Y - cuboid.SizeY / 2);
            minZ = Math.Min(minZ, cuboid.Position.Z - cuboid.SizeZ / 2);
            maxX = Math.Max(maxX, cuboid.Position.X + cuboid.SizeX / 2);
            maxY = Math.Max(maxY, cuboid.Position.Y + cuboid.SizeY / 2);
            maxZ = Math.Max(maxZ, cuboid.Position.Z + cuboid.SizeZ / 2);
        }
    
        var sizeX = maxX - minX; var sizeY = maxY - minY; var sizeZ = maxZ - minZ;
        var position = new Vector3(minX + sizeX / 2, minY + sizeY / 2, minZ + sizeZ / 2);
    
        return new RectangularCuboid(position, sizeX, sizeY, sizeZ);
    }
}
```
