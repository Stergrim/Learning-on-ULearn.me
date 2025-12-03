# Упражнение. Точка и прямая

**Point:** реализовать класс `Point` с неизменяемыми координатами `x` и `y` (*final*), конструктором, геттерами и методом получения расстояния от текущей до другой точки.

**Line:** реализовать класс `Line`, с конструктором, все поля класса должны быть *private*, а также геттерами для получения концов отрезка, методом `toString`, а также методом, который будет проверять лежит ли переданная точка на прямой, содержащей текущий отрезок и метод поиска длины отрезка.

**PolygonalLine:** необходимо дописать методы: `setPoints`, `addPoint`, `getLength`.

**Все тесты пройдены, задача сдана:**
```java
public class Line {
    private final Point startPoint;
    private final Point endPoint;
    
    public Line(Point startPoint, Point endPoint) {
        this.startPoint = startPoint;
        this.endPoint = endPoint;
    }
    
    public Point getStartPoint() {
        return startPoint;
    }
    
    public Point getEndPoint() {
        return endPoint;
    }
    
    public double getLength() {
        return startPoint.getDistance(endPoint);
    }
    
    public boolean hasPoint(Point p) {
        double x1 = startPoint.getX();
        double y1 = startPoint.getY();
        double x2 = endPoint.getX();
        double y2 = endPoint.getY();
        double x = p.getX();
        double y = p.getY();
    
        return (x - x1) * (y2 - y1) == (y - y1) * (x2 - x1);
    }
    
    @Override
    public String toString() {
        return startPoint.toString() + ", " + endPoint.toString();
    }
}

public class Point {
    private final int x;
    private final int y;
    
    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }
    
    public int getX() {
        return x;
    }
    
    public int getY() {
        return y;
    }
    
    @Override
    public String toString() {
        return "[" + x + ", " + y + "]";
    }
    
    public double getDistance(Point p2) {
        double dx = this.x - p2.x;
        double dy = this.y - p2.y;
        return Math.sqrt(dx * dx + dy * dy);
    }
}

public class PolygonalLine {
    private ArrayList<Point> points = new ArrayList<>();
    
    public double getLength() {
        if (points.size() < 2) {
            return 0;
        }
    
        double length = 0;
        for (int i = 0; i < points.size() - 1; i++) {
            Point current = points.get(i);
            Point next = points.get(i + 1);
            length += current.getDistance(next);
        }
        return length;
    }
    
    public void addPoint(Point p) {
        points.add(p);
    }
    
    public void setPoints(ArrayList<Line> lines) {
        points.clear();
        if (lines == null || lines.isEmpty()) {
            return;
        }
    
        points.add(lines.get(0).getStartPoint());
    
        for (Line line : lines) {
            points.add(line.getEndPoint());
        }
    }
}
```
