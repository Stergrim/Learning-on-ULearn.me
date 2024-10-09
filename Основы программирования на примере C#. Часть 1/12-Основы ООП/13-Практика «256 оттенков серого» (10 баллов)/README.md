# Практика «256 оттенков серого»

Некто хочет использовать вашу геометрическую библиотеку для рисования. Для этого ему необходимо, чтобы у вашего класса Segment появился цвет. Однако, вам кажется, что добавлять цвета в чисто геометрическую сущность — плохая идея.

[Скачайте проект](GeometryPainting.zip), установите в нем reference на вашу библиотеку, и после этого сделайте так, чтобы методы GetColor и SetColor появились в вашем классе Segment.

Если цвет не задан, GetColor возвращает Color.Black.


Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;
using Avalonia.Media;
using GeometryTasks;

namespace GeometryPainting
{
    public static class SegmentExtensions
    {
        public static Dictionary<Segment, Color> SegmentColor =
                      new Dictionary<Segment, Color>();
    
        public static void SetColor(this Segment segment, Color color)
        {
            if (SegmentColor.ContainsKey(segment)) SegmentColor[segment] = color;
            else SegmentColor.Add(segment, color);
        }
    
        public static Color GetColor(this Segment segment)
        {
            if (!SegmentColor.ContainsKey(segment)) SegmentColor.Add(segment, new Color());
            if (SegmentColor[segment].Equals(null)) return Colors.Black;
            else return SegmentColor[segment];
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[GeometryPainting Edit](GeometryPainting_Edit.zip)
