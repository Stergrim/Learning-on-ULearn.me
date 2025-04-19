# Практика «Генератор отчетов»

Некто N разработал [генератор отчетов в проекте Delegates.Reports](Delegates.Reports.zip), который считает простую статистику о погоде по нескольким параметрам за несколько дней. Его генератор расширяем, и он написал два отчета с его помощью: отчет в HTML, считающий среднее и стандартное отклонение, и отчет в Markdown, считающий медианы.

Однако, что делать, если нужно посчитать медианы и вывести результат в HTML? Что, если нужен будет третий отчет в HTML? Текущее решение крайне неудобно для таких ситуаций.

Помогите N отрефакторить код, переведя его с наследования на делегирование. Разделите ответственности по оформлению отчета и по вычислению показателей в разные классы. В результате сам класс ReportMaker должен кардинально упроститься.

Все тесты пройдены, задача сдана:
```cs
using System.Text;

namespace Delegates.Reports;

public class ReportMaker
{
    public static string MakeReport(
        string caption,
        Func<IEnumerable<double>, object> makeStatistics,
        Func<string, string?, string?, string> makeMarkup,
        IEnumerable<Measurement> measurements
        )
    {
        var data = measurements.ToList();
        var temperature = makeStatistics(data.Select(m => m.Temperature)).ToString();
        var humidity = makeStatistics(data.Select(m => m.Humidity)).ToString();
        return makeMarkup(caption, temperature, humidity);
    }
}

public static class Markup
{
    public static string MakeMarkdown(string caption, string? temperature, string? humidity)
    {
        var markup = new StringBuilder();
        markup.Append($"## {caption}\n\n");
        markup.Append($" * **Temperature**: {temperature}\n\n");
        markup.Append($" * **Humidity**: {humidity}\n\n");
        return markup.ToString();
    }
    
    public static string MakeHtml(string caption, string? temperature, string? humidity)
    {
        var markup = new StringBuilder();
        markup.Append($"<h1>{caption}</h1>");
        markup.Append("<ul>");
        markup.Append($"<li><b>Temperature</b>: {temperature}");
        markup.Append($"<li><b>Humidity</b>: {humidity}");
        markup.Append("</ul>");
        return markup.ToString();
    }
}

public static class Statistics
{
    public static object MakeMedian(IEnumerable<double> doubles)
    {
        var list = doubles.OrderBy(z => z).ToList();
        if (list.Count % 2 == 0)
            return (list[list.Count / 2] + list[list.Count / 2 - 1]) / 2;
    
        return list[list.Count / 2];
    }
    
    public static object MakeMeanAndStd(IEnumerable<double> doubles)
    {
        var doublesList = doubles.ToList();
        var mean = doublesList.Average();
        var std = Math.Sqrt(doublesList.Select(z => Math.Pow(z - mean, 2)).Sum() / (doublesList.Count - 1));
    
        return new MeanAndStd { Mean = mean, Std = std };
    }
}

public static class ReportMakerHelper
{
    public static string MeanAndStdHtmlReport(IEnumerable<Measurement> data) 
        => ReportMaker.MakeReport("Mean and Std",
            Statistics.MakeMeanAndStd,
            Markup.MakeHtml,
            data);
    
    public static string MedianMarkdownReport(IEnumerable<Measurement> data)
        => ReportMaker.MakeReport("Median",
            Statistics.MakeMedian,
            Markup.MakeMarkdown,
            data);
    
    public static string MeanAndStdMarkdownReport(IEnumerable<Measurement> measurements)
        => ReportMaker.MakeReport("Mean and Std",
            Statistics.MakeMeanAndStd,
            Markup.MakeMarkdown,
            measurements);
    
    public static string MedianHtmlReport(IEnumerable<Measurement> measurements)
        => ReportMaker.MakeReport("Median",
            Statistics.MakeMedian,
            Markup.MakeHtml,
            measurements);
}
```
