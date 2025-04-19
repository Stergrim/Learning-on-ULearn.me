# Практика «Fractal Painter. DI-container»

*При решении данной практики не бойтесь раскрывать подсказки, если возникли какие-либо трудности. В случае успешного выполнения пункта, их так же рекомендуется раскрывать, чтобы сверить ваше решение с образцом.*

Продолжайте в том же проекте. Теперь, когда все базовые приготовления сделаны, приступим к внедрению DI-контейнера:

1. **Исправляем MainWindow**
- В классе DIContainerTask переделайте метод CreateMainWindow так, чтобы объект класса MainWindow создавался контейнером
- Произведите биндинг всех классов унаследованных от IUiAction (Искать в *ActionsTask.cs* и *DIContainerTask.cs*). А все зависимости для инициализации Action-объектов временно забиндите на обращение к классу Services через метод ToConstant. Все биндинги должны быть в методе `ConfigureContainer`.
- Удалите из класса MainWindow конструктор без параметров и так же произведите биндинг всех необходимых для инициализации аргументов

На данном этапе программа должна корректно работать. Как вы можете видеть контейнер самостоятельно создает все необходимые зависимости при создании MainWindow и уже сейчас становится видно, как программа становится более простой и гибкой. Но не всегда все идет так гладко, как хотелось бы.

**Подсказки**
- Классы унаследованные от IUiAction можно забиндить так: `container.Bind<IInterface>().To<Class>()`;
- Необходимые зависимости можно забиндить так: `container.Bind<IInterface>().ToConstant(Services.Get...())`;
- В том числе получение MainWindow `container.Bind<Func<Window>>()...`;


2. **Исправляем KochFractalAction**
- Отрефакторите класс KochFractalAction, также как в первой практике
- Изучите KochFractalAction и поймите, что на самом деле IImageController и Pallette ему не нужны. Измените его так, чтобы он принимал только KochPainter.
- После наших действий программа работает некорректно. Подумайте из-за чего это может и попробуйте решить проблему.
- Убедитесь, что всё ещё работает

**Подсказки**
- Сложность в том, что при инициализации MainWindow необходимо перед этим создать KochFractalAction, а для него необходим KochPainter, а KochPainter в конструкторе обращается к IImageController, который еще не инициализирован, а инициализация происходит в конструкторе MainWindow. Т.е. получается такая картина: MainWindow → KochFractalAction → KochPainter → IImageController. Если разобраться, то станет ясно, что классу KochFractalAction KochPainter необходим лишь во время выполнения метода Execute. Подумайте, как сделать так, чтобы он создавался немного позже.
- Чтобы разорвать этот круг, необходимо использовать [класс Lazy](https://ulearn.me/course/cs2/Tsiklicheskie_zavisimosti_5130e1c9-fea8-465e-8064-44ab208fafd4), при этом очень важно в конструкторе не производить обращение к Value, иначе круг снова сомкнется


3. **Исправляем DragonFractalAction**
- Избавьтесь от использования класса Services в DragonFractalAction
- Убедитесь, что все работает


4. **Ребиндим IImageController и Palette**
- Перебиндите IImageController и Palette, так чтобы контейнер их создавал сам не обращаясь к классу Services
- Убедитесь, что дракон рисуется, а палитра изменяет цвет прорисовки кривой Коха

**Подсказки**
- IImageController и Palette должны биндиться в единственном экземпляре
- Пример биндинга в единственном экземпляре: `container.Bind<IInterface>.To<Class>().InSingletonScope()`;
- Биндинг IImageController и AvaloniaImageController должен указывать на один и тот же объект AvaloniaImageController. Пример такого биндинга: `container.Bind<IInterface, SomeClass>().To<SomeClass>()`;


5. 1. **Улучшаем DragonFractalAction**
- Давайте также сделаем более гибким DragonFractalAction и будем принимать все необходимые зависимости через конструктор. Дополнительное ограничение — нельзя менять публичный интерфейс DragonPainter. Особенность в том, что одна из зависимостей DragonPainter — DragonSettings оказывается известной только в процессе работы экшена. Из-за этого вы не можете просить инжектировать в конструктор уже готовый Painter. Вместо этого инжектируйте [фабрику DragonPainter-ов](https://github.com/ninject/Ninject.Extensions.Factory/wiki/Factory-interface). Интерфейс фабрики назовите IDragonPainterFactory, настройте его так чтобы, контейнер сам сгенерировал класс реализующий интерфейс фабрики.
- Если вы все сделали правильно, то IImageController теперь нам больше не нужен и от него можно избавиться. Сделайте это.

**Подсказки**
- Создайте интерфейс для фабрики. У него должен быть один метод, принимающий аргументы, необходимые для создания DragonPainter, значения которых определяются только во время выполнения, и возвращающий DragonPainter. После этого в коде DragonAction можно получить в конструктор объект этого интерфейса и использовать его метод для создания зависимости. Реализовывать интерфейс фабрики не нужно —— это сделает контейнер.
- Пример объявления метода в интерфейсе фабрики: `DragonPainter CreateDragonPainter(...)`
- В контейнере есть договоренность - имена параметров метода в фабрике должны совпадать с именами параметров конструктора создаваемой сущности
- Необязательно в параметры метода фабрики принимать все необходимые аргументы. Если какие-то аргументы можно достать из контейнера, например в нашем случае это `IImageController`, то фабрика сама сделает это, даже если они не были переданы в метод.
- Не забудьте забиндить особым образом фабрику
- Пример биндинга фабрики: `container.Bind<IFactory>().ToFactory()`;


5. 2. **Улучшаем DragonFractalAction (Необязательно)**
- Для создания DragonPainter-а можно также использовать [Func-фабрику](https://github.com/ninject/Ninject.Extensions.Factory/wiki/Func). Закомментируйте в конструкторе предыдущую фабрику и инжектируйте Func-фабрику в DragonFractalAction. *(!) Интерфейс фабрики и её биндинг необходимо оставить, т.к. это проверяется в тестах.*
- Убедитесь, что все работает

**Подсказки**
- Func-фабрика идентична обычной фабрике. Отличие заключается лишь в том, что вместо определения интерфейса и последующего принятие объекта интерфейса через конструктор, мы просто сразу принимаем Func-объект. Это очень удобно тем, что нам не надо ничего биндить и определять интерфейс фабрики, контейнер сам поймет, что требуется Func-фабрика.
- Пример Func-фабрики принимаемой в конструктор: `Func<Some, Necessary, Args, NecessaryObject> CreateNecessaryObject`
- В Func-фабрику так же можно не передавать все необходимые аргументы, а можно ограничиться лишь теми, которые нельзя достать из контейнера


6. **Улучшаем DragonPainter**
- Переведите DragonPainter на использование цветов палитры, как это сделано в KochPainter
- Убедитесь, что экшен настройки палитры работает как надо

Если вы всё сделали правильно, то для добавления зависимости вам не пришлось править код работы с контейнером вообще. Магия!


7. **Избавляемся от зависимостей класса Services**
- Избавиться от обращений к классу Services при создании AppSettings, ImageSettings. Используйте [ToMethod](https://github.com/ninject/Ninject/wiki/Providers,-Factory-Methods-and-the-Activation-Context#factory-methods), чтобы доставать нужные зависимости из контейнера: `ToMethod(context => context.Kernel.Get<TService>() ... )`
- Избавиться от обращений к классу Services при создании `Func<Window>`
- Убедитесь, что окно настройки размера изображения работают. Фрактал и кривая Коха должны адаптироваться под изменения изображения и не выходить за рамки в случае маленьких размеров.

**Подсказки**
- Чтобы создать SettingsManager и при этом не нарушить гибкость программы, необходимо обратиться к ядру контейнера через ToMethod и запросить создать SettingsManager. Так в будущем это нам с легкостью позволит при необходимости поменять SettingsManager путем изменения одной строчки биндинга, при этом целостность программы не нарушится.
- Пример обращения к ядру: `*.ToMethod(c => c.Kernel.Get<SomeObject>();`
- Чтобы контейнер мог создать `SettingsManager`, необходимо забиндить аргументы, которые необходимы для его создания
- `ImageSettings` нужно получать из `AppSettings`, который мы получаем из `SettingsManager-а`. Здесь вам снова поможет `ToMethod`
- Чтобы не нарушить гибкость программы, запросите у ядра `AppSettings` и верните переданные настройки
- `AppSettings` и `ImageSettings` должны быть забинджены в единственном экземпляре
- Контейнер при запросе Window и MainWindow должен возвращать один и тот же объект


8. **Избавляемся от класса Services**
- Теперь к классу Services не происходит никаких обращений и его можно полностью удалить. Сделайте это.
- Убедитесь, что все работает

*По прохождению данного пункта все тесты должны быть зелеными!*


9. **Автоматизация биндинга классов (Необязательно)**
- В больших проектах таких классов, как унаследованные от IUiActions, может быть сотни и тысячи. Подумайте, как сделать так, чтобы контейнер сам их находил и биндил. *Не забудьте подключить библиотеку* `Ninject.Extensions.Conventions;`
- Убедитесь, что все работает

**Подсказки**
- Посмотрите еще раз лекцию [Conventions](https://ulearn.me/course/cs2/Conventions_6ef071ee-ad2b-4e0c-80fa-5a3f96b7905f)
- Пример биндинга: `container.Bind(x => x.FromThisAssembly().SelectAllClasses().InheritedFrom<IInterface>().BindAllInterfaces());`


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Linq;
using Avalonia;
using Avalonia.Media;
using Avalonia.Controls;
using FractalPainting.UI;
using FractalPainting.App.Fractals;
using FractalPainting.Infrastructure.Common;
using FractalPainting.Infrastructure.UiActions;
using Ninject;
using Ninject.Extensions.Factory;
using Ninject.Extensions.Conventions;

namespace FractalPainting.App;

public static class DIContainerTask
{
    public static MainWindow CreateMainWindow() => ConfigureContainer().Get<MainWindow>();
    
    public static StandardKernel ConfigureContainer()
    {
        var container = new StandardKernel();
        container.Bind(x => x.FromThisAssembly().SelectAllClasses().InheritedFrom<IUiAction>().BindAllInterfaces());
    
        container.Bind<Palette>().To<Palette>().InSingletonScope();
    
        container.Bind<MainWindow>().ToSelf().InSingletonScope();
        container.Bind<Func<Window>>().ToMethod(context => () => context.Kernel.Get<MainWindow>().ShowAndReturn());
    
        container.Bind<AppSettings>().ToMethod(context =>
                  context.Kernel.Get<SettingsManager>().Load()).InSingletonScope();
        container.Bind<ImageSettings>().ToMethod(context =>
                  context.Kernel.Get<AppSettings>().ImageSettings).InSingletonScope();
    
        container.Bind<IImageController, AvaloniaImageController>().To<AvaloniaImageController>().InSingletonScope();
        container.Bind<IObjectSerializer>().To<XmlObjectSerializer>().WhenInjectedInto<SettingsManager>();
        container.Bind<IBlobStorage>().To<FileBlobStorage>().WhenInjectedInto<SettingsManager>();
    
        container.Bind<IDragonPainterFactory>().ToFactory();
    
        return container;
    }
}

public class KochFractalAction : IUiAction
{
    private readonly Lazy<KochPainter> kochPainter;
    public MenuCategory Category => MenuCategory.Fractals;
    public string Name => "Кривая Коха";
    public event EventHandler? CanExecuteChanged;
    
    public KochFractalAction(Lazy<KochPainter> kochPainter)
        => this.kochPainter = kochPainter;
    
    public bool CanExecute(object? parameter) => true;
    public void Execute(object? parameter) => kochPainter.Value.Paint();
}

public class DragonFractalAction : IUiAction
{
    private readonly Func<Window> GetMainWindow;
    private readonly Func<DragonSettings, DragonPainter> dragonPainterFactory;
    public MenuCategory Category => MenuCategory.Fractals;
    public string Name => "Дракон";
    public event EventHandler? CanExecuteChanged;
    
    public DragonFractalAction(Func<Window> GetMainWindow, Func<DragonSettings, DragonPainter> dragonPainterFactory)
    {
        this.GetMainWindow = GetMainWindow;
        this.dragonPainterFactory = dragonPainterFactory;
    }
    
    public bool CanExecute(object? parameter) => true;
    
    public async void Execute(object? parameter)
    {
        var dragonSettings = CreateRandomSettings();
        await new SettingsForm(dragonSettings).ShowDialog(GetMainWindow());
        var painter = dragonPainterFactory(dragonSettings);
        painter.Paint();
    }
    
    private static DragonSettings CreateRandomSettings()
        => new DragonSettingsGenerator(new Random()).Generate();
}

public interface IDragonPainterFactory
{
    public DragonPainter CreateDragonPainter(DragonSettings dragonSettings);
}

public class DragonPainter
{
    private readonly Palette palette;
    private readonly DragonSettings settings;
    private readonly IImageController imageController;
    
    public DragonPainter(IImageController imageController, Palette palette, DragonSettings settings)
    {
        this.palette = palette;
        this.settings = settings;
        this.imageController = imageController;
    }
    
    public void Paint()
    {
        using var ctx = imageController.CreateDrawingContext();
        var imageSize = imageController.GetImageSize();
        var size = Math.Min(imageSize.Width, imageSize.Height) / 2.1f;
        var backgroundBrush = new SolidColorBrush(palette.BackgroundColor);
        var primaryBrush = new SolidColorBrush(palette.PrimaryColor);
        ctx.FillRectangle(backgroundBrush, new Rect(0, 0, imageSize.Width, imageSize.Height));
    
        var r = new Random();
        var cosa = (float)Math.Cos(settings.Angle1); var sina = (float)Math.Sin(settings.Angle1);
        var cosb = (float)Math.Cos(settings.Angle2); var sinb = (float)Math.Sin(settings.Angle2);
        var shiftX = settings.ShiftX * size * 0.8f; var shiftY = settings.ShiftY * size * 0.8f;
        var scale = settings.Scale;
        var p = new Point(0, 0);
        foreach (var i in Enumerable.Range(0, settings.IterationsCount))
        {
            ctx.FillRectangle(primaryBrush,
                new Rect(imageSize.Width / 3f + p.X, imageSize.Height / 2f + p.Y, 1, 1));
            if (r.Next(0, 2) == 0) p = new Point(scale * (p.X * cosa - p.Y * sina),
                scale * (p.X * sina + p.Y * cosa));
            else p = new Point(scale * (p.X * cosb - p.Y * sinb) + (float)shiftX,
                scale * (p.X * sinb + p.Y * cosb) + (float)shiftY);
        }
        imageController.UpdateUi();
    }
}

public static class WindowExtensions
{
    public static Window ShowAndReturn(this Window window)
    {
        window.Show();
        return window;
    }
}
```

**Проект со всеми внесенными решениями.**
[DI-container.FractalPainter Edit](DI-container.FractalPainter_Edit.zip)
