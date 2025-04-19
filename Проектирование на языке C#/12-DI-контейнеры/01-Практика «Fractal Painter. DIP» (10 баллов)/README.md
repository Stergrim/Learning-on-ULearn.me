# Практика «Fractal Painter. DIP»

Скачайте [проект DI-container.FractalPainter](DI-container.FractalPainter.zip) и запустите его. Перед вами программа, которая умеет рисовать фракталы с заданными настройками. Также можно задавать дополнительные настройки размера изображения и цвета отрисовки (*пока что цвет отрисовки можно менять только у кривой Коха*). Рекомендуем изучить сам проект и принцип его работы. Это в будущем позволит вам лучше понять, как внедрение DI-контейнера упрощает развитие программы. Начните изучение с файла *App\Program.cs.* В файлах *App\ActionsTask.cs* и *App\DIContainerTask.cs* специально собраны все необходимые классы для выполнения задания. В реальном проекте, конечно, стоило бы каждый класс поместить в собственный файл.

DI-контейнер — очень мощный инструмент в больших проектах, но чтобы приступить к его внедрению, для начала необходимо сделать небольшой рефакторинг проекта в соответствии с Dependency Inversion Principle (DIP). Работайте в файле *App\ActionsTask.cs*. Вам необходимо отрефакторить все классы, унаследованные от IUiAction, так чтобы в них перестал использоваться класс Services (находящийся в файле DIContainerTask.cs), а все необходимые для работы зависимости принимались через единственный конструктор и хранились в private-полях. Обращения к классу Services должны переместится в конструктор без параметров класса MainWindow.

Все тесты пройдены, задача сдана:
```cs
using System;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Platform.Storage;
using FractalPainting.Infrastructure.Common;
using FractalPainting.Infrastructure.UiActions;
using Avalonia.Markup.Xaml;
using FractalPainting.UI;
using ImageController = FractalPainting.Infrastructure.Common.AvaloniaImageController;

namespace FractalPainting.App;

public class ImageSettingsAction : IUiAction
{
    private readonly ImageSettings imageSettings;
    private readonly IImageController imageHolder;
    private readonly Func<Window> GetMainWindow;
    
    public ImageSettingsAction(IImageController imageHolder, ImageSettings imageSettings, Func<Window> GetMainWindow)
    {
        this.imageSettings = imageSettings;
        this.imageHolder = imageHolder;
        this.GetMainWindow = GetMainWindow;
    }
    
    public MenuCategory Category => MenuCategory.Settings;
    public event EventHandler? CanExecuteChanged;
    public string Name => "Изображение...";
    
    public bool CanExecute(object? parameter) => true;
    
    public async void Execute(object? parameter)
    {
        await new SettingsForm(imageSettings).ShowDialog(GetMainWindow());
        imageHolder.RecreateImage(imageSettings);
    }
}

public class SaveImageAction : IUiAction
{
    private readonly IImageController imageHolder;
    private readonly Func<Window> GetMainWindow;
    
    public SaveImageAction(IImageController imageHolder, Func<Window> GetMainWindow)
    {
        this.imageHolder = imageHolder;
        this.GetMainWindow = GetMainWindow;
    }
    
    public MenuCategory Category => MenuCategory.File;
    public event EventHandler? CanExecuteChanged;
    public string Name => "Сохранить...";
    
    public bool CanExecute(object? parameter) => true;
    
    public async void Execute(object? settings)
    {
        var topLevel = TopLevel.GetTopLevel(GetMainWindow());
        if (topLevel is null) return;
    
        var options = new FilePickerSaveOptions
        {
            Title = "Сохранить изображение",
            SuggestedFileName = "image.bmp",
        };
        var saveFile = await topLevel.StorageProvider.SaveFilePickerAsync(options);
        if (saveFile is not null)
            imageHolder.SaveImage(saveFile.Path.AbsolutePath);
    }
}

public class PaletteSettingsAction : IUiAction
{
    private readonly Palette palette;
    private readonly Func<Window> GetMainWindow;
    
    public PaletteSettingsAction(Palette palette, Func<Window> GetMainWindow)
    {
        this.palette = palette;
        this.GetMainWindow = GetMainWindow;
    }
    
    public MenuCategory Category => MenuCategory.Settings;
    public event EventHandler? CanExecuteChanged;
    public string Name => "Палитра...";
    
    public bool CanExecute(object? parameter) => true;
    
    public async void Execute(object? parameter)
        => await new SettingsForm(palette).ShowDialog(GetMainWindow());
}

public partial class MainWindow : Window
{
    private const int MenuSize = 32;
    private Menu? menu;
    private ImageControl? image;
    
    private void InitializeComponent()
    {
        AvaloniaXamlLoader.Load(this);
    
        menu = this.FindNameScope()?.Find<Menu>("Menu");
        image = this.FindNameScope()?.Find<ImageControl>("Image");
    }
    
    public MainWindow(IUiAction[] actions, ImageController imageController)
    {
        InitializeComponent();
        var imageSettings = CreateSettingsManager().Load().ImageSettings;
        ClientSize = new Size(imageSettings.Width, imageSettings.Height + MenuSize);
        menu.ItemsSource = actions.ToMenuItems();
        Title = "Fractal Painter";
        imageController.SetControl(image);
        imageController.RecreateImage(imageSettings);
    }
    
    private static SettingsManager CreateSettingsManager()
        => new SettingsManager(new XmlObjectSerializer(), new FileBlobStorage());
}
```
