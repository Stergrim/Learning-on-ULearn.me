# Практика «TaxiOrder»

Продолжайте в том же проекте [Ddd.Taxi](Ddd.Taxi.zip).

Изучите пару классов `TaxiOrder` и `TaxiApi` — это модель предметной области по заказу такси.

TaxiOrder — типичная анемичная модель. Вся логика, связанная с этим классом находится в TaxiApi.

Переработайте класс TaxiOrder согласно принципам DDD. А именно:
- Сгруппируйте связанные свойства TaxiOrder во вспомогательные классы: PersonName, Address, Driver. Для этого можно как использовать уже готовые классы из пространства имён Domain, так и создавать собственные классы. При проверке этой задачи ValueType из предыдущей задачи будет доступен — используйте его! Где возможно замените использование отдельных свойств на использование новых классов.
- При подходе DDD работой с базой данных занимаются репозитории. Задача репозитория читать и записывать данные в БД и ничего более. В частности репозиторий водителей ничего недолжен знать про заказы. Переделайте репозиторий так, чтобы в нем остался только метод получения водителя и не осталось ссылок на заказы.
- Перенесите всю логику из методов TaxiApi в методы TaxiOrder. В TaxiApi должен остаться только тестовый код и вызовы соответствующих методов TaxiOrder.
- Добавьте проверки на валидность действий — кидайте InvalidOperationException, если действие в данном состоянии заказа невалидно.
- Замените все поля на свойства и закройте у свойств сеттеры. Впрочем, можно закрыть и сами свойства.

Проверяйте результат запуском тестов.

При переработке кода, публичный интерфейс TaxiApi нужно сохранить, чтобы система тестирования смогла проверить результат вашего рефакторинга. Однако, в реальном проекте переработка TaxiOrder наверняка повлекла бы за собой переработку и TaxiApi.

Все тесты пройдены, задача сдана:
```cs
using System.Globalization;
using Ddd.Taxi.Infrastructure;

namespace Ddd.Taxi.Domain;

// In real aplication it whould be the place where database is used to find driver by its Id.
// But in this exercise it is just a mock to simulate database
public class DriversRepository { }

public class TaxiApi : ITaxiApi<TaxiOrder>
{
    private readonly DriversRepository driversRepo;
    private readonly Func<DateTime> currentTime;
    private int idCounter;
    
    public TaxiApi(DriversRepository driversRepo, Func<DateTime> currentTime)
    {
        this.driversRepo = driversRepo;
        this.currentTime = currentTime;
    }
    
    public TaxiOrder CreateOrderWithoutDestination(string firstName, string lastName,
                                                   string street, string building)
    {
        return
            new TaxiOrder(
                idCounter++,
                new PersonName(firstName, lastName),
                new Address(street, building),
                currentTime()
            );
    }
    
    public void UpdateDestination(TaxiOrder order, string street, string building)
        => order.UpdateDestination(new Address(street, building));
    
    public void AssignDriver(TaxiOrder order, int driverId)
        => order.AssignDriver(driverId, currentTime());
    
    public void UnassignDriver(TaxiOrder order)
        => order.UnassignDriver();
    
    public string GetDriverFullInfo(TaxiOrder order)
        => order.GetDriverFullInfo();
    
    public string GetShortOrderInfo(TaxiOrder order)
        => order.GetShortOrderInfo();
    
    private DateTime GetLastProgressTime(TaxiOrder order)
        => order.GetLastProgressTime();
    
    public void Cancel(TaxiOrder order)
        => order.Cancel(currentTime());
    
    public void StartRide(TaxiOrder order)
        => order.StartRide(currentTime());
    
    public void FinishRide(TaxiOrder order)
        => order.FinishRide(currentTime());
}
public class Driver : Entity<int>
{
    public readonly int? Identificator;
    public PersonName Name { get; private set; }
    public Car Car { get; private set; }
    
    public Driver(int? id, PersonName name, string carModel,
                  string carColor, string carPlateNumber) : base(0)
    {
        Identificator = id;
        Name = name;
        Car = new Car(carModel, carColor, carPlateNumber);
    }
}

public class Car : ValueType<Car>
{
    public string Model { get; private set; }
    public string Color { get; private set; }
    public string PlateNumber { get; private set; }
    
    public Car(string model, string color, string plateNumber)
    {
        Model = model;
        Color = color;
        PlateNumber = plateNumber;
    }
}
public class TaxiOrder : Entity<int>
{
    private readonly int id;
    public PersonName ClientName { get; private set; }
    public Address Start { get; private set; }
    public Address Destination { get; private set; }
    public Driver Driver { get; private set; }
    public TaxiOrderStatus Status { get; private set; }
    public DateTime CreationTime { get; private set; }
    public DateTime DriverAssignmentTime { get; private set; }
    public DateTime CancelTime { get; private set; }
    public DateTime StartRideTime { get; private set; }
    public DateTime FinishRideTime { get; private set; }
    
    public TaxiOrder(int id, PersonName clientName, Address startAddress, DateTime dateTime) : base(id)
    {
        this.id = id;
        ClientName = clientName;
        Start = startAddress;
        CreationTime = dateTime;
    }
    
    public void Cancel(DateTime cancelTime)
    {
        if (Status == TaxiOrderStatus.InProgress) throw new InvalidOperationException();
        Status = TaxiOrderStatus.Canceled;
        CancelTime = cancelTime;
    }
    
    public void UpdateDestination(Address destinationAddress) 
        => Destination = destinationAddress;
    
    public void StartRide(DateTime startTime)
    {
        if (Driver == null) throw new InvalidOperationException();
        Status = TaxiOrderStatus.InProgress;
        StartRideTime = startTime;
    }
    
    public void FinishRide(DateTime finishTime)
    {
        if (Status != TaxiOrderStatus.InProgress) throw new InvalidOperationException();
        Status = TaxiOrderStatus.Finished;
        FinishRideTime = finishTime;
    }
    
    public DateTime GetLastProgressTime() => Status switch
    {
        TaxiOrderStatus.WaitingForDriver => CreationTime,
        TaxiOrderStatus.WaitingCarArrival => DriverAssignmentTime,
        TaxiOrderStatus.InProgress => StartRideTime,
        TaxiOrderStatus.Finished => FinishRideTime,
        TaxiOrderStatus.Canceled => CancelTime,
        _ => throw new NotSupportedException(Status.ToString())
    };
    
    public string GetShortOrderInfo()
    {
        var gotDriver = Driver == null || Driver.Name == null;
        var driver = gotDriver ? "" : FormatName(Driver.Name);
        var destination = Destination == null ? "" : FormatAddress(Destination);
        return string.Join(" ",
            "OrderId: " + id,
            "Status: " + Status,
            "Client: " + FormatName(ClientName),
            "Driver: " + driver,
            "From: " + FormatAddress(Start),
            "To: " + destination,
            "LastProgressTime: " + GetLastProgressTime().ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture));
    }
    
    public string GetDriverFullInfo()
    {
        if (Status == TaxiOrderStatus.WaitingForDriver) return null;
        return string.Join(" ",
            "Id: " + Driver.Identificator,
            "DriverName: " + FormatName(Driver.Name),
            "Color: " + Driver.Car.Color,
            "CarModel: " + Driver.Car.Model,
            "PlateNumber: " + Driver.Car.PlateNumber);
    }
    
    public void UnassignDriver()
    {
        if (Status == TaxiOrderStatus.InProgress || Driver == null)
            throw new InvalidOperationException(Status.ToString());
        Driver = new Driver(null, null, null, null, null);
        Status = TaxiOrderStatus.WaitingForDriver;
    }
    
    public void AssignDriver(int driverId, DateTime assignTime)
    {
        if (Driver == null)
            if (driverId == 15)
            {
                Driver = new Driver(driverId, new PersonName("Drive", "Driverson"),
                                   "Lada sedan", "Baklazhan", "A123BT 66");
                DriverAssignmentTime = assignTime;
                Status = TaxiOrderStatus.WaitingCarArrival;
            }
            else throw new Exception("Unknown driver id " + driverId);
        else throw new InvalidOperationException();
    }
    
    private string FormatName(PersonName name)
        => string.Join(" ", new[] { name.FirstName, name.LastName }.Where(n => n != null));
    
    private string FormatAddress(Address address)
        => string.Join(" ", new[] { address.Street, address.Building }.Where(n => n != null));
}
```

**Проект со всеми внесенными решениями.**
[Ddd.Taxi Edit](Ddd.Taxi_Edit.zip)
