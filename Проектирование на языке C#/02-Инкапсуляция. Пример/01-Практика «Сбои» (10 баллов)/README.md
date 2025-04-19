# Практика «Сбои»

Некто N. написал код, выводящий список устройств, в которых до определенной даты случились критические сбои. К сожалению, N. учился программированию в начале 90-х годов, и не знаком с современными практиками.

Скачайте [проект Incapsulation.Failures](Incapsulation.Failures.zip) и помогите N. отрефакторить его код:
1. Выделите новый статический метод FindDevicesFailedBeforeDate. Метод должен принимать не более 4-х аргументов. В сигнатуре метода не должно быть `Dictionary`-типов и коллекций с вложенными дженерик-типами, например, `List<List<object>>`.
2. Значения в аргументах devices и failureTypes должны быть инкапсулированы в сущности Device и Failure.
3. IsFailureSerious, очевидно, не на своем месте.
4. С day и times тоже не все в порядке.

Сигнатуру старого метода сохраните, чтобы проходили тесты. Старый метод должен преобразовывать аргументы и вызывать новый метод.

Все тесты пройдены, задача сдана:
```cs
namespace Incapsulation.Failures;

public class Device
{
    public int DeviceId { get; private set; }
    public string Name { get; private set; }
    public Failure Failures { get; set; }
    
    public Device(int deviceId, string Name, Failure failure)
    {
        this.Name = Name;
        DeviceId = deviceId;
        Failures = failure;
    }
}

public class Failure
{
    public FailureType FailType { get; }
    public DateTime DateFailed { get; }
    public Failure(DateTime dateFailed, FailureType failType)
    {
        DateFailed = dateFailed;
        FailType = failType;
    }
    
    public bool IsFailureSerious()
    {
        return FailType is FailureType.UnexpectedShutdown or FailureType.HardwareFailures;
    }
}

public enum FailureType
{
    UnexpectedShutdown = 0,
    ShortNonResponding = 1,
    HardwareFailures = 2,
    ConnectionProblems = 3
}

public class ReportMaker
{
    /// <summary>
    /// </summary>
    /// <param name="day"></param>
    /// <param name="failureTypes">
    /// 0 for unexpected shutdown, 
    /// 1 for short non-responding, 
    /// 2 for hardware failures, 
    /// 3 for connection problems
    /// </param>
    /// <param name="deviceId"></param>
    /// <param name="times"></param>
    /// <param name="devices"></param>
    /// <returns></returns>
    public static List<string> FindDevicesFailedBeforeDateObsolete(
        int day,
        int month,
        int year,
        int[] failureTypes, 
        int[] deviceId, 
        object[][] times,
        List<Dictionary<string, object>> devices)
    {
        var devicesList = new List<Device>();
    
        for (int i = 0; i < devices.Count; ++i)
            devicesList.Add(new Device(
                deviceId[i],
                (string)devices[i]["Name"],
                new Failure(
                    new DateTime((int)times[i][2], (int)times[i][1], (int)times[i][0]),
                    (FailureType)failureTypes[i]))
                );
    
        var lastDate = new DateTime(year, month, day);
        return FindDevicesFailedBeforeDate(devicesList, lastDate);
    }
    
    
    public static List<string> FindDevicesFailedBeforeDate(List<Device> devices, DateTime lastDate)
    {
        return devices
            .Where(a => a.Failures.IsFailureSerious())
            .Where(a => a.Failures.DateFailed <= lastDate)
            .Select(a => a.Name).ToList();
    }
}
```
