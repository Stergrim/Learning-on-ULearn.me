# Практика «Предприятие»

Некто M. написал код, описывающий предприятие. Он даже озаботился проверкой целостности для полей этого класса, но, к сожалению, он учился программировать в конце 90-х годов, и знаком лишь со слегка устаревшими практиками проверки целостности.

Скачайте [проект Incapsulation.EnterpriseTask](Incapsulation.EnterpriseTask.zip) и помогите M. отрефакторить его код.

Все тесты пройдены, задача сдана:
```cs
namespace Incapsulation.EnterpriseTask;

public class Enterprise
{
    public readonly Guid Guid;
    
    public Enterprise(Guid guid)
    {
        Guid = guid;
    }
    
    public string Name { get; set; }
    
    private string inn;
    public string Inn
    {
        get { return inn; }
        set
        {
            if (value.Length != 10 || !value.All(z => char.IsDigit(z)))
                throw new ArgumentException();
            inn = value;
        }
    }
    
    public DateTime EstablishDate { get; set; }
    public TimeSpan ActiveTimeSpan { get => DateTime.Now - EstablishDate; }
    
    public double GetTotalTransactionsAmount()
    {
        DataBase.OpenConnection();
        var amount = 0.0;
        foreach (Transaction t in DataBase.Transactions().Where(z => z.EnterpriseGuid == Guid))
            amount += t.Amount;
        return amount;
    }
}
```
