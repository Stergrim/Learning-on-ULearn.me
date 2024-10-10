# Практика «Счет из отеля»

[Скачайте проект](HotelAccounting.zip)

В файле AccountingModel.cs создайте класс AccountingModel, унаследованный от ModelBase, со следующими свойствами.
- double Price — цена за одну ночь. Должна быть неотрицательной.
- int NightsCount — количество ночей. Должно быть положительным.
- double Discount — скидка в процентах. Никаких дополнительных ограничений.
- double Total — сумма счёта. Должна быть не отрицательна и должна быть согласована с предыдущими тремя свойствами по правилу: Total == Price * NightsCount * (1-Discount/100).

Все поля должны иметь сеттеры. При установке Price, NightsCount и Discount должна соответствующим образом устанавливаться Total, при установке Total — соответствующим образом меняться Discount. В случае установки значения, нарушающего хоть одно условие целостности, необходимо выкидывать ArgumentException.

При изменении значения любого свойства необходимо дополнительно сигнализировать об этом с помощью вызова метода Notify, передавая ему имя изменяемого свойства. Здесь можно воспользоваться ключевым словом nameof.

**Обсуждение**

Ситуация, когда свойства вот так взаимозависимы и при этом каждое имеет сеттер, довольно редка. В норме, Total бы имело только геттер. Однако, классы такого вида тоже встречаются, например в качестве класса-модели для пользовательского интерфейса. Библиотека WPF, Avalonia и некоторые другие библиотеки для построения пользовательских интерфейсов дают возможность так называемого двухстороннего связывания контролах окна со свойствами в объекте модели. При изменении значения в контролах меняются значения свойств объекта, и наоборот, при изменении свойств меняются значения в контролах.

Загляните в файлы MainWindow.xaml и MainWindow.cs, вы увидите, что там нет кода для обновления полей, пересчёта значений и выделения поля с ошибочным вводом. Убедитесь что там нет этой логики ведь формат Xaml скорее всего вам знаком с прошлых практик. Зато можно увидеть как устанавливается связь между полями графического интерфейса и свойствами модели. Такой подход называется View-ViewModel, в котором MainWindow — это View, а сам класс AccountingModel — ViewModel.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HotelAccounting
{
    public class AccountingModel : ModelBase
    {
        private double price;
        private double discount;
        private int nightsCount;
        private double total;
        
        public AccountingModel()
        {
            price = 0;
            nightsCount = 1;
            discount = 0;
            total = 0;
        }
    
        public double Price
        {
            get => price;
            set
            {
                price = SetPrice(value);
                Notify(nameof(Price));
                Notify(nameof(Total));
            }
        }
    
        public int NightsCount
        {
            get => nightsCount;
            set
            {
                nightsCount = SetNightsCount(value);
                Notify(nameof(NightsCount));
                Notify(nameof(Total));
            }
        }
    
        public double Discount
        {
            get => discount;
            set
            {
                discount = SetDiscount(value);
                Notify(nameof(Discount));
                Notify(nameof(Total));
            }
        }
    
        public double Total
        {
            get => total;
            set
            {
                total = SetTotal(value);
                Notify(nameof(Total));
                Notify(nameof(Discount));
            }
        }
    
        private double SetPrice(double price)
        {
            total = price * nightsCount * (1 - (double)(discount / 100));
            return price < 0 ? throw new ArgumentException() : price;
        }
    
        private int SetNightsCount(int nightsCount)
        {
            total = price * nightsCount * (1 - (double)(discount / 100));
            return nightsCount <= 0 ? throw new ArgumentException() : nightsCount;
        }
    
        private double SetDiscount(double discount)
        {
            total = price * nightsCount * (1 - (double)(discount / 100));
            if (total < 0) { throw new ArgumentException(); }
            return discount;
        }
    
        private double SetTotal(double total)
        {
            if (total < 0) { throw new ArgumentException(); }
            if (price == 0) { discount = 100; }
            discount = 100 * (1 - (double)(total / (price * nightsCount)));
            return total;
        }
    }
}
```
