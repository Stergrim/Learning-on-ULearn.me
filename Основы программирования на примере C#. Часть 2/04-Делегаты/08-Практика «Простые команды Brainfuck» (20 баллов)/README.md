# Практика «Простые команды Brainfuck»

Продолжайте работу в том же проекте [brainfuck](brainfuck.zip).

Изучите класс Brainfuck.cs, в частности то, как он использует реализованный ранее класс VirtualMachine.

В классе BrainfuckBasicCommands реализуйте метод, регистрирующий следующие простые команды в виртуальную машину:

| **Символ** | **Значение**                                                                              |
|------------|-------------------------------------------------------------------------------------------|
| .  | Вывести байт памяти, на который указывает указатель, преобразовав в символ согласно ASCII |
| +  | Увеличить байт памяти, на который указывает указатель                                     |
| -  | Уменьшить байт памяти, на который указывает указатель                                     |
| ,  | Ввести символ и сохранить его ASCII-код в байт памяти, на который указывает указатель     |
| >  | Сдвинуть указатель памяти вправо на 1 байт                                                |
| <  | Сдвинуть указатель памяти влево на 1 байт                                                 |
| A-Z, a-z, 0-9  | сохранить ASCII-код этого символа в байт памяти, на который указывает указатель           |


Например, программа `++>+++.<.` выводит два символа с ASCII кодами 2 и 3, а память после выполнения команды будет выглядеть так [2, 3, 0, 0, ... 0].

Для ввода и вывода используйте переданные в метод Run функции `Func<int> read` и `Action<char> write`.

Тут `read` по аналогии с `Console.Read` возвращает либо код введенного символа, либо -1, если ввод закончился. Считайте, что на вход будут подаваться только символы с кодами 0..255 — они точно помещаются в один байт.

Детали реализации инструкций восстановите по тестам в классе BrainfuckBasicCommandsTests. Сделайте так, чтобы все тесты в этом файле проходили.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace func.brainfuck
{
    public class BrainfuckBasicCommands
    {
        public static void RegisterTo(IVirtualMachine vm, Func<int> read, Action<char> write)
        {
            vm.RegisterCommand('.', b => write(Convert.ToChar(vm.Memory[vm.MemoryPointer])));
            vm.RegisterCommand('+', b => { 
                int a = vm.Memory[vm.MemoryPointer]; a++; if (a > 255) a = 0; 
                vm.Memory[vm.MemoryPointer] = Convert.ToByte(a);
            });
            vm.RegisterCommand('-', b => {
                int a = vm.Memory[vm.MemoryPointer]; a--; if (a < 0) a = 255; 
                vm.Memory[vm.MemoryPointer] = Convert.ToByte(a);
            });
            vm.RegisterCommand(',', b => vm.Memory[vm.MemoryPointer] =
                               Convert.ToByte(read().ToString()));
            vm.RegisterCommand('>', b => {
                vm.MemoryPointer++;
                if (vm.MemoryPointer >= vm.Memory.Length) vm.MemoryPointer = 0;
            });
            vm.RegisterCommand('<', b => {
                vm.MemoryPointer--;
                if (vm.MemoryPointer < 0) vm.MemoryPointer = vm.Memory.Length-1;
            });
            var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
            foreach(var c in chars)
                vm.RegisterCommand(c, b => vm.Memory[vm.MemoryPointer] = Convert.ToByte(c));
        }
    }
}
```
