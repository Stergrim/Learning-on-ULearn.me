# Профилирование

Коллеги из соседнего отдела начали жаловаться, что код Васи слишком медленный. А он и не знает, где ошибка, функционала-то он написал много. Василий лишь может подозревать, в каком из его модулей закралась ошибка.

Чтобы определить источник проблемы, Васе необходимо выполнить профилизацию всех своих функций. Помогите ему это сделать с помощью библиотеки cProfile!

У Васи есть 5 функций для профилирования: `load_files`, `read_database`, `get_id`, `get_user_data`, `generate_words`. Отпрофилируйте их именно в этом порядке и выведите результаты профилирования в консоль в следующем формате:

```
*время выполнения функции с точностью 4 знака после запятой*: *процент времени выполнения от общего времени выполнения*
```

Статистику профилирования можно получить при помощи класса Stats из библиотеки pstats, передав в него экземпляр профилировщика. В том числе и общее время работы.

**Формат ввода**

```
1.2345: 12%
6.7890: 66%
1.1111: 11%
0.1000: 1%
1.1122: 11%
```

|                         |              |
|:------------------------|:-------------|
| **Ограничение времени** | **5 секунд** |
| **Ограничение памяти**  | **64.0 Мб**  |


Все тесты пройдены, задача сдана:
```py
import cProfile
from functions_to_profile import load_files, read_database, get_id, get_user_data, generate_words
import pstats

TASK_FUNCTIONS_ORDER = [load_files, read_database, get_id, get_user_data, generate_words]

def profile_functions(order):
    with cProfile.Profile() as profile:
        for function in order:
            function()
    stats = pstats.Stats(profile)
    stats = stats.sort_stats('cumulative')
    return stats

def return_stats(stats):
    profilings = []
    total_time = stats.total_tt
    for function in TASK_FUNCTIONS_ORDER:
        name = function.__name__
        for (file, line, fname), data in stats.stats.items():
            if fname == name:
                percent = (data[3] / total_time) * 100
                profilings.append(f"{data[3]:.4f}: {int(round(percent))}%")
                break
    return profilings

def main():
    stats = profile_functions(TASK_FUNCTIONS_ORDER)
    profilings = return_stats(stats)
    print('\n'.join(profilings))

if __name__ == '__main__':
    main()
```
