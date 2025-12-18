# Интересное о вакансиях

После выполнения прошлой задачи, Вася решил бежать из этой конторы. Ему захотелось чего-то нового, но он не знает, на какую профессию учиться и какие скиллы качать. И тут он осознал, что в прошлой задаче ему дали настоящий список вакансий. Он хочет использовать его в своих целях: проанализировать и решить, на кого учиться.

Вася стал задаваться вопросами: У каких профессий самые высокие и низкие зарплаты? Какие скиллы самые популярные? В каких городах самая высокая ЗП?

Выведи на экран соответствующие рейтинги (смотри формат вывода)

Дополнительные требования:
- Сохраняются все требования к парсингу CSV из прошлой задачи
- Для всей аналитики необходимо учитывать только те вакансии, у которых ЗП в рублях
- Все дробные значения, возникающие при делении, округлять вниз до целого числа
- Сортировку осуществлять только по требуемому параметру, дополнительную сортировку не применять. В случае одинакового значения, порядок должен быть как в CSV-файле
- Выводить не больше 10 позиций в рейтинге
- Все существительные, идущее за числительным, должны иметь соответствующее склонение
- Выводить только те города, в которых число вакансий больше или равно 1% от общего числа рублёвых вакансий (при вычислении 1% применять округление вниз)
- Между отдельными рейтингами пустая строка


**Формат ввода**

В csv-файле используется следующая структура:

```
name,key_skills,premium,employer_name,salary_from,salary_to,salary_currency,area_name
Программист,"Информационные технологии
Работа в команде
Автоматизированное рабочее место (АРМ)”,FALSE,Контур,70000,110000,RUR,Екатеринбург
Инженер,"Ответственность
Работа в команде
Умение работать руками
пунктуальность
Ответственный подход к работе”,FALSE, <p>Сервисный центр ЭКСПЕРТ</p>,30000,60000,RUR,Санкт-Петербург
```

[Скачать пример файла](example_vacancies.zip)

**Формат вывода**

```
Самые высокие зарплаты:
    1) Программист в компании "Контур” - 90000 рублей (г. Екатеринбург)
    2) Инженер в компании "Сервисный центр ЭКСПЕРТ” - 45000 рублей (г. Санкт-Петербург)

Самые низкие зарплаты:
    1) Инженер в компании "Сервисный центр ЭКСПЕРТ” - 45000 рублей (г. Санкт-Петербург)
    2) Программист в компании "Контур” - 90000 рублей (г. Екатеринбург)

Из 7 скиллов, самыми популярными являются:
    1) Работа в команде - упоминается 2 раза
    2) Информационные технологии - упоминается 1 раз
    3) Автоматизированное рабочее место (АРМ) - упоминается 1 раз
    4) Ответственность - упоминается 1 раз
    5) Умение работать руками - упоминается 1 раз
    6) пунктуальность - упоминается 1 раз
    7) Ответственный подход к работе - упоминается 1 раз

Из 2 городов, самые высокие средние ЗП:
    1) Екатеринбург - средняя зарплата 90000 рублей (1 вакансия)
    2) Санкт-Петербург - средняя зарплата 45000 рублей (1 вакансия)
```

|                         |                        |
|:------------------------|:-----------------------|
| **Ограничение времени** | **3 секунды**          |
| **Ограничение памяти**  | **512.0 Мб**           |
| **Ввод**                | **название CSV файла** |
| **Вывод**               | **стандартный вывод**  |


Все тесты пройдены, задача сдана:
```py
import csv
import re
from collections import Counter
import math

def declension(value: int, one, few, many) -> str:
    if 11 <= value % 100 <= 14:
        return many
    elif value % 10 == 1:
        return one
    elif 2 <= value % 10 <= 4:
        return few
    else:
        return many

def normalize_html(text):
    text = re.sub(r"<[^>]+>", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text

def normalize_cell(value):
    if not value.strip():
        return 'Нет данных'
    parts = [normalize_html(part) for part in value.split('\n') if part.strip()]
    return '; '.join(parts) if parts else 'Нет данных'

def parse_csv(filename):
    with open(filename, 'r', encoding='utf-8-sig', newline='') as file:
        csv_reader = csv.reader(file)
        headers = next(csv_reader)

        data = []
        for line in csv_reader:
            record = {}
            for index, value in enumerate(line):
                record[headers[index]] = normalize_cell(value)
            data.append(record)
    return data

def get_average_salary(record):
    try:
        salary_from = float(record['salary_from'])
        salary_to = float(record['salary_to'])
        return int((salary_from + salary_to) // 2)
    except (TypeError, ValueError):
        if record.get("salary_to") == "Нет данных":
            return int(float(record.get("salary_from")) // 2)
        elif record.get("salary_from") == "Нет данных":
            return int(float(record.get("salary_to")) // 2)

def print_highest_salaries(rub_vacancies):
    high_salary = sorted(rub_vacancies, key=lambda x: get_average_salary(x), reverse=True)

    print("Самые высокие зарплаты:")
    for i, record in enumerate(high_salary[:10], 1):
        avg_salary = get_average_salary(record)
        salary_word = declension(avg_salary, "рубль", "рубля", "рублей")
        city = record.get('area_name', 'Нет данных')
        if city.startswith('г. '):
            city = city[3:]
        print(f"    {i}) {record['name']} в компании \"{record['employer_name']}\" - {avg_salary} {salary_word} (г. {city})")

def print_lowest_salaries(rub_vacancies):
    low_salary = sorted(rub_vacancies, key=lambda x: get_average_salary(x))

    print("Самые низкие зарплаты:")
    for i, record in enumerate(low_salary[:10], 1):
        avg_salary = get_average_salary(record)
        salary_word = declension(avg_salary, "рубль", "рубля", "рублей")
        city = record.get('area_name', 'Нет данных')
        if city.startswith('г. '):
            city = city[3:]
        print(f"    {i}) {record['name']} в компании \"{record['employer_name']}\" - {avg_salary} {salary_word} (г. {city})")

def print_most_popular_skills(rub_vacancies):
    skills_counter = Counter()
    for record in rub_vacancies:
        skills = record.get('key_skills', '').split('; ')
        for skill in skills:
            if skill and skill != 'Нет данных':
                skills_counter[skill] += 1

    sorted_skills = sorted(skills_counter.items(), key=lambda x: x[1], reverse=True)
    total_skills = len(skills_counter)
    skill_word = declension(total_skills, "скилла", "скиллов", "скиллов")
    print(f"Из {total_skills} {skill_word}, самыми популярными являются:")
    for i, (skill, count) in enumerate(sorted_skills[:10], 1):
        count_word = declension(count, "раз", "раза", "раз")
        print(f"    {i}) {skill} - упоминается {count} {count_word}")

def print_cities_highest_avg_salaries(rub_vacancies):
    city_stats = {}
    for record in rub_vacancies:
        city = record.get('area_name', '')
        if city.startswith('г. '):
            city = city[3:]

        if city and city != 'Нет данных':
            avg_salary = get_average_salary(record)
            if city not in city_stats:
                city_stats[city] = {'total_salary': 0, 'count': 0}
            city_stats[city]['total_salary'] += avg_salary
            city_stats[city]['count'] += 1

    total_rub_vacancies = len(rub_vacancies)
    min_city_vacancies = math.floor(total_rub_vacancies * 0.01)
    filtered_cities = {city: stats for city, stats in city_stats.items()
                       if stats['count'] >= min_city_vacancies}

    city_avg_salaries = []
    for city, stats in filtered_cities.items():
        avg_salary = math.floor(stats['total_salary'] / stats['count'])
        city_avg_salaries.append((city, avg_salary, stats['count']))

    city_avg_salaries.sort(key=lambda x: x[1], reverse=True)

    total_cities = len(city_stats)
    city_word = declension(total_cities, "города", "городов", "городов")
    print(f"Из {total_cities} {city_word}, самые высокие средние ЗП:")
    for i, (city, avg_salary, count) in enumerate(city_avg_salaries[:10], 1):
        vacancy_word = declension(count, "вакансия", "вакансии", "вакансий")
        salary_word = declension(avg_salary, "рубль", "рубля", "рублей")
        print(f"    {i}) {city} - средняя зарплата {avg_salary} {salary_word} ({count} {vacancy_word})")

def main():
    data = parse_csv(input())
    rub_vacancies = [record for record in data if record.get('salary_currency') == 'RUR']

    print_highest_salaries(rub_vacancies)
    print()
    print_lowest_salaries(rub_vacancies)
    print()
    print_most_popular_skills(rub_vacancies)
    print()
    print_cities_highest_avg_salaries(rub_vacancies)

if __name__ == "__main__":
    main()
```
