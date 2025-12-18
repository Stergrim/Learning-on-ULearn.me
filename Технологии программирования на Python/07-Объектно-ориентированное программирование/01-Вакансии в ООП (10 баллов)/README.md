# Вакансии в ООП

После финальных правок от старшего аналитика Василий отправил свою программу на код-ревью для старших коллег-программистов, чтобы можно было запустить программу в эксплуатацию.

Как и следовало ожидать, Василию прилетело много замечаний по архитектуре его программы.

Основное замечание было к тому, что без использования ООП, такую программу в дальнейшем будет сложно расширять и масштабировать. Поэтому Васе предложили для начала превратить все вакансии в csv-файле в объекты некого класса `Vacancy`. Чтобы убедиться, что Вася сделает всё правильно, они сказали ему вывести архитектуру объектов с помощью библиотеки `var_dump`
1. На ввод подается название csv-файла.
2. На вывод подается результат работы функции `var_dump` в библиотеке `var_dump`.
3. Должно быть реализовано два класса: `Vacancy` и `Salary`.
4. `Vacancy` состоит из:
   - Строка name
   - Строка description
   - Строка key_skills
   - Строка experience_id
   - Строка premium
   - Строка employer_name
   - Объект salary
   - Строка area_name
   - Строка published_at
5. `Salary` состоит из следующих полей:
   - Строка salary_from
   - Строка salary_to
   - Строка salary_gross
   - Строка salary_currency


**Формат ввода**

```
vacancies.csv
```

[Скачать пример файла](example_vacancies.zip)

**Формат вывода**

```
#0 list(1)
    [0] => object(Vacancy) (9)
        name => str(51) "Оперативный дежурный (инженер) ситуационного центра"
        description => str(932) "Обязанности:  мониторинг теле- и радиовещания мониторинг интернет-сайтов, электронной почты работа с массивами данных (сбор, обработка и системный анализ) подготовка справок, докладов, отчетов и презентаций контроль маршрутов перемещения автотранспорта контроль состояния датчиков охранной и пожарной сигнализации оперативное оповещение и информирование  Требования:  высшее образование в сфере связи опыт работы не менее 3 лет знание основ построения сетей связи владение ПК на уровне опытного пользователя (пакет Ms.Office) высокий уровень навыков работы с оргтехникой и средствами связи  Условия:  сменный график работы (сутки через двое) оформление в строгом соответствии с ТК РФ ежемесячные выплаты к должностному окладу  добровольное медицинское страхование дополнительные оплачиваемые выходные дни (5 к/дней в году) материальная помощь к ежегодному отпуску иные гарантии и компенсации в соответствии с коллективным договором "
        key_skills => str(185) "MS Word, MS Excel, MS Visio, MS Internet Explorer, MS Outlook, Исполнительность, Внимательность, Ответственность, Дисциплинированность, Обучаемость, Способность быстро принимать решения"
        experience_id => str(13) "От 3 до 6 лет"
        premium => str(3) "Нет"
        employer_name => str(10) "ГРЧЦ, ФГУП"
        salary => object(Salary) (4)
            salary_from => str(5) "48300"
            salary_to => str(5) "48300"
            salary_gross => str(18) "Без вычета налогов"
            salary_currency => str(5) "Рубли"
        area_name => str(15) "Санкт-Петербург"
        published_at => str(19) "11:44:58 14/06/2022"
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **1 секунда** |
| **Ограничение памяти**  | **64.0 Мб**   |


Все тесты пройдены, задача сдана:
```py
import csv
from var_dump import var_dump

def csv_reader(file_name):
    with open(file_name, 'r', encoding='utf-8-sig', newline='') as file:
        reader = csv.DictReader(file)
        titles = reader.fieldnames
        vacancies = list(reader)

    return titles, vacancies

class Vacancy:
    def __init__(self, vacancy, salary_obj):
        self.name = vacancy.get('name', '')
        self.description = vacancy.get('description', '')
        self.key_skills = vacancy.get('key_skills', '')
        self.experience_id = vacancy.get('experience_id', '')
        self.premium = vacancy.get('premium', '')
        self.employer_name = vacancy.get('employer_name', '')
        self.salary = salary_obj
        self.area_name = vacancy.get('area_name', '')
        self.published_at = vacancy.get('published_at', '')

class Salary:
    def __init__(self, vacancy):
        self.salary_from = vacancy.get('salary_from', '')
        self.salary_to = vacancy.get('salary_to', '')
        self.salary_gross = vacancy.get('salary_gross', '')
        self.salary_currency = vacancy.get('salary_currency', '')

def main():
    filename = input()
    titles, vacancies = csv_reader(filename)
    result_vacancies = []
    for vacancy in vacancies:
        salary_obj = Salary(vacancy)
        vacancy_obj = Vacancy(vacancy, salary_obj)
        result_vacancies.append(vacancy_obj)
    var_dump(result_vacancies)

if __name__ == '__main__':
    main()
```
