# XML-RPC

В отделе разработки, где работает Вася, после великой оптимизации осталось 2 человека. Вася и Кристина. Начальство хочет оставить только одного.

Наступило время эпического противостояния в отделе разработки, которое впишется в летопись компании как легендарное сражение за выживание. Вася и Кристина, два гения программирования, встали друг против друга, словно два титана, готовых бороться за своё место под солнцем в этом царстве кода и инноваций.

Им дают самую сложную задачку, которую только смогли найти! Написать RPC-сервер с помощью библиотеки [xmlrpc](https://docs.python.org/3/library/xmlrpc.client.html), чтобы, как и в предыдущие разы, можно было бы к нему можно обратится через следующий код:

```py
import xmlrpc.client


client = xmlrpc.client.ServerProxy('http://localhost:8000')
while True:
    command = input()

    if command == 'exit':
        method = getattr(client, command)
        method()
        break

    command = command.split()
    method_name = command[0]
    method_value = command[1]

    method = getattr(client, command[0])

    if method_name == 'get_vacancy_by_id':
        result = method(int(method_value))
        print(result)
    elif method_name == 'get_vacancies_by_city':
        result = method(str(method_value))
        print(result)
    elif method_name == 'get_vacancies_by_min_salary':
        result = method(int(method_value))
        print(result)
```

На этот раз работать придется с файлом [vacancies.csv](vacancies.zip)

Предстоит написать четыре метода:

**get_vacancy_by_id**

Возвращает информацию о вакансии по ID (Название вакансии, минимальная зарплата, максимальная зарплата, город)

**Формат ввода:**

```
get_vacancy_by_id 123
```

**Формат вывода:**

```
{'Название вакансии': 'Специалист по продажам', 'Зарплата от': 35000.0, 'Зарплата до': 60000.0, 'Город': 'Москва'}
```

**get_vacancies_by_city**

Возвращает информацию о вакансиях по Городу (ID, Название вакансии, минимальная зарплата, максимальная зарплата, город)

**Формат ввода:**

```
get_vacancies_by_city Курган
```

**Формат вывода:**

```
{'127': {'Название вакансии': 'Менеджер телефонных продаж', 'Зарплата от': 15000.0, 'Зарплата до': 30000.0, 'Город': 'Курган'}, '444': {'Название вакансии': 'Помощник вебмастера', 'Зарплата от': 15000.0, 'Зарплата до': 25000.0, 'Город': 'Курган'}}
```

**get_vacancies_by_min_salary**

Возвращает информацию о вакансиях, у которых зарплата не меньше X (Название вакансии, минимальная зарплата, максимальная зарплата, город)

**Формат ввода:**

```
get_vacancies_by_min_salary 230000
```

**Формат вывода:**

```
{'151': {'Название вакансии': 'Chief Executive Officer', 'Зарплата от': 500000.0, 'Зарплата до': 600000.0, 'Город': 'Москва'}, '477': {'Название вакансии': 'Rust разработчик', 'Зарплата от': 230000.0, 'Зарплата до': 300000.0, 'Город': 'Санкт-Петербург'}}
```

**exit**

Выключает сервер.

Вася не джентльмен. И в благородство он играть не собирается. Помогите Васе написать код и уволить Кристину!

Реализацию exit можно подглядеть [здесь](https://stackoverflow.com/questions/32761913/shutdown-an-simplexmlrpcserver-server-in-python)

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **20 секунд** |
| **Ограничение памяти**  | **512 Мб**    |


Все тесты пройдены, задача сдана:
```py
from xmlrpc.server import SimpleXMLRPCServer
import pandas as pd

def get_vacancy_by_id(id):
    global df
    values = df[df.index == id][["name", "from", "to", "city"]].values.tolist()[0]
    return {
        "Название вакансии": values[0],
        "Зарплата от": values[1],
        "Зарплата до": values[2],
        "Город": values[3],
    }

def get_vacancies_by_city(city):
    return get_vacancies_by("city", city)

def get_vacancies_by_min_salary(salary):
    return get_vacancies_by("from", salary)

def get_vacancies_by(by, param):
    global df
    copy_df = df.copy(deep=True)
    if by == "city":
        copy_df = copy_df[copy_df[by] == param][["name", "from", "to", "city"]]
    else:
        copy_df = copy_df[copy_df[by] >= param][["name", "from", "to", "city"]]
    copy_df.columns = ["Название вакансии", "Зарплата от", "Зарплата до", "Город"]
    d = copy_df.to_dict(orient="index")
    return {str(key): value for (key, value) in d.items()}

def exit():
    global stop_server
    stop_server = True

def start_server():
    global df
    global stop_server
    df = pd.read_csv("vacancies.csv", names=["name", "from", "to", "cur", "city", "date"])
    stop_server = False

    with SimpleXMLRPCServer(("localhost", 8000), allow_none=True) as server:
        server.register_function(get_vacancy_by_id)
        server.register_function(get_vacancies_by_city)
        server.register_function(get_vacancies_by_min_salary)
        server.register_function(exit)
        while not stop_server:
            server.handle_request()
```
