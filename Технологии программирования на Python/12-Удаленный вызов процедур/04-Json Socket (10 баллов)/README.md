# Json Socket

Первую волну уволнений прошли не все... Отдел поредел на добрые 50%. Но на этом оптимизация отдела Васи ещё не закончилась. Появился ещё один тёмный-тёмный кабинет с яркой-яркой лампой. Оставшимся сотрудникам предстояло снова побороться за своё место в компании!

И вот снова Вася входит в этот кабинет. Жюри всем видом показывает, что Васю очень хотят уволить. И дают ему следующую задачу: сделать сервер, в котором есть несколько методов, и который принимает запросы в формате JSON.

Как и в предыдущий раз, ему дали код клиента, с помощью которого можно обратиться к серверу:

```py
import socket
import json


def send_request(data):
    host = "127.0.0.32"
    port = 12345

    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((host, port))

    data_string = json.dumps(data)
    client_socket.sendall(data_string.encode())

    response = client_socket.recv(1024).decode()
    client_socket.close()

    return response


data = {
    "command": "get_data",
    "operation": "get_website",
    "name": "Bauer-Weiss"
}

response_str = send_request(data)
response_data = json.loads(response_str)
result = response_data.get("result")

print(result)
```

Васе предстоит написать четыре метода:
1. get_website - на название организации возвращает вебсайт.
2. get_country - на название организации возвращает страну.
3. get_number_of_employees - на название организации возвращает количество сотрудников.
4. get_description - на название организации возвращает описание.

Помогите Васе написать эти четыре метода и удержать рабочее место Васи за ним!...

**Формат ввода**

```
{"command": "get_data", "operation": "get_country", "name": "Bauer-Weiss"}
```

**Формат вывода**

```
United States of America
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **20 секунд** |
| **Ограничение памяти**  | **512 Мб**    |


Все тесты пройдены, задача сдана:
```py
import socket
import json
import pandas as pd

def get_website(df, name):
    return str(df[df["Name"] == name].iloc[0]["Website"])

def get_country(df, name):
    return str(df[df["Name"] == name].iloc[0]["Country"])

def get_number_of_employees(df, name):
    return str(df[df["Name"] == name].iloc[0]["Number of employees"])

def get_description(df, name):
    return str(df[df["Name"] == name].iloc[0]["Description"])

def execute_func(df, json_row):
    match json_row["operation"]:
        case "get_website":
            return get_website(df, json_row["name"])
        case "get_country":
            return get_country(df, json_row["name"])
        case "get_number_of_employees":
            return get_number_of_employees(df, json_row["name"])
        case "get_description":
            return get_description(df, json_row["name"])

def start_server():
    df = pd.read_csv("organizations.csv")
    host = "127.0.0.32"
    port = 12345

    server = socket.socket()
    server.bind((host, port))
    server.listen(5)

    client, _ = server.accept()
    while True:
        client_message = client.recv(1024).decode()
        if not client_message:
            break
        json_row = json.loads(client_message)
        result = execute_func(df, json_row)
        client.send(json.dumps({"result": result}).encode())

    client.close()
    server.close()
```
