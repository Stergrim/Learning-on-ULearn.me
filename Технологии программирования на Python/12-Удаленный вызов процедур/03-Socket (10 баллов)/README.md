# Socket

Компания, в которой работает Вася, начала получать убытки. Поэтому, решила провести оптимизацию и уволить лишних сотрудников.

У каждого сотрудника решили проверить знания. Начали по одному заводить в тёмную-тёмную комнату, ставить перед лицом яркую-яркую лампу и спрашивать всякое. Все, кто туда входил, почему-то выходили очень грустными...

И вот, когда очередь дошла до Васи. Его завели в тёмную-тёмную комнату, включили яркую-яркую лампу (так, чтобы она его слепила), и добровольно принудительно предложили разработать свой сервер с помощью библиотеки [Socket](https://docs.python.org/3/library/socket.html).

Да не абы какой, а такой, чтобы он мог отвечать на следующий код:

```py
import socket


def start_client():
    host = "127.0.0.32"
    port = 12345

    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((host, port))

    while True:
        message = input() # Сюда подается имя организации из organizations.csv
        client_socket.sendall(message.encode())
        data = client_socket.recv(1024).decode()
        print(data)
        if message == "exit":
            break

    client_socket.close()


if __name__ == "__main__":
    start_client()
```

Васе, помимо этого кода, дают еще и [organizations.csv](organizations.zip), с которой ему предстоит работать.

Сервер должен получать имя организации и возвращать её вебсайт и страну.

Взаимодействие с сервером всегда заканчивается командой *exit*.

**Формат ввода**

```
Farmer, Edwards and Andrade
exit
```

**Формат вывода**

```
Сайт: http://wolfe-boyd.com/. Страна: Norfolk Island
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **20 секунд** |
| **Ограничение памяти**  | **512 Мб**    |


Все тесты пройдены, задача сдана:
```py
import socket
import pandas as pd

def start_server():
    df = pd.read_csv("organizations.csv")
    host = "127.0.0.32"
    port = 12345
    server = socket.socket()
    server.bind((host, port))
    server.listen(5)

    client, _ = server.accept()
    while True:
        organization_name = client.recv(1024).decode()
        if organization_name == "exit":
            break
        data = df[df["Name"] == organization_name][["Website", "Country"]].to_dict(orient="records")[0]
        client.send(f"Сайт: {data['Website']}. Страна: {data['Country']}".encode())

    client.close()
    server.close()
```
