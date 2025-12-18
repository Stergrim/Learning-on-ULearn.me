# Simple XML-RPC Server

Вы узнали что такое XML-RPC. Попробуйте написать простой сервер, с помощью которого можно будет получать корень из дискриминанта при заданных `a`, `b`, `c`.

Пример клиентского сервера:

```py
import xmlrpc.client

server = xmlrpc.client.ServerProxy('http://127.0.0.32:12345')

a, b, c = map(int, input().split(' '))

result = server.get_sqrt_of_discriminant(a, b, c)
print(result)
```

Гарантируется, что `a`, `b`, `c` - целые числа, а корень от дискриминанта, образованный из этих чисел - неотрицательное целое число.

**Формат ввода:**

```
1 -3 2
```

**Формат вывода:**

```
1
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **10 секунд** |
| **Ограничение памяти**  | **128 Мб**    |


Все тесты пройдены, задача сдана:
```py
from xmlrpc.server import SimpleXMLRPCServer
import math

host = "127.0.0.32"
port = 12345

with SimpleXMLRPCServer((host, port)) as server:

    def get_sqrt_of_discriminant(a, b, c):
        return math.sqrt(b * b - 4 * a * c)

    server.register_function(get_sqrt_of_discriminant)
    server.serve_forever()
```
