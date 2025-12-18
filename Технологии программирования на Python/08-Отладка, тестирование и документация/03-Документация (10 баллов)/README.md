# Документация

Вася отправил свой код на код-ревью наставнику, ожидая похвалы и премии, но получил в ответ вот что: "Василий, где документация к вашим функциям? Вообще же ничего не понятно! Тому, кто будет пользоваться вашим модулем придётся переходить в него и читать ваши комментарии! Так никто не делает. Переделайте ваш код, добавив в него документацию при помощи **docstring!**"

Помогите Васе задокументировать его функции!

Документацию составьте по комментариям Василия, код менять не надо. Формат документации смотрите в примерах ниже.

**Формат ввода**

```
def example(a, b, c):
    # function example
    # a - name
    # b - phone
    # c - email
    return do_something()  # user ip address
```

**Формат вывода**

```
def example(a, b, c):
    """
    function example
    :param a: name
    :param b: phone
    :param c: email
    :return: user ip address
    """
    return do_something()
```

|                         |               |
|:------------------------|:--------------|
| **Ограничение времени** | **1 секунда** |
| **Ограничение памяти**  | **64.0 Мб**   |


Все тесты пройдены, задача сдана:
```py
import random

def get_sum(a, b):
    """
    Sums up two numbers
    :param a: first number
    :param b: second number
    :return: sum of a and b
    """
    return a + b

def generate_words(count, word_len, alpha):
    """
    Generates list of random words
    :param count: word count
    :param word_len: word length
    :param alpha: alphabet
    :return: list of random words
    """
    return [''.join([random.choice(alpha) for _ in range(word_len)]) for _ in range(count)]

def get_id(length):
    """
    Generates random id
    :param length: id length
    :return: generated random id
    """
    return ''.join(map(str, [random.randint(0, 9) for _ in range(length)]))

def register_new_user(name, age, email):
    """
    Register new user in system
    :param name: user name
    :param age: user age
    :param email: user email
    :return: registered user
    """
    return MyUser(name, age, email)

class MyUser:
    def __init__(self, name, age, email):
        """
        initialisation of class
        :param name: user name
        :param age: user age
        :param email: user email
        """
        self.name = name
        self.age = age
        self.email = email
        self.user_id = get_id(6)

    def get_card(self):
        """
        Get user data
        :return: user data in string format
        """
        return f'{self.user_id}: {self.name} ({self.age})'

    def upload_to_database(self):
        """
        Upload user data to database
        :return: None
        """
        raise NotImplementedError
```
