# Models в Django

Продолжайте в том же проекте.

Маршрутизацию реализовали, однако данные по маршрутам берутся не из базы данных, так как были заранее заготовлены для тестирования маршрутов.

Пора это исправить!

Для начала необходимо создать подключение к базе данных. Для этого необходимо реализовать классы моделей в файле `models.py`.

Создайте классы моделей следующим образом:
1. Класс SiteUser:
   - Поле first_name - CharField с максимальной длиной строки 255 символов
   - Поле last_name - CharField с максимальной длиной строки 255 символов
   - Функция get_name выводит имя пользователя: сначала имя (first_name), потом фамилия (last_name) через пробел
   - Класс Meta: название таблицы в базе данных: site_users
2. Класс Vacancy:
   - Поле name - TextField
   - Поле salary - FloatField
   - Поле area_name - TextField
   - Поле published_at - TextField
   - Класс Meta: название таблицы в базе данных: vacancies

Все тесты пройдены, задача сдана:
```py
from django.db import models

class SiteUser(models.Model):
    first_name = models.CharField("Имя", max_length=255)
    last_name = models.CharField("Фамилия", max_length=255)

    def get_name(self):
        return f"{self.first_name} {self.last_name}"
    
    class Meta:
        db_table = "site_users"

class Vacancy(models.Model):
    name = models.TextField()
    salary = models.FloatField()
    area_name = models.TextField()
    published_at = models.TextField()
    
    class Meta:
        db_table = "vacancies"
```
