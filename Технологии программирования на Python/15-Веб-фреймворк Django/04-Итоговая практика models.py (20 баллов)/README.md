# Итоговая практика: models.py

Скачайте [проект](task.zip).

В этой задаче вам необходимо создать 6 моделей для работы с ORM Django, работайте в файле djangotask/models.py.

**Модель MyUser**

Модель пользователя. Она должна иметь следующие поля:
- first_name - Имя
- last_name - Фамилия
- age - Возраст
- email - Электронная почта (Должна происходить валидация)
- password - Пароль

Кроме того, этот класс должен иметь следующие методы:
- get_name(): Возвращает имя и фамилию, разделенные пробелом.
- hash_password(password): Статический метод, хэширующий пароль.
- verify_password(password): Метод экземпляра, проверяющий переданный пароль с захэшированным в базе данных. Перед хэшированием пароль должен быть закодирован при помощи encode().
- verify_age(age): Статический метод валидации возраста (пользователь должен быть от 18 до 122 лет включительно).
- skills: Свойство, возвращающее все навыки пользователя. Возвращаемое значение: список названий навыков.

В этой модели поле email должно быть уникальным.

Название таблицы: my_user

**Модель Vacancy**

Модель вакансии.

Поля:
- name: Название вакансии
- salary: Зарплата
- area_name: Город

Методы класса:
- skills: Свойство, аналогичное из класса MyUser, должно возвращать список названий навыков, необходимых для трудоустройства на эту вакансию.

Название таблицы: vacancy

**Модель Skill**

Модель навыков.

Поля:
- name: Название навыка

Методы класса:
- get_all_skills: Возвращает все существующие навыки в следующем формате: "скилл 1, скилл 2, скилл 3".

Название таблицы: skill

**Модель VacancySkill**

Модель для связи Vacancy и Skill.

Поля:
- vacancy: Внешний ключ, ссылающийся на модель Vacancy
- skill: Внешний ключ, ссылающийся на модель Skill

В этой модели должно быть установлено ограничение уникальности комбинации этих двух полей.

Название таблицы: vacancy_skill

**Модель UserSkill**

Аналогично предыдущей модели, модель для связи модели MyUser с Skill.

Поля: * user: Внешний ключ, ссылающийся на модель MyUser * skill: Внешний ключ, ссылающийся на модель Skill

В этой модели должно быть установлено ограничение уникальности комбинации этих двух полей.

Название таблицы: user_skill

**Модель UserResponse**

Модель отклика пользователя на вакансию.

Поля:
- vacancy: Внешний ключ, ссылающийся на модель Vacancy
- user: Внешний ключ, ссылающийся на модель MyUser
- message: Сопроводительное письмо при отклике

Название таблицы: user_response

**Формат вывода**

В поле для кода вставьте итоговое состояние файла models.py.

Все тесты пройдены, задача сдана:
```py
from django.db import models
import hashlib
import datetime

class MyUser(models.Model):
    first_name = models.CharField("Имя", max_length=64)
    last_name = models.CharField('Фамилия', max_length=64)
    age = models.IntegerField()
    email = models.EmailField(unique=True)
    password = models.CharField('Пароль', max_length=64)
    skills_list = []

    class Meta:
        db_table = "my_user"

    def get_name(self):
        return f'{self.first_name} {self.last_name}'

    @staticmethod
    def hash_password(password):
        return hashlib.sha1(password.encode()).hexdigest()

    def verify_password(self, password):
        return self.password == hashlib.sha1(password.encode()).hexdigest()

    @staticmethod
    def verify_age(age):
        return 18 <= int(age) <= 150

    @property
    def skills(self):
        return [user_skill.skill.name for user_skill in UserSkill.objects.filter(user=self)]

class Vacancy(models.Model):
    name = models.TextField()
    salary = models.IntegerField()
    area_name = models.TextField()
    created_at = models.DateTimeField(default=datetime.datetime.now)
    skills_list = []

    @property
    def skills(self):
        return [vacancy_skill.skill.name for vacancy_skill in VacancySkill.objects.filter(vacancy=self)]

    class Meta:
        db_table = 'vacancy'

class Skill(models.Model):
    name = models.CharField("Имя", max_length=64)

    class Meta:
        db_table = 'skill'
        
    @classmethod
    def get_all_skills(self):
        return list(self.objects.all())

class VacancySkill(models.Model):
    vacancy = models.ForeignKey(Vacancy, on_delete=models.CASCADE)
    skill = models.ForeignKey(Skill, on_delete=models.CASCADE)

    class Meta:
        db_table = 'vacancy_skill'
        unique_together = ('vacancy', 'skill')

class UserSkill(models.Model):
    user = models.ForeignKey(MyUser, on_delete=models.CASCADE)
    skill = models.ForeignKey(Skill, on_delete=models.CASCADE)

    class Meta:
        db_table = 'user_skill'
        unique_together = ('user', 'skill')

class UserResponse(models.Model):
    vacancy = models.ForeignKey(Vacancy, on_delete=models.CASCADE)
    user = models.ForeignKey(MyUser, on_delete=models.CASCADE)
    message = models.TextField()

    class Meta:
        db_table = 'user_response'
```
