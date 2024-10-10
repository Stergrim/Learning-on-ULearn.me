# Управление роботом

В воскресенье Вася пошел в кружок робототехники и увидел там такой код управления боевым роботом:

```cs
public static bool ShouldFire(bool enemyInFront, string enemyName, int robotHealth)
{
    bool shouldFire = true;
    if (enemyInFront == true)
    {
        if (enemyName == "boss")
        {
            if (robotHealth < 50) shouldFire = false;
            if (robotHealth > 100) shouldFire = true;
        }
    }
    else
    {
        return false;
    }
    return shouldFire;
    }
```

Код показался Васе слишком длинным, а к тому же еще и неряшливым. Вася поспорил с автором, что сможет написать функцию, делающую ровно то же самое, но всего в один оператор.

Кажется, Вася погорячился... Или нет? Помогите ему не проиграть в споре!

Все тесты пройдены, задача сдана:
```cs
static bool ShouldFire2(bool enemyInFront, string enemyName, int robotHealth)
{
    return enemyInFront && (enemyName != "boss" || robotHealth  >= 50);
}
```

Вывод программы:
```cs
Functions are the same!
```
