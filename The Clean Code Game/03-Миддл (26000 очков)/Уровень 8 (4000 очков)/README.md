# Уровень 8

Время закрепить освоенные знания!

Исходный код:
```cs
///<summary>Обрабатывает столкновение героя с врагом</summary>
void CollisionHandler(GameObject hero, GameObject enemy)
{
    //If hero and enemy collided
    if ((hero.X-enemy.X)*(hero.X-enemy.X) + (hero.Y-enemy.Y)*(hero.Y-enemy.Y) 
        < (hero.Radius + enemy.Radius)*(hero.Radius + enemy.Radius))
    {
        hero.Life--;
        if (!hero.IsAlive && OnHeroDeath != null) //нужно оповестить подписчиков
            OnHeroDeath(hero);
    }
}
```

Исправленный код:
```cs
void HandleCollision(GameObject hero, GameObject enemy)
{
    if (Collided(hero, enemy))
    {
        hero.Life--;
        if (!hero.IsAlive && OnHeroDeath != null) 
            OnHeroDeath(hero);
    }
}
```

Объяснения:
- XML-комментарии бессмысленны, если не несут новую информацию.
- Не используйте комментарии там, где можно выделить метод с говорящим именем.
- Методы — это действия, называйте их глаголами или глагольными фразами.
- Комментарии дословно повторяющие код бессмысленны.
