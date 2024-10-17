# Уровень 3

Крохотные методы в одну-две строки часто могут значительно повысить читаемость кода.

Исходный код:
```cs
//If employee deserves full benefits
if (employee.IsHourly() && employee.Age > 65 || employee.HasSpecialReward))
    Pay(employee, fullBenefitsAmount); //Pay largeAmount
else
    Pay(employee, reducedAmount);
```

Исправленный код:
```cs
if (employee.DeservesFullBenefits()))
    Pay(employee, fullBenefitsAmount); 
else
    Pay(employee, reducedAmount);
```

Объяснения:
- Вместо поясняющего комментария лучше изменить код так, чтобы в комментарии не было нужды.
- Сложные булевы выражения стоит выделять в методы или вспомогательные переменные, давая им говорящие названия. Это улучшит читаемость.
- Комментарии повторяющие код не нужны. Они легко могут устареть!
