# Закон Бенфорда

Вчера Вася узнал про удивительный [закон Бенфорда](https://ru.wikipedia.org/wiki/%D0%97%D0%B0%D0%BA%D0%BE%D0%BD_%D0%91%D0%B5%D0%BD%D1%84%D0%BE%D1%80%D0%B4%D0%B0#%D0%9E%D0%B3%D1%80%D0%B0%D0%BD%D0%B8%D1%87%D0%B5%D0%BD%D0%B8%D1%8F) и хочет проверить его действие. Для этого он взял текст с актуальными данными по самым высоким зданиям в мире и хочет получить статистику: сколько раз каждая цифра стоит на месте старшего разряда в числах из его текста.

Метод GetBenfordStatistics должен вернуть массив чисел, в котором на i-ой позиции находится статистика для цифры i.

Значение переменной tallestBuildings в коде ниже

Самые высокие здания в мире по состоянию на 2020 год

```cs
Burj Khalifa 830
Petronius (oil platform) 640
Tokyo Skytree 634
KVLY-TV mast 629
Canton Tower 604
Abraj Al Bait Towers 601
Bullwinkle (oil platform) 529
Troll A platform 472
Lualualei VLF transmitter 458
Petronas Twin Towers 452
Willis Tower 442
Ekibastuz GRES-Two Power Station 420
Dimona Radar Facility 400
Kiev TV Tower 385
Zhoushan Island Overhead Powerline Tie 370
Gerbrandy Tower 367
TV Tower Vinnytsia 354
Millau Viaduct 342
Amazon Tall Tower Observatory 325
Lakihegy Tower 314
Jinping-I Dam 305
Star Tower 291
H-One Tower 274
Djamaa el Djazaïr 265
Mohammed bin Rashid Al Maktoum Solar Park 262
LR 248
GE wind turbine at Naturstromspeicher Gaildorf 247
Statue of Unity 240
Noble Lloyd Noble 214
Kalisindh Thermal Power Station 202
Gateway Arch 192
Main tower of Kuwait Towers 187
Anaconda Smelter Stack 178
Olympic Stadium 175
San Jacinto Monument 174
Niederaussem Power Station 172
Jeddah Flagpole 171
High Roller 168
Mole Antonelliana 168
Ulmer Münster 162
Vehicle Assembly Building 160
Santa Cruz del Valle de los Caídos 152
Arecibo Telescope 150
Kingda Ka 139
Great Pyramid of Giza 139
Kuala Lumpur International Airport Two Control Tower 141
Zumanjaro: Drop of Doom 139
Kockums Crane 138
Jetavanaramaya 122
Gliwice Radio Tower 118
Gasometer Oberhausen 118
Schapfen Mill Tower 115
Pillar of third section of Gletscherbahn Kaprun 114
Joseph Chamberlain Memorial Clock Tower 100
Éole 96
Mjøstårnet 85
Ericsson Globe 85
Île Vierge Lighthouse 83
Murudeshwara Temple 76
```
Источник: [wikipedia](https://en.wikipedia.org/wiki/List_of_tallest_buildings_and_structures#Tallest_structure_by_category)

```cs
public static void Main()
{
    PrintNumbers(GetBenfordStatistics("1"));
    PrintNumbers(GetBenfordStatistics("abc"));
    PrintNumbers(GetBenfordStatistics("123 456 789"));
    PrintNumbers(GetBenfordStatistics("abc 123 def 456 gf 789 i"));
    PrintNumbers(GetBenfordStatistics(tallestBuildings));
}
```

Все тесты пройдены, задача сдана:
```cs
public static int[] GetBenfordStatistics(string text)
{
    var statistics = new int[10];
	for (int i = 0; i < text.Length; i++)
	{
		if (i == 0)
			if (Char.IsDigit(text[i])) statistics[(int)Char.GetNumericValue(text[i])]++;
		if (i != 0)
			if ((Char.IsDigit(text[i])) && (text[i-1] == ' '))
				statistics[(int)Char.GetNumericValue(text[i])]++;
	}
    return statistics;
}
```

Вывод программы:
```cs
0, 1, 0, 0, 0, 0, 0, 0, 0, 0
0, 0, 0, 0, 0, 0, 0, 0, 0, 0
0, 1, 0, 0, 1, 0, 0, 1, 0, 0
0, 1, 0, 0, 1, 0, 0, 1, 0, 0
0, 24, 9, 8, 6, 1, 5, 1, 4, 1
```
