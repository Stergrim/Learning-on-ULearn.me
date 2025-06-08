# Stayin' Alive

Мы выяснили, что Haskell — нестрогий язык. Хотя в нём есть некоторые операции, запускающие вычисление, например, pattern matching. Если какое-то ещё невычисленное значение, нужно сопоставить с образцом, то оно будет вычислено настолько, насколько нужно:

```hs
--            v сопоставление с 0
matchWithZero 0 = "This is zero"
matchWithZero _ = "This is not zero"
```

```hs
-- это выражение  v  придётся вычислить, чтобы сравнить с 0
> matchWithZero (1 + undefined)
"*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\Err.hs:79:14 in base:GHC.Err
  undefined, called at EvaluationOrder.hs:77:16 in main:Main
```

Но и тут скрываются [неожиданности](https://wiki.haskell.org/Newtype#The_messy_bits)!

Посмотрите на объявление следующих типов и функций:

```hs
data Foo = Foo Int
data BangedFoo = BangedFoo !Int
newtype Bar = Bar Int

matchFoo :: Foo -> String
matchFoo (Foo _) = "Still alive!"

matchBangedFoo :: BangedFoo -> String
matchBangedFoo (BangedFoo _) = "Still alive!"

matchBar :: Bar -> String
matchBar (Bar _) = "Still alive!"

whatEver :: Int -> String
whatEver _ = "Still alive!"
```

И скажите, какое из значений, перечисленных ниже, [останется в живых](https://www.youtube.com/watch?v=fNFzfwLM72c).

1. Выбери строчки, которые при выполнении выведут "Still alive!" (5 из 5 баллов)
   * ❌ **matchFoo undefined** (Чтобы убедиться, что аргумент создан с помощью конструктора Foo, нужно начать вычисление аргумента.)
   * ✅ **matchFoo (Foo undefined)** (Правильно! Конструктор Foo ведёт себя лениво, не выполняет свой аргумент. А для pattern matching достаточно убедиться, что значение — Foo.)
   * ❌ **matchBangedFoo undefined** (`!` не успевает ни на что повлиять. Поведение не отличается от `matchFoo undefined`.)
   * ❌ **matchBangedFoo (BangedFoo undefined)** (Раз есть `!`, значит аргумент конструктора нужно выполнить.)
   * ✅ **matchBar undefined** (Правильно! В runtime newtype исчезает, matchBar становится таким же, как whatEver, а `BangedFoo undefined` становится просто`undefined`.)
   * ✅ **matchBar (Bar undefined)** (Правильно! В runtime newtype исчезает, matchBar становится таким же, как whatEver.)
   * ✅ **whatEver undefined** (Правильно! Всё соответствует __, поэтому значение можно не выполнять.)
