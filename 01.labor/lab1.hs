-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Num a => a -> a -> a
osszeg a b = a+b
kulonbseg :: Num a => a -> a -> a
kulonbseg a b = a-b
szorzat :: Num a => a -> a -> a
szorzat a b = a*b

hanyados :: Fractional a => a -> a -> a
hanyados a b = a/b

hanyados_2 :: Integral a => a -> a -> a
hanyados_2 = div

osztmar :: Integral a => a -> a -> a
osztmar a b = a `mod` b

-- - egy első fokú egyenlet gyökét,

egyenlet :: Fractional a => a -> a -> a
egyenlet a b = (-b)/a
-- - egy szám abszulút értékét,

abszolut :: (Ord a, Num a) => a -> a
abszolut a
    | a <0 = -a
    | otherwise = a


-- - egy szám előjelét,

elojel :: (Ord a, Num a) => a -> String
elojel a
    | a < 0 = "negativ"
    | otherwise = "pozitiv"
-- - két argumentuma közül a maximumot,
maximum_2 :: Ord a => a -> a -> a

maximum_2 a b
    | a > b = a
    | otherwise = b

-- - két argumentuma közül a minimumot,

minimum_2 :: Ord a => a -> a -> a
minimum_2 a b
    | a < b = a
    | otherwise = b


-- - egy másodfokú egyenlet gyökeit,
-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
-- - az n szám faktoriálisát (3 módszer),
-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
-- - az első n négyzetszámot,
-- - az első n természetes szám köbét,
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
-- - x hatványait adott n-ig,
-- - egy szám páros osztóinak listáját,
-- - n-ig a prímszámok listáját,
-- - n-ig az összetett számok listáját,
-- - n-ig a páratlan összetett számok listáját,
-- - az n-nél kisebb Pitágorászi számhármasokat,
-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.




