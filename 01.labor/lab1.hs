import Language.Haskell.TH (prim)
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
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

hatvanyXN x n
    | n<0 = error "neg kitevo"
    | otherwise = x ** n

hatvanyXN2 x n
    | n<0 = error "neg kitevo"
    | otherwise = x ^ n

hatvanyXN3 x n
    | n<0 = error "neg kitevo"
    | n==0 = 1
    | otherwise = x * hatvanyXN3 x (n-1)

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyok n =[sqrt i | i<-[1..n]]
-- - az első n négyzetszámot,
negyzetszam n = [i**2 | i<-[1..n]]
-- - az első n természetes szám köbét,
kob n = [i^3 | i<-[1..n]]
-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzet n = [i | i<-[1..n], (sqrt i^2) /= i]
-- - x hatványait adott n-ig,
hatvanyok x n = [x^i | i<-[0..n]]

-- - egy szám páros osztóinak listáját,
parosOsztok x =[i | i <- [1..x], mod x i==0, mod i 2 == 0]
-- - n-ig a prímszámok listáját,

osztok x = [i | i <- [1..x], mod x i == 0]

primszam x = osztok x == [1,x]

primSzamokN n = [i | i <- [2..n], primszam i]

primSzamokN2 n = [i | i<-[2..n], primszamL i]
    where
        primszamL ns = osztokL ns == [1,ns]
        osztokL ns2 = [i | i <- [1..ns2], mod ns2 i == 0]
-- - n-ig az összetett számok listáját,
oszetett n = [i| i<-[1..n], primszam i == False]

-- - n-ig a páratlan összetett számok listáját,
paratlanOszetett n = [i| i<-[1, 3..n], not (primszam i)]

-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a,b,c) | c<-[1..n], b<-[1..c], a<-[1..b], a**2 + b**2 == c**2]


-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a'..'z'] [0..]

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok = zip [0..5] [5,4..0]

szamok1 n = zip [0..n] [n, n-1..0]

szamok2 n = [(i, n-i) | i<-[0..n]]
-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
trueFalse n = [even i | i<-[0..n-1]]

tfLista n = take n ls
    where ls = [even i | i<-[0..]]
    




