-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength2 [] res = res
myLength2 (x:xs) res = myLength2 xs (res + 1)

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myProduct2 ls = foldr(\x -> (*) x) ls
-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [x] = x
myMinimum (x1 :x2 : xs) = if x1<x2 then myMinimum(x1 :xs) else myMinimum(x2:xs)

myMinimum4 ls = minimum ls
-- - meghatározza egy lista legnagyobb elemét (myMaximum),
myMaximum [x] = x
myMaximum (x1 :x2 : xs) = if x1>x2 then myMaximum(x1 :xs) else myMaximum(x2:xs)

-- - meghatározza egy lista n-ik elemét (!!),

-- - egymásután fűzi a paraméterként megadott két listát (++),
-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,
-- - meghatározza egy egész szám számjegyeinek listáját,
-- - a lista első elemét elköltözteti a lista végére,
-- - meghatározza egy egész elemű lista elemeinek átlagértékét,
-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.
aLs = [3,-2,5,-7]

x0=2

poli [] _ = 0
poli(a : aLs) x = a + x * (poli aLs x)

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.

type Pont = (Double,Double)

lsP :: [Pont]
lsP = [(3.4,2.4),(1.3,2.8),(2.6,5.3)]


p :: Pont
p = (3.4,1.7)


tavolsag (x1,y1) (x2,y2) = sqrt ((x1-x2) ** 2+(y1-y2)**2)

minTavolsag lsP p = foldl1 aux lsP
    where
        aux p1 p2 = if tavolsag p1 p <tavolsag p2 p then p1 else p2

minTavolsag2 [] _ = error "Ures lista"
minTavolsag2 [p1] _ = p1
minTavolsag2 (p1 : p2: lsP) p
    | tavolsag p1 p < tavolsag p2 p = minTavolsag2 (p1 :lsP) p
    | otherwise = minTavolsag2 (p2 : lsP) p






