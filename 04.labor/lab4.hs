import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
negyzet n = take n [i^2|i<-[2, 4 ..]]

negyzet2 n = take n $ map (\i -> i ^ 2) [2,4..]

-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,
szamokLs 1 = replicate 1 1
szamokLs n = szamokLs(n-1) ++ replicate n n

--meghivas szamokLs2 4 1
szamokLs2 n i
    |i/=n = replicate i i ++ szamokLs2 n (i+1)
    |otherwise = replicate i i
-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,

szamokLsszer2 n i
    |i/=n = replicate i (i*2) ++ szamokLsszer2 n (i+1)
    |otherwise = replicate i (i*2)
-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,

szamokLs5 n = [n,n-1..1]++[1..n]
-- - váltakozva tartalmazzon True és False értékeket,
tfLista n = take n ls
    where ls = [even i | i<-[0..]]

tfLista2 n = take n [even i | i<-[0..]]
-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.

valtakozo n = take n ls
    where ls = [0,1,-1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok x = length [i | i<- [1..x], mod x i==0]
-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
paratlanosztok x = last [i | i<- [1..x], mod x i==0 && mod i 2==1]
-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,
-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
-- - tükrözi egy lista elemeit,
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.
