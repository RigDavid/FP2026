{- I. Könyvtárfüggvények használata nélkül,
definiáljuk azt a függvényt, amely meghatározza: -}

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat n = (mod n 10) * szjSzorzat (div n 10)

szjSzorzat2 n
  | n < 0 = szjSzorzat2 (abs n)
  | div n 10 == 0 = mod n 10
  | otherwise = mod n 10 * szjSzorzat2 (div n 10)

ls1 = [324, 56, 98, 72, -1, 0]

szjSzorzatLs ls = map szjSzorzat2 ls

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg n
  | n < 0 = szjOsszeg (abs n)
  | div n 10 == 0 = mod n 10
  | otherwise = mod n 10 + szjOsszeg (div n 10)

szjOsszeg2 n res
  | n < 0 = szjOsszeg2 (abs n) res
  | n < 10 = res + n
  | otherwise = szjOsszeg2 (div n 10) (res + mod n 10)

szjOsszegLs ls = map szjOsszeg ls

szjOsszegLs2 ls = map (\x -> (x, szjOsszeg2 x 0)) ls

-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam n
  | n < 0 = szjSzam (abs n)
  | div n 10 == 0 = 1
  | otherwise = 1 + szjSzam (div n 10)

szjSzam2 n res
  | n < 0 = szjSzam2 (abs n) res
  | n < 10 = res + 1
  | otherwise = szjSzam2 (div n 10) (res + 1)

szjSzamLs ls = map szjSzam ls

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
szamOsszeg n szj
  | szj >= 10 = error "nem szj"
  | div n 10 == 0 = if utolsoSzj == szj then szj else 0
  | otherwise = if utolsoSzj == szj then szj + szamOsszeg (div n 10) szj else szamOsszeg (div n 10) szj
  where
    utolsoSzj = mod n 10

szamOsszeg2 n szj res
  | szj >= 10 = error "nem szj."
  | n < 10 = if n == szj then (res + 1) * szj else res * szj
  | otherwise = if mod n 10 == szj then szamOsszeg2 (div n 10) szj (res + 1) else szamOsszeg2 (div n 10) szj res

ls2 = [(577723707, 7), (0, 1), (2847, 2)]

szamOsszegLs = map (uncurry szamOsszeg) ls2

-- - egy szám páros számjegyeinek számát,
parosSzj n
  | n < 0 = parosSzj (abs n)
  | n < 10 = if even n then 1 else 0
  | otherwise = if even (mod n 10) then 1 + parosSzj (div n 10) else parosSzj (div n 10)

parosSzjLs = map parosSzj ls1

-- - egy szám legnagyobb számjegyét,
lgSzj n lg
  | n < 0 = lgSzj (abs n) lg
  | n < 10 = max lg n
  | otherwise = if mod n 10 > lg then lgSzj (div n 10) (mod n 10) else lgSzj (div n 10) lg

lgSzjLs = map (\x -> lgSzj x (-1)) ls1

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```
bSzamdSzj n b d
  | n < 0 = error "neg. szam"
  | n < b = if n == d then 1 else 0
  | otherwise = if mod n b == d then 1 + bSzamdSzj (div n b) b d else bSzamdSzj (div n b) b d

ls3 = [(7673573, 10, 7), (1024, 2, 1), (1023, 2, 1), (345281, 16, 4)]

bSzamdSzjLs = map (\(n, b, d) -> bSzamdSzj n b d) ls3

-- - az 1000-ik Fibonacci számot.
fiboN n = fibo 0 1 0 n
  where
    fibo _ _ res 0 = res
    fibo a b res n1 = fibo b res (b + res) (n1 - 1)

fiboSg a b res n
  | n == 0 = res
  | otherwise = fiboSg b res (b + res) (n - 1)

fiboSzamok n = map (fiboSg 0 1 0) [0 .. n]

fiboN2 n = (fiboSzamok n) !! n

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.