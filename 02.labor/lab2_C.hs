-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat x = mod x 10 * szjSzorzat (div x 10)

szjSzorzat2 x
  | x < 0 = szjSzorzat2 (abs x)
  | div x 10 == 0 = x
  | otherwise = mod x 10 * szjSzorzat2 (div x 10)

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg x
  | x < 0 = szjOsszeg (abs x)
  | div x 10 == 0 = x
  | otherwise = mod x 10 + szjOsszeg (div x 10)

-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam x
  | x < 0 = szjSzam (abs x)
  | div x 10 == 0 = 1
  | otherwise = 1 + szjSzam (div x 10)

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
-- pl. meghivasra szjElof 577723707 7 0
szjElof x szj elof
  | x < 0 || szj < 0 = error "neg bemenet"
  | szj > 9 = error "nem szj"
  | div x 10 == 0 = elof * szj
  | otherwise = if mod x 10 == szj then szjElof (div x 10) szj (elof + 1) else szjElof (div x 10) szj elof

szjElof2 x szj
  | x < 0 || szj < 0 = error "neg bemenet"
  | szj > 9 = error "nem szj"
  | div x 10 == 0 = 0
  | otherwise = if mod x 10 == szj then szj + szjElof2 (div x 10) szj else szjElof2 (div x 10) szj

-- - egy szám páros számjegyeinek számát,
szParosSzj x
  | x < 0 = szParosSzj (abs x)
  | div x 10 == 0 = if even x then 1 else 0
  | otherwise = if mod (mod x 10) 2 == 0 then 1 + szParosSzj (div x 10) else szParosSzj (div x 10)

szParosSzj2 x
  | x < 0 = szParosSzj (abs x)
  | div x 10 == 0 = if even x then 1 else 0
  | otherwise = if even utolsoSzj then 1 + szParosSzj (div x 10) else szParosSzj (div x 10)
  where
    utolsoSzj = mod x 10

-- - egy szám legnagyobb számjegyét,
legnagyobbSzj x = max1 x 0
  where
    max1 0 maxSzj = maxSzj
    max1 szam maxSzj = if mod szam 10 > maxSzj then max1 (div szam 10) (mod szam 10) else max1 (div szam 10) maxSzj

-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```

bSzamDSzj n b d
  | n < 0 = bSzamDSzj (abs n) b d
  | n < b = if n == d then 1 else 0
  | otherwise = if mod n b == d then 1 + bSzamDSzj (div n b) b d else bSzamDSzj (div n b) b d

ls3 = [(7673573, 10, 7), (1024, 2, 1), (1023, 2, 1), (345281, 16, 4)]

bSzamDSzjLs = map (\(n, b, d) -> bSzamDSzj n b d) ls3

-- bSzamDSzjLs2 = map (uncurry bSzamDSzj) ls3 -- hibat ad, mivel az uncurry kételemű tuple értékekkel működik

-- - az 1000-ik Fibonacci számot.
fiboN n = fiboSg 0 1 0 n
  where
    fiboSg a b res n
      | n == 0 = res
      | otherwise = fiboSg b res (res + b) (n - 1)

fibo _ _ res 0 = res
fibo a b res n = fibo b res (res + b) (n - 1)

fiboN2 n = fibo 0 1 0 1000

fiboSzamok n = map (fibo 0 1 0) [0 .. n]

fiboSzamok2 n = map (\x -> fibo 0 1 0 x) [0 .. n]

fiboN3 n = fiboSzamok n !! n

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

main :: IO ()
main = do
  putStrLn "szam szamjegyeinek szorzata"
  let fel1 = szjSzorzat2 1234
  print fel1
  let szam = 8263
  putStrLn (show szam ++ " szamjegyeinek szorzata " ++ show (szjSzorzat2 szam))
  print (szjOsszeg 1234)
  print (szjSzam 1238791)
