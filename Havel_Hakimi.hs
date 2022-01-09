module Main where
low = minBound :: Int

hledejMax nejlepsi pocet [] = (pocet,nejlepsi)
hledejMax nejlepsi pocet (x:xs)
 | x>nejlepsi = hledejMax x 1 xs
 | x==nejlepsi = hledejMax x (pocet+1) xs
 | otherwise = hledejMax nejlepsi pocet xs

serad a x
 | null x = a
 | otherwise = serad (a++maxima) (filter (<snd nejvetsi) x)
 where nejvetsi = hledejMax low 0 x
       maxima = uncurry replicate nejvetsi

odecti n [] = []
odecti n zbytek@(x:xs)
 | n==0 = zbytek
 | x==0 = [-1]
 | otherwise = (x-1) : odecti (n-1) xs

uberVrchol x = odecti (head x) (tail x)

skore x
 | last m <0 = "Neni skore"
 | head m ==0 = "Je skore"
 | otherwise = skore (uberVrchol m)
 where m = serad [] x

main = do
    print (skore [3,3,1,1])