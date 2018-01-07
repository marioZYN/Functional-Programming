-- Homework Assignment 1 


-- Validating Credit Card Numbers

toDigits :: Integer -> [Integer]
toDigits n = (reverse . toDigitsRev) n

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | otherwise = mod n 10 : toDigitsRev (div n 10) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = doubleEveryOther' (reverse l) [] where
    doubleEveryOther' [] res = res
    doubleEveryOther' lst res = 
        let cur = length res + 1
            h = head lst
            t = tail lst
        in if (mod cur 2) == 0 then doubleEveryOther' t ((h*2):res)
                             else doubleEveryOther' t (h:res)

sumDigits :: [Integer] -> Integer
sumDigits lst = sumDigits' lst 0 where
    sumDigits' [] res = res
    sumDigits' (x:xs) res 
        | x >= 10 = let x1 = mod x 10
                        x2 = div x 10
                    in sumDigits' xs (res+x1+x2)
        | otherwise = sumDigits' xs (res+x)

validate :: Integer -> Bool
validate n = 
    let x = (sumDigits . doubleEveryOther . toDigits) n
    in if mod x 10 == 0 then True else False


-- The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 source dest spare = [(source, dest)]
hanoi num source dest spare = 
    hanoi (num-1) source spare dest ++
    hanoi 1 source dest spare ++
    hanoi (num-1) spare dest source
