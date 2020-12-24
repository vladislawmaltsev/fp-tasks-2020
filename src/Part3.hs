module Part3 where

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 m = isPrime m 2
  where
    isPrime :: Integer -> Integer -> Bool
    isPrime m i
      | i * i > m = True
      | m `rem` i == 0 = False
      | otherwise = isPrime m (i + 1)

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = 2 * n == sum (divisors n)

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 = divisors

sqrt' :: Integral a => a -> a
sqrt' x = round (sqrt (fromIntegral x))

divisors :: Integer -> [Integer]
divisors n = halfDivisors ++ allDivisors n halfDivisors []
  where
    halfDivisors = filter isDivisor [1..(sqrt' n)]
    isDivisor candidate = n `mod` candidate == 0

allDivisors :: Integer -> [Integer] -> [Integer] -> [Integer]
allDivisors n [] acc = acc
allDivisors n (x:xs) acc =
  let a = (n `div` x)
  in if a == x
    then allDivisors n xs acc
    else allDivisors n xs (a : acc)

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 []    = 0
prob22 input = product $ (map lettersCount) (words input)
    where
        lettersCount :: String -> Integer
        lettersCount word = toInteger $ length (filter (=='i') word)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 inputString = return inputString >>= parseInput >>= getSlice
    where
        parseInput :: String -> Maybe ParseResult
        parseInput input = do
            let left = read $ takeWhile (/= '-') input
            let right = read $ takeWhile (/= ':') $ tail $ dropWhile (/= '-') input
            let string = tail $ dropWhile (/= ' ') input
            return ParseResult { leftBound = left, rightBound = right, stringToSlice = string }

        getSlice :: ParseResult -> Maybe String
        getSlice (ParseResult left right string)
            | left > length string || right > length string = Nothing
            | right >= left = Just $ leftToRightSlice left right
            | otherwise = Just $ reverse $ leftToRightSlice right left
            where
                leftToRightSlice :: Int -> Int -> String
                leftToRightSlice l r = take r $ drop (l - 1) string

data ParseResult = ParseResult
    {
        leftBound :: Int,
        rightBound :: Int,
        stringToSlice :: String
    }


------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 number = iterTriangle 1 0
    where
        iterTriangle :: Integer -> Integer -> Bool
        iterTriangle currentNum currentSum
            | currentSum == number = True
            | currentSum > number = False
            | otherwise = iterTriangle (succ currentNum) (currentSum + currentNum)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 number = getDigits number == (reverse . getDigits) number
    where
        getDigits :: Integer -> [Integer]
        getDigits 0 = [0]
        getDigits current = digitsInternal current
            where
                digitsInternal 0 = []
                digitsInternal x = x `mod` 10 : digitsInternal (x `div` 10)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 left right = sum (divisors left) == left + right && sum (divisors right) == left + right

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
