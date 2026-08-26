--[1, 2, 3 ,4] :: [Int]
--[(5, True), (7, True)] :: [(Int, Bool)]

--type String = [Char]
--

sumList :: [Int] -> Int
sumList [] = 0
sumList (a:as) = a + sumList as

double :: [Int] -> [Int]
double [] = []
double (a:as) = [a*2] ++ double (as)

member :: [Int] -> Int -> Bool
member [] n = False
member (a:as) n | a == n = True
                | otherwise = member (as) n

memberChar :: [Char] -> Char -> Bool
memberChar [] n = False
memberChar (a:as) n | a == n = True
                | otherwise = memberChar (as) n

digits :: String -> String
digits [] = []
digits (a:as) | memberChar ['1' .. '9'] a == True = [a] ++ digits (as)
              | otherwise = digits (as)

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (a:as) = [snd(a) + fst(a)] ++ sumPairs (as)
