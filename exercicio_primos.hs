-- numero é primo se de 1->n só 1 divide ele
-- ou seja, dado n, caso base x == n 
--                 caso indutivo divide (x+1) n

divide :: Int -> Int -> Bool
divide x n | x >= n = True
           | mod n x == 0 = False
           | otherwise = divide (x+1) n

ehPrimo :: Int -> Bool
ehPrimo n = divide 2 n

euclides :: Int -> Int -> Int
euclides a b | b == 1 = 1
             | mod a b == 0 = mod a b 
             | otherwise = euclides b (mod a b)

primosEntreSi :: Int -> Int -> Bool
primosEntreSi a b | euclides a b == 1 = True
                  | otherwise = False
