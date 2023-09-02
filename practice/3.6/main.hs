simple :: p -> p
simple = (\x -> x)

makeChange :: Integer -> Integer -> Integer
makeChange = (\owed given -> if given - owed > 0 then given - owed else 0)

inc :: Integer -> Integer
inc = (\x -> x + 1)

double :: Integer -> Integer
double = (\x -> x * 2)

square :: Integer -> Integer
square = (\x -> x ^ 2)

counter :: Num a => a -> a
counter x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))
