inc :: Num a => a -> a
inc n = n + 1

double :: Num a => a -> a
double n = n * 2

square :: Num a => a -> a
square n = n * n

ifEven :: Integral a => a -> a
ifEven n = if even n then n - 2 else 3 * n + 1
