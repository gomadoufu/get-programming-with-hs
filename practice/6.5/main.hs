repeat :: a -> [a]
repeat n = cycle [n]

subseq :: Int -> Int -> [a] -> [a]
subseq start end xs = take difference (drop start xs)
  where
    difference = end - start
