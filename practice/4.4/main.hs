compareLastNames :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareLastNames name1 name2 = if result == EQ then compare (fst name1) (fst name2) else result
  where
    result = compare lastName1 lastName2
    lastName1 = snd name1
    lastName2 = snd name2
