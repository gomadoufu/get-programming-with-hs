myElem :: Eq a => a -> [a] -> Bool
myElem val myList = (length filteredList) /= 0
  where
    filteredList = filter (== val) myList

isPalindrome text = processedText == reverse processedText
  where
    noSpaces = filter (/= ' ') text
    processedText = map toLower noSpaces

harmonic n = sum (take n seriesValues)
  where
    seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs
    seriesPairs = zip (cycle [1.0]) [1.0, 2.0 ..]
