-- モジュールの名前がPalindromeで、isPalindrome関数をエクスポートすることを宣言
module Palindrome (isPalindrome) where

-- Data.Charモジュール全体をインポートすることも可能だが、
-- 必要なのは指定された3つの関数だけである
import Data.Char (isPunctuation, isSpace, toLower)

-- コードの残りの部分は本書の他のコードと同じ
stripWhiteSpace :: String -> String
stripWhiteSpace text = filter (not . isSpace) text

stripPunctuation :: String -> String
stripPunctuation text = filter (not . isPunctuation) text

toLowerCase :: String -> String
toLowerCase text = map toLower text

preprocess :: String -> String
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == (reverse cleanText)
  where
    cleanText = preprocess text
