module Main where -- モジュール名を明示的に宣言

import qualified Palindrome

-- -- isPalindromeの単純な実装
-- isPalindrome :: String -> Bool
-- isPalindrome text = text == reverse text

-- main IOアクションでは、ユーザー入力を読み取り、入力が回文かどうかをチェックし、
-- 結果を出力する
main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  let response =
        if Palindrome.isPalindrome text
          then "it is!"
          else "it's not!"
  print response
