-- メッセージ オブジェクト の形になることに注意

-- cup オブジェクト
cup flOz = \message -> message flOz

-- メッセージ
getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

-- インスタンス
coffeeCup = cup 12

-- ヘルパー関数
afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]
