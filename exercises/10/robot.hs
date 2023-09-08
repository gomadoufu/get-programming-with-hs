-- オブジェクト
robot (name, attack, hp) = \message -> message (name, attack, hp)

-- アクセサ

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, _) -> robot (n, a, newHP))

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHP aRobot = aRobot hp

-- メッセージ

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where
    attack = if getHP aRobot > 10 then getAttack aRobot else 0

-- ヘルパー関数
printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

-- インスタンス
killerRobot = robot ("Kill3r", 25, 200)

gentleGiant = robot ("Mr. Friendly", 10, 300)

powerRobot = robot ("Power", 15, 200)

fastRobot = robot ("Speed", 20, 150)
