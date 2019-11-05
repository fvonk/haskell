-- cup ml = \_ -> ml

-- constructor
cup ml = \message -> message ml
-- instance
coffeeCup = cup 500

getMl aCup = aCup (\ml -> ml)
-- drink aCup mlAmount = cup (max 0 newValue)
--   where newValue = getMl aCup - mlAmount
drink aCup mlAmount = cup (max 0 mlDiff)
  where ml = getMl aCup
        mlDiff = ml - mlAmount

isEmpty aCup = getMl aCup == 0

--

afterManySips = foldl drink coffeeCup [30, 30, 30, 30, 30]

--
-- ROBOTS
robot (name, attack, hp) = \message -> message (name, attack, hp)
killerRobot = robot ("Killer", 25, 200)
goodRobot = robot ("good", 40, 180)
name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

printRobot aRobot = aRobot (\(n, a, hp) -> n ++ " attack:" ++ (show a) ++ " health:" ++ (show hp))

-- fight
damage aRobot value = aRobot (\(n, a, hp) -> robot (n, a, hp - value))
fight aRobot bRobot = damage bRobot amount
  where amount = if getHP aRobot > 0
                  then getAttack aRobot
                  else 0


-- setName aRobot value = robot (value, getAttack aRobot, getHP aRobot)
setName aRobot value = aRobot (\(n, a, hp) -> robot (value, a, hp))
setAttack aRobot value = aRobot (\(n, a, hp) -> robot (n, value, hp))
setHP aRobot value = aRobot (\(n, a, hp) -> robot (value, a, value))

g1 = fight killerRobot goodRobot
k1 = fight g1 killerRobot
g2 = fight k1 g1

--
-- superRobot = robot ("Super", 50, 200)

--
-- threeRoundFight a b = fight (fight a b) a
threeRoundFight a b = (\b1 -> fight (\b1 -> fight (fight a b1)) b1) b
