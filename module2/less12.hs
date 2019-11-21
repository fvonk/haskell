data Patient = Patient { name :: Name
                        , sex :: Sex
                        , age :: Int
                        , height :: Int
                        , weight :: Int
                        , bloodType :: BloodType }

data Name = Name String String
            | NameWithMiddle String String String

showName :: Name -> String
showName (Name f l) = f ++ "_" ++ l
showName (NameWithMiddle f m l) = f ++ "_" ++ m ++ "_" ++ l

data BloodType = BloodType ABOType RhType
showBloodType :: BloodType -> String
showBloodType (BloodType a r) = showABO a ++ showRh r

data ABOType = A | B | AB | O
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

data RhType = Pos | Neg
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

data Sex = Male | Female
showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

jack :: Patient
jack = Patient { name = Name "Jack" "Fucker"
                , age = 15
                , sex = Male
                , height = 159
                , weight = 60
                , bloodType = BloodType A Pos }

john = jack { bloodType = BloodType B Neg }

patientSummary :: Patient -> String
patientSummary p = stars
                   ++ "Name: " ++ (showName (name p))
                   ++ "\nSex: " ++ (showSex (sex p))
                   ++ "\nAge: " ++ (show (age p))
                   ++ "\nHeight: " ++ (show (height p))
                   ++ "\nWeight: " ++ (show (weight p))
                   ++ "\nBlood type: " ++ (showBloodType (bloodType p))
                   ++ stars
  where stars = "\n" ++ (foldl (++) "" (take 15 (repeat "*"))) ++ "\n"



canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient p1 p2 = canDonateTo (bloodType p1) (bloodType p2)
