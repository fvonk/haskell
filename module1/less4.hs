import Data.List


--


getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> fst name ++ " " ++ snd name)


addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

nyOffice name = nameText ++ ": aflsjkdgfaisugf aoisdf"
  where nameText = (fst name) ++ " " ++ (snd name)

sfOffice name = nameText ++ ">>>> sssssssß"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = "Best " ++ nameText ++ " faskdfkasgd fjasgdf"
  where nameText = snd name


--

names = [ ("qwe2", "ZAbc"),
  ("qwe", "0Abc"),
  ("zwe", "1Abc"),
  ("qwe", "1Abc") ]

compareNames name1 name2 = compare lastName1 lastName2

      where lastName1 = snd name1
            lastName2 = snd name2

--

ifEven func x = if even x
  then func x
  else x
