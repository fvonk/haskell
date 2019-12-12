import Control.Monad
import Control.Applicative


-- instance Semigroup HINQ where
--   (<>) =

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery =
      HINQ (_select (fst . fst))
           (_join studentEnrollments courses snd courseId)
           (_where ((== courseName) . courseTitle . snd))

studentEnrollmentsQ =
  HINQ_ (_select (\(st, en) -> (studentName st, course en)))
        (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ =
  HINQ (_select (fst . fst))
       (_join studentEnrollments courses snd courseId)
       (_where ((== "English") . courseTitle . snd))


data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) =
  _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) =
  _hinq sClause jClause (_where (\_ -> True))


query1 :: HINQ [] (Teacher, Course) Name
query1 =
  HINQ (_select (teacherName . fst))
       (_join teachers courses teacherId teacher)
       (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers


possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

missingCourse :: Maybe Course
missingCourse = Nothing

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
  HINQ (_select (teacherName . fst))
       (_join possibleTeacher possibleCourse teacherId teacher)
       (_where ((== "France") . courseTitle . snd))

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 =
 HINQ (_select (teacherName . fst))
      (_join possibleTeacher missingCourse teacherId teacher)
      (_where ((== "France") . courseTitle . snd))

-- HINQ

teacherLastName :: [String]
teacherLastName = _hinq (_select lastName)
                        finalResult
                        (_where (\_ -> True))

finalResult :: [Name]
finalResult =
  _hinq (_select (teacherName . fst))
        (_join teachers courses teacherId teacher)
        (_where ((== "English") . courseTitle . snd))


_hinq selectQuery joinQuery whereQuery =
  (\joinData ->
    (\whereResult ->
      selectQuery whereResult
    ) (whereQuery joinData)
  ) joinQuery


joinData = (_join teachers courses teacherId teacher)
whereResult = _where ((== "English") . courseTitle . snd) joinData
selectResult = _select (teacherName . fst) whereResult



_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2)
  guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
  return dpairs


_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
  val <- vals
  guard (test val)
  return val

startsWith :: Char -> String -> Bool
startsWith c s = c == (head s)


_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
  val <- vals
  return (prop val)

-- Teachers

data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Name } deriving Show

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simona" "asbsbsb")
           ,Teacher 200 (Name "Susan" "abs3b3b3")]

data Course = Course { courseId :: Int
                     , courseTitle :: String
                     , teacher :: Int } deriving Show

courses :: [Course]
courses = [Course 101 "France" 100
          ,Course 201 "English" 200]


-- Students

data Name = Name { firstName :: String
                 , lastName :: String }
instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman
                | Sophmore
                | Junior
                | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student
  { studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name } deriving Show

students :: [Student]
students = [(Student 1 Senior (Name "asdf" "KASDF"))
           ,(Student 2 Freshman (Name "asvsdf" "KAS23DF"))
           ,(Student 3 Senior (Name "abbf" "KAS23DF"))
           ,(Student 4 Sophmore (Name "asvdf" "BASDF"))
           ,(Student 5 Senior (Name "abbdf" "AAS1D1F"))
           ,(Student 6 Junior (Name "asdsdf" "BAS1DF"))]


data Enrollment = Enrollment { student :: Int
                             , course :: Int } deriving Show

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101), (Enrollment 2 101)
              , (Enrollment 2 201), (Enrollment 3 101)
              , (Enrollment 4 201), (Enrollment 4 101)
              , (Enrollment 5 101), (Enrollment 6 201)]
