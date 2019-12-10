import qualified Data.Map as Map

-- tasks

--

data Grade = F | D | C | B | A
  deriving (Show, Ord, Eq, Enum, Read)

data Degree = HS | BA | MS | PhD
  deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving Show

--

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement


-- List

candidates :: [Candidate]
candidates = [cand1, cand2, cand3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

-- MAP

cand1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = BA }

cand2 = Candidate
  { candidateId = 2
  , codeReview = C
  , cultureFit = A
  , education = PhD }

cand3 = Candidate
  { candidateId = 3
  , codeReview = A
  , cultureFit = B
  , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, cand1), (2, cand2), (3, cand3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

checkCandidate :: Maybe String -> String
checkCandidate (Just val) = val
checkCandidate _ = "error: id not found"


viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

-- IO


assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement



readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readGradeDo :: IO Grade
readGradeDo = do
  grade <- getLine
  (return . read) grade

readDegree :: IO Degree
readDegree = getLine >>= (return . read)


readCandidate :: IO Candidate
readCandidate = do
  putStrLn "enter id"
  cId <- readInt
  putStrLn "enter grade"
  codeGrade <- readGrade
  putStrLn "enter culture"
  cultureGrade <- readGrade
  putStrLn "enter education"
  degree <- readDegree
  return (Candidate { candidateId = cId
                    , codeReview = codeGrade
                    , cultureFit = cultureGrade
                    , education = degree })


--

maxPairM :: (Monad m, Ord a) => m (a, a) -> m a
maxPairM v = v >>= (\(x, y) -> return (max x y))

main :: IO ()
main = do
  a <- readInt
  b <- readInt
  res <- (maxPairM . pure) (a, b)
  print res


-- readInt :: IO Int
-- readInt = read <$> getLine

wrapperVerbose :: IO ()
wrapperVerbose = putStrLn "Enter The line" >>
                 getLine >>= putStrLn

echo :: IO ()
echo = do
  putStrLn "Enter The line"
  line <- getLine
  putStrLn line
