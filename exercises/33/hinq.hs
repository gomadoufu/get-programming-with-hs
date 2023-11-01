import Control.Applicative
import Control.Monad

data Name = Name {firstName :: String, lastName :: String}

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student {studentId :: Int, gradeLevel :: GradeLevel, studentName :: Name} deriving (Show)

students :: [Student]
students =
  [ Student 1 Senior (Name "Audre" "Lorde"),
    Student 2 Junior (Name "Leslie" "Silko"),
    Student 3 Freshman (Name "Judith" "Butler"),
    Student 4 Senior (Name "Guy" "Debord"),
    Student 5 Sophomore (Name "Jean" "Baudrillard"),
    Student 6 Junior (Name "Julia" "Kristeva")
  ]

-- _select :: (a -> b) -> [a] -> [b]
_select prop vals = do
  prop <$> vals

-- _where :: (a -> Bool) -> [a] -> [a]
_where test vals = do
  val <- vals
  guard (test val)
  return val

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

data Teacher = Teacher {teacherId :: Int, teacherName :: Name} deriving (Show)

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior"), Teacher 200 (Name "Susan" "Sontag")]

data Course = Course {courseId :: Int, courseTitle :: String, teacher :: Int} deriving (Show)

courses :: [Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

-- _join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = do
  d1 <- data1
  d2 <- data2
  let p1 = prop1 d1
  let p2 = prop2 d2
  guard (p1 == p2)
  return (d1, d2)

_hinq selectQuery joinQuery whereQuery = (selectQuery . whereQuery) joinQuery

finalResult :: [Name]
finalResult =
  _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

teacherFirstName :: [String]
teacherFirstName = _hinq (_select firstName) finalResult (_where (const True))

_select :: Monad m => (a -> b) -> m a -> m b
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)

data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ selectQuery joinQuery whereQuery) = _hinq selectQuery joinQuery whereQuery
runHINQ (HINQ_ selectQuery joinQuery) = _hinq selectQuery joinQuery (_where (const True))

query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ (_select (teacherName . fst)) (_join teachers courses teacherId teacher) (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers
