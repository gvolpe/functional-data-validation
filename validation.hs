import Data.Semigroup

data Validation err a = Failure err | Success a deriving (Eq, Show)

type Error    = String
type Result a = Validation [Error] a

data Address = 
  Address {
    getHouseNumber :: Int,
    getStreetName :: String
  } deriving (Eq, Show)

instance Semigroup err => Semigroup (Validation err a) where
  (Success _) <> s@(Success _) = s
  (Failure x) <> (Failure y)   = Failure (x <> y)
  _           <> f@(Failure _) = f
  f@(Failure _) <> _           = f

instance Functor (Validation a) where
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success $ f x

instance Semigroup err => Applicative (Validation err) where
  pure = Success
  (Success f) <*> (Failure e) = Failure e
  (Failure e) <*> (Success x) = Failure e
  (Failure e) <*> (Failure t) = Failure $ e <> t
  (Success f) <*> (Success x) = Success $ f x

numberGreaterThanZero :: Int -> Result Int
numberGreaterThanZero x 
  | x > 0     = Success x
  | otherwise = Failure ["Number must be greater than zero!"]

notEmpty :: String -> Error -> Result String
notEmpty x m
  | x == ""   = Failure [m]
  | otherwise = Success x

streetNameNotEmpty :: String -> Result String
streetNameNotEmpty x = notEmpty x "Street name must not be empty!"

makeAddress :: Int -> String -> Result Address
makeAddress x y = Address <$> numberGreaterThanZero x <*> streetNameNotEmpty y

------------ SECOND PART --------------

streetNameLength :: String -> Result String
streetNameLength x
  | length x > 10 = Failure ["Name must not exceed 10 characters!"]
  | otherwise     = Success x

streetNameValidation :: String -> Result String
streetNameValidation x = streetNameNotEmpty x <> streetNameLength x

makeAddress' :: Int -> String -> Result Address
makeAddress' x y = Address <$> numberGreaterThanZero x <*> streetNameValidation y

------------ THIRD PART -------------

data Person =
  Person {
    getName :: String,
    getAddress :: Address
  } deriving (Eq, Show)

nameNotEmpty :: String -> Result String
nameNotEmpty x = notEmpty x "Name must not be empty!"

makePerson :: String -> Result Address -> Result Person
makePerson n a = Person <$> (nameNotEmpty n) <*> a

----------- INTEGRATION ------------

data WebForm = 
  WebForm { 
    getFormName :: String,
    getFormHouseNumber :: Int,
    getFormStreetName:: String
  } deriving (Eq, Show)

-- Integration
main :: IO ()
main = putStrLn $ show result 
  where result  = makePerson (getFormName form) address
        address = makeAddress (getFormHouseNumber form) (getFormStreetName form)
        form    = WebForm "Gabi" 22 "Dawson"
