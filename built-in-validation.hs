import Control.Applicative
import Control.Lens
import Data.Semigroup
import Data.Validation

type Error    = String
type Result a = AccValidation [Error] a

data Address = 
  Address {
    getHouseNumber :: Int,
    getStreetName :: String
  } deriving (Eq, Show)

numberGreaterThanZero :: Int -> Result Int
numberGreaterThanZero x 
  | x > 0     = _Success # x
  | otherwise = _Failure # ["Number must be greater than zero!"]

notEmpty :: String -> Error -> Result String
notEmpty x m
  | x == ""   = _Failure # [m]
  | otherwise = _Success # x

streetNameNotEmpty :: String -> Result String
streetNameNotEmpty x = notEmpty x "Street name must not be empty!"

makeAddress :: Int -> String -> Result Address
makeAddress x y = Address <$> numberGreaterThanZero x <*> streetNameNotEmpty y

------------ SECOND PART --------------

streetNameLength :: String -> Result String
streetNameLength x
  | length x > 10 = _Failure # ["Name must not exceed 10 characters!"]
  | otherwise     = _Success # x

makeAddress' :: Int -> String -> Result Address
makeAddress' x y = 
  Address 
  <$> numberGreaterThanZero x 
  -- Note: In this case we can use *> because this rules won't happen at the same time, otherwise we just need to use <> from Semigroup
  <*> (streetNameNotEmpty y *> streetNameLength y) 

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
