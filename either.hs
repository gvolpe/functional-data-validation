data Address =
  Address {
    getHouseNumber :: Int,
    getStreetName :: String
  } deriving (Eq, Show)

type Error = String

streetNumberGreaterThanZero :: Int -> Either Error Int
streetNumberGreaterThanZero x
  | x <= 0    = Left "Number must be greater than zero!"
  | otherwise = Right x

notEmpty :: String -> Error -> Either Error String
notEmpty x err
  | x == ""   = Left err
  | otherwise = Right x

streetNameNotEmpty :: String -> Either Error String
streetNameNotEmpty x = notEmpty x "Street name must not be empty!"

makeAddress :: Int -> String -> Either Error Address
makeAddress x y = do
  number <- streetNumberGreaterThanZero x
  name   <- streetNameNotEmpty y
  Right $ Address number name

--------- SECOND PART ----------

data Person =
  Person {
    getName :: String,
    getAddress :: Address
  } deriving (Eq, Show)

nameNotEmpty :: String -> Either Error String
nameNotEmpty x = notEmpty x "Name must not be empty!"

makePerson :: String -> Either Error Address -> Either Error Person
makePerson n a = do
  name    <- nameNotEmpty n
  address <- a
  Right $ Person name address

makePerson' :: String -> Either Error Address -> Either Error Person
makePerson' n a = 
  nameNotEmpty n >>= \name ->  
  (Person name)  <$> a

