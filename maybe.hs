data Address =
  Address {
    getHouseNumber :: Int,
    getStreetName :: String
  } deriving (Eq, Show)

makeAddress :: Int -> String -> Maybe Address
makeAddress x y
  | x <= 0 || y == "" = Nothing
  | otherwise         = Just $ Address x y

------ SECOND PART ------

data Person =
  Person {
    getName :: String,
    getAddress :: Address
  } deriving (Eq, Show)

makePerson :: String -> Maybe Address -> Maybe Person
makePerson n address
  | n == ""   = Nothing
  | otherwise = address >>= \a -> Just $ Person n a
