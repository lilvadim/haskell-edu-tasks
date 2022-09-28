import Data.Char

-- #1
toDecimal :: Int -> String -> String
toDecimal 1 snumber = if any (/='1') snumber then error "Wrong Input!" else show $ length snumber - 1
toDecimal _ "0" = "0"
toDecimal base snumber | (base > 61) || (base <= 0) = error "Base number is out of range."
                       | any (\x -> chartoint x > base) snumber = error "Wrong number"
                       | otherwise = show $ foldl (\acc x -> chartoint x + acc*base) 0 snumber
                       where
                            chartoint c | (ord '0' <= ord c) && (ord c <= ord '9') = ord c - ord '0'
                                        | (ord 'a' <= ord c) && (ord c <= ord 'z') = ord c - ord 'a' + 10
                                        | (ord 'A' <= ord c) && (ord c <= ord 'Z') = ord c - ord 'A' + 36
                                        | otherwise = error "Wrong Input"
-- #2
fromDecimal :: Int -> String -> String
fromDecimal _ "0" = "0"
fromDecimal 1 snumber = if any (\x -> (ord x < ord '0') || (ord x > ord '9')) snumber then error "Not a decimal number" else replicate (read snumber + 1) '1'
fromDecimal toBase snumber | (toBase > 61) || (toBase <= 0) = error "Base number is out of range."
                           | otherwise = helper [] (read snumber::Int) toBase 
                           where
                                helper xs 0 toBase = xs
                                helper xs n toBase = helper (inttochar (n `mod` toBase):xs) (n `div` toBase) toBase where
                                                                                                                           inttochar x | x <= 9 = chr (x + ord '0')
                                                                                                                                       | x <= 35 = chr (x + ord 'a' - 10)
                                                                                                                                       | x <= 61 = chr (x + ord 'A' - 36)
                                                                                                                                       | otherwise = error "Wrong Input"
                           
-- #3
convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase $ toDecimal fromBase snumber
