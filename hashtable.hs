import Data.Char
import Data.List (find)

hash :: (Show a) => a -> Int
hash key = foldl (\acc c -> (ord c) + acc) 0 (show key) 
       
data HashTable k v = HashTable { values :: [[(k,v)]], loadfactor :: Double, size :: Int, capacity :: Int } deriving Show

defaultHashtable :: HashTable k v 
defaultHashtable = hashtableWithCap 100

fromList :: (Eq k, Show k, Ord k) => [(k,v)] -> HashTable k v 
fromList (x:xs) = foldl (\table x -> insert (table) (fst x) (snd x)) defaultHashtable (x:xs)
fromList [] = defaultHashtable

clear :: HashTable k v -> HashTable k v
clear table = hashtableWithCap (capacity table)

erase :: (Show k, Eq k) => HashTable k v -> k -> HashTable k v 
erase table key | not (contHelper table key hashRes) = table
                | otherwise = table { values = eraser (values table) key, 
                                      loadfactor = fromIntegral (size table - 1) / fromIntegral (capacity table), 
                                      size = (size table) - 1, 
                                      capacity = capacity table } where  
                                                                        {- eraser values key capacity = (take ((hashRes) `mod` capacity) values) 
                                                                                                      ++ (filter (\(x,y) -> x /= key) (values !! ((hashRes) `mod` capacity))) : []
                                                                                                       ++ (drop (((hashRes) `mod` capacity) + 1) values) -}
                                                                        eraser values key = map (filter (\(x,y) -> x /= key)) (values)
                                                                        hashRes = hash key


insert :: (Ord k, Show k, Eq k) => HashTable k v -> k -> v -> HashTable k v 
insert table key value | contHelper table key hashRes = table { values = updater key value (eraser (values table) key) (capacity table), 
                                                                loadfactor = loadfactor table, 
                                                                size = size table, 
                                                                capacity = capacity table }
                       | loadfactor table > 0.9 = fromListResize (toList table) (capacity table * 2)
                       | otherwise = table { values = updater key value (values table) (capacity table), 
                                             loadfactor = fromIntegral (size table + 1) / fromIntegral (capacity table), 
                                             size = size table + 1, 
                                             capacity = capacity table } where
                                                                               {- updater key value values capacity = (take ((hashRes) `mod` capacity) values)
                                                                                                                    ++ ((values !! ((hashRes) `mod` capacity)) ++ [(key,value)]) : []
                                                                                                                     ++ (drop (((hashRes) `mod` capacity) + 1) values) -}
                                                                               updater key value values capacity = replacer values key value (hashRes `mod` capacity)
                                                                               eraser values key = map (filter (\(x,y) -> x /= key)) (values)
                                                                               hashRes = hash key
                                                                               replacer (x:xs) k newVal 0 = (x ++ [(k,newVal)]):xs
                                                                               replacer (x:xs) k newVal n = x:replacer (xs) k newVal (n-1)
         
contains :: (Eq k, Show k) => HashTable k v -> k -> Bool
contains table key = elem key (map fst (filter (\(x,y) -> x == key) ((values table) !! ((hash key) `mod` (capacity table)))))

at :: (Eq k, Show k) => HashTable k v -> k -> Maybe v 
at table key = lookup key (filter (\(x,y) -> x == key) ((values table) !! ((hash key) `mod` capacity table)))

empty :: (Eq k, Show k) => HashTable k v -> Bool
empty table = size table == 0

-- My Helper Functions -- 

toList :: (Eq k, Show k) => HashTable k v -> [(k,v)]
toList HashTable { values = [x], loadfactor = _, size = _, capacity = _ } = x
toList table = head (values table) ++ toList (table { values = tail (values table), loadfactor = loadfactor table, size = size table, capacity = capacity table })

fromListResize :: (Eq k, Show k, Ord k) => [(k,v)] -> Int -> HashTable k v 
fromListResize (x:xs) n = foldl (\table x -> insert (table) (fst x) (snd x)) (hashtableWithCap n) (x:xs)                                                     

hashtableWithCap :: Int -> HashTable k v 
hashtableWithCap n = HashTable { values = replicate n [],
                                 loadfactor = 0.0,
                                 size = 0,
                                 capacity = n }

contHelper :: (Eq k, Show k) => HashTable k v -> k -> Int -> Bool
contHelper table key hashRes = elem key (map fst (filter (\(x,y) -> x == key) ((values table) !! ((hashRes) `mod` (capacity table)))))
