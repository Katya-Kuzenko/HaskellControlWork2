{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
import Data.List (intercalate)

{-
Dictionaries are one of the most common data structure.
They are associative collections Dict k v
indexed by key of type k with values of type v.

Their inner structure can be different, but they have a common interface:

insert       :: k -> v -> Dict k v -> Dict k v
maybeGet     :: k -> Dict k v -> Maybe v
getOrDefault :: k -> Dict k v -> v -> v
contains     :: k -> Dict k v -> Bool
delete       :: k -> Dict k v -> Dict k v
elems        :: Dict k v -> [v]
keys         :: Dict k v -> [k]
size         :: Dict k v -> Int
empty        :: Dict k v 
-}


-- 1. Populate a following typeclass
--    d denotes the actual type of the dictionary (its type constructor)   
class IDict d k v where
  insert       :: k -> v -> d k v -> d k v
  maybeGet     :: k -> d k v -> Maybe v
  getOrDefault :: k -> d k v -> v -> v
  contains     :: k -> d k v -> Bool
  delete       :: k -> d k v -> d k v
  elems        :: d k v -> [v]
  keys         :: d k v -> [k]
  size         :: d k v -> Int
  empty        :: d k v 


-- 2. Propose a naive implementation of the typeclass above 
data Dict k v = Dict [(k, v)]

instance (Eq k) => IDict Dict k v where
  -- вставка ключа та значення в словник 
  insert k v (Dict dict) =  Dict (insert' k v dict)
    where
      insert' k v [] = [(k, v)]
      insert' k v ((k', v'):rest) 
        | k == k' = [(k, v)] ++ rest
        | otherwise = [(k', v')] ++ insert' k v rest

  -- отримати значення за ключем
  maybeGet k (Dict dict) = maybeGet' k dict
    where
      maybeGet' k [] = Nothing
      maybeGet' k ((k', v'):rest) 
        | k == k' = Just v'                               
        | otherwise = maybeGet' k rest

  -- значення за ключем || дефолтне значення
  getOrDefault k (Dict dict) v = getOrDefault' k dict v
    where
      getOrDefault' k [] v = v
      getOrDefault' k ((k', v'):rest) v 
        | k == k' = v'
        | otherwise = getOrDefault' k rest v 

  -- чи є ключ в словнику 
  contains k (Dict dict) = any (\(k', _) -> k' == k) dict

  -- видалити ключ
  delete k (Dict dict) = Dict (delete' k dict)
    where
      delete' k [] = []
      delete' k ((k', v'):rest)
        | k == k' = rest
        | otherwise = (k', v') : delete' k rest

  -- отримати список всіх значень 
  elems (Dict dict) = map (\(k, v) -> v) dict
  
  -- отримати список всіх ключів
  keys (Dict dict) = map (\(k, v) -> k) dict
  
  -- розмір словника
  size (Dict dict) = length dict
  
  -- пустий словник
  empty = Dict []


-- 3. Implement the Show typeclass to represent your dictionary 
--    in the form {key : value} 
instance (Show k, Show v) => Show (Dict k v) where
  show :: Dict k v -> String
  show (Dict x) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ " : " ++ show v) x) ++ "}"


main = do
  print $ (fromPairs kvPairs :: Dict Int Char) -- ось це та наступне виправлення пыдказав PowerShell. По-іншому не буде працювати
  where
    kvPairs   = [(1,'h'), (2,'e'), (3, 'l'), (4,'l'), (5, 'o')]
    fromPairs = foldl (insert' :: Dict Int Char -> (Int, Char) -> Dict Int Char) empty
    insert' dict (k, v) = insert k v dict
