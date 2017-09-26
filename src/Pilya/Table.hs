module Pilya.Table
    ( Table
    , empty
    , tableList
    , toList
    , append
    , tlookup
    , find
    ) where

import           Data.List  (intercalate)
import qualified Data.Map   as M
import           Data.Maybe (fromJust)

data Table a = Table
    { tableLookup :: M.Map Int a
    , tableSize   :: Int
    }

instance Show a => Show (Table a) where
    show (Table tl size) =
        "Table {" ++ intercalate ", " (fmap (\ind ->
            show ind ++ ": " ++ show (fromJust $ M.lookup ind tl)
        ) [0..size-1]) ++ "}"

instance Foldable Table where
    foldMap f tbl = foldMap f (toList tbl)

empty :: Table a
empty = Table M.empty 0

tableList :: [a] -> Table a
tableList = foldl (\tbl el -> snd $ append tbl el) empty

toList :: Table a -> [a]
toList tbl = fmap (fromJust . tlookup tbl) [0..tableSize tbl - 1]

append :: Table a -> a -> (Int, Table a)
append (Table oldLookup oldSize) el =
    (oldSize, Table newLookup newSize)
    where
        newLookup = M.insert oldSize el oldLookup
        newSize = oldSize + 1

tlookup :: Table a -> Int -> Maybe a
tlookup table index = M.lookup index (tableLookup table)

find :: Eq a => Table a -> a -> Maybe Int
find table searched = M.foldlWithKey search Nothing (tableLookup table)
    where
        search Nothing ind el
            | el == searched = Just ind
            | otherwise = Nothing
        search (Just ind) _ _ = Just ind
