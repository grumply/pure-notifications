module Pure.Notifications.Between
  ( between
  ) where

-- Apply a function to a sliding window of 2 values from the list.
-- 
-- >>> let fibs = between (+) ([0,1] ++ fibs) in take 10 fibs
-- [1,2,3,5,8,13,21,34,55,89]
between :: (a -> a -> b) -> [a] -> [b]
between _ [] = []
between _ [_] = []
between f as = Prelude.map (uncurry f) (zip as (tail as))