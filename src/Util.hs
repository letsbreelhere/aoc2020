module Util where

steadyState :: Eq a => (a -> a) -> a -> a
steadyState f a =
  let a' = f a in
      if a == a'
         then a
         else steadyState f (f a')
