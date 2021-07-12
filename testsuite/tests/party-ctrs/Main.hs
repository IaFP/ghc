{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

module Main where

-- import GHC.IO.Encoding
-- import System.Environment.ExecutablePath
-- nelist :: NonEmpty String
-- nelist = "a" :| ["b", "c", "d"]
-- import System.IO
-- newtype (Ord a) => N a = N a

-- import ConstrainedMonads
-- import qualified Arr as A
-- import qualified Data.Array.Unboxed as UA
-- import qualified BST as B

-- import FAlgebra
import ConstrainedMonads

-- set1 = fromList [1,2,3,4]
-- set2 = fromList ['a', 'b', 'c', 'd']

{-
setPair :: Set a -> Set b -> Set (a, b)
setPair s s' = do
  m <- s
  m' <- s'
  return (m, m')

arr_int :: UArray Int Int
arr_int =  uarray (1, 10) [(i, i) | i <- [1,10]]
-}
-- arr_int' :: A.UArray Int Int
-- arr_int' = A.uarray (1, 10) [(i, i) | i <- [1,10]]

-- arr_int'' :: UA.UArray Int Int
-- arr_int'' = UA.array (1, 10) [(i, i) | i <- [1,10]]

-- bst1 :: B.BST Int
-- bst1 = B.merge (B.insert'bst 3 B.Leaf) (B.insert'bst 2 B.Leaf)


main :: IO ()
-- main = do
--    putStrLn "opening"
--    h <- openFile "./sample.txt" ReadMode True
--    putStrLn "opened"
--    return ()
   
   -- args <- getArgs
   -- traverse_ (\s -> putStr (s ++ " ")) args
-- return ()
   -- s <- getLine
   -- putStrLn s
-- putStrLn "Thats all folks!"

main = do
  let s1 = fromList [1,2]
  let ps = do a <- s1
              return (a, show a)
  putStrLn $ show ps
  -- putStrLn "Starting"
  -- putStrLn $ show (setPair set1 set2)
  -- putStrLn $ show (arr_int')
  -- putStrLn $ show (UA.bounds arr_int'')
  -- putStrLn $ show (fmap show arr_int')
  -- putStrLn $ "FAlgebra"
  -- putStrLn $ v2 ++ "~>" ++ show v1
  -- putStrLn $ show v3
