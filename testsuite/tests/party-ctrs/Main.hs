{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts, FlexibleContexts #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where
import ConstrainedMonads
import BST


main :: IO ()
main = do
  let s1 = fromList [1,2]
  let ps = do a <- s1
              return (a, show a)
  putStrLn $ show ps

  let bst = insert'bst 5 (insert'bst 3 (insert'bst 7 empty'bst))
  putStrLn $ "original bst: " ++  show bst
  let bstinc = fmap (+1) bst
  putStrLn $ "fmap (+1) bst: " ++ show bstinc
