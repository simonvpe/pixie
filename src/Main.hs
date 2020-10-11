module Main where

data Expr
  = Num Integer
  | Var Name
  | Add Expr Expr
  | Mul Expr Expr

type Name = String

main :: IO ()
main = print "hello, world!"
