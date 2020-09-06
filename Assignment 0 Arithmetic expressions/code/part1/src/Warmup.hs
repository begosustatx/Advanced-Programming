module Warmup where

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)
move West  (x,y) = (x-1, y)

moves :: [Direction] -> Pos -> Pos
moves [] x = x 
moves (x:xs) y = moves xs (move x y)

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add x Zero =  x 
add x (Succ y) = add (Succ x ) y 

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult x (Succ y) = add (mult x y) x

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node n left right)
    | x == n    = Node n left right
    | x < n     = Node n (insert x left) right
    | otherwise = Node n left(insert x right)