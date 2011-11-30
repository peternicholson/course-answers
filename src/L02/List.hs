-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Note the existence of the library function max :: Int -> Int -> Int which will help you with Exercise 9.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

-- TOTAL marks:    /66

module L02.List where

import Prelude hiding (sum, length, map, filter, maximum, reverse)

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :| List t deriving Eq

-- Right-associative
infixr 5 :|

instance (Show t) => Show (List t) where
  show = show . foldRight (:) []

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :| t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :| t) = let b' = f b h in b' `seq` foldLeft f b' t

reduceRight :: (a -> a -> a) -> List a -> a
reduceRight _ Nil      = error "bzzt. reduceRight on empty list"
reduceRight f (h :| t) = foldRight f h t

reduceLeft :: (a -> a -> a) -> List a -> a
reduceLeft _ Nil      = error "bzzt. reduceLeft on empty list"
reduceLeft f (h :| t) = foldLeft f h t

-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
-- Relative Difficulty: 1
-- Correctness: 2.0 marks
-- Performance: 0.5 mark
-- Elegance: 0.5 marks
-- Total: 3
headOr :: List a -> a -> a
headOr Nil a    = a
headOr (h:|_) _ = h

-- Exercise 2
-- Relative Difficulty: 2
-- Correctness:   2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
sum :: List Int -> Int
sum = foldLeft (+) 0

-- Exercise 3
-- Relative Difficulty: 2
-- Correctness: 2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
length :: List a -> Int
length = foldLeft (const . succ) 0

-- Exercise 4
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.0 mark
-- Elegance: 1.5 marks
-- Total: 7
map :: (a -> b) -> List a -> List b
map f = foldRight (\a b -> f a :| b) Nil

-- Exercise 5
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
filter :: (a -> Bool) -> List a -> List a
filter f = foldRight (\a -> if f a then (a:|) else id) Nil

-- Exercise 6
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
append :: List a -> List a -> List a
append = flip (foldRight (:|))

-- Exercise 7
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
flatten :: List (List a) -> List a
flatten = foldRight append Nil

-- Exercise 8
-- Relative Difficulty: 7
-- Correctness: 5.0 marks
-- Performance: 1.5 marks
-- Elegance: 1.5 mark
-- Total: 8
flatMap :: (a -> List b) -> List a -> List b
flatMap f = flatten . map f

-- Exercise 9
-- Relative Difficulty: 8
-- Correctness: 3.5 marks
-- Performance: 3.0 marks
-- Elegance: 2.5 marks
-- Total: 9
maximum :: List Int -> Int
maximum = reduceLeft max

-- Exercise 10
-- Relative Difficulty: 10
-- Correctness: 5.0 marks
-- Performance: 2.5 marks
-- Elegance: 2.5 marks
-- Total: 10
reverse :: List a -> List a
reverse = foldLeft (flip (:|)) Nil

-- END Exercises

-- BEGIN Utility

all ::
  (a -> Bool)
  -> List a
  -> Bool
all p =
  foldRight (&&) True . map p

isEmpty ::
  List a
  -> Bool
isEmpty Nil    = True
isEmpty (_:|_) = False

contains ::
  Eq a =>
  List a
  -> a
  -> Bool
contains Nil    _ = False
contains (h:|t) e = h == e || contains t e

-- END Utility

