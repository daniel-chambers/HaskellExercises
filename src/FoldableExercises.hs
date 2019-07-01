module FoldableExercises where

import           Prelude hiding (replicate, sum, filter, foldr, take, map)
import qualified Data.Foldable as DF (foldr)

data MyList a
  = (:|) a (MyList a)
  | EndOfList
  deriving stock Show
  deriving stock Eq

infixr 5 :|

-- The replicate function creates a list that contains the `x` parameter count `times`.
-- eg. replicate 3 "a" == "a" :| "a" :| "a" :| EndOfList
replicate :: Int -> a -> MyList a
replicate count x =
  undefined


-- Write out all the evaluation steps of replicate 3 "a". Put each
-- step on its own line.
-- Here's the start:
-- replicate 3 "a"
-- "a" :| replicate (3 - 1) "a"
-- ... (continue here)


-- Implement the take function that returns a list that is only
-- the first n elements of the input list
-- eg. take 2 ('a' :| 'b' :| 'c' :| 'd' :| EndOfList) == ('a' :| 'b' :| EndOfList)
-- eg. take 2 ('a' :| EndOfList) == ('a' :| EndOfList)
take :: Int -> MyList a -> MyList a
take count list =
  undefined


-- If we combined replicate and take like so:
-- take 2 (replicate 5 "a")
-- Do you think that we would construct a list 5 elements long?
-- Remember that Haskell has lazy evaluation and find out by
-- writing out the evaluation steps. Put each step on its own line.
-- Here's the start:
-- take 2 (replicate 5 "a")
-- take 2 ("a" :| replicate (5 - 1) "a")
-- ... (continue here)


-- Implement `countDownFrom` which counts down to zero from the number you pass in
-- using recursion.
-- eg. countDownFrom 3 == Cons (3,Cons (2,Cons (1,Cons (0,EndOfList))))
countDownFrom :: Int -> MyList Int
countDownFrom start =
  undefined


-- Implement a function that maps any value of a into a value of b
-- for all the elements of a list. The order of the list should be
-- retained.
-- eg. map (+1) (1 :| 3 :| 5 :| EndOfList) == (2 :| 4 :| 6 :| EndOfList)
map :: (a -> b) -> MyList a -> MyList b
map fn list =
  undefined


-- Implement a function that filters a list using a predicate function.
-- The list elements should retain their original order
-- eg. filter (/=3) (1 :| 3 :| 5 :| EndOfList) == (1 :| 5 :| EndOfList)
filter :: (a -> Bool) -> MyList a -> MyList a
filter predicate list =
  undefined


-- Implement a function that takes all the strings in the input list
-- and returns a single list of all the characters
-- eg. charsFromLines ("te" :| "st" :| EndOfList) == 't' :| 'e' :| 's' :| 't' :| EndOfList
charsFromLines :: MyList String -> MyList Char
charsFromLines lines =
  undefined


-- You may have noticed a pattern in your recursive functions so far, especially
-- if you inspect the evaluation steps that you wrote out.
-- Each appends the the remainder of the recursion as "the rest of the list"!
-- Foldr is a generalisation of this technique, where the :| function can
-- be passed in by the caller.
-- Implement foldr below.
-- eg. foldr (\a s -> a + 1 :| s) EndOfList (1 :| 2 :| 3 :| EndOfList) == (2 :| 3 :| 4 :| EndOfList)
foldr :: (a -> s -> s) -> s -> MyList a -> s
foldr folder state list =
  undefined


-- Reimplement your map function using your new fancy foldr
mapUsingFoldr :: (a -> b) -> MyList a -> MyList b
mapUsingFoldr fn list =
  undefined


-- Do you think your mapUsingFoldr can be used on infinite lists
-- or very long lists without materialising the whole list?

-- Reimplement your filter function using your foldr
filterUsingFoldr :: (a -> Bool) -> MyList a -> MyList a
filterUsingFoldr predicate list =
  undefined

-- Reimplement your charsFromLines using your foldr.
-- HINT: You'll also need DF.foldr to fold over the Strings ([Char])
charsFromLinesWithFoldr :: MyList String -> MyList Char
charsFromLinesWithFoldr lines =
  undefined


-- Implement the sum function that adds up all the ints in the list
-- and returns the total. Use recursion.
sum :: MyList Int -> Int
sum list =
  undefined


-- Write out the evaluation steps of sum (1 :| 2 :| 3 :| EndOfList). Put each
-- step on its own line.
-- Here's the start:
-- sum (1 :| 1 :| EndOfList)
-- 1 + sum (2 :| 3:| EndOfList)
-- ... (continue here)


-- Haskell is lazy, so a standard recursive implementation of sum
-- will use a lot of memory unnecessarily (known as a space leak).
-- You saw above how you needed to recurse through the entire list
-- before you could start adding up. This is because the + operator
-- cannot evaluate until it knows the concrete value of both sides of the
-- operator. So you end up with the entire expression expanded out in
-- memory before you're able to start adding! This is a space leak.
--
-- Observe this behaviour by calling sumUpAndPrint from your app
-- entry point (in app/Main.hs) and then running your app with
-- Haskell GC summary like so:
-- > stack exec HaskellExercises-exe -- +RTS -s
-- Take a look at the high maximum residency number printed out.
--
-- Modify the length of the list in sumUpAndPrint below and try it again
-- to see how the size of the list affects the maximum residency.
sumUpAndPrint :: IO ()
sumUpAndPrint = print $ sum $ countDownFrom 1000000


-- In order to resolve this problem, we need to write sum using tail-recursion
-- instead. Tail recursion is where the last thing executed in the function
-- is the recursive call to the function itself, returning its value.
-- Writing sum in tail recursive fashion will allow you to pass two concrete
-- values to the addition operator, and allow GHC's strictness optimiser to
-- identify that the addition should be done strictly and not accumulate lazily.
-- (For more on strictness optimisation: https://wiki.haskell.org/Performance/Strictness)
sumTailRec :: MyList Int -> Int
sumTailRec xs =
  undefined


-- Write out the evaluation steps of sumTailRec (1 :| 2 :| 3 :| EndOfList).
-- Assume the strictness optimiser is NOT in use.
-- Put each step on its own line.
-- Here's the start:
-- sumTailRec (1 :| 2 :| 3 :| EndOfList)
-- ... (continue here)


-- Now assume that the strictness optimiser IS in use and write out the evaluation
-- steps. Put each step on its own line.
-- Here's the start:
-- sumTailRec (1 :| 2 :| 3 :| EndOfList)
-- ... (continue here)


-- Observe the change in memory usage by calling sumUpTailRecAndPrint from your app
-- entry point (in app/Main.hs) and then running your app with
-- Haskell GC summary like so:
-- > stack exec HaskellExercises-exe -- +RTS -s
-- Compare the memory residency number to what you saw before.
-- Try changing the length of the list and seeing how that affects the residency.
sumUpTailRecAndPrint :: IO ()
sumUpTailRecAndPrint = print $ sumTailRec $ countDownFrom 10000000


-- Implement a function that takes a list and returns it in reverse order.
-- Consider carefully whether this should be tail recursive or not
reverseList :: MyList a -> MyList a
reverseList list =
  undefined


-- Implement the partition function. Partition uses a predicate function to
-- split a list into two sublists. The order of the list elements should
-- be retained.
-- For example:
-- isOdd x = x `mod` 2 == 1
-- partition isOdd (3 :| 4 :| 5 :| EndOfList) == ((3 :| 5 :| EndOfList), (4 :| EndOfList))
partition :: (a -> Bool) -> MyList a -> (MyList a, MyList a)
partition predicate list =
  undefined


-- Implement the partition function using tail recursion. Partition uses a
-- predicate function to split a list into two sublists. The order of the
-- list elements should be retained.
-- For example:
-- isOdd x = x `mod` 2 == 1
-- partition isOdd (3 :| 4 :| 5 :| EndOfList) == ((3 :| 5 :| EndOfList), (4 :| EndOfList))
-- HINT: You will need to use your reverseList function
partitionTailRec :: (a -> Bool) -> MyList a -> (MyList a, MyList a)
partitionTailRec predicate list =
  undefined
