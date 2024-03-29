-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
-- splitSort :: Ord a => [a] -> [[a]] 
splitSort ns = foldl comb [[head ns]] (tail ns) where
  comb :: (Ord a, Num a) => [[a]] -> a -> [[a]]
  comb a b = let diff = last (last a) - head (last a)
                 len = length (last a)
             in if (last (last a) < b && (len == 1 || diff > 0))
                then
                  init a ++ [(last a ++ [b])]
                else if (last (last a) > b && (len == 1 || diff < 0))
                then
                  init a ++ [(last a ++ [b])]
                else if (last (last a) == b && (len == 1 || diff == 0))
                then
                  init a ++ [(last a ++ [b])]
                else
                  a ++ [[b]]

-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList xs = []

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress ms = False

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms = Third 

-- Exercise 5
-- good version with maximum check
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = 
	let 
	gr = (sqrt (5) + 1) / 2 
	a = x' - (x' - x) / gr 
	b = x  + (x' - x) / gr
	in if abs (a - b) > eps
		then if d a > d b 
				then hillClimb d x b eps
				else hillClimb d a x' eps
		else
			(x' + x) / 2

-- hillClimb to find minimum
hillClimbSqrt :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimbSqrt d x x' eps = 
	let 
	gr = (sqrt (5) + 1) / 2 
	a = x' - (x' - x) / gr 
	b = x  + (x' - x) / gr
	in if abs (a - b) > eps
		then if (d a)^2 < (d b)^2
				then hillClimbSqrt d x b eps
				else hillClimbSqrt d a x' eps
		else
			(x' + x) / 2
			
-- helper function which returns a lambda given a list of coefficients
constructLambda :: [Float] -> Int -> (Float -> Float)
constructLambda [] i x = 0
constructLambda (c:cs) i x = c*(x^i) + constructLambda cs (i+1) x  

-- Exercise 6
-- make another hillClimb algorithm to find minimum for a fct?
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimbSqrt (constructLambda xs 0) x x' eps

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence [] ins = []
executeInstructionSequence ns [] = ns
executeInstructionSequence [a] _ = [a]
executeInstructionSequence (x:y:ns) (i:ins) | i==Add = (x+y) : executeInstructionSequence ns ins
					    | i==Multiply = (x*y) : executeInstructionSequence ns ins
					    | i==Duplicate = [x,x] ++ executeInstructionSequence (y:ns) ins
					    | i==Pop = executeInstructionSequence (y:ns) ins

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence 2 = [Duplicate, Multiply]
optimalSequence n | n `mod` 2 == 0 = optimalSequence (n `div `2) ++ [Duplicate, Multiply]
		  | otherwise = Duplicate : (optimalSequence (n-1) ++ [Multiply]) 

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = []

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

-- Exercise 12
-- extract a message hidden using a simple steganography technique
decodeMessage :: String -> String
decodeMessage "" = ""
decodeMessage [x] = x : ""
decodeMessage (x:y:s) | (x=='0' && y=='0') = 'a' : decodeMessage s
                      | (x=='0' && y=='1') = 'b' : decodeMessage s
                      | (x=='1' && y=='0') = 'c' : decodeMessage s
                      | (x=='1' && y=='1') = 'd' : decodeMessage s

extractMessage :: String -> String
extractMessage s = decodeMessage (concatMap (\x -> if x == '0' || x == '1' then [x] else []) s)

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream ss = []

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f 0 0

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = False
