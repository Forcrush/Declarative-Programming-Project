--Implement your solution here
--SEE THE PROJECT CODING GUIDELINES ON THE LMS FOR DETAILS OF
--THE CRITERIA THAT WILL BE EMPLOYED IN ASSESSING YOUR CODE.
--Edit this file as you like; just keep the below declaration.

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

type GameState = [[Location]]
newtype Location = Location String
type Score = (Int, Int, Int)
instance Show Location where show (Location str) = str
instance Eq Location where (Location s1) == (Location s2) = (s1 == s2)

-- toLocation function
toLocation :: String -> Maybe Location
toLocation loc
    | length loc /= 2 = Nothing
    | loc `elem` legalPos = Just (Location loc)
    | otherwise = Nothing
    where
        legalPos = [[x,y] | x <- ['A'..'H'], y <- ['1'..'4']]

-- Location to String
puretoLocation :: String -> Location
puretoLocation s = Location s

-- Location to String
toString :: Location -> String
toString loc = show loc


-- feedback function
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback target (g1:g2:g3:empty) = totalScore s1 s2 s3
    where
        s1 = computeScore target g1
        s2 = computeScore target g2
        s3 = computeScore target g3

totalScore :: Score -> Score -> Score -> Score
totalScore (a,b,c) (d,e,f) (g,h,i) = (a+d+g,b+e+h,c+f+i)
        
computeScore :: [Location] -> Location -> Score
computeScore tar gue
    | mindis == 0 = (1,0,0)
    | mindis == 1 = (0,1,0)
    | mindis == 2 = (0,0,1)
    -- the distance > 2
    | otherwise   = (0,0,0)
    where
        mindis = findmin disarray
        disarray = map (computeDistance gue) tar
        
-- find the minimum of a list
findmin :: [Int] -> Int
-- in 4*8 grid the distance couldn't greater than 1000000
findmin [] = 1000000
findmin (x:xs) = min x (findmin xs)

-- for 'target' 'guess' the two input, length == 2
computeDistance :: Location -> Location -> Int
computeDistance guess target = max xdis ydis
    where
        xdis = abs (fromEnum (head (toString target)) - fromEnum (head (toString guess)))
        ydis = abs (fromEnum (last (toString target)) - fromEnum (last (toString guess)))
        
initialGuess :: ([Location],GameState)
initialGuess = (guess, initialState)
    where
        guess = map Location ["A1", "G3", "H3"]

        -- method 1 to get the whole states, but costs lots of memory and time
        -- legalPos = [[x,y] | x <- ['A'..'H'], y <- ['1'..'4']]
        -- allguess = [ele | ele <- sublist legalPos, length ele == 3]

        -- method 2 to get the whole states
        allguess = [map Location [[a,b],[c,d],[e,f]] | a <- ['A'..'H'], c <- ['A'..'H'], e <- ['A'..'H'],
                     b <- ['1'..'4'], d <- ['1'..'4'], f <- ['1'..'4'],
                    cond1 [a,b] [c,d] && cond1 [c,d] [e,f]]
        initialState = mydelete guess allguess

-- if sort an sequence with monotone ordering, then can generate the combination
-- e.g. find all combination of 3 elements: e1 e2 e3
-- if order them with the rule: e3 > e2 > e1 / (e3 < e2 < e1),
-- finally you can get all possible Combination (not Permutation/Arrangement)
cond1 :: String -> String -> Bool
cond1 [x1,y1] [x2,y2] = xval2 > xval1 || (xval2 == xval1 && yval2 > yval1)
    where
        xval1 = fromEnum x1
        xval2 = fromEnum x2
        yval1 = fromEnum y1
        yval2 = fromEnum y2
        
-- sublist of a String
sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (e:es) = addEach e rest ++ rest
    where
        rest = sublist es

addEach :: a -> [[a]] -> [[a]]
addEach h [[]] = [[h]]
addEach h (x:xs) = (h:x):addEach h xs

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (lastGuess,currentState) lastScore
    | checkScore lastScore == 3 = (nextGuess,nextState) 
    | otherwise = (nextGuess,nextState)
    where
        nextState' = [st | st <- currentState, feedback st lastGuess == lastScore]
        nextGuess = if nextState' /= [] then head nextState' else lastGuess
        nextState = mydelete nextGuess nextState'

-- check score
checkScore :: Score -> Int
checkScore (a,b,c) = a + b + c


-- delete a element from a list
mydelete :: Eq a => a -> [a] -> [a] 
mydelete _ [] = []
mydelete ele (x:xs)
    | ele == x = mydelete ele xs
    | otherwise = x:mydelete ele xs


