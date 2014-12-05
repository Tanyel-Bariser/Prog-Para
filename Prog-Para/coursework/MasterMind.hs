{- 	@author Tanyel Bariser
	@course MSc Computer Science
	@deadline Sunday, 12th January, 23:55
	MasterMind.hs -}
module MasterMind where

import Data.List (nub, intersect, permutations, isInfixOf, (\\))
import Data.Char (toLower)
import System.Random (randomRIO)

			{- FUNCTIONS WORK OUT THE PLAYER FEEDBACK IN Ks & Ws -}

-- gives player feedback in Ks and Ws
playerFeedback :: String -> String -> String
playerFeedback pattern guess = ks ++ ws
    where numOfKs pattern guess = length(filter id(zipWith(==)pattern guess))
          numOfWs pattern guess = length(nub(intersect pattern guess))
          ks = replicate (numOfKs pattern guess) 'K'
          ws = replicate ((numOfWs pattern guess) - length(ks) + dups) 'W'
          dups = duplicates pattern guess
			
-- tells playerFeedback function to add another W for every duplicate guessed correctly
duplicates :: Eq a => [a] -> [a] -> Int
duplicates pattern guess = if (isInfixOf pDup gDup || isInfixOf gDup pDup) then min lpDup lgDup
                           else 0
    where pDup = pattern \\ (nub pattern)
          gDup = guess \\ (nub guess)
          lpDup = length pDup
          lgDup = length gDup


			{- HELPER FUNCTIONS FOR MAIN PLAY -}

-- checks user input is 4 letters long and only contains characters r,o,y,g,b,v
correctInput :: String -> Bool
correctInput guess = correctLength && correctLetters
    where correctLength = length(guess) == 4
          correctLetters = all(\x -> isInfixOf [x] "roygbv")guess

-- allows the feedback as Ks and Ws to be well spaced
prettyKW :: String -> String
prettyKW feedback = unwords $ map spaces feedback
    where spaces x = [x] ++ " "

-- converts the characters roygbv to its corresponding colour names so "guess" has nice output
pretty :: String -> String
pretty guess = unwords $ map colour guess
    where colour x
               | x == 'r' = "Red    "
               | x == 'o' = "Orange "
               | x == 'y' = "Yellow "
               | x == 'g' = "Green  "
               | x == 'b' = "Blue   "
               | x == 'v' = "Violet "
			
-- returns a number from 0 to 100000 (inclusive)
randomPattern :: IO Int
randomPattern = do
    r <- randomRIO(0, 100000)
    return r
			
-- returns random pattern allowing up to 4 duplicates, nice use of LAZY EVALUTATION
pattern :: Int -> String
pattern r = take 4((permutations "roygbvroygbvroygbvroygbv")!!r)

-- removes spaces and converts to lower case 
clean :: String -> String
clean input = map toLower $ filter(/= ' ') input


			{- FUNCTIONS RESPONSIBLE FOR MAIN PLAY -}

-- main function allows user control over play
main :: IO()
main = do
        putStr "\nWant to play MasterMind? (\"y\" for yes, \"t\" for test and \"n\" for no): "
        uncleanGame <- getLine
        let game = clean uncleanGame
        case game of
            "y"         ->  do  r <- randomPattern
                                play (pattern r) ""
                                main
            "t"         ->  do  putStr "\nEnter a four colour pattern (4 from r o y g b v): "
                                testPattern <- getLine
                                let pattern = clean testPattern
                                if correctInput pattern then play pattern ""
                                else putStrLn "Invalid input!"
                                main
            "n"         ->  do  putStrLn "\nOkay game over!\n"
            otherwise   ->  do  putStrLn "\nInvalid input!"
                                main

-- control centre for game
play :: String -> String -> IO()
play pattern message = do
        putStr "\nGuess the pattern (4 from r o y g b v): "
        uncleanGuess <- getLine
        let guess = clean uncleanGuess
        if correctInput guess then do
            let feedback = playerFeedback pattern guess
            let prettyGuess = pretty guess
            let prettyFeedback = prettyKW feedback
            let feedbackMessage = prettyGuess ++ "\t  " ++ prettyFeedback ++ "\n" ++ message
            if feedback == "KKKK" then
                putStrLn ("\n" ++ feedbackMessage ++ "\nWell Done! You guessed the correct pattern!")
            else do
                putStr ("\n" ++ feedbackMessage)
                play pattern feedbackMessage
        else do
            putStrLn "\nInvalid guess, try again!"
            play pattern message