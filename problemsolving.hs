module ProblemSolving where

--Can a number be made out of 20's, 6's, and 9's?

--invalidNumbers :: Integer -> Integer -> [Integer] -> [Integer]
-- Don't know if it properly checks anything or works, this damn thing infinite loops and doesn't print anything to screen
-- This is why we need IO()
invalidNumbers currentNumber workingNumber numberList
    | (workingNumber - 20) > 0 = invalidNumbers currentNumber (workingNumber - 20) numberList
    | (workingNumber - 9)  > 0 = invalidNumbers currentNumber (workingNumber - 9)  numberList
    | (workingNumber - 6)  > 0 = invalidNumbers currentNumber (workingNumber - 6)  numberList
    | (workingNumber - 20) == 0 || (workingNumber - 9) == 0 || (workingNumber - 6) == 0 = invalidNumbers (currentNumber + 1) (currentNumber + 1) numberList
    | (length numberList) > 1000000 = (tail numberList) ++ []
    | otherwise = invalidNumbers (currentNumber + 1) (currentNumber + 1) (currentNumber : numberList)

-- Real thing to do is rewrite the above function so that it just checks if the number is an invalid one or not. Maybe returns (#,True/False) tuple. Then do the looping in main so that I can show it.
 

--Returns true if isValidNumber has a "True" in its tuple. Pattern-matching FTW!
isValidNumberTrue (_,True) = True
isValidNumberTrue (_,False) = False

-- Checks if a number can be made from 20's, 9's and 6's
isValidNumber :: Integral a => a -> a -> (a,Bool)
isValidNumber currentNumber workingNumber
    | (workingNumber - 20) > 0 && (rem workingNumber 6 /= 0 || rem workingNumber 20 == 0)&& (rem workingNumber 9  /= 0 || rem workingNumber 20 == 0) && isValidNumberTrue (isValidNumber (workingNumber - 20) (workingNumber - 20)) = isValidNumber currentNumber (workingNumber - 20) 
    | (workingNumber - 9)  > 0 && (rem workingNumber 6 /= 0 || rem workingNumber 9 == 0) && (rem workingNumber 20 /= 0 || rem workingNumber 9 == 0) && isValidNumberTrue (isValidNumber (workingNumber - 9) (workingNumber - 9)) = isValidNumber currentNumber (workingNumber - 9)  
    | (workingNumber - 6)  > 0 && (rem workingNumber 9 /= 0 || rem workingNumber 6 == 0) && (rem workingNumber 20 /= 0 || rem workingNumber 6 == 0) && isValidNumberTrue (isValidNumber (workingNumber - 6) (workingNumber - 6)) = isValidNumber currentNumber (workingNumber - 6)  
    | (workingNumber - 20) == 0 || (workingNumber - 9) == 0 || (workingNumber - 6) == 0 = (currentNumber, True)
    | otherwise = (currentNumber, False)

isValidNumberLightweightTest :: Integral a => a -> a -> (a,Bool) -- Might actually take more time to run, idk
isValidNumberLightweightTest currentNumber workingNumber         -- looks nicer on the screen though & is easier to understand
    | workingNumber > 20 && isValidNumberTrue (isValidNumberLightweightTest (workingNumber - 20) (workingNumber - 20)) = isValidNumberLightweightTest currentNumber (workingNumber - 20) 
    | workingNumber > 9  && isValidNumberTrue (isValidNumberLightweightTest (workingNumber - 9) (workingNumber - 9))   = isValidNumberLightweightTest currentNumber (workingNumber - 9)  
    | workingNumber > 6                                                                                                = isValidNumberLightweightTest currentNumber (workingNumber - 6)  
    | (workingNumber - 20) == 0 || (workingNumber - 9) == 0 || (workingNumber - 6) == 0 = (currentNumber, True)
    | otherwise = (currentNumber, False)


allNumbers :: (Integer,Bool) -> IO ()
allNumbers (number1,truefalse) = do
    print (isValidNumber (number1+1) (number1+1))
    allNumbers ((isValidNumber (number1+1) (number1+1)))

allNumbersLightweightTest :: (Integer,Bool) -> IO ()
allNumbersLightweightTest (number1,truefalse) = do
    print (isValidNumberLightweightTest (number1+1) (number1+1))
    allNumbers ((isValidNumberLightweightTest (number1+1) (number1+1)))