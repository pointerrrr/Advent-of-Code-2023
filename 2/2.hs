import Data.Char ( isDigit, digitToInt )
import Debug.Trace

main = do
    contents <- getContents
    let lined = lines contents
    let digits = map findDigits lined
    let nrs = map (\x -> (head x * 10) + last x) digits
    print $ sum nrs

findDigits :: String -> [Int]
findDigits [] = []
findDigits (x:xs)
    | isDigit x = digitToInt x : findDigits xs
    | otherwise = case findWrittenDigit (x:xs) of -1 -> findDigits xs
                                                  num -> num : findDigits xs

findWrittenDigit :: String -> Int
findWrittenDigit text = case matchedDigits of [] -> -1
                                              _ -> head matchedDigits
    where
        matchedDigits = [x + 1 | x <- [0..8], matchString (digitsAsWords !! x) text]

matchString :: String -> String -> Bool
matchString [] _ = True
matchString _ [] = False
matchString (x:xs) (y:ys) = x == y && matchString xs ys

digitsAsWords :: [String]
digitsAsWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]