import Data.Char ( isDigit, digitToInt )

main = do
    cont <- getContents
    let ls = map findDigits $ lines cont
    let l2s = map (\x -> (head x * 10) + last x) ls
    print l2s
    print $ sum l2s

findDigits :: String -> [Int]
findDigits [] = []
findDigits (x:xs)
    | isDigit x = digitToInt x : findDigits xs
    | otherwise = case found of NotFound -> findDigits xs
                                Found num rest -> num : findDigits rest
        where
            found = findWrittenDigit (x:xs)

findWrittenDigit :: String -> Found
findWrittenDigit text = case matchedDigits of [] -> NotFound
                                              _ -> Found (nr + 1) rest
    where
        matchedDigits = [x | x <- [0..8], matchString (digitsAsWords !! x) text]
        nr = head matchedDigits
        rest = drop (length (digitsAsWords !! nr)) text

matchString :: String -> String -> Bool
matchString [] rest = True
matchString _ [] = False
matchString (x:xs) (y:ys) = x == y && matchString xs ys

digitsAsWords :: [String]
digitsAsWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

data Found = NotFound | Found Int String