import Data.Char ( isDigit )

main = do
    cont <- getContents
    let ls = map (filter isDigit) (lines cont)
    print $ foldr ((\x y -> read x + y) . (\x -> head x : [last x])) 0 ls