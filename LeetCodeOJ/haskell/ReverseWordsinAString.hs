import           Test.QuickCheck

{-
    Given an input string, reverse the string word by word.

    For example,
    Given s = "the sky is blue",
    return "blue is sky the".
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

split' :: [Char] -> [[Char]]
split' [] = []
split' cs = [takeW] ++ split' (drop ((length takeW) + 1) cs)
    where
        takeW = (takeWhile (\c -> c /= ' ') cs)

reverse' :: [Char] -> [Char]
reverse' [] = []
reverse' sss = foldl1 (\a b -> b ++ " " ++ a) (split' sss)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main :: IO ()
main = do
    {- split' -}
    quickCheck (split' "the sky is blue" == ["the", "sky", "is", "blue"])
    {- reverse' -}
    quickCheck (reverse' "the sky is blue" == "blue is sky the")
