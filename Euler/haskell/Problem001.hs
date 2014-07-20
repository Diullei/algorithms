import           Test.QuickCheck

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

prob001 :: Integer
prob001 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main :: IO ()
main = do
    quickCheck (prob001 == 233168)
