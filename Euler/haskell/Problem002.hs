import           Test.QuickCheck

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

prob002 :: Integer
prob002 = sum [x | x <- takeWhile(< 4000000) fib, even x]
	where
		fib = 1:1:zipWith (+) fib (tail fib)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main :: IO ()
main = do
    quickCheck (prob002 == 4613732)
