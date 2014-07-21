import           Data.List       (delete)
import           Test.QuickCheck
import           System.Random

{-
    Given n points on a 2D plane, find the maximum number of points that lie on the same straight line.
    https://oj.leetcode.com/problems/max-points-on-a-line/
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x:ys | x <- xs, ys <- permutations (delete x xs)]

indexToIten :: (Eq p, Fractional p) => [Int] -> [(p, p)] -> [(p, p)]
indexToIten [] _ = []
indexToIten (i:is) xs = (xs !! i) : indexToIten is xs

generatePermutations :: (Eq p, Fractional p) => [(p, p)] -> [[(p, p)]]
generatePermutations ps =
    [pss | per <- permutations [0..(length ps - 1)], pss <- [indexToIten per ps]]

-- (head, last [middle])
pointToArrange :: (Eq p, Fractional p) => [(p, p)] -> ((p, p), (p, p), [(p, p)])
pointToArrange ps =
    (head ps, last ps, (init (tail ps)))

verifyPoints :: (Eq p, Fractional p) => ((p, p), (p, p), [(p, p)]) -> [(p, p)]
verifyPoints arrange = do
    let (p1, p2, points) = arrange
    let (x1, y1) = p1
    let (x2, y2) = p2
    let a = ((y2 - y1) / (x2 - x1))
    let b = y1 - a * x1
    p1:p2:[(x', y') | (x', y') <- points, a * x' + b == y']

maxPoints :: (Eq p, Fractional p) => [(p, p)] -> Int
maxPoints ps | length ps < 3 = 2
             | otherwise = maximum [qt |
                per <- generatePermutations ps,
                qt <- [length (verifyPoints (pointToArrange per))]
             ]

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

rndPoints :: (Random p, Eq p, Fractional p) => Int -> [(p, p)]
rndPoints size = zip x y
  where
    x = take size $ randomRs (25, 1000) (mkStdGen 1)
    y = take size $ randomRs (25,  775) (mkStdGen 1)

-- plotList []

main :: IO ()
main = do
    {- permutations -}
    quickCheck (permutations [0, 1, 2] == [[0,1,2],[0,2,1],[1,0,2],[1,2,0],[2,0,1],[2,1,0]])
    {- indexToIten -}
    quickCheck (indexToIten [1, 2, 0] [(1, 3), (3, 5), (2, 4)] == [(3, 5), (2, 4), (1, 3)])
    {- generatePermutations -}
    quickCheck (generatePermutations [(1, 2), (2, 5), (3, 2)] == [
        [(1, 2), (2, 5), (3, 2)],
        [(1, 2), (3, 2), (2, 5)],
        [(2, 5), (1, 2), (3, 2)],
        [(2, 5), (3, 2), (1, 2)],
        [(3, 2), (1, 2), (2, 5)],
        [(3, 2), (2, 5), (1, 2)]])
    {- pointToArrange -}
    quickCheck (pointToArrange [(3, 2), (2, 5), (1, 2)] == ((3, 2), (1, 2), [(2, 5)]))
    {- verifyPoints -}
    quickCheck (verifyPoints ((3, 2), (1, 2), [(2, 5)]) == [(3, 2), (1, 2)])
    quickCheck (verifyPoints ((1, 1), (6, 6), [(2, 2), (3, 3), (5, 5), (7, 1)]) == [(1, 1), (6, 6), (2, 2), (3, 3), (5, 5)])
    {- maxPoints -}
    quickCheck (maxPoints [(1, 1), (2, 2), (3, 3), (5, 5), (7, 1), (6, 6)] == 5)
    quickCheck (maxPoints [(1, 1), (2, 3)] == 2)
    quickCheck (maxPoints [(1, 1), (1, 1), (2, 3)] == 3)
    quickCheck (maxPoints [(1, 1), (1, 1), (2, 3), (3, 10)] == 3)
    quickCheck (maxPoints [(1, 1), (1, 1), (2, 3), (3, 10), (3, 5)] == 4)
