import Test.QuickCheck

{-
    # Evaluate Reverse Polish Notation

    https://oj.leetcode.com/problems/evaluate-reverse-polish-notation/

    Evaluate the value of an arithmetic expression in Reverse Polish Notation.

    Valid operators are +, -, *, /. Each operand may be an integer or another expression.

    Some examples:
      ["2", "1", "+", "3", "*"] -> ((2 + 1) * 3) -> 9
      ["4", "13", "5", "/", "+"] -> (4 + (13 / 5)) -> 6
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

isOp :: String -> Bool
isOp s =
    s `elem` ["+", "-", "*", "/"]

applyOp :: (Num a, Fractional a, Read a) => String -> a -> a -> a
applyOp op left right =
    case op of
        "+" -> left + right
        "-" -> left - right
        "/" -> left / right
        "*" -> left * right

eval :: (Num a, Fractional a, Read a) => [String] -> (a, [String])
eval [] = (0, [])
eval (x:xs) =
    if isOp x then
        do
            let evaluated = eval xs
            let right = fst evaluated
            let secondEvaluated = eval (snd evaluated)
            let left = fst secondEvaluated
            (applyOp x left right, snd secondEvaluated)
    else
        (read x, xs)

pnEval :: (Num a, Fractional a, Read a) => [String] -> a
pnEval xs =
    fst (eval (reverse xs))

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main :: IO()
main = do
    {- isOp -}
    quickCheck (isOp "+" == True)
    quickCheck (isOp "-" == True)
    quickCheck (isOp "*" == True)
    quickCheck (isOp "/" == True)
    quickCheck (isOp "@" == False)
    {- applyOp -}
    quickCheck (applyOp "+" 1 1 == 2)
    quickCheck (applyOp "+" 1 5 == 6)
    quickCheck (applyOp "-" 1 1 == 0)
    quickCheck (applyOp "*" 3 5 == 15)
    quickCheck (applyOp "/" 15 5 == 3)
    quickCheck (applyOp "/" 13 5 == 2.6)
    {- eval -}
    quickCheck (eval (reverse ["1", "1", "+"]) == (2, []))
    quickCheck (eval (reverse ["1", "5", "+"]) == (6, []))
    quickCheck (eval (reverse ["1", "1", "-"]) == (0, []))
    quickCheck (eval (reverse ["3", "5", "*"]) == (15, []))
    quickCheck (eval (reverse ["15", "5", "/"]) == (3, []))
    quickCheck (eval (reverse ["13", "5", "/"]) == (2.6, []))
    {- pnEval -}
    quickCheck (pnEval ["1", "1", "+"] == 2)
    quickCheck (pnEval ["1", "5", "+"] == 6)
    quickCheck (pnEval ["1", "1", "-"] == 0)
    quickCheck (pnEval ["3", "5", "*"] == 15)
    quickCheck (pnEval ["15", "5", "/"] == 3)
    quickCheck (pnEval ["13", "5", "/"] == 2.6)
    quickCheck (pnEval ["4", "13", "5", "*", "+"] == 69)
    quickCheck (pnEval ["4", "13", "5", "/", "+"] == 6.6)
    quickCheck (pnEval ["2", "1", "+", "3", "*"] == 9)
