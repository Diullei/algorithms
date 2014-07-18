open System

(*
    # Evaluate Reverse Polish Notation
    
    https://oj.leetcode.com/problems/evaluate-reverse-polish-notation/
    
    Evaluate the value of an arithmetic expression in Reverse Polish Notation.

    Valid operators are +, -, *, /. Each operand may be an integer or another expression.

    Some examples:
      ["2", "1", "+", "3", "*"] -> ((2 + 1) * 3) -> 9
      ["4", "13", "5", "/", "+"] -> (4 + (13 / 5)) -> 6
*)

////////////////////////////////////////////////////////////////////////
// Help functions
////////////////////////////////////////////////////////////////////////
let quickCheck test =
    if test then
        Console.WriteLine("+++ OK, passed 1 tests.")
    else
        Console.WriteLine("*** Failed! Falsifiable (after 1 test):")
////////////////////////////////////////////////////////////////////////

let isOp (s: string) = 
    List.exists (fun x -> x = s) ["+"; "-"; "*"; "/"] 

let applyOp op (left: float) (right: float) = 
    match op with
    | "+" -> left + right
    | "-" -> left - right
    | "/" -> left / right
    | "*" -> left * right
    | _   -> 0.0

let rec eval cs =
    match cs with
    | [] -> (0.0, [])
    | x::xs -> 
        if isOp x then
            let evaluated           = eval xs
            let left                = fst evaluated
            let secondEvaluated     = eval (snd evaluated)
            let right               = fst secondEvaluated
            (applyOp x left right, snd secondEvaluated)
        else
            (Convert.ToDouble x, xs)

let pnEval xs = 
    fst (eval (List.rev xs))

(* isOp *)
quickCheck (isOp "+" = true)
quickCheck (isOp "-" = true)
quickCheck (isOp "*" = true)
quickCheck (isOp "/" = true)
quickCheck (isOp "@" = false)

(* applyOp *)
quickCheck (applyOp "+" 1.0 1.0 = 2.0)
quickCheck (applyOp "+" 1.0 5.0 = 6.0)
quickCheck (applyOp "-" 1.0 1.0 = 0.0)
quickCheck (applyOp "*" 3.0 5.0 = 15.0)
quickCheck (applyOp "/" 15.0 5.0 = 3.0)
quickCheck (applyOp "/" 13.0 5.0 = 2.6)

(* eval *)
quickCheck (eval (List.rev ["1"; "1"; "+"]) = (2.0, []))
quickCheck (eval (List.rev ["1"; "5"; "+"]) = (6.0, []))
quickCheck (eval (List.rev ["1"; "1"; "-"]) = (0.0, []))
quickCheck (eval (List.rev ["3"; "5"; "*"]) = (15.0, []))
quickCheck (eval (List.rev ["15"; "5"; "/"]) = (3, []))
quickCheck (eval (List.rev ["13"; "5"; "/"]) = (2.9, []))

(* pnEval *)
quickCheck (pnEval ["1"; "1"; "+"] = 2.0)
quickCheck (pnEval ["1"; "5"; "+"] = 6.0)
quickCheck (pnEval ["1"; "1"; "-"] = 0.0)
quickCheck (pnEval ["3"; "5"; "*"] = 15.0)
quickCheck (pnEval ["15"; "5"; "/"] = 3)
quickCheck (pnEval ["13"; "5"; "/"] = 2.6)
quickCheck (pnEval ["4"; "13"; "5"; "*"; "+"] = 69.0)
quickCheck (pnEval ["4"; "13"; "5"; "/"; "+"] = 6.6)
quickCheck (pnEval ["2", "1", "+", "3", "*"] = 9.0)
