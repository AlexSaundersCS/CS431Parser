module Main where
main :: IO()

main = do
 putStrLn $ testOutput parse "parse" test_empty expected_true
 putStrLn $ testOutput parse "parse" test_single expected_true
 putStrLn $ testOutput parse "parse" test_addition expected_true
 putStrLn $ testOutput parse "parse" test_par_addition expected_true
 putStrLn $ testOutput parse "parse" test_bad_addition expected_false
 putStrLn $ testOutput parse "parse" test_missing_rightpar expected_false
 putStrLn $ testOutput parse "parse" test_multiplication expected_true
 putStrLn $ testOutput parse "parse" test_bad_multiplication expected_false
 putStrLn $ testOutput parse "parse" test_math expected_true
 where
   test_empty = []
   test_single = [Id]
   test_addition = [Value, Add, Value]
   test_par_addition = [LeftPar, Value, Add, Value, RightPar]
   test_bad_addition = [LeftPar, Value, Add, RightPar]
   test_missing_rightpar = [Value, Add, Value, RightPar]
   test_multiplication = [Value, Multiply, Value]
   test_bad_multiplication = [Multiply, Value, Value]
   test_math = [LeftPar, Value, Add, Value, RightPar, Multiply, Value]
   expected_true = True
   expected_false = False
   

--expression and expression tail
expression :: [Token] -> [Token]
expression [] = []
expression (l:ls) = exp' (term (l:ls))

exp' :: [Token] -> [Token]
exp' [] = []
exp' (l:ls)
 | l == Add = exp' (term(consume(l:ls) Add))
 | l == Subtract = exp' (term(consume(l:ls) Subtract))
 | (l:ls) == [] = (l:ls)
 | otherwise = (l:ls)


--term and term tail
term :: [Token] -> [Token]
term (l:ls) = term' (factor (l:ls))

term' :: [Token] -> [Token]
term' [] = []
term' (l:ls)
 | l == Multiply = term' (factor(consume(l:ls) Multiply))
 | l == Divide = term' (factor(consume(l:ls) Divide))
 | (l:ls) == [] = (l:ls)
 | otherwise = (l:ls)


--Factor function
factor :: [Token] -> [Token]
factor [] = []
factor (l:ls)
 | l == Id = consume (l:ls) Id
 | l == Value = consume (l:ls) Value
 | l == LeftPar = expression (consume(l:ls) LeftPar)
 | l == RightPar = consume (l:ls) RightPar
 | otherwise = (l:ls)


--Consume Function
consume :: [Token] -> Token -> [Token]
consume [] RightPar = []
consume (l:ls) etv
 | etv == l = ls
 | otherwise = (l:ls)


--Parse function
parse :: [Token] -> Bool
parse [] = True
parse (l:ls) = if expression(l:ls) == [] then True else False


--Token data type
data Token = Id | Value
    | Add | Subtract | Multiply | Divide 
    | LeftPar | RightPar | Undefined
    deriving (Show, Eq)


--test function
testOutput :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> a ->  b -> String
testOutput f name input expected = concat [ name, " ",  show input, " should be ", show expected, " -->" ,
                                                                                                                let result = f input in
                                                                                                                 if result == expected
                                                                                                                 then "PASS"
                                                                                                                 else concat["FAIL (actual: ", show result, ")"]]