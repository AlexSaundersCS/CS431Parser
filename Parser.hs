module Main where

main :: IO()
main = do
 putStrLn $ testOutput parse "parse" test_input_1 expected_value_1
 putStrLn $ testOutput parse "parse" test_input_2 expected_value_2
 putStrLn $ testOutput parse "parse" test_input_3 expected_value_3
 putStrLn $ testOutput parse "parse" test_input_4 expected_value_4
 where test_input_1 = "(5 +3 ) * 9 - 4"
       test_input_2 = " 5 / ( x * x - 4 * 5) + 3"
       test_input_3 = "(((5 + x) * 3)) - 2) / 7"
       test_input_4 = "(5 +* x x ) + 3 / 7"
       expected_value_1 = True
       expected_value_2 = True
       expected_value_3 = True
       expected_value_4 = False



--all functions from Project 2
exp :: [Token] -> [Token]
exp tlst = exp' (term tlst)

exp' :: [Token] -> [Token]
exp' tlst = if tlst head == "+" || "-" || []
              then consume tlst
            else
              parse []

term :: [Token] -> [Token]
term tlst = (factor tlst) (term' tlst)

term' :: [Token] -> [Token]
term' tlst = if tlst head == "*" || "/" || []
              then consume tlst
            else
              parse []

factor :: [Token] -> [Token]
factor tlst = if tlst head == Value || Id || []
               then consume tlst
              else
               parse []

consume :: [Token] -> Token -> [Token]
consume (l:ls) etv
 | etv == l = ls
 | otherwise = error "Syntax error."

parse :: [Token] -> Bool
parse [] = False
parse tlst = Main.exp tlst


-- all from Project 1
split :: [Char] -> [[Char]]
split[] = []
split (x:xs)
 | isLetter x = word : (split.eatWord) xs
 | isDigit x = number : (split.eatInt) xs
 | isChar x = [x] : split xs
 | otherwise = split xs
 where word = takeWhile (\x -> elem x(['A' .. 'Z'] ++ ['a' .. 'z'])) (x:xs)
       number = takeWhile(\x -> elem x (['0'..'9'])) (x:xs)
       eatWord = dropWhile (\x -> elem x (['A' .. 'Z'] ++ ['a' .. 'z']))
       eatInt = dropWhile (\x -> elem x(['0'..'9']))
       isLetter x = elem x (['A' .. 'Z'] ++ ['a' .. 'z'])
       isDigit x = elem x ['0' .. '9']
       isChar x = elem x ['(',')','+','-','*','/']


makeToken :: String -> Token
makeToken s@(x:xs) 
 | elem x ['0'.. '9'] = Value (read s :: Int)
 | "(" == s = LeftPar
 | ")" == s = RightPar
 | "+" == s = Add
 | "-" == s = Subtract
 | "*" == s = Multiply
 | "/" == s = Divide
 | elem x (['A' .. 'Z'] ++ ['a' .. 'z']) = Id
 | otherwise = Undefined


tokenize :: String -> [(String, Token)]
tokenize [] = []
tokenize s = zipWith(,) (split s) (map makeToken (split s))


data Token = Id | Value Int
    | Add | Subtract | Multiply | Divide 
    | LeftPar | RightPar | Undefined
    deriving (Show, Eq)


testOutput :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> a ->  b -> String
testOutput f name input expected = concat [ name, " ",  show input, " should be ", show expected, " -->" ,
                                                                                                                let result = f input in
                                                                                                                 if result == expected
                                                                                                                 then "PASS"
                                                                                                                 else concat["FAIL (actual: ", show result, ")"]]