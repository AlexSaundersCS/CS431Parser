--expression and expression tail
expression :: [Token] -> [Token]
expression tlst = exp' (term tlst)

exp' :: [Token] -> [Token]
exp' tlst = undefined

--term and term tail
term :: [Token] -> [Token]
term tlst = term' (factor tlst)

term' :: [Token] -> [Token]
term' (l:ls)
 | l == Multiply = term'(factor(l:ls))


--Factor function
factor :: [Token] -> [Token]
factor (l:ls)
 | l == Id = consume (l:ls) Id
 | l == Value = consume (l:ls) Value
 | l == LeftPar = expression (consume(l:ls) LeftPar)
 | l == RightPar = consume (l:ls) RightPar
 | otherwise = consume [] Value

--Consume Function
consume :: [Token] -> Token -> [Token]
consume (l:ls) etv
 | etv == l = ls
 | otherwise = error "Syntax error."


--Parse function
parse :: [Token] -> Bool
parse [] = True
parse tlst = undefined


data Token = Id | Value
    | Add | Subtract | Multiply | Divide 
    | LeftPar | RightPar | Undefined
    deriving (Show, Eq)