module Main where

main :: IO()
main = do
 putStrLn $ testOutput parse "parse" test_input_1 expected_value_1
 putStrLn $ testOutput parse "parse" test_input_2 expected_value_2
 putStrLn $ testOutput parse "parse" test_input_3 expected_value_3
 putStrLn $ testOutput parse "parse" test_input_4 expected_value_4
 putStrLn $ testOutput parse "parse" test_input_5 expected_value_5
 where test_input_1 = "(5 +3 ) * 9 - 4"
       test_input_2 = " 5 / ( x * x - 4 * 5) + 3"
       test_input_3 = "(((5 + x) * 3)) - 2) / 7"
       test_input_4 = "(5 +* x x ) + 3 / 7"
       expected_value_1 = True
       expected_value_2 = True
       expected_value_3 = True
       expected_value_4 = False


exp :: [(Token)] -> [(Token)]
exp = undefined

exp' :: [(Token)] -> [(Token)]
exp' = undefined

term :: [(Token)] -> [(Token)]
term = undefined

term' :: [(Token)] -> [(Token)]
term' = undefined

factor :: [(Token)] -> [(Token)]
factor = undefined

consume :: [(Token)] -> [(Token)]
consume tlst = tlst tail

parse :: [(Token)] -> Bool
parse = undefined