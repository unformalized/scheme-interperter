
import Text.ParserCombinators.Parsec ( parse, Parser )
import Value
import Parse ( parseExpr, parseString, parseChar )
import VectorSpec (test)

showParseResult :: Show a => Parser a -> String -> String
showParseResult pa s = case parse pa "test: " s of
        Left err -> "Parse error"
        Right value -> "Parse correct: " ++ show value

testParseLispVal :: String -> String
testParseLispVal = showParseResult parseExpr

testParseString :: String -> String 
testParseString = showParseResult parseString

testParseChar :: String -> String
testParseChar = showParseResult parseChar

testLispVal :: [String]
testLispVal = [
  show "this is a string",
  show "this is a string having a quote \" end",
  "12394",
  "0.123",
  "#o123",
  "#\\a",
  "#xabc",
  -- list
  "(a test)",
  "(a (nested) test)",
  "(a (dotted . list) test)",
  "(a '(quoted (dotted . list)) test)",
  "(a '(imbalanced parens)"
  ]

testChar = [
  "#\\a",
  "#\\newline"
  ]

main :: IO ()
main = do
    putStrLn "test start: "
    -- mapM_ (putStrLn . testParseChar) testChar
    -- mapM_ (putStrLn . testParseLispVal) testLispVal
    test
