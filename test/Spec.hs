
import Text.ParserCombinators.Parsec ( parse, Parser )
import Value
import Parse (parseString, parseEscape)

showParseResult :: Show a => Parser a -> String -> String
showParseResult pa s = case parse pa "test: " s of
        Left err -> "Parse error"
        Right value -> "Parse correct: " ++ show value

testParseEscape :: String -> String
testParseEscape = showParseResult parseEscape

testParseString :: String -> String 
testParseString = showParseResult parseString

testS :: [String]
testS = ["this is a string", "this is a string having a quote \" end"]

testEscpae :: [String]
testEscpae = ["\\\"", "\\\n", "\\\t"]

main :: IO ()
main = do
    putStrLn "test start: "
    mapM_ (putStrLn . testParseString . show) testS
