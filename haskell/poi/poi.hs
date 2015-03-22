-- 为输入的文字追加语气词poi
-- 若文字末尾非空格，则追加一个空格，再添加语气词poi

poit :: String -> String
poit msg
     | msg !! (len - 1) == ' ' = msg ++ "poi"
     | otherwise = msg ++ " poi"
     where len = length msg

main :: IO ()
main = do
     msg <- getLine
     putStrLn $ poit msg
