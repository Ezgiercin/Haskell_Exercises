import Data.List (sort)

copyFile :: String -> String -> IO ()
copyFile source target = 
  readFile source >>= \xs -> writeFile target xs

doTwice :: IO a -> IO (a, a)
doTwice io = 
  io >>= \a1 -> io >>= \a2 -> return (a1, a2)

sortFile :: FilePath -> FilePath -> IO ()
sortFile inFile outFile = do
  contents <- readFile inFile
  let sortedLines = sort (lines contents)
  writeFile outFile (unlines sortedLines)

printTable :: [String] -> IO ()
printTable strs = mapM_ putStrLn numbered
  where 
    numbered = zipWith (\i str -> show i ++ ": " ++ str) [1..] strs
