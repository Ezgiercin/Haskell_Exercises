module Main where
import Foreign (new)

main :: IO ()
main = putStrLn "Hello Haskell"



takeLine :: String -> String   --takeLine "abc\ndef\nghi\n" == "abc"
takeLine [] = []
takeLine (x:xs) 
    | x == '\n' = []
    | otherwise = x : takeLine xs


takeWhile' :: (a -> Bool)-> [a]-> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []


dropWhile' :: (a -> Bool)-> [a]-> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = x:xs


lines' :: String -> [String]  --lines' "abc\ndef\nghi\n" == ["abc", "def", "ghi"]
lines' [] = []
lines' xs = 


segments' :: (a -> Bool)-> [a]-> [[a]]
segments' p [] = []



words' :: String -> [String]  --words' "abc def ghi" == ["abc", "def", "ghi"]



copyFile :: String -> String -> IO ()   
copyFile source target =
    readFile source >>= \s -> writeFile target s


copyFile' :: String -> String -> IO ()
copyFile' source target =
    readFile source >>= f
    where f :: String -> IO ()
          f = writeFile target

g = \x -> x + 1


doTwice :: IO a -> IO (a,a)
doTwice io = 
    io >>= \a1 -> io >>= \a2 -> return (a1,a2)


doNot :: IO a -> IO ()
doNot io = return ()
 

copyFileDo :: String -> String -> IO ()
copyFileDo source target = do
    s <- readFile source
    writeFile target s


sortFile :: FilePath -> FilePath -> IO ()
sortFile inFile outFile = do
    contents <- readFile inFile
    let sortedLines = sort (lines contents)
    writeFile outFile (unlines sortedLines)


printTable :: [String] -> IO ()
printTable strs = mapM_ putStrln numbered
        where numbered = zipWith (\i str -> show i ++ ": " ++ str  ) [1..] strs