module Main where


import           Bloxorz (bloxorz, bloxorzPath)




initialTerrainStr :: String
initialTerrainStr = unlines ["ooo-------",
                             "oSoooo----",
                             "ooooooooo-",
                             "-ooooooooo",
                             "-----ooooo",
                             "------oTo-"]


pathString :: String -> String
pathString str = case bloxorzPath str of
                  Left err   -> err
                  Right path -> show path


main :: IO ()
main = putStrLn "Provide a board:" >> getContents >>= \board -> (putStrLn . pathString $ board) >> (putStrLn . bloxorz $ board)
