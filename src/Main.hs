import System.Environment
import System.Exit

main = getArgs >>= parseArgs >>= printNFSM . convertToNFSM . loadRLG

loadRLG x = x
convertToNFSM x = x
printNFSM x = putStrLn x

parseArgs ("-i":xs) = putStrLn "Loaded RLG" >> parseArgs xs >> exit
parseArgs ("-1":xs) = putStrLn "Converted RLG to RRG" >> exit
parseArgs ("-2":xs) = putStrLn "Converted RLG to NFSM" >> exit
parseArgs (x:[]) = putStrLn "Reading from file" >> exit

parseArgs [] = getContents

exit = exitWith ExitSuccess
