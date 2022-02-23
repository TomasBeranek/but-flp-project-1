import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess), ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

data RLG = RLG
  { nonTerminals :: [Char]
  , terminals :: [Char]
  , startingSymbol  :: Char
  , rules :: [(Char,[Char])]
  }

main = getArgs >>= parseArgs

loadRLG str = createRLG (lines str)

createRLG xs = RLG{ nonTerminals =  myUnique (removeChar ',' (xs !! 0)),
                    terminals = myUnique (removeChar ',' (xs !! 1)),
                    startingSymbol = (xs !! 2) !! 0,
                    rules = myUnique (map parseRule (drop 3 xs))}

removeChar c str = concat (mySplitOn ',' str)

mySplitOn delim [] = []
mySplitOn delim xs = [(takeWhile (/= delim) xs)] ++ (mySplitOn delim (safeTail (dropWhile (/= delim) xs)))

myUnique [] = []
myUnique (x:xs) =
  if x `elem` xs
    then myUnique xs
    else [x] ++ myUnique xs

safeTail [] = []
safeTail xs = tail xs

parseRule xs =  createRule (removeRightArrow (mySplitOn '-' xs))

removeRightArrow (x:[]) = [safeTail x]
removeRightArrow (x:xs) = [x] ++ removeRightArrow(xs)

createRule xs = ((xs !! 0) !! 0, xs !! 1)

convertToNFSM x = x
printNFSM x = putStrLn "TBD"
convertToRRG x = x
printRLG rlg = putStrLn ("nonTerminals: " ++ show (nonTerminals rlg)) >>
               putStrLn ("terminals: " ++ show (terminals rlg)) >>
               putStrLn ("startingSymbol: " ++ show (startingSymbol rlg)) >>
               putStrLn ("rules: " ++ show (rules rlg))

printRRG x = putStrLn "TBD"

parseArgs ("-i":xs) = hPutStrLn stderr "DEBUG: Print RLG" >> loadInput xs >>= printRLG . loadRLG >> exit
parseArgs ("-1":xs) = hPutStrLn stderr "DEBUG: Print RRG" >> loadInput xs >>= printRRG . convertToRRG . loadRLG  >> exit
parseArgs ("-2":xs) = hPutStrLn stderr "DEBUG: Print NFSM" >> loadInput xs >>= printNFSM . convertToNFSM . convertToRRG . loadRLG
parseArgs (x:_) = hPutStrLn stderr ("ERROR: Unknown argument '" ++ x ++ "'.") >> exitError

loadInput (x:[]) = hPutStrLn stderr ("DEBUG: Reading file: " ++ x) >> readFile x
loadInput [] = hPutStrLn stderr "DEBUG: Reading stdin" >> getContents
loadInput _ = hPutStrLn stderr "ERROR: Invalid number of files (max 1)." >> exitError

exit = exitWith ExitSuccess
exitError = exitWith (ExitFailure 1)
