import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess), ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Debug.Trace (trace)

data GrammarType = GrammarType {  nonTerminals :: [Char],
                  terminals :: [Char],
                  startingSymbol  :: Char,
                  rules :: [(Char,[Char])] }

main = getArgs >>= parseArgs

loadRLG str = createRLG (lines str)

createRLG xs = GrammarType {  nonTerminals =  myUnique (removeChar ',' (xs !! 0)),
                              terminals = myUnique (removeChar ',' (xs !! 1)),
                              startingSymbol = (xs !! 2) !! 0,
                              rules = myUnique (map parseRule (drop 3 xs)) }

removeChar c str = concat (mySplitOn c str)

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

convertToRRG rlg = GrammarType {  nonTerminals =  x,
                                  terminals = terminals rlg,
                                  startingSymbol = startingSymbol rlg,
                                  rules = y }
                 where (x,y) = splitRLGRules (nonTerminals rlg) (rules rlg)

splitRLGRules nonTerminals [] = ("XY", [('X', "uX")])
splitRLGRules nonTerminals (rule:rs) = if ruleIsRRG (snd rule)  --
                                          then trace ("Is RRG") splitRLGRules nonTerminals rs

                                          --(nonTerminals ++ x, [rule] ++ y)
                                          else trace ("Not RRG") splitRLGRules nonTerminals rs
                                          --(nonTerminals ++ x, (spliteSingleRLGRule rule) ++ y)
                                       --where (x,y) = splitRLGRules nonTerminals rs

ruleIsRRG ruleRightSide = if  ruleRightSide == "#"
                              || ((length ruleRightSide == 2)
                                  && ((ruleRightSide !! 0) `elem` ['a'..'z'])
                                  && ((ruleRightSide !! 1) `elem` ['A'..'Z']))
                            then True
                            else False

spliteSingleRLGRule rule = rule


convertToNFSM rlg = rlg


printNFSM x = putStrLn "TBD"

printGrammar rlg = putStrLn ("nonTerminals: " ++ show (nonTerminals rlg)) >>
               putStrLn ("terminals: " ++ show (terminals rlg)) >>
               putStrLn ("startingSymbol: " ++ show (startingSymbol rlg)) >>
               putStrLn ("rules: " ++ show (rules rlg))

printRLG rlg = printGrammar rlg

printRRG rrg = printGrammar rrg

parseArgs ("-i":xs) = hPutStrLn stderr "DEBUG: Print RLG" >> loadInput xs >>= printRLG . loadRLG >> exit
parseArgs ("-1":xs) = hPutStrLn stderr "DEBUG: Print RRG" >> loadInput xs >>= printRRG . convertToRRG . loadRLG  >> exit
parseArgs ("-2":xs) = hPutStrLn stderr "DEBUG: Print NFSM" >> loadInput xs >>= printNFSM . convertToNFSM . convertToRRG . loadRLG
parseArgs (x:_) = hPutStrLn stderr ("ERROR: Unknown argument '" ++ x ++ "'.") >> exitError
parseArgs [] = hPutStrLn stderr ("ERROR: No arguments passed (try '-i', '-1' or '-2').") >> exitError

loadInput (x:[]) = hPutStrLn stderr ("DEBUG: Reading file: " ++ x) >> readFile x
loadInput [] = hPutStrLn stderr "DEBUG: Reading stdin" >> getContents
loadInput _ = hPutStrLn stderr "ERROR: Invalid number of files (max 1)." >> exitError

exit = exitWith ExitSuccess
exitError = exitWith (ExitFailure 1)
