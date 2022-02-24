import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess), ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Debug.Trace (trace)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

data GrammarType = GrammarType {  nonTerminals :: [Char],
                  terminals :: [Char],
                  startingSymbol  :: Char,
                  rules :: [(Char,[Char])] }

data NFSM = NFSM {  states :: [Int],
                    alphabet :: [Char],
                    transitions  :: [(Int,Char,Int)],
                    startState :: Int,
                    finalStates :: [Int] }

main = getArgs >>= parseArgs

loadRLG str = createRLG (lines str)

createRLG xs = GrammarType {  nonTerminals =  sort (myUnique (removeChar ',' (xs !! 0))),
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

convertToRRG rlg = GrammarType {  nonTerminals =  sort x,
                                  terminals = sort (terminals rlg),
                                  startingSymbol = startingSymbol rlg,
                                  rules = y }
                 where (x,y) = splitRLGRules (nonTerminals rlg) (rules rlg)

splitRLGRules nonTerminals [] = (nonTerminals, [])
splitRLGRules nonTerminals (rule:rs) = if ruleIsRRG (snd rule)  --ruleIsRRG only needs the right side of the rule
                                          then (restOfNonRerminals, [rule] ++ restOfRules)
                                          else (allNonTerminals, restOfRules ++ newRules)
                                       where  (restOfNonRerminals, restOfRules) = splitRLGRules nonTerminals rs
                                              (allNonTerminals, newRules) = splitSingleRLGRule restOfNonRerminals rule

ruleIsRRG ruleRightSide = if  ruleRightSide == "#"
                              || ((length ruleRightSide == 2)
                                  && ((ruleRightSide !! 0) `elem` ['a'..'z'])
                                  && ((ruleRightSide !! 1) `elem` ['A'..'Z']))
                            then True
                            else False

splitSingleRLGRule nonTerminals rule = if (last (snd rule)) `elem` ['A'..'Z']  --if the rule ends with nonTerminal
                                          then splitSingleRLGNonTerminalRule nonTerminals (snd rule) (fst rule)
                                          else splitSingleRLGTerminalRule nonTerminals (snd rule) (fst rule)

splitSingleRLGNonTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 2
    then (nonTerminals, [(lastNT, ruleRightSide)])
    else (allNonTerminals, [(lastNT, (head ruleRightSide):[freeNonTerminal])] ++ restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGNonTerminalRule (freeNonTerminal:nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals ['A'..'Z']

splitSingleRLGTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 0
    then (nonTerminals, [(lastNT, "#")])
    else (allNonTerminals, [(lastNT, (head ruleRightSide):[freeNonTerminal])] ++ restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGTerminalRule (freeNonTerminal:nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals ['A'..'Z']

getFreeNonTerminal existingNT [] = '@' --hPutStrLn stderr "ERROR: Run out of possible non-terminals." >> exitError
getFreeNonTerminal existingNT (possibleNT:nts) = if possibleNT `elem` existingNT
                                                  then getFreeNonTerminal existingNT nts
                                                  else possibleNT

convertToNFSM rrg = NFSM {  states = renameNonTerminals (nonTerminals rrg) (nonTerminals rrg),
                            alphabet = terminals rrg,
                            transitions = createTransitions (nonTerminals rrg) (rules rrg),
                            startState = fromJust $ elemIndex (startingSymbol rrg) (nonTerminals rrg),
                            finalStates = createFinalStates (nonTerminals rrg) (rules rrg) }

renameNonTerminals _ [] = []
renameNonTerminals allNonTerminals (nt:nts) = (fromJust $ elemIndex nt allNonTerminals):renameNonTerminals allNonTerminals nts

createTransitions _ [] = []
createTransitions allNonTerminals (r:rs) = if snd r == "#"
                                              then createTransitions allNonTerminals rs
                                              else (startState, symbol, endState):(createTransitions allNonTerminals rs)
                                           where startState = fromJust $ elemIndex (fst r) allNonTerminals
                                                 symbol = head (snd r)
                                                 endState = fromJust $ elemIndex (last (snd r)) allNonTerminals

createFinalStates _ [] = []
createFinalStates allNonTerminals (r:rs) = if snd r == "#"
                                            then (fromJust $ elemIndex (fst r) allNonTerminals):(createFinalStates allNonTerminals rs)
                                            else createFinalStates allNonTerminals rs

printNFSM nfsm = putStrLn ("states: " ++ show (states nfsm)) >>
                 putStrLn ("alphabet: " ++ show (alphabet nfsm)) >>
                 putStrLn ("transitions: " ++ show (transitions nfsm)) >>
                 putStrLn ("startState: " ++ show (startState nfsm)) >>
                 putStrLn ("finalStates: " ++ show (finalStates nfsm))

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
