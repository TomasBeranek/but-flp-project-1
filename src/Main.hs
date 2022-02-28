import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess), ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Debug.Trace (trace)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

data RLG = RLG {  nonTerminalsRLG :: [Char],
                  terminalsRLG :: [Char],
                  startingSymbolRLG  :: Char,
                  rulesRLG :: [(Char,[Char])] }

instance Show RLG where
  show = showRLG

showRLG rlg = nonTerminalsStr ++ "\n" ++ (terminalsRLG rlg) ++ "\n" ++ [(startingSymbolRLG rlg)] ++ "\n" ++ rulesStr
    where nonTerminalsStr = convertRLGNonterminalsToStr (nonTerminalsRLG rlg)
          rulesStr = convertRLGRulesToStr (rulesRLG rlg)

convertRLGNonterminalsToStr (nt:[]) = [nt]
convertRLGNonterminalsToStr (nt:nts) = [nt] ++ "," ++  convertRLGNonterminalsToStr nts

convertRLGRulesToStr (r:[]) = convertSingleRLGRuleToStr r
convertRLGRulesToStr (r:rs) = convertSingleRLGRuleToStr r ++ "\n" ++ convertRLGRulesToStr rs

convertSingleRLGRuleToStr r = [(fst r)] ++ "->" ++ (snd r)

data RRG = RRG {  nonTerminalsRRG :: [[Char]], -- might contain new terminals named "A123", "B21", ...
                  terminalsRRG :: [Char],
                  startingSymbolRRG  :: Char,
                  rulesRRG :: [([Char],[Char])] } -- might contain new terminals on both sides

instance Show RRG where
  show = showRRG

showRRG rrg = nonTerminalsStr ++ "\n" ++ (terminalsRRG rrg) ++ "\n" ++ [(startingSymbolRRG rrg)] ++ "\n" ++ rulesStr
    where nonTerminalsStr = convertRRGNonterminalsToStr (nonTerminalsRRG rrg)
          rulesStr = convertRRGRulesToStr (rulesRRG rrg)

convertRRGNonterminalsToStr (nt:[]) = nt
convertRRGNonterminalsToStr (nt:nts) = nt ++ "," ++  convertRRGNonterminalsToStr nts

convertRRGRulesToStr (r:[]) = convertSingleRRGRuleToStr r
convertRRGRulesToStr (r:rs) = convertSingleRRGRuleToStr r ++ "\n" ++ convertRRGRulesToStr rs

convertSingleRRGRuleToStr r = (fst r) ++ "->" ++ (snd r)

data NFSM = NFSM {  states :: [Int],
                    alphabet :: [Char],
                    transitions  :: [(Int,Char,Int)],
                    startState :: Int,
                    finalStates :: [Int] }

instance Show NFSM where
  show = showNFSM

showNFSM nfsm = statesStr ++ "\n" ++ (alphabet nfsm) ++ "\n" ++ show (startState nfsm) ++ "\n" ++ finalStatesStr ++ "\n" ++ transitionsStr
    where statesStr = convertNFSMStatesToStr (states nfsm)
          finalStatesStr = convertNFSMStatesToStr (finalStates nfsm)
          transitionsStr = convertNFSMTransitionsToStr (transitions nfsm)

convertNFSMStatesToStr (s:[]) = show s
convertNFSMStatesToStr (s:sx) = (show s) ++ "," ++  convertNFSMStatesToStr sx

convertNFSMTransitionsToStr (t:[]) = convertSingleNFSMTransitionToStr t
convertNFSMTransitionsToStr (t:ts) = convertSingleNFSMTransitionToStr t ++ "\n" ++ convertNFSMTransitionsToStr ts

convertSingleNFSMTransitionToStr t = show (myFst t) ++ "," ++ [mySnd t] ++ "," ++ show (myThd t)

myFst (a,_,_) = a
mySnd (_,b,_) = b
myThd (_,_,c) = c

main = getArgs >>= parseArgs

loadRLG str = createRLG (lines str)

createRLG xs = RLG {  nonTerminalsRLG =  sort (myUnique (removeChar ',' (xs !! 0))),
                      terminalsRLG = sort (myUnique (removeChar ',' (xs !! 1))),
                      startingSymbolRLG = (xs !! 2) !! 0,
                      rulesRLG = sort (myUnique (map parseRule (drop 3 xs))) }

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

convertToRRG rlg = RRG {  nonTerminalsRRG =  sort x,
                          terminalsRRG = terminalsRLG rlg,
                          startingSymbolRRG = startingSymbolRLG rlg,
                          rulesRRG = sort y }
                 where (x,y) = splitRLGRules (nonTerminalsRLG rlg) (rulesRLG rlg)

splitRLGRules nonTerminals [] = (map (:[]) nonTerminals, [])
splitRLGRules nonTerminals (rule:rs) = if ruleIsRRG (snd rule)  --ruleIsRRG only needs the right side of the rule
                                          then (restOfNonRerminals, ( [fst rule], snd rule ) : restOfRules) --convert left side of the rule to string
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
    then splitSingleRLGNonTerminalRule nonTerminals (snd rule) [(fst rule)] --nonTerminals names need to be converted to strings
    else splitSingleRLGTerminalRule nonTerminals (snd rule) [(fst rule)] --nonTerminals names need to be converted to strings

splitSingleRLGNonTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 2
    then (nonTerminals, [(lastNT, ruleRightSide)])
    else (allNonTerminals, [(lastNT, (head ruleRightSide):freeNonTerminal)] ++ restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGNonTerminalRule (freeNonTerminal:nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals getAllNonTerminalsNames

splitSingleRLGTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 0
    then (nonTerminals, [(lastNT, "#")])
    else (allNonTerminals, [(lastNT, (head ruleRightSide):freeNonTerminal)] ++ restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGTerminalRule (freeNonTerminal:nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals getAllNonTerminalsNames

getAllNonTerminalsNames = [ [x] | x <- ['A'..'Z'] ] ++ [ [x] ++ (show y) | y <- [1..], x <- ['A'..'Z'] ]

getFreeNonTerminal existingNT (possibleNT:nts) = if possibleNT `elem` existingNT
                                                  then getFreeNonTerminal existingNT nts
                                                  else possibleNT

convertToNFSM rrg = NFSM {  states = renameNonTerminals (nonTerminalsRRG rrg) (nonTerminalsRRG rrg),
                            alphabet = terminalsRRG rrg,
                            transitions = createTransitions (nonTerminalsRRG rrg) (rulesRRG rrg),
                            startState = fromJust $ elemIndex [startingSymbolRRG rrg] (nonTerminalsRRG rrg),
                            finalStates = sort (createFinalStates (nonTerminalsRRG rrg) (rulesRRG rrg)) }

renameNonTerminals _ [] = []
renameNonTerminals allNonTerminals (nt:nts) = (fromJust $ elemIndex nt allNonTerminals):renameNonTerminals allNonTerminals nts

createTransitions _ [] = []
createTransitions allNonTerminals (r:rs) = if snd r == "#"
                                              then createTransitions allNonTerminals rs
                                              else (startState, symbol, endState):(createTransitions allNonTerminals rs)
                                           where startState = fromJust $ elemIndex (fst r) allNonTerminals
                                                 symbol = head (snd r)
                                                 endState = fromJust $ elemIndex (tail (snd r)) allNonTerminals

createFinalStates _ [] = []
createFinalStates allNonTerminals (r:rs) = if snd r == "#"
                                            then (fromJust $ elemIndex (fst r) allNonTerminals):(createFinalStates allNonTerminals rs)
                                            else createFinalStates allNonTerminals rs

printNFSM nfsm = putStrLn (show nfsm)

printRLG rlg = putStrLn (show rlg)

printRRG rrg = putStrLn (show rrg)

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
