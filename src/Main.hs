--  File    Main.hs
--  Author  Tomas Beranek <xberan46@stud.fit.vutbr.cz>
--  Brief   RLG to RRG/NFSM converter in Haskell
--  Date    1.3.2022
--  Up2date sources can be found at: https://github.com/TomasBeranek/but-flp-project-1

import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Control.Exception (catch, ErrorCall)

data RLG = RLG {  nonTerminalsRLG :: String,
                  terminalsRLG :: String,
                  startingSymbolRLG  :: Char,
                  rulesRLG :: [(Char,String)] }

instance Show RLG where
  show = showRLG

showRLG :: RLG -> String
showRLG rlg = nonTerminalsStr ++ "\n" ++ terminalsStr ++ "\n" ++ [startingSymbolRLG rlg] ++ "\n" ++ rulesStr
    where nonTerminalsStr = convertRLGSymbolsToStr (nonTerminalsRLG rlg)
          terminalsStr = convertRLGSymbolsToStr (terminalsRLG rlg)
          rulesStr = convertRLGRulesToStr (rulesRLG rlg)

convertRLGSymbolsToStr :: String -> String
convertRLGSymbolsToStr [] = []
convertRLGSymbolsToStr [nt] = [nt]
convertRLGSymbolsToStr (nt:nts) = [nt] ++ "," ++  convertRLGSymbolsToStr nts

convertRLGRulesToStr :: [(Char, String)] -> String
convertRLGRulesToStr [] = []
convertRLGRulesToStr [r] = convertSingleRLGRuleToStr r
convertRLGRulesToStr (r:rs) = convertSingleRLGRuleToStr r ++ "\n" ++ convertRLGRulesToStr rs

convertSingleRLGRuleToStr :: (Char, String) -> String
convertSingleRLGRuleToStr r = [fst r] ++ "->" ++ snd r

data RRG = RRG {  nonTerminalsRRG :: [String], --might contain new terminals named "A123", "B21", ...
                  terminalsRRG :: String,
                  startingSymbolRRG  :: Char,
                  rulesRRG :: [(String,String)] } --might contain new terminals on both sides

instance Show RRG where
  show = showRRG

showRRG :: RRG -> String
showRRG rrg = nonTerminalsStr ++ "\n" ++ terminalsStr ++ "\n" ++ [startingSymbolRRG rrg] ++ "\n" ++ rulesStr
    where nonTerminalsStr = convertRRGNonterminalsToStr (nonTerminalsRRG rrg)
          terminalsStr = convertRRGTerminalsToStr (terminalsRRG rrg)
          rulesStr = convertRRGRulesToStr (rulesRRG rrg)

convertRRGNonterminalsToStr :: [String] -> String
convertRRGNonterminalsToStr [] = []
convertRRGNonterminalsToStr [nt] = nt
convertRRGNonterminalsToStr (nt:nts) = nt ++ "," ++  convertRRGNonterminalsToStr nts

convertRRGTerminalsToStr :: String -> String
convertRRGTerminalsToStr [] = []
convertRRGTerminalsToStr [nt] = [nt]
convertRRGTerminalsToStr (nt:nts) = [nt] ++ "," ++  convertRRGTerminalsToStr nts

convertRRGRulesToStr :: [(String, String)] -> String
convertRRGRulesToStr [] = []
convertRRGRulesToStr [r] = convertSingleRRGRuleToStr r
convertRRGRulesToStr (r:rs) = convertSingleRRGRuleToStr r ++ "\n" ++ convertRRGRulesToStr rs

convertSingleRRGRuleToStr :: (String, String) -> String
convertSingleRRGRuleToStr r = fst r ++ "->" ++ snd r

data NFSM = NFSM {  states :: [Int],
                    alphabet :: String,
                    transitions  :: [(Int,Char,Int)],
                    startState :: Int,
                    finalStates :: [Int] }

instance Show NFSM where
  show = showNFSM

showNFSM :: NFSM -> String
showNFSM nfsm = statesStr ++ "\n" ++ alphabet nfsm ++ "\n" ++ show (startState nfsm) ++ "\n" ++ finalStatesStr ++ "\n" ++ transitionsStr
    where statesStr = convertNFSMStatesToStr (states nfsm)
          finalStatesStr = convertNFSMStatesToStr (finalStates nfsm)
          transitionsStr = convertNFSMTransitionsToStr (transitions nfsm)

convertNFSMStatesToStr :: Show a => [a] -> String
convertNFSMStatesToStr [] = []
convertNFSMStatesToStr [s] = show s
convertNFSMStatesToStr (s:sx) = show s ++ "," ++  convertNFSMStatesToStr sx

convertNFSMTransitionsToStr :: (Show a1, Show a2) => [(a1, Char, a2)] -> String
convertNFSMTransitionsToStr [] = []
convertNFSMTransitionsToStr [t] = convertSingleNFSMTransitionToStr t
convertNFSMTransitionsToStr (t:ts) = convertSingleNFSMTransitionToStr t ++ "\n" ++ convertNFSMTransitionsToStr ts

convertSingleNFSMTransitionToStr :: (Show a1, Show a2) => (a1, Char, a2) -> String
convertSingleNFSMTransitionToStr t = show (myFst t) ++ "," ++ [mySnd t] ++ "," ++ show (myThd t)

myFst :: (a, b, c) -> a
myFst (a,_,_) = a

mySnd :: (a, b, c) -> b
mySnd (_,b,_) = b

myThd :: (a, b, c) -> c
myThd (_,_,c) = c

main :: IO ()
main = do
  args <- getArgs
  catch (parseArgs args) handler
      where
          handler :: ErrorCall -> IO ()
          handler err = hPutStrLn stderr (head (lines (show err))) >> exitError

loadRLG :: String -> RLG
loadRLG str = if str == "" || str == "\n" -- in unix-like systems a file ends with a newline
  then error "ERROR: Empty input."
  else createRLG (lines str)

createRLG :: [String] -> RLG
createRLG xs
  | null xs        = error "ERROR: Empty input."
  | length xs == 1 = error "ERROR: Empty terminal set."
  | length xs == 2 = error "ERROR: Missing starting non-terminal."
  | length xs == 3 = error "ERROR: Empty rule set."
  | otherwise      = RLG {  nonTerminalsRLG = nonTerminals,
                            terminalsRLG = terminals,
                            startingSymbolRLG = startingSymbol,
                            rulesRLG = rules }
                          where nonTerminals = sort (myUnique (removeChar ',' (checkNonTerminalsFormat (head xs))))
                                terminals =  sort (myUnique (removeChar ',' (checkTerminalsFormat (xs !! 1))))
                                startingSymbol = checkStartingSymbolFormat (xs !! 2) nonTerminals
                                rules = sort (myUnique (map parseRule (checkRulesFormat (drop 3 xs) nonTerminals terminals)))

checkNonTerminalsFormat :: String -> String
checkNonTerminalsFormat nts
  | null nts = error "ERROR: Empty non-terminal set."
  | strHasCSVFormat nts ['A'..'Z'] "item" "ERROR: Incorrect format of non-terminal set." = nts
  | otherwise = error "ERROR: Incorrect format of non-terminal set."

checkTerminalsFormat :: String -> String
checkTerminalsFormat ts
  | null ts = ts  -- alphabet can be empty
  | strHasCSVFormat ts ['a'..'z'] "item" "ERROR: Incorrect format of terminal set." = ts
  | otherwise = error "ERROR: Incorrect format of terminal set."

strHasCSVFormat :: String -> String -> String -> String -> Bool
strHasCSVFormat [] _ curr errLine = (curr == "delim") || error errLine
strHasCSVFormat (x : xs) items curr errLine
   | curr == "item" = if x `elem` items
     then strHasCSVFormat xs items "delim" errLine
     else error errLine
   | x == ',' = strHasCSVFormat xs items "item" errLine
   | otherwise = error errLine

checkStartingSymbolFormat :: String -> String -> Char
checkStartingSymbolFormat str nts
  | null str = error "ERROR: Missing starting symbol."
  | length str > 1 = error "ERROR: Incorrect format of starting symbol."
  | head str `notElem` nts = error "ERROR: Starting symbol is non-existing non-terminal."
  | otherwise = head str

checkRulesFormat :: [String] -> String -> String -> [String]
checkRulesFormat rs nts ts
  | null rs = error "ERROR: Empty rules set."
  | individualRulesHaveCorrectFormat rs nts ts = rs
checkRulesFormat _ _ _ = undefined

individualRulesHaveCorrectFormat :: [String] -> String -> String -> Bool
individualRulesHaveCorrectFormat [] _ _ = True
individualRulesHaveCorrectFormat (r:rs) nts ts = ruleHasCorrectFormat r nts ts "left" && individualRulesHaveCorrectFormat rs nts ts

ruleHasCorrectFormat :: String -> String -> String -> String -> Bool
ruleHasCorrectFormat (x:xs) nts ts curr
  | curr == "left" && x `elem` nts = ruleHasCorrectFormat xs nts ts "-"
  | curr == "-" && x == '-' = ruleHasCorrectFormat xs nts ts ">"
  | curr == ">" && x == '>' = ruleHasCorrectFormat xs nts ts "right"
  | curr == "right" && x == '#' && null xs = True
  | curr == "right" && x `elem` ts && null xs = True
  | curr == "right" && x `elem` ts = ruleHasCorrectFormat xs nts ts "rightT"
  | curr == "rightT" && x `elem` ts && null xs = True
  | curr == "rightT" && x `elem` ts = ruleHasCorrectFormat xs nts ts "rightT"
  | curr == "rightT" && x `elem` nts && null xs = True
  | otherwise = error (determineError curr x)
ruleHasCorrectFormat [] _ _ _ = undefined

determineError :: String -> Char -> String
determineError curr x
  | curr == "left" && x `elem` ['A'..'Z'] = "ERROR: Rules contain non-existing non-terminal '" ++ [x] ++ "'."
  | curr == "right" && x `elem` ['a'..'z'] = "ERROR: Rules contain non-existing terminal '" ++ [x] ++ "'."
  | curr == "rightT" && x `elem` ['a'..'z'] = "ERROR: Rules contain non-existing terminal '" ++ [x] ++ "'."
  | curr == "rightT" && x `elem` ['A'..'Z'] = "ERROR: Rules contain non-existing non-terminal '" ++ [x] ++ "'."
  | otherwise = "ERROR: Incorrect format of rule set."

removeChar :: Eq a => a -> [a] -> [a]
removeChar c str = concat (mySplitOn c str)

mySplitOn :: Eq a => a -> [a] -> [[a]]
mySplitOn _ [] = []
mySplitOn delim xs = takeWhile (/= delim) xs : mySplitOn delim (safeTail (dropWhile (/= delim) xs))

myUnique :: Eq a => [a] -> [a]
myUnique [] = []
myUnique (x:xs) = if x `elem` xs
                    then myUnique xs
                    else x : myUnique xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

parseRule :: String -> (Char, String)
parseRule xs =  createRule (removeRightArrow (mySplitOn '-' xs))

removeRightArrow :: [String] -> [String]
removeRightArrow [] = undefined
removeRightArrow [x] = [safeTail x]
removeRightArrow (x:xs) = x : removeRightArrow xs

createRule :: [String] -> (Char, String)
createRule xs = (head (head xs), xs !! 1)

convertToRRG :: RLG -> RRG
convertToRRG rlg = RRG {  nonTerminalsRRG =  sort x,
                          terminalsRRG = terminalsRLG rlg,
                          startingSymbolRRG = startingSymbolRLG rlg,
                          rulesRRG = sort y }
                 where (x,y) = splitRLGRules (nonTerminalsRLG rlg) (rulesRLG rlg)

splitRLGRules :: String -> [(Char, String)] -> ([String], [(String, String)])
splitRLGRules nonTerminals [] = (map (:[]) nonTerminals, [])
splitRLGRules nonTerminals (rule:rs) = if ruleIsRRG (snd rule)  --ruleIsRRG only needs the right side of the rule
                                          then (restOfNonRerminals, ( [fst rule], snd rule ) : restOfRules) --convert left side of the rule to string
                                          else (allNonTerminals, restOfRules ++ newRules)
                                       where  (restOfNonRerminals, restOfRules) = splitRLGRules nonTerminals rs
                                              (allNonTerminals, newRules) = splitSingleRLGRule restOfNonRerminals rule

ruleIsRRG :: String -> Bool
ruleIsRRG ruleRightSide = ruleRightSide == "#" ||
                          ((length ruleRightSide == 2)
                              && (head ruleRightSide `elem` ['a'..'z'])
                              && ((ruleRightSide !! 1) `elem` ['A'..'Z']))

splitSingleRLGRule :: [String] -> (Char, String) -> ([String], [(String, String)])
splitSingleRLGRule nonTerminals rule = if last (snd rule) `elem` ['A'..'Z']  --if the rule ends with nonTerminal
    then splitSingleRLGNonTerminalRule nonTerminals (snd rule) [fst rule] --nonTerminals names need to be converted to strings
    else splitSingleRLGTerminalRule nonTerminals (snd rule) [fst rule] --nonTerminals names need to be converted to strings

splitSingleRLGNonTerminalRule :: [String] -> String -> String -> ([String], [(String, String)])
splitSingleRLGNonTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 2
    then (nonTerminals, [(lastNT, ruleRightSide)])
    else (allNonTerminals, (lastNT, head ruleRightSide : freeNonTerminal) : restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGNonTerminalRule (freeNonTerminal : nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals getAllNonTerminalsNames

splitSingleRLGTerminalRule :: [String] -> String -> String -> ([String], [(String, String)])
splitSingleRLGTerminalRule nonTerminals ruleRightSide lastNT = if null ruleRightSide
    then (nonTerminals, [(lastNT, "#")])
    else (allNonTerminals, (lastNT, head ruleRightSide : freeNonTerminal) : restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGTerminalRule (freeNonTerminal : nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals getAllNonTerminalsNames

getAllNonTerminalsNames :: [String]
getAllNonTerminalsNames = [ [x] | x <- ['A'..'Z'] ] ++ [ x : show y | y <- [1,2] :: [Integer], x <- ['A'..'Z'] ]

getFreeNonTerminal :: Foldable t => t String -> [String] -> String
getFreeNonTerminal _ [] = "@"     --won't happen (just to silence GHC), since the second parameter is infinite array
getFreeNonTerminal existingNT (possibleNT:nts) = if possibleNT `elem` existingNT
                                                  then getFreeNonTerminal existingNT nts
                                                  else possibleNT

convertToNFSM :: RRG -> NFSM
convertToNFSM rrg = NFSM {  states = renameNonTerminals (nonTerminalsRRG rrg) (nonTerminalsRRG rrg),
                            alphabet = terminalsRRG rrg,
                            transitions = createTransitions (nonTerminalsRRG rrg) (rulesRRG rrg),
                            startState = fromJust $ elemIndex [startingSymbolRRG rrg] (nonTerminalsRRG rrg),
                            finalStates = sort (createFinalStates (nonTerminalsRRG rrg) (rulesRRG rrg)) }

renameNonTerminals :: Eq a => [a] -> [a] -> [Int]
renameNonTerminals _ [] = []
renameNonTerminals allNonTerminals (nt:nts) = fromJust (elemIndex nt allNonTerminals) : renameNonTerminals allNonTerminals nts

createTransitions :: [String] -> [(String, String)] -> [(Int, Char, Int)]
createTransitions _ [] = []
createTransitions allNonTerminals (r:rs) = if snd r == "#"
                                              then createTransitions allNonTerminals rs
                                              else (fromState, symbol, toState) : createTransitions allNonTerminals rs
                                           where fromState = fromJust $ elemIndex (fst r) allNonTerminals
                                                 symbol = head (snd r)
                                                 toState = fromJust $ elemIndex (tail (snd r)) allNonTerminals

createFinalStates :: Eq a => [a] -> [(a, String)] -> [Int]
createFinalStates _ [] = []
createFinalStates allNonTerminals (r:rs) = if snd r == "#"
                                            then fromJust (elemIndex (fst r) allNonTerminals) : createFinalStates allNonTerminals rs
                                            else createFinalStates allNonTerminals rs

parseArgs :: [String] -> IO ()
parseArgs ("-i":xs) = loadInput xs >>= print . loadRLG >> exitSuccess
parseArgs ("-1":xs) = loadInput xs >>= print . convertToRRG . loadRLG  >> exitSuccess
parseArgs ("-2":xs) = loadInput xs >>= print . convertToNFSM . convertToRRG . loadRLG >> exitSuccess
parseArgs _ = error "ERROR: No valid arguments passed (try '-i', '-1' or '-2')."

loadInput :: [String] -> IO String
loadInput [] = getContents
loadInput ("-i":_) = error "ERROR: Invalid number of options (must be only 1)."
loadInput ("-1":_) = error "ERROR: Invalid number of options (must be only 1)."
loadInput ("-2":_) = error "ERROR: Invalid number of options (must be only 1)."
loadInput [x] = readFile x
loadInput _ = error "ERROR: Invalid number of arguments (max 2)."

exitError :: IO a
exitError = exitWith (ExitFailure 1)
