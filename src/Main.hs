import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitSuccess), ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

data RLG = RLG {  nonTerminalsRLG :: [Char],
                  terminalsRLG :: [Char],
                  startingSymbolRLG  :: Char,
                  rulesRLG :: [(Char,[Char])] }

instance Show RLG where
  show = showRLG

showRLG :: RLG -> [Char]
showRLG rlg = nonTerminalsStr ++ "\n" ++ (terminalsRLG rlg) ++ "\n" ++ [(startingSymbolRLG rlg)] ++ "\n" ++ rulesStr
    where nonTerminalsStr = convertRLGNonterminalsToStr (nonTerminalsRLG rlg)
          rulesStr = convertRLGRulesToStr (rulesRLG rlg)

convertRLGNonterminalsToStr :: [Char] -> [Char]
convertRLGNonterminalsToStr [] = []     --just to silence GHC
convertRLGNonterminalsToStr (nt:[]) = [nt]
convertRLGNonterminalsToStr (nt:nts) = [nt] ++ "," ++  convertRLGNonterminalsToStr nts

convertRLGRulesToStr :: [(Char, [Char])] -> [Char]
convertRLGRulesToStr [] = []     --just to silence GHC
convertRLGRulesToStr (r:[]) = convertSingleRLGRuleToStr r
convertRLGRulesToStr (r:rs) = convertSingleRLGRuleToStr r ++ "\n" ++ convertRLGRulesToStr rs

convertSingleRLGRuleToStr :: (Char, [Char]) -> [Char]
convertSingleRLGRuleToStr r = [(fst r)] ++ "->" ++ (snd r)

data RRG = RRG {  nonTerminalsRRG :: [[Char]], --might contain new terminals named "A123", "B21", ...
                  terminalsRRG :: [Char],
                  startingSymbolRRG  :: Char,
                  rulesRRG :: [([Char],[Char])] } --might contain new terminals on both sides

instance Show RRG where
  show = showRRG

showRRG :: RRG -> [Char]
showRRG rrg = nonTerminalsStr ++ "\n" ++ (terminalsRRG rrg) ++ "\n" ++ [(startingSymbolRRG rrg)] ++ "\n" ++ rulesStr
    where nonTerminalsStr = convertRRGNonterminalsToStr (nonTerminalsRRG rrg)
          rulesStr = convertRRGRulesToStr (rulesRRG rrg)

convertRRGNonterminalsToStr :: [[Char]] -> [Char]
convertRRGNonterminalsToStr [] = []     --just to silence GHC
convertRRGNonterminalsToStr (nt:[]) = nt
convertRRGNonterminalsToStr (nt:nts) = nt ++ "," ++  convertRRGNonterminalsToStr nts

convertRRGRulesToStr :: [([Char], [Char])] -> [Char]
convertRRGRulesToStr [] = []     --just to silence GHC
convertRRGRulesToStr (r:[]) = convertSingleRRGRuleToStr r
convertRRGRulesToStr (r:rs) = convertSingleRRGRuleToStr r ++ "\n" ++ convertRRGRulesToStr rs

convertSingleRRGRuleToStr :: ([Char], [Char]) -> [Char]
convertSingleRRGRuleToStr r = (fst r) ++ "->" ++ (snd r)

data NFSM = NFSM {  states :: [Int],
                    alphabet :: [Char],
                    transitions  :: [(Int,Char,Int)],
                    startState :: Int,
                    finalStates :: [Int] }

instance Show NFSM where
  show = showNFSM

showNFSM :: NFSM -> [Char]
showNFSM nfsm = statesStr ++ "\n" ++ (alphabet nfsm) ++ "\n" ++ show (startState nfsm) ++ "\n" ++ finalStatesStr ++ "\n" ++ transitionsStr
    where statesStr = convertNFSMStatesToStr (states nfsm)
          finalStatesStr = convertNFSMStatesToStr (finalStates nfsm)
          transitionsStr = convertNFSMTransitionsToStr (transitions nfsm)

convertNFSMStatesToStr :: Show a => [a] -> [Char]
convertNFSMStatesToStr [] = []     --just to silence GHC
convertNFSMStatesToStr (s:[]) = show s
convertNFSMStatesToStr (s:sx) = (show s) ++ "," ++  convertNFSMStatesToStr sx

convertNFSMTransitionsToStr :: (Show a1, Show a2) => [(a1, Char, a2)] -> [Char]
convertNFSMTransitionsToStr [] = []     --just to silence GHC
convertNFSMTransitionsToStr (t:[]) = convertSingleNFSMTransitionToStr t
convertNFSMTransitionsToStr (t:ts) = convertSingleNFSMTransitionToStr t ++ "\n" ++ convertNFSMTransitionsToStr ts

convertSingleNFSMTransitionToStr :: (Show a1, Show a2) => (a1, Char, a2) -> [Char]
convertSingleNFSMTransitionToStr t = show (myFst t) ++ "," ++ [mySnd t] ++ "," ++ show (myThd t)

myFst :: (a, b, c) -> a
myFst (a,_,_) = a

mySnd :: (a, b, c) -> b
mySnd (_,b,_) = b

myThd :: (a, b, c) -> c
myThd (_,_,c) = c

main :: IO b
main = getArgs >>= parseArgs

loadRLG :: String -> RLG
loadRLG str = createRLG (lines str)

createRLG :: [[Char]] -> RLG
createRLG xs = RLG {  nonTerminalsRLG =  sort (myUnique (removeChar ',' (xs !! 0))),
                      terminalsRLG = sort (myUnique (removeChar ',' (xs !! 1))),
                      startingSymbolRLG = (xs !! 2) !! 0,
                      rulesRLG = sort (myUnique (map parseRule (drop 3 xs))) }

removeChar :: Eq a => a -> [a] -> [a]
removeChar c str = concat (mySplitOn c str)

mySplitOn :: Eq a => a -> [a] -> [[a]]
mySplitOn _ [] = []
mySplitOn delim xs = [(takeWhile (/= delim) xs)] ++ (mySplitOn delim (safeTail (dropWhile (/= delim) xs)))

myUnique :: Eq a => [a] -> [a]
myUnique [] = []
myUnique (x:xs) =
  if x `elem` xs
    then myUnique xs
    else [x] ++ myUnique xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

parseRule :: [Char] -> (Char, [Char])
parseRule xs =  createRule (removeRightArrow (mySplitOn '-' xs))

removeRightArrow :: [[Char]] -> [[Char]]
removeRightArrow [] = []      --just to silence GHC
removeRightArrow (x:[]) = [safeTail x]
removeRightArrow (x:xs) = [x] ++ removeRightArrow xs

createRule :: [[Char]] -> (Char, [Char])
createRule xs = ((xs !! 0) !! 0, xs !! 1)

convertToRRG :: RLG -> RRG
convertToRRG rlg = RRG {  nonTerminalsRRG =  sort x,
                          terminalsRRG = terminalsRLG rlg,
                          startingSymbolRRG = startingSymbolRLG rlg,
                          rulesRRG = sort y }
                 where (x,y) = splitRLGRules (nonTerminalsRLG rlg) (rulesRLG rlg)

splitRLGRules :: [Char] -> [(Char, [Char])] -> ([[Char]], [([Char], [Char])])
splitRLGRules nonTerminals [] = (map (:[]) nonTerminals, [])
splitRLGRules nonTerminals (rule:rs) = if ruleIsRRG (snd rule)  --ruleIsRRG only needs the right side of the rule
                                          then (restOfNonRerminals, ( [fst rule], snd rule ) : restOfRules) --convert left side of the rule to string
                                          else (allNonTerminals, restOfRules ++ newRules)
                                       where  (restOfNonRerminals, restOfRules) = splitRLGRules nonTerminals rs
                                              (allNonTerminals, newRules) = splitSingleRLGRule restOfNonRerminals rule

ruleIsRRG :: [Char] -> Bool
ruleIsRRG ruleRightSide = if  ruleRightSide == "#"
                              || ((length ruleRightSide == 2)
                                  && ((ruleRightSide !! 0) `elem` ['a'..'z'])
                                  && ((ruleRightSide !! 1) `elem` ['A'..'Z']))
                            then True
                            else False

splitSingleRLGRule :: [[Char]] -> (Char, [Char]) -> ([[Char]], [([Char], [Char])])
splitSingleRLGRule nonTerminals rule = if (last (snd rule)) `elem` ['A'..'Z']  --if the rule ends with nonTerminal
    then splitSingleRLGNonTerminalRule nonTerminals (snd rule) [(fst rule)] --nonTerminals names need to be converted to strings
    else splitSingleRLGTerminalRule nonTerminals (snd rule) [(fst rule)] --nonTerminals names need to be converted to strings

splitSingleRLGNonTerminalRule :: [[Char]] -> [Char] -> [Char] -> ([[Char]], [([Char], [Char])])
splitSingleRLGNonTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 2
    then (nonTerminals, [(lastNT, ruleRightSide)])
    else (allNonTerminals, [(lastNT, (head ruleRightSide):freeNonTerminal)] ++ restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGNonTerminalRule (freeNonTerminal:nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals getAllNonTerminalsNames

splitSingleRLGTerminalRule :: [[Char]] -> [Char] -> [Char] -> ([[Char]], [([Char], [Char])])
splitSingleRLGTerminalRule nonTerminals ruleRightSide lastNT = if length ruleRightSide == 0
    then (nonTerminals, [(lastNT, "#")])
    else (allNonTerminals, [(lastNT, (head ruleRightSide):freeNonTerminal)] ++ restOfRules)
  where (allNonTerminals, restOfRules) = splitSingleRLGTerminalRule (freeNonTerminal:nonTerminals) (tail ruleRightSide) freeNonTerminal
        freeNonTerminal = getFreeNonTerminal nonTerminals getAllNonTerminalsNames

getAllNonTerminalsNames :: [[Char]]
getAllNonTerminalsNames = [ [x] | x <- ['A'..'Z'] ] ++ [ [x] ++ show y | y <- [1,2] :: [Integer], x <- ['A'..'Z'] ]

getFreeNonTerminal :: Foldable t => t [Char] -> [[Char]] -> [Char]
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
renameNonTerminals allNonTerminals (nt:nts) = (fromJust $ elemIndex nt allNonTerminals):renameNonTerminals allNonTerminals nts

createTransitions :: [[Char]] -> [([Char], [Char])] -> [(Int, Char, Int)]
createTransitions _ [] = []
createTransitions allNonTerminals (r:rs) = if snd r == "#"
                                              then createTransitions allNonTerminals rs
                                              else (fromState, symbol, toState):(createTransitions allNonTerminals rs)
                                           where fromState = fromJust $ elemIndex (fst r) allNonTerminals
                                                 symbol = head (snd r)
                                                 toState = fromJust $ elemIndex (tail (snd r)) allNonTerminals

createFinalStates :: Eq a => [a] -> [(a, [Char])] -> [Int]
createFinalStates _ [] = []
createFinalStates allNonTerminals (r:rs) = if snd r == "#"
                                            then (fromJust $ elemIndex (fst r) allNonTerminals):(createFinalStates allNonTerminals rs)
                                            else createFinalStates allNonTerminals rs

printNFSM :: Show a => a -> IO ()
printNFSM nfsm = putStrLn (show nfsm)

printRLG :: Show a => a -> IO ()
printRLG rlg = putStrLn (show rlg)

printRRG :: Show a => a -> IO ()
printRRG rrg = putStrLn (show rrg)

parseArgs :: [[Char]] -> IO a
parseArgs ("-i":xs) = loadInput xs >>= printRLG . loadRLG >> exit
parseArgs ("-1":xs) = loadInput xs >>= printRRG . convertToRRG . loadRLG  >> exit
parseArgs ("-2":xs) = loadInput xs >>= printNFSM . convertToNFSM . convertToRRG . loadRLG >> exit
parseArgs (x:_) = hPutStrLn stderr ("ERROR: Unknown argument '" ++ x ++ "'.") >> exitError
parseArgs [] = hPutStrLn stderr ("ERROR: No arguments passed (try '-i', '-1' or '-2').") >> exitError

loadInput :: [[Char]] -> IO String
loadInput (x:[]) = readFile x
loadInput [] = getContents
loadInput _ = hPutStrLn stderr "ERROR: Invalid number of files (max 1)." >> exitError

exit :: IO a
exit = exitWith ExitSuccess

exitError :: IO a
exitError = exitWith (ExitFailure 1)
