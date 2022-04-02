# RLG to NFSM/RRG Converter

RLG to NFSM/RRG Converter is an application written in Haskell that allows you to convert RLGs (right linear grammars) into NFSMs (non-deterministic finite state machines) or RRGs (right regular grammars). RLGs cannot contain simple rules (e.g. ```A -> B```).

## Installation
To compile the app it is necessary to have the GHC compiler installed. To create the ```flp21-fun``` binary run:

```
git clone https://github.com/TomasBeranek/but-flp-project-1.git
cd but-flp-project-1/
make
```

## Usage
The app can be run:
```
flp21-fun OPTION [INPUT_FILE]
```

where
- ```OPTION``` is one of:
  - ```-i``` prints the loaded RLG to ```stdout```. The printed RLG is in the standard format, i.e. without duplicates, alphabetically sorted, etc.
  - ```-1``` prints the converted RRG to ```stdout``` in the same standard format as the input RLG. The exception is newly created non-terminals, which can be named ```A1```, ```B1```, etc.
  - ```-2``` prints the converted NFSM to ```stdout```.
- ```INPUT_FILE``` is an optional parameter containing the name of the input file. If the file is not specified, the program reads the input from ```stdin```.

For examples of RLG input formats and RRG/NFSM output formats, see the ```tests/``` directory.

## Tests
To run the tests, run:

```
make test
```

Each test is run in both ```stdin``` and ```INPUT_FILE``` mode.

## Test descriptions

- ```test01``` - RRG from the assignment. Output NFSM.
- ```test02``` - RRG with duplicities. Output RLG.
- ```test03``` - RLG with single simple full-terminal rule. Output RLG.
- ```test04``` - RRG with duplicities and wrong alphabetical order. Output RRG.
- ```test05``` - RLG with single simple full-terminal rule. Output RRG.
- ```test06``` - RLG with more complex full-terminal and non-terminal rules. Output RRG.
- ```test07``` - exceeding simple non-terminal names by splitting rules. Output RRG.
- ```test08``` - same as ```test05```. Output NFSM.
- ```test09``` - same as ```test06```. Output NFSM.
- ```test10``` - same as ```test07```. Output NFSM.
- ```test11``` - random RLG. Output NFSM.
- ```test12``` - random RLG. Output NFSM.
- ```test13``` - random RLG. Output NFSM.
- ```test14``` - incorrect program option. Output ERROR.
- ```test15``` - more than one correct argument. Output ERROR.
- ```test16``` - zero arguments. Output ERROR.
- ```test17``` - more than one file. Output ERROR.
- ```test18``` - empty input. Output ERROR.
- ```test19``` - empty non-terminal set. Output ERROR.
- ```test20``` - incorrect format of non-terminal set. Output ERROR.
- ```test21``` - empty terminal set. Output ERROR.
- ```test22``` - incorrect format of terminal set. Output ERROR.
- ```test23``` - no starting symbol. Output ERROR.
- ```test24``` - incorrect format of starting symbol. Output ERROR.
- ```test25``` - empty rule set. Output ERROR.
- ```test26``` - incorrect format of rule set. Output ERROR.
- ```test27``` - rules contain non-existing non-terminal. Output ERROR.
- ```test28``` - rules contain non-existing terminal. Output ERROR.
- ```test29``` - terminal set is empty and rule set contains only #-rules. Output NFSM.
- ```test30``` - starting symbol is non-existing non-terminal. Output ERROR.

## Features implemented beyond requirements
- checking of the format of program input arguments. See ```test14-17```.
- checking of the format of input RLG. See ```test18-30```.
- set of tests.
- script ```tests/tester.py``` for automated testing.
