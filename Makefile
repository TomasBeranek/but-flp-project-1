BIN=flp21-fun
BIN_OPT=-i
SRC_DIR=src
GHC_OPT=-Wall
TESTS_DIR=tests

all: run

run:
	./$(BIN) $(BIN_OPT)

install: compile

compile: clean
	ghc $(GHC_OPT) -o $(BIN) $(SRC_DIR)/*.hs

clean:
	rm -rf $(BIN) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi flp-fun-xberan46.zip

zip: clean
	zip -r flp-fun-xberan46.zip Makefile $(SRC_DIR) doc/ $(TESTS_DIR)

lint:
	hlint $(SRC_DIR)

test: compile
	python3 tests/tester.py $(BIN) $(TESTS_DIR)
