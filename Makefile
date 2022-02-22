BIN=flp21-fun
SRC_DIR=src
GHC_OPT=-Wall

all: run

run:
	./$(BIN)

install: compile

compile: clean
	ghc $(GHC_OPT) -o $(BIN) $(SRC_DIR)/*.hs

clean:
	rm -rf $(BIN) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi flp-fun-xberan46.zip

zip: clean
	zip -r flp-fun-xberan46.zip Makefile src/ doc/ test/
