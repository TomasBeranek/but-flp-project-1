BIN=plg-nka
SRC_DIR=src

all: run

run:
	./$(BIN)

install: compile

compile: clean
	ghc -o $(BIN) $(SRC_DIR)/*.hs

clean:
	rm -rf $(BIN) $(SRC_DIR)/*.o $(SRC_DIR)/*.hi
