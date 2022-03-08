#  File    Makefile
#  Author  Tomas Beranek <xberan46@stud.fit.vutbr.cz>
#  Date    1.3.2022
#  Up2date sources can be found at: https://github.com/TomasBeranek/but-flp-project-1

BIN=flp21-fun
BIN_OPT=-i
SRC_DIR=src
GHC_OPT=-Wall
TESTS_DIR=tests
TEST_SRC_NUM=1
TEST_DST_NUM=1

all: install

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

copy_test:
	cp $(TESTS_DIR)/test$(TEST_SRC_NUM).args $(TESTS_DIR)/test$(TEST_DST_NUM).args
	cp $(TESTS_DIR)/test$(TEST_SRC_NUM).in $(TESTS_DIR)/test$(TEST_DST_NUM).in
	cp $(TESTS_DIR)/test$(TEST_SRC_NUM).out $(TESTS_DIR)/test$(TEST_DST_NUM).out
	cp $(TESTS_DIR)/test$(TEST_SRC_NUM).rc $(TESTS_DIR)/test$(TEST_DST_NUM).rc

format:
	cat $(SRC_DIR)/Main.hs | hindent > tmp
	cat tmp > $(SRC_DIR)/Main.hs
	rm tmp
