SRC_DIR=src
BIN_DIR=ebin
DOC_DIR=doc
CC=erlc
ERL=erl
CC_FLAGS=+debug_info -I include
BEAMS=ebin/mdb_app.beam ebin/mdb_element.beam ebin/mdb_store.beam ebin/mdb_sup.beam ebin/mushdb.beam ebin/mdb_event.beam ebin/mdb_store_sup.beam
PROJECT=mushdb



all: $(BEAMS)

clean: 
	@rm $(BIN_DIR)/*.beam $(DOC_DIR)/*

ebin/%.beam: src/%.erl
	$(CC) $(CC_FLAGS) -o $(BIN_DIR) $<

run: all
	$(ERL) -pa $(BIN_DIR) -eval "application:start(mushdb)."

doc:
	$(ERL) -eval 'edoc:application($(PROJECT), "", []).'
