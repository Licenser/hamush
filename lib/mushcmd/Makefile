SRC_DIR=src
BIN_DIR=ebin
DOC_DIR=doc
CC=erlc
ERL=erl
CC_FLAGS=+debug_info -I include
BEAMS=ebin/mcmd_app.beam ebin/mcmd_sup.beam ebin/mcmd_worker.beam ebin/mushcmd.beam ebin/mcmd_worker_sup.beam ebin/mcmd_cmd_storage.beam
PROJECT=mushcmd



all: $(BEAMS)

clean: 
	@rm $(BIN_DIR)/*.beam $(DOC_DIR)/*

ebin/%.beam: src/%.erl
	$(CC) $(CC_FLAGS) -o $(BIN_DIR) $<

run: all
	$(ERL) -pa ../mushdb-0.0.0/ebin -pa $(BIN_DIR) -eval "application:start($(PROJECT))."

doc:
	$(ERL) -eval 'edoc:application($(PROJECT), "", []).'
