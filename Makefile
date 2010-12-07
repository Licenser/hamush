SRC_DIR=src
BIN_DIR=ebin
DOC_DIR=doc
CC=erlc
ERL=erl
SUB_PROJECTS=mushdb mushcmd
CC_FLAGS=+debug_info -I include -pa lib/mushdb/ebin -pa lib/mushcmd/ebin
RUN_FLAGS=-pa lib/mushdb/ebin -pa lib/mushcmd/ebin
BEAMS=ebin/ham_app.beam ebin/mcon_sup.beam ebin/mcon_con_sup.beam ebin/mcon_listener.beam ebin/mcon_connection.beam ebin/hamush.beam ebin/cmds_interaction.beam ebin/cmds_movement.beam ebin/cmds_initial.beam ebin/cmds_creation.beam ebin/ham_sup.beam ebin/ham_lisp.beam ebin/ham_fun_storage.beam ebin/fun_core.beam ebin/fun_communication.beam ebin/fun_lists.beam ebin/fun_privileged.beam
PROJECT=hamush



all: $(BEAMS)
	for p in $(SUB_PROJECTS); do (cd lib/$$p; make); done

clean: 
	-rm $(BIN_DIR)/*.beam $(DOC_DIR)/*
	-for p in $(SUB_PROJECTS); do (cd lib/$$p; make clean); done

ebin/%.beam: src/%.erl
	$(CC) $(CC_FLAGS) -o $(BIN_DIR) $<

run: all
	$(ERL) $(RUN_FLAGS) -pa $(BIN_DIR) -eval "toolbar:start(), application:start(sasl), application:start(mushdb), application:start(mushcmd), application:start($(PROJECT))."

doc:
	$(ERL) -eval 'edoc:application($(PROJECT), "", []).'
