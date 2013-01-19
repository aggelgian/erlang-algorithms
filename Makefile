ERLC=erlc
ERLC_FLAGS= +native
ERL_FILES=graph.erl graph_lib.erl dijkstra.erl bfs.erl e.erl
BEAM_FILES=$(patsubst %.erl,%.beam,$(ERL_FILES))

default: $(BEAM_FILES)

%.beam: %.erl $(HRL_FILES)
	$(ERLC) $(ERLC_FLAGS) $<

clean:
	rm $(BEAM_FILES)
