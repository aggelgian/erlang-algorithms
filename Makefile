.PHONY: clean all

TOP = $(PWD)
SRC = src
EBIN = ebin
ERLC = erlc

WARNS = +warn_exported_vars +warn_unused_import +warn_missing_spec
ERLC_FLAGS = +native +debug_info $(WARNS)

SRC_MODULES = \
	graph \
	graph_lib \
	dijkstra \
	bfs \
	dfs \
	heap \
	demo

TARGETS = \
	src_target

ERL_DIRS = \
	src

vpath %.erl $(ERL_DIRS)

default: $(TARGETS)

all: default dialyze

src_target: $(SRC_MODULES:%=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

dialyze: $(TARGETS)
	dialyzer -n -Wunmatched_returns $(EBIN)/*.beam

clean:
	rm $(EBIN)/*.beam

