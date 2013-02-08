.PHONY: clean all distclean

TOP = $(PWD)
SRC = src
EBIN = ebin
DOC = doc
ERLC = erlc

WARNS = +warn_exported_vars +warn_unused_import +warn_missing_spec
ERLC_FLAGS = +native +debug_info $(WARNS)

SRC_MODULES = \
	graph \
	graph_lib \
	dijkstra \
	bfs \
	dfs \
	kruskal \
	heap \
	union_find \
	demo \
	doc

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

edoc: $(TARGETS)
	@(./makedoc.rb)

dialyze: $(TARGETS)
	dialyzer -n -Wunmatched_returns $(EBIN)/*.beam

clean:
	$(RM) $(EBIN)/*.beam

distclean: clean
	$(RM) $(DOC)/*.html $(DOC)/*.css $(DOC)/*.png $(DOC)/edoc-info

