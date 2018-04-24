.PHONY: all clean demo dialyzer edoc distclean

TOP = $(PWD)
SRC  = $(PWD)/src
EBIN = $(PWD)/ebin
DEMO = $(PWD)/demo/src
DEMO_DATA = $(PWD)/demo/data
DOC = doc
ERLC = erlc

WARNS = +warn_exported_vars +warn_unused_import +warn_missing_spec
DIALYZER_APPS = erts kernel stdlib compiler crypto syntax_tools
DIALYZER_FLAGS = -Wunmatched_returns
ERLC_FLAGS = +native +debug_info $(WARNS)
ERLC_MACROS = -DDEMO_DATA=\"$(DEMO_DATA)\"

SRC_MODULES = \
	graph \
	graph_lib \
	dijkstra \
	bfs \
	dfs \
	kruskal \
	heap \
	union_find \
	edmonds_karp \
	a_star

DEMO_MODULES = \
	demo \
	graph_demo \
	heap_demo \
	union_find_demo \
	bfs_demo \
	dfs_demo \
	dijkstra_demo \
	kruskal_demo \
	flow_demo \
	a_star_demo \
	import_export_demo

EDOC_MODULES = \
	doc \

TARGETS = \
	src_target \
	demo_target \
	edoc_target

ERL_DIRS = \
	$(SRC) \
	$(DEMO)

vpath %.erl $(ERL_DIRS)

default: src_target demo_target

all: $(TARGETS) dialyzer

src_target: $(SRC_MODULES:%=$(EBIN)/%.beam)

demo_target: $(DEMO_MODULES:%=$(EBIN)/%.beam)

edoc_target: $(EDOC_MODULES:%=$(EBIN)/%.beam)

$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $(ERLC_MACROS) -o $(EBIN) $<

edoc: $(TARGETS)
	@(./makedoc.rb)

demo: $(TARGETS)
	@(./rundemo.rb)

dialyzer: .plt $(TARGETS)
	dialyzer -n -nn --plt $< $(DIALYZER_FLAGS) $(EBIN)/*.beam

.plt:
	dialyzer --build_plt --output_plt $@ --apps $(DIALYZER_APPS)

clean:
	$(RM) $(EBIN)/*.beam

distclean: clean
	$(RM) $(DOC)/*.html $(DOC)/*.css $(DOC)/*.png $(DOC)/edoc-info .plt
