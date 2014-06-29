-module(union_find_demo).
-export([uf1/0]).

-spec uf1() -> ok.

uf1() ->
  L = [a,b,c,d,e,f,g,h,i,j],
  F = union_find:singletons_from_list(L),
%%  union_find:pprint(F),
  10 = union_find:number_of_sets(F),
  true = union_find:union(F, a, e),
%%  union_find:pprint(F),
  true = union_find:union(F, a, d),
%%  union_find:pprint(F),
  true = union_find:union(F, g, i),
%%  union_find:pprint(F),
  true = union_find:union(F, h, f),
  union_find:pprint(F),
  6 = union_find:number_of_sets(F),
  [a,d,e] = lists:sort(union_find:set_elements(F, e)),
  g = union_find:find(F, i),
  3 = union_find:set_size(F, e),
  true = union_find:delete(F),
  ok.
