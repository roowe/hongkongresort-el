-module(data_file).
-export([start/1]).

start(Table) ->
    atom_to_list(Table).
