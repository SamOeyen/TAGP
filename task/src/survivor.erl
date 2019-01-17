-module(survivor).
-export([start/0, init/0]).
-export([entry/1]).
	
% Creation
start() ->
	(whereis(survivor) =:= undefined) orelse unregister(survivor),
	register(survivor, spawn(?MODULE, init, [])).

init() ->
	(ets:info(logbook) =:= undefined) orelse ets:delete(logbook),
	ets:new(logbook, [named_table, ordered_set, public]),
	loop().

% Loop
loop() ->
	receive
		stop ->
			ok
	end.

% Commands
entry(Data) ->
	ets:insert(logbook, {{now(), self()}, Data}).