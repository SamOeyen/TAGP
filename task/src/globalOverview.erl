-module(globalOverview).
-export([create/0, init/0]).
-export([get_parts/1, add_part/3, get_part/2, remove/1]).

% Creation
create() ->
	spawn(?MODULE, init, []).

init() ->
	survivor:entry(global_created),
	Parts = #{},
	loop(Parts).

% Loop
loop(Parts) ->
	receive
		{get_parts, ReplyFn} ->
			ReplyFn(Parts),
			loop(Parts);
		{add_part, {Key, Value}, ReplyFn} ->
			ReplyFn(ok),
			loop(Parts#{Key => Value});
		{get_part, Key, ReplyFn} ->
			#{Key := Part_Pid} = Parts,
			ReplyFn(Part_Pid),
			loop(Parts);
		remove ->
			stopped
	end.

% Commands
get_parts(Global_Pid) ->
	msg:get(Global_Pid, get_parts).

add_part(Global_Pid, Key, Value) ->
	msg:get(Global_Pid, add_part, {Key, Value}).

get_part(Global_Pid, Key) ->
	msg:get(Global_Pid, get_part, Key).

remove(Global_Pid) ->
	Global_Pid ! remove.