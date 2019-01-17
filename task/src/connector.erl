-module(connector).
-export([create/2, init/2]).
-export([get_resInst/1, get_type/1, get_connection/1, connect/2, disconnect/1, remove/1]).

% Creation
create(ResInst_Pid, Type_Pid) ->
	spawn(?MODULE, init, [ResInst_Pid, Type_Pid]).

init(ResInst_Pid, Type_Pid) ->
	survivor:entry(connector_created),
	loop(ResInst_Pid, Type_Pid, no_connection).

% Loop
loop(ResInst_Pid, Type_Pid, Connection_Pid) ->
	receive
		{get_resInst, ReplyFn} ->
			ReplyFn(ResInst_Pid),
			loop(ResInst_Pid, Type_Pid, Connection_Pid);
		{get_type, ReplyFn} ->
			ReplyFn(Type_Pid),
			loop(ResInst_Pid, Type_Pid, Connection_Pid);
		{get_connection, ReplyFn} ->
			ReplyFn(Connection_Pid),
			loop(ResInst_Pid, Type_Pid, Connection_Pid);
		{connect, Connection} ->
			survivor:entry({connection_made, self(), to, Connection, for, ResInst_Pid}),
			loop(ResInst_Pid, Type_Pid, Connection);
		disconnect ->
			loop(ResInst_Pid, Type_Pid, no_connection);
		remove ->
			stopped
	end.

% Commands
get_resInst(Connector_Pid) ->
	msg:get(Connector_Pid, get_resInst).
	
get_type(Connector_Pid) ->
	msg:get(Connector_Pid, get_type).

get_connection(Connector_Pid) ->
	msg:get(Connector_Pid, get_connection).

connect(Connector_Pid, Connection_Pid) ->
	Connector_Pid ! {connect, Connection_Pid}.

disconnect(Connector_Pid) ->
	Connector_Pid ! disconnect.

remove(Connector_Pid) ->
	Connector_Pid ! remove.