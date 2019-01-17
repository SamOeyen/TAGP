-module(location).
-export([create/2, init/2]).
-export([get_resInst/1, get_type/1, get_visitor/1, arrive/2, depart/1, remove/1]).

% Creation
create(ResInst_Pid, Type_Pid) ->
	spawn(?MODULE, init, [ResInst_Pid, Type_Pid]).

init(ResInst_Pid, Type_Pid) ->
	survivor:entry(location_created),
	loop(ResInst_Pid, Type_Pid, vacant).

% Loop
loop(ResInst_Pid, Type_Pid, Visitor_Pid) ->
	receive
		{get_resInst, ReplyFn} ->
			ReplyFn(ResInst_Pid),
			loop(ResInst_Pid, Type_Pid, Visitor_Pid);
		{get_type, ReplyFn} ->
			ReplyFn(Type_Pid),
			loop(ResInst_Pid, Type_Pid, Visitor_Pid);
		{get_visitor, ReplyFn} ->
			ReplyFn(Visitor_Pid),
			loop(ResInst_Pid, Type_Pid, Visitor_Pid);
		{arrive, Visitor} ->
			loop(ResInst_Pid, Type_Pid, Visitor);
		depart ->
			loop(ResInst_Pid, Type_Pid, vacant);
		remove ->
			stopped
	end.

% Commands
get_resInst(Location_Pid) ->
	msg:get(Location_Pid, get_resInst).
	
get_type(Location_Pid) ->
	msg:get(Location_Pid, get_type).

get_visitor(Location_Pid) ->
	msg:get(Location_Pid, get_visitor).

arrive(Location_Pid, Visitor_Pid) ->
	Location_Pid ! {arrive, Visitor_Pid}.

depart(Location_Pid) ->
	Location_Pid ! depart.

remove(Location_Pid) ->
	Location_Pid ! remove.