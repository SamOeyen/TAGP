-module(fluidumInst).
-export([create/4, init/4]).
-export([get_circuit/1]).

% Creation
create(Host, ResType_Pid, Root_Pid, Name) ->
	{ok, spawn(?MODULE, init, [Host, ResType_Pid, Root_Pid, Name])}.

init(Host, ResType_Pid, Root_Pid, Name) ->
	{ok, State} = resource_type:get_initial_state(ResType_Pid, self(), [Root_Pid, Name]),
	survivor:entry({fluidumInst_created, State}),
	loop(Host, ResType_Pid, State).

% Loop
loop(Host, ResType_Pid, State) ->
	receive
		{get_host, ReplyFn} ->
			ReplyFn(Host),
			loop(Host, ResType_Pid, State);
		{get_type, ReplyFn} ->
			ReplyFn(ResType_Pid),
			loop(Host, ResType_Pid, State);
		{get_state, ReplyFn} ->
			ReplyFn(State),
			loop(Host, ResType_Pid, State);
		{get_locations, ReplyFn} ->
			{ok, L_List} = resource_type:get_locations(ResType_Pid, State),
			ReplyFn(L_List),
			loop(Host, ResType_Pid, State);
		{get_connectors, ReplyFn} ->
			{ok, C_List} = resource_type:get_connectors(ResType_Pid, State),
			ReplyFn(C_List),
			loop(Host, ResType_Pid, State);
		{get_ops, ReplyFn} ->
			ReplyFn([get_circuit]),
			loop(Host, ResType_Pid, State);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = resource_type:get_flow_influence(ResType_Pid, State),
			ReplyFn(InfluenceFn),
			loop(Host, ResType_Pid, State);
		{get_circuit, ReplyFn} ->
			{ok, Circuit} = msg:get(ResType_Pid, get_circuit, State),
			ReplyFn(Circuit),
			loop(Host, ResType_Pid, State)
	end.

% Commands
get_circuit(FluidumInst_Pid) ->
	msg:get(FluidumInst_Pid, get_circuit).