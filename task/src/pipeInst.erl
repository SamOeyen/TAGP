-module(pipeInst).
-export([create/3, init/3]).

% Creation
create(Host, ResType_Pid, TypeOptions) ->
	{ok, spawn(?MODULE, init, [Host, ResType_Pid, TypeOptions])}.

init(Host, ResType_Pid, TypeOptions) ->
	{ok, State} = resource_type:get_initial_state(ResType_Pid, self(), TypeOptions),
	survivor:entry({pipeInst_created, State}),
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
			ReplyFn([]),
			loop(Host, ResType_Pid, State);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = resource_type:get_flow_influence(ResType_Pid, State),
			ReplyFn(InfluenceFn),
			loop(Host, ResType_Pid, State)
	end.