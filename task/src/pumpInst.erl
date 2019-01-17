-module(pumpInst).
-export([create/5, init/5]).
-export([is_on/1, switch_on/1, switch_off/1]).

% Creation
create(Host, ResType_Pid, PipeType_Pid, RealWorldCmdFn, Name) ->
	{ok, spawn(?MODULE, init, [Host, ResType_Pid, PipeType_Pid, RealWorldCmdFn, Name])}.

init(Host, ResType_Pid, PipeType_Pid, RealWorldCmdFn, Name) ->
	{ok, PipeInst_Pid} = resource_instance:create(pipeInst, [self(), PipeType_Pid, stand_in]),
	{ok, State} = resource_type:get_initial_state(ResType_Pid, self(), [PipeInst_Pid, RealWorldCmdFn, Name]),
	survivor:entry({pumpInst_created, State}),
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
			ReplyFn([is_on, switch_on, switch_off]),
			loop(Host, ResType_Pid, State);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = resource_type:get_flow_influence(ResType_Pid, State),
			ReplyFn(InfluenceFn),
			loop(Host, ResType_Pid, State);
		{is_on, ReplyFn} ->
			{ok, OnOff} = msg:get(ResType_Pid, is_on, State),
			ReplyFn(OnOff),
			loop(Host, ResType_Pid, State);
		switch_on ->
			{ok, NewState} = msg:get(ResType_Pid, switch_on, State),
			loop(Host, ResType_Pid, NewState);
		switch_off ->
			{ok, NewState} = msg:get(ResType_Pid, switch_off, State),
			loop(Host, ResType_Pid, NewState)
	end.
	
% Commands
is_on(PumpInst_Pid) ->
	msg:get(PumpInst_Pid, is_on).

switch_on(PumpInst_Pid) ->
	PumpInst_Pid ! switch_on.

switch_off(PumpInst_Pid) ->
	PumpInst_Pid ! switch_off.