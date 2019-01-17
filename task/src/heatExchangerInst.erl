-module(heatExchangerInst).
-export([create/5, init/5]).
-export([get_temp_influence/1]).

% Creation
create(Host, ResType_Pid, PipeType_Pid, HE_Link, Name) ->
	{ok, spawn(?MODULE, init, [Host, ResType_Pid, PipeType_Pid, HE_Link, Name])}.

init(Host, ResType_Pid, PipeType_Pid, HE_Link, Name) ->
	{ok, PipeInst_Pid} = resource_instance:create(pipeInst, [self(), PipeType_Pid, stand_in]),
	{ok, State} = resource_type:get_initial_state(ResType_Pid, self(), [PipeInst_Pid, Name]),
	survivor:entry({heatExchangerInst_created, State}),
	loop(Host, ResType_Pid, State, HE_Link).

% Loop
loop(Host, ResType_Pid, State, HE_Link) ->
	receive
		{get_host, ReplyFn} ->
			ReplyFn(Host),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_type, ReplyFn} ->
			ReplyFn(ResType_Pid),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_state, ReplyFn} ->
			ReplyFn(State),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_locations, ReplyFn} ->
			{ok, L_List} = resource_type:get_locations(ResType_Pid, State),
			ReplyFn(L_List),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_connectors, ReplyFn} ->
			{ok, C_List} = resource_type:get_connectors(ResType_Pid, State),
			ReplyFn(C_List),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_ops, ReplyFn} ->
			ReplyFn([get_temp_influence]),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = resource_type:get_flow_influence(ResType_Pid, State),
			ReplyFn(InfluenceFn),
			loop(Host, ResType_Pid, State, HE_Link);
		{get_temp_influence, ReplyFn} ->
			ReplyFn(heatExchangeLink:get_temp_influence(HE_Link)),
			loop(Host, ResType_Pid, State, HE_Link)
	end.

% Commands
get_temp_influence(HeatExchangerInst_Pid) ->
	msg:get(HeatExchangerInst_Pid, get_temp_influence).