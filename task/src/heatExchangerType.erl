-module(heatExchangerType).
-export([create/0, init/0]).
-export([flow/1]).

% Creation
create() ->
	{ok, spawn(?MODULE, init, [])}.

init() ->
	survivor:entry(heatExchangerType_created),
	loop().

% Loop
loop() ->
	receive
		{initial_state, [ResInst_Pid, [PipeInst_Pid, Name]], ReplyFn} ->
			ReplyFn(#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid, name => Name}), 
			loop();
		{location_list, State, ReplyFn} ->
			#{pipeInst := PipeInst} = State,
			{ok, L_List} = resource_instance:get_locations(PipeInst),
			ReplyFn(L_List),
			loop();
		{connector_list, State, ReplyFn} ->
			#{pipeInst := PipeInst} = State,
			{ok, C_List} = resource_instance:get_connectors(PipeInst),
			ReplyFn(C_List),
			loop();
		{flow_influence, _, ReplyFn} ->
			FlowInfluenceFn = fun(Flow) -> flow(Flow) end,
			ReplyFn(FlowInfluenceFn),
			loop()
	end.

% Flow
flow(N) ->
	- 0.01 * N.