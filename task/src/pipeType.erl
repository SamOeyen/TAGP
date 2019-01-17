-module(pipeType).
-export([create/0, init/0]).
-export([flow/1]).

% Creation
create() ->
	{ok, spawn(?MODULE, init, [])}.

init() ->
	survivor:entry(pipeType_created),
	loop().

% Loop
loop() ->
	receive
		{initial_state, [ResInst_Pid, TypeOptions], ReplyFn} ->
			Location = location:create(ResInst_Pid, empty_space),
			In = connector:create(ResInst_Pid, simple_pipe),
			Out = connector:create(ResInst_Pid, simple_pipe),
			ReplyFn(#{resInst => ResInst_Pid, locations => [Location], connectors => [In, Out], type_options => TypeOptions, name => TypeOptions}), 
			loop();
		{location_list, State, ReplyFn} ->
			#{locations := L_List} = State,
			ReplyFn(L_List),
			loop();
		{connector_list, State, ReplyFn} ->
			#{connectors := C_List} = State,
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