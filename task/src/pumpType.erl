-module(pumpType).
-export([create/0, init/0]).
-export([flow/2]).

% Creation
create() ->
	{ok, spawn(?MODULE, init, [])}.

init() ->
	survivor:entry(pumpType_created),
	loop().

% Loop
loop() ->
	receive
		{initial_state, [ResInst_Pid, [PipeInst_Pid, RealWorldCmdFn, Name]], ReplyFn} ->
			ReplyFn(#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid, on_off => off, rw_cmd => RealWorldCmdFn, name => Name}), 
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
		{flow_influence, State, ReplyFn} ->
			#{on_off := OnOff} = State,
			FlowInfluenceFn = fun(Flow) -> flow(Flow, OnOff) end,
			ReplyFn(FlowInfluenceFn),
			loop();
		{is_on, State, ReplyFn} ->
			#{on_off := OnOff} = State,
			ReplyFn(OnOff),
			loop();
		{switch_on, State, ReplyFn} ->
			#{rw_cmd := ExecFn} = State,
			ExecFn(on),
			ReplyFn(State#{on_off := on}),
			loop();
		{switch_off, State, ReplyFn} ->
			#{rw_cmd := ExecFn} = State,
			ExecFn(off),
			ReplyFn(State#{on_off := off}),
			loop()
	end.

% Flow
flow(Flow, on) ->
	(250 - 5 * Flow - 2 * Flow * Flow);

flow(_, _) ->
	0.