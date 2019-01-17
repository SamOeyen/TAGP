-module(flowMeterInst).
-export([create/5, init/5]).
-export([add_fluidum/2, estimate_flow/1, measure_flow/1]).

% Creation
create(Host, ResType_Pid, PipeType_Pid, RealWorldCmdFn, Name) ->
	{ok, spawn(?MODULE, init, [Host, ResType_Pid, PipeType_Pid, RealWorldCmdFn, Name])}.

init(Host, ResType_Pid, PipeType_Pid, RealWorldCmdFn, Name) ->
	{ok, PipeInst_Pid} = resource_instance:create(pipeInst, [self(), PipeType_Pid, stand_in]),
	{ok, State} = resource_type:get_initial_state(ResType_Pid, self(), [PipeInst_Pid, RealWorldCmdFn, Name]),
	survivor:entry({flowMeterInst_created, State}),
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
			ReplyFn([add_fluidum, estimate_flow, measure_flow]),
			loop(Host, ResType_Pid, State);
		{get_flow_influence, ReplyFn} ->
			{ok, InfluenceFn} = resource_type:get_flow_influence(ResType_Pid, State),
			ReplyFn(InfluenceFn),
			loop(Host, ResType_Pid, State);
		{add_fluidum, Fluidum, ReplyFn} ->
			{ok, NewState} = msg:get(ResType_Pid, add_fluidum, [State, Fluidum]),
			ReplyFn(ok),
			loop(Host, ResType_Pid, NewState);
		{estimate_flow, ReplyFn} ->
			{ok, Flow} = msg:get(ResType_Pid, estimate_flow, State),
			ReplyFn(Flow),
			loop(Host, ResType_Pid, State);
		{measure_flow, ReplyFn} ->
			{ok, Answer} = msg:get(ResType_Pid, measure_flow, State),
			ReplyFn(Answer),
			loop(Host, ResType_Pid, State)
	end.

% Commands
add_fluidum(FlowMeterInst_Pid, Fluidum) ->
	msg:get(FlowMeterInst_Pid, add_fluidum, Fluidum).
	
estimate_flow(FlowMeterInst_Pid) ->
	msg:get(FlowMeterInst_Pid, estimate_flow).

measure_flow(FlowMeterInst_Pid) ->
	msg:get(FlowMeterInst_Pid, measure_flow).