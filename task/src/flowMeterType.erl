-module(flowMeterType).
-export([create/0, init/0]).
-export([compute_flow/2, get_circuitFn/3, compute/2, evaluate/3]).
-export([flow/1]).

% Creation
create() ->
	{ok, spawn(?MODULE, init, [])}.

init() ->
	survivor:entry(flowMeterType_created),
	loop().

% Loop
loop() ->
	receive
		{initial_state, [ResInst_Pid, [PipeInst_Pid, RealWorldCmdFn, Name]], ReplyFn} ->
			{ok, [Loc | _]} = resource_instance:get_locations(PipeInst_Pid),
			{ok, Fluidum} = location:get_visitor(Loc),
			ReplyFn(#{resInst => ResInst_Pid, pipeInst => PipeInst_Pid, fluidum => Fluidum, rw_cmd => RealWorldCmdFn, name => Name}), 
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
			loop();
		{add_fluidum, [State, Fluidum], ReplyFn} ->
			ReplyFn(State#{fluidum := Fluidum}),
			loop();
		{estimate_flow, State, ReplyFn} ->
			#{fluidum := Fluidum} = State,
			{ok, Circuit} = fluidumInst:get_circuit(Fluidum),
			#{resInst := FlowMeter_Pid} = State,
			Flow = compute_flow(Circuit, FlowMeter_Pid),
			ReplyFn(Flow),
			loop();
		{measure_flow, State, ReplyFn} ->
			#{rw_cmd := ExecFn} = State,
			ExecFn(),
			ReplyFn(ok),
			loop()
	end.

% Flow
flow(N) ->
	- 0.01 * N.

% Calculation
compute_flow(Circuit, FlowMeter_Pid) ->
	Interval = {0, 10},
	CircuitFn = get_circuitFn(maps:next(maps:iterator(Circuit)), [], FlowMeter_Pid),
	compute(Interval, CircuitFn).

get_circuitFn(none, Acc, _) ->
	Acc;
	
get_circuitFn({FlowMeter_Pid, _, T}, Acc, FlowMeter_Pid) ->
	FlowInfluenceFn = fun(Flow) -> flow(Flow) end,
	get_circuitFn(maps:next(T), [FlowInfluenceFn | Acc], FlowMeter_Pid);

get_circuitFn({Pid, _, T}, Acc, FlowMeter_Pid) ->
	{ok, FlowInfluenceFn} = resource_instance:get_flow_influence(Pid),
	get_circuitFn(maps:next(T), [FlowInfluenceFn | Acc], FlowMeter_Pid).
	
compute({Low, High}, _) when (High - Low) < 1 ->
	(Low + High) / 2;

compute({Low, High}, CircuitFn) ->
	L = evaluate(Low, CircuitFn, 0),
	H = evaluate(High, CircuitFn, 0),
	Middle = (L + H) / 2,
	M = evaluate(Middle, CircuitFn, 0),
	if
		M > 0 ->
			compute({Low, Middle}, CircuitFn);
		true ->
			compute({Middle, High}, CircuitFn)
	end.

evaluate(_, [], Acc) ->
	Acc;

evaluate(Flow, [Fn | T], Acc) ->
	evaluate(Flow, T, Acc + Fn(Flow)).