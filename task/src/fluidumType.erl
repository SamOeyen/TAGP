-module(fluidumType).
-export([create/0, init/0]).
-export([discover_circuit/1, discover_circuit/2, process_connector/4, extract/1, extract/2, extract_stand_in/2]).
-export([flow/1]).

% Creation
create() ->
	{ok, spawn(?MODULE, init, [])}.

init() ->
	survivor:entry(fluidumType_created),
	loop().

% Loop
loop() ->
	receive
		{initial_state, [ResInst_Pid, [Root_Pid, TypeOptions]], ReplyFn} ->
			Circuit = discover_circuit(Root_Pid),
			ReplyFn(#{resInst => ResInst_Pid, circuit => Circuit, type_options => TypeOptions}), 
			loop();
		{location_list, _, ReplyFn} ->
			ReplyFn([]),
			loop();
		{connector_list, _, ReplyFn} ->
			ReplyFn([]),
			loop();
		{flow_influence, _, ReplyFn} ->
			FlowInfluenceFn = fun(Flow) -> flow(Flow) end,
			ReplyFn(FlowInfluenceFn),
			loop();
		{get_circuit, State, ReplyFn} ->
			#{circuit := Circuit} = State,
			{_, CircuitMap} = Circuit,
			ReplyFn(extract(CircuitMap)),
			loop()
	end.

% Flow
flow(_) ->
	error.

% Calculation
discover_circuit(Root_Pid) ->
	Circuit = discover_circuit([Root_Pid], #{}),
	{Root_Pid, Circuit}.
	
discover_circuit([], Circuit) ->
	Circuit;

discover_circuit([no_connection | Todo], Circuit) ->
	discover_circuit(Todo, Circuit);

discover_circuit([Pid | Todo], Circuit) ->
	{Updated_Todo, Updated_Circuit} = process_connector(Pid, maps:find(Pid, Circuit), Todo, Circuit),
	discover_circuit(Updated_Todo, Updated_Circuit).
	
process_connector(Pid, error, Todo, Circuit) ->
	Updated_Circuit = Circuit#{Pid => processed},
	{ok, Connection_Pid} = connector:get_connection(Pid),
	Updated_Todo = [Connection_Pid | Todo],
	{ok, ResInst_Pid} = connector:get_resInst(Pid),
	{ok, C_List} = resource_instance:get_connectors(ResInst_Pid),
	{C_List ++ Updated_Todo, Updated_Circuit};

process_connector(_, _, Todo, Circuit) ->
	{Todo, Circuit}.

extract(Circuit) ->
	extract(maps:next(maps:iterator(Circuit)), #{}).

extract(none, NewCircuit) ->
	NewCircuit;

extract({Pid, _, T}, NewCircuit) ->
	{ok, PipeInst_Pid} = connector:get_resInst(Pid),
	{ok, PipeState} = resource_instance:get_state(PipeInst_Pid),
	#{type_options := TypeOptions} = PipeState,
	StandIn_Pid = extract_stand_in(PipeInst_Pid, TypeOptions),
	extract(maps:next(T), NewCircuit#{StandIn_Pid => processed}).

extract_stand_in(PipeInst_Pid, stand_in) ->
	{ok, ResInst_Pid} = resource_instance:get_host(PipeInst_Pid),
	ResInst_Pid;

extract_stand_in(PipeInst_Pid, _) ->
	PipeInst_Pid.