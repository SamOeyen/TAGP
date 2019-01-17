-module(system).
-export([main/0]).
-export([create_types/0, create_pipe/3, create_pump/4, create_flow_meter/4, create_heat_exchanger/5, create_fluidum/4]).
-export([rw_pump/0, rw_flow_meter/0]).
-export([get_in_connector/1, get_out_connector/1, connect/2]).
-export([get_circuit/1, extract/2, get_ordered_circuit/1, get_ordered_circuit/3, extract_stand_in/2, get_named_circuit/1, name/2]).
-export([check_pump/1, toggle_pump/1, toggle_pump/2]).
-export([fill_flowMeter/2, check_flowMeter/1]).
-export([check_heatExchanger/3]).

main() ->
	survivor:start(),
	GlobalOverview_Pid = create_globalOverview(),
	
	% Types
	{PipeType, PumpType, FlowMeterType, HeatExchangerType, FluidumType} = create_types(),
	
	% Instances
	P1 = create_pipe(PipeType, GlobalOverview_Pid, pipe_1),
	P2 = create_pipe(PipeType, GlobalOverview_Pid, pipe_2),
	P3 = create_pipe(PipeType, GlobalOverview_Pid, pipe_3),
	P4 = create_pipe(PipeType, GlobalOverview_Pid, pipe_4),

	Pump = create_pump(PumpType, PipeType, GlobalOverview_Pid, pump),

	FlowMeter = create_flow_meter(FlowMeterType, PipeType, GlobalOverview_Pid, flow_meter),
	
	HE_Link_1 = #{delta => 10},
	HeatExchanger_1 = create_heat_exchanger(HeatExchangerType, PipeType, HE_Link_1, GlobalOverview_Pid, heat_exchanger_1),
	
	HE_Link_2 = #{delta => 1},
	HeatExchanger_2 = create_heat_exchanger(HeatExchangerType, PipeType, HE_Link_2, GlobalOverview_Pid, heat_exchanger_2),
	
	% Connections
	connect(HeatExchanger_1, P1),
	connect(P1, Pump),
	connect(Pump, P2),
	connect(P2, HeatExchanger_2),
	connect(HeatExchanger_2, P3),
	connect(P3, FlowMeter),
	connect(FlowMeter, P4),
	connect(P4, HeatExchanger_1),

	% Fluidum
	Fluidum = create_fluidum(FluidumType, P1, GlobalOverview_Pid, water),
	fill_flowMeter(FlowMeter, Fluidum),

	GlobalOverview_Pid.

% Creation
create_globalOverview() ->
	globalOverview:create().

create_types() ->
	{ok, PipeType} = resource_type:create(pipeType, []),
	{ok, PumpType} = resource_type:create(pumpType, []),
	{ok, FlowMeterType} = resource_type:create(flowMeterType, []),
	{ok, HeatExchangerType} = resource_type:create(heatExchangerType, []),
	{ok, FluidumType} = resource_type:create(fluidumType, []),
	{PipeType, PumpType, FlowMeterType, HeatExchangerType, FluidumType}.

create_pipe(PipeType, GlobalOverview_Pid, Name) ->
	{ok, Pipe} = resource_instance:create(pipeInst, [self(), PipeType, Name]),
	{ok, ok} = globalOverview:add_part(GlobalOverview_Pid, Name, Pipe),
	Pipe.

create_pump(PumpType, PipeType, GlobalOverview_Pid, Name) ->
	{ok, Pump} = resource_instance:create(pumpInst, [self(), PumpType, PipeType, rw_pump(), Name]),
	{ok, ok} = globalOverview:add_part(GlobalOverview_Pid, Name, Pump),
	Pump.
	
rw_pump() ->  
	fun(Answer) -> Answer end.
	
create_flow_meter(FlowMeterType, PipeType, GlobalOverview_Pid, Name) ->
	{ok, FlowMeter} = resource_instance:create(flowMeterInst, [self(), FlowMeterType, PipeType, rw_flow_meter(), Name]),
	{ok, ok} = globalOverview:add_part(GlobalOverview_Pid, Name, FlowMeter),
	FlowMeter.

rw_flow_meter() ->  
	fun() -> ok end.

create_heat_exchanger(HeatExchangerType, PipeType, HE_Link, GlobalOverview_Pid, Name) ->
	{ok, HeatExchanger} = resource_instance:create(heatExchangerInst, [self(), HeatExchangerType, PipeType, HE_Link, Name]),
	{ok, ok} = globalOverview:add_part(GlobalOverview_Pid, Name, HeatExchanger),
	HeatExchanger.

create_fluidum(FluidumType, Root_Pid, GlobalOverview_Pid, Name) ->
	{ok, Fluidum} = resource_instance:create(fluidumInst, [self(), FluidumType, get_out_connector(Root_Pid), Name]),
	{ok, ok} = globalOverview:add_part(GlobalOverview_Pid, Name, Fluidum),
	Fluidum.

% Connections
get_in_connector(ResInst_Pid) ->
	{ok, [C, _]} = resource_instance:get_connectors(ResInst_Pid),
	C.
	
get_out_connector(ResInst_Pid) ->
	{ok, [_, C]} = resource_instance:get_connectors(ResInst_Pid),
	C.
	
connect(ResInst_1, ResInst_2) ->
	C1 = get_out_connector(ResInst_1),
	C2 = get_in_connector(ResInst_2),
	connector:connect(C1, C2),
	connector:connect(C2, C1).

% Circuit
get_circuit(Fluidum_Pid) ->
	{ok, CircuitMap} = fluidumInst:get_circuit(Fluidum_Pid),
	extract(maps:next(maps:iterator(CircuitMap)), []).

extract(none, Acc) ->
	Acc;

extract({Pid, _, T}, Acc) ->
	extract(maps:next(T), [Pid | Acc]).

get_ordered_circuit(Fluidum_Pid) ->
	[Pid | _] = get_circuit(Fluidum_Pid),
	Connector = get_out_connector(Pid),
	{ok, Connection} = connector:get_connection(Connector),
	{ok, Pipe_Pid} = connector:get_resInst(Connection),
	{ok, PipeState} = resource_instance:get_state(Pipe_Pid),
	#{type_options := TypeOptions} = PipeState,
	NextPid = extract_stand_in(Pipe_Pid, TypeOptions),
	get_ordered_circuit(Pid, NextPid, [Pid]).

get_ordered_circuit(Root, Root, Ordered) ->
	Ordered;

get_ordered_circuit(Root, Pid, Ordered) ->
	Connector = get_out_connector(Pid),
	{ok, Connection} = connector:get_connection(Connector),
	{ok, Pipe_Pid} = connector:get_resInst(Connection),
	{ok, PipeState} = resource_instance:get_state(Pipe_Pid),
	#{type_options := TypeOptions} = PipeState,
	NextPid = extract_stand_in(Pipe_Pid, TypeOptions),
	get_ordered_circuit(Root, NextPid, [Pid | Ordered]).

extract_stand_in(Pipe_Pid, stand_in) ->
	{ok, ResInst_Pid} = resource_instance:get_host(Pipe_Pid),
	ResInst_Pid;

extract_stand_in(Pipe_Pid, _) ->
	Pipe_Pid.

get_named_circuit(Fluidum_Pid) ->
	Circuit = get_ordered_circuit(Fluidum_Pid),
	name(Circuit, []).

name([], Named) ->
	Named;

name([Pid | T], Named) ->
	{ok, State} = resource_instance:get_state(Pid),
	#{name := Name} = State,
	name(T, [Name | Named]).

% Pump
check_pump(Pump_Pid) ->
	{ok, Answer} = pumpInst:is_on(Pump_Pid),
	Answer.
	
toggle_pump(Pump_Pid) ->
	Answer = check_pump(Pump_Pid),
	toggle_pump(Pump_Pid, Answer).
	
toggle_pump(Pump_Pid, off) ->
	pumpInst:switch_on(Pump_Pid);
	
toggle_pump(Pump_Pid, _) ->
	pumpInst:switch_off(Pump_Pid).

% Flow meter
fill_flowMeter(FlowMeter_Pid, Fluidum_Pid) ->
	flowMeterInst:add_fluidum(FlowMeter_Pid, Fluidum_Pid).

check_flowMeter(FlowMeter_Pid) ->
	{ok, Flow} = flowMeterInst:estimate_flow(FlowMeter_Pid),
	{ok, ok} = flowMeterInst:measure_flow(FlowMeter_Pid),
	Flow.

% Heat exchanger
check_heatExchanger(HeatExchanger_Pid, FlowMeter_Pid, InTemp) ->
	{ok, TempInfluenceFn} = heatExchangerInst:get_temp_influence(HeatExchanger_Pid),
	{ok, Flow} = flowMeterInst:estimate_flow(FlowMeter_Pid),
	TempInfluenceFn(InTemp, Flow).