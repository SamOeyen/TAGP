-module(control).
-export([main/0]).
-export([create_system/0, get_overview/1, get_circuit/2]).
-export([check_pump/2, toggle_pump/2]).
-export([check_flowMeter/2]).
-export([check_heatExchanger/2]).

main() ->
	GlobalOverview_Pid = create_system(),
	
	HE_1 = {heat_exchanger_1, flow_meter, 10},
	HE_2 = {heat_exchanger_2, flow_meter, 1},

	erlang:display(pid_list),
	erlang:display(get_overview(GlobalOverview_Pid)),
	erlang:display(circuit),
	erlang:display(get_circuit(GlobalOverview_Pid, water)),
	erlang:display(flow_temp1_and_temp2_when_pump_off),
	erlang:display(check_flowMeter(GlobalOverview_Pid, flow_meter)),
	erlang:display(check_heatExchanger(GlobalOverview_Pid, HE_1)),
	erlang:display(check_heatExchanger(GlobalOverview_Pid, HE_2)),
	toggle_pump(GlobalOverview_Pid, pump),
	erlang:display(flow_temp1_and_temp2_when_pump_on),
	erlang:display(check_flowMeter(GlobalOverview_Pid, flow_meter)),
	erlang:display(check_heatExchanger(GlobalOverview_Pid, HE_1)),
	erlang:display(check_heatExchanger(GlobalOverview_Pid, HE_2)),
	ok.

% System
create_system() ->
	system:main().

get_overview(GlobalOverview_Pid) ->
	{ok, Parts} = globalOverview:get_parts(GlobalOverview_Pid),
	Parts.

get_circuit(GlobalOverview_Pid, Fluidum_Name) ->
	{ok, Fluidum_Pid} = globalOverview:get_part(GlobalOverview_Pid, Fluidum_Name),
	system:get_named_circuit(Fluidum_Pid).

check_pump(GlobalOverview_Pid, Pump_Name) ->
	{ok, Pump_Pid} = globalOverview:get_part(GlobalOverview_Pid, Pump_Name),
	system:check_pump(Pump_Pid).

toggle_pump(GlobalOverview_Pid, Pump_Name) ->
	{ok, Pump_Pid} = globalOverview:get_part(GlobalOverview_Pid, Pump_Name),
	system:toggle_pump(Pump_Pid).

check_flowMeter(GlobalOverview_Pid, FlowMeter_Name) ->
	{ok, FlowMeter_Pid} = globalOverview:get_part(GlobalOverview_Pid, FlowMeter_Name),
	system:check_flowMeter(FlowMeter_Pid).

check_heatExchanger(GlobalOverview_Pid, {HeatExchanger_Name, FlowMeter_Name, InTemp}) ->
	{ok, HeatExchanger_Pid} = globalOverview:get_part(GlobalOverview_Pid, HeatExchanger_Name),
	{ok, FlowMeter_Pid} = globalOverview:get_part(GlobalOverview_Pid, FlowMeter_Name),
	system:check_heatExchanger(HeatExchanger_Pid, FlowMeter_Pid, InTemp).