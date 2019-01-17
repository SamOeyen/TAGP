-module(resource_type).
-export([create/2]).
-export([get_initial_state/3, get_locations/2, get_connectors/2, get_flow_influence/2]).

% Creation
create(Selector, ParameterList) ->
	apply(Selector, create, ParameterList).

% Commands
get_initial_state(ResType_Pid, ResInst_Pid, TypeOptions) ->
	msg:get(ResType_Pid, initial_state, [ResInst_Pid, TypeOptions]).
	
get_locations(ResType_Pid, State) ->
	msg:get(ResType_Pid, location_list, State).

get_connectors(ResType_Pid, State) ->
	msg:get(ResType_Pid, connector_list, State).
	
get_flow_influence(ResType_Pid, State) ->
	msg:get(ResType_Pid, flow_influence, State).