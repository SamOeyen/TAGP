-module(resource_instance).
-export([create/2]).
-export([get_host/1, get_type/1, get_state/1, get_locations/1, get_connectors/1, get_ops/1, get_flow_influence/1]).

% Creation
create(Selector, Environment) ->
	apply(Selector, create, Environment).
	
% Commands
get_host(ResInst_Pid) ->
	msg:get(ResInst_Pid, get_host).
	
get_type(ResInst_Pid) ->
	msg:get(ResInst_Pid, get_type).
	
get_state(ResInst_Pid) ->
	msg:get(ResInst_Pid, get_state).

get_locations(ResInst_Pid)	->
	msg:get(ResInst_Pid, get_locations).

get_connectors(ResInst_Pid) ->
	msg:get(ResInst_Pid, get_connectors).

get_ops(ResInst_Pid) ->
	msg:get(ResInst_Pid, get_ops).

get_flow_influence(ResInst_Pid) ->
	msg:get(ResInst_Pid, get_flow_influence).