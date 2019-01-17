%%%-------------------------------------------------------------------
%% @doc task public API
%% @end
%%%-------------------------------------------------------------------

-module(task_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	control:main(),
    task_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================