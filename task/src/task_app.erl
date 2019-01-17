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
	%Dispatch = cowboy_router:compile([{'_', [{"/", control_handler, []}]}]),
    %{ok, _} = cowboy:start_clear(my_http_listener,[{port, 8080}],#{env => #{dispatch => Dispatch}}),
    timer:apply_after(1000, control, main, []),
	task_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
