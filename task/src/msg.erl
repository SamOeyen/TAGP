-module(msg).
-export([get/2, get/3]).
-define(TimeOut, 5000).

% Requests
get(Pid, Key) ->
	Pid ! {Key, replier(Ref = make_ref())},
	receive
		{Ref, Info} ->
			{ok, Info}
		after ?TimeOut ->
			{error, timed_out, Pid, Key, Ref}
	end.

get(Pid, Key, Extra) ->
	Pid ! {Key, Extra, replier(Ref = make_ref())},
	receive
		{Ref, Info} ->
			{ok, Info}
		after ?TimeOut ->
			{error, timed_out, Pid, Key, Ref}
	end.

replier(Ref) ->
    Sender = self(),
	fun(Msg) -> Sender ! {Ref, Msg} end.