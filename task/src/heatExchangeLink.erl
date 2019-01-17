-module(heatExchangeLink).
-export([get_temp_influence/1]).

% Commands
get_temp_influence(HE_Link) ->
	fun(InTemp, Flow) ->
		#{delta := Difference} = HE_Link,
		InTemp + (Difference / Flow)
	end.