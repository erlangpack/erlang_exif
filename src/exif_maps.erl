-module(exif_maps).

-export([new/0, merge/2, store/3, size/1, find/2]).

-ifdef(otp17_or_higher).

new() ->
    #{}.

merge(Map1, Map2) ->
    maps:merge(Map1, Map2).

store(K, V, Map) ->
    Map#{ K => V }.

size(Map) ->
    maps:size(Map).

find(Key, Map) ->
    maps:find(Key, Map).

-else.

new() -> throw(maps_not_available).

merge(_Map1, _Map2) -> throw(maps_not_available).

store(_K, _V, _Map) -> throw(maps_not_available).

size(_Map) -> throw(maps_not_available).

find(_Key, _Map) -> throw(maps_not_available).

-endif.
