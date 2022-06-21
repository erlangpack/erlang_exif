%% @doc EXIF maps<br/>
%% @private
%% @end
-module(erlang_exif_maps).

-export([new/0, merge/2, store/3, size/1, find/2]).

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
