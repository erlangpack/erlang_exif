-module(erlang_exif_dict).

-export([new/0, merge/2, store/3, size/1, find/2]).

new() ->
    dict:new().

merge(Dict1, Dict2) ->
    dict:merge(fun(_K, _V1, V2) -> V2 end, Dict1, Dict2).

store(K, V, Dict) ->
    dict:store(K, V, Dict).

size(Dict) ->
    dict:size(Dict).

find(Key, Dict) ->
    dict:find(Key, Dict).

