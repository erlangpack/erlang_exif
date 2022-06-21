-module(reader).
-behaviour(gen_server).
-export([start_link/0]). % convenience call for startup
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]). % gen_server callbacks
		
-define(SERVER, ?MODULE). % macro just defines this module as server

%%% convenience method for startup
start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init([]) ->
	{ok, []}.

handle_call(Request, _From, State) when is_binary(Request); is_list(Request)->
	FilePath = Request,
	Reply = case FilePath of 
		FilePath when is_list(Request) ->
			case filelib:is_regular(FilePath) of
				true ->
					{ok, read(FilePath)};
				false ->
					{stop,badarg,[]}
			end;
		_ ->
			{ok, read(FilePath)}
	end,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreplay, State}.

handle_info(_Info, State) ->
	{noreplay, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% Internal functions
read(Path) ->
	erlang_exif:read(Path).
