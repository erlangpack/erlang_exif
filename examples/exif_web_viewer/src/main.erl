-module(main).
-behaviour(gen_server).
-export([start_link/0]). % convenience call for startup
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]). % gen_server callbacks
-define(SERVER, ?MODULE). % macro just defines this module as server

%%% convenience function for startup
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init([]) ->
        ServerPid = startServer(),
		{ok, [ServerPid]}.

handle_call(_Request, _From, State) ->
        {noreplay, State}.

handle_cast(_Msg, State) ->
        {noreplay, State}.

handle_info(_Info, State) ->
        {noreplay, State}.

terminate(_Reason, _State) ->
        
	ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%% Internal functions

startServer() ->
    % The listener name is used to refer to this listener in future calls
	Name = http,
			
	TransportOpts = [inet, {port, 1234}],

	ProtocolOpts = #{ 
					middlewares => [ 
						% ... add your dispatcher middlware
						controller,
						cowmachine 
					] 
				},	
	{ok, ListenerPid} = cowboy:start_clear(Name,
								TransportOpts,
								ProtocolOpts
								),
	ListenerPid.

