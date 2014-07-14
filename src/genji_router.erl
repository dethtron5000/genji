-module(genji_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,call_endpoint/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	{ok, Args}.

handle_call({Submodule,Action,Parameters,Body,Client},From,State) ->
	proc_lib:spawn_link(?MODULE,call_endpoint,[{Submodule,Action,Parameters,Body,Client},From,State]),
	{noreply,State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

call_endpoint({Submodule,Action,Parameters,Body,Client},From,State) ->
	Endpoints = genji_config:lookup(<<"endpoints">>),
	{Submodule,Actions} = proplists:lookup(Submodule, Endpoints), 
	{Action,Info} = proplists:lookup(Action, Actions), 
	Method = proplists:get_value(<<"method">>, Info),
	Url = parse_url(proplists:get_value(<<"endpoint">>, Info), Parameters),
	Reply = oauth2c:request(Method, json, list_to_binary(Url), [200], [], Body, Client) ,
	gen_server:reply(From,  {reply,Reply,State}).



parse_url(URLStem,Parameters) ->
	{ok,Re2} = re:compile("\\{(?<Param>[a-zA-Z0-9]+)\\}"),
	Matches = re:run(URLStem,Re2,[{capture,['Param'],binary},global]),
	io:format("matches: ~p~n", [Matches]),
	{Stem, Parameters2} = case Matches of
		{match, PathParams}  -> substitute_path_params(URLStem,PathParams,Parameters);
		_ -> {URLStem,Parameters}
	end,
	io:format("Stem: ~p~n", [Stem]),
	%%St = binary_to_list(Stem),
	restc:construct_url(binary_to_list(Stem), Parameters2).

substitute_path_params(URL,[],Parameters) ->
	{URL,Parameters};

substitute_path_params(URL,PathParams,Parameters) ->
	Params = lists:flatten(PathParams),
	lists:foldl(fun url_folder/2, {URL,Parameters}, Params).

url_folder(Parameter,{URL,Parameters}) ->
	Val = proplists:get_value(Parameter, Parameters),
	case Val of 
		undefined -> {URL,Parameters};
		_ -> {binary:replace(URL, <<"{",Parameter/binary,"}">>, Val),proplists:delete(Parameter, Parameters)}
	end.


