-module(genji_config).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, lookup/1, lookup/2, set/2]).

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

%% stolen from leptus
-spec lookup(any()) -> any() | undefined.
lookup(Key) ->
    lookup(Key, undefined).

%% stolen from leptus
-spec lookup(any(), Default) -> any() | Default.
lookup(Key, Default) ->
    case ets:lookup(?MODULE, Key) of
        [] ->
            Default;
        [{_, undefined}] ->
            Default;
        [{_, V}] ->
            V
    end.

%% stolen from leptus
%% set a key/value
-spec set(any(), any()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, {Key, Value}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ets:new(?MODULE, [set, named_table, protected]),
    io:format("Launching config server ~w ~n", [Args]), 
    {ok, ?MODULE}.


handle_call({set, Arg}, _From, State) ->
	io:format("setting: ~w ~w ~n", [Arg,State]),
    true = ets:insert(State, Arg),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
