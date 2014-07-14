-module(genji_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    L = genji_sup:start_link(),
    io:format("priv dir is: ~p~n", [code:priv_dir(genji) ]),
    {ok,Endpoints} = file:consult(code:priv_dir(genji)++"/endpoints.config"),
    io:format("priv data is: ~p~n", [Endpoints]), 
    genji_config:set(<<"endpoints">>, Endpoints), 
    L. 
    % .

stop(_State) ->
    ok.
