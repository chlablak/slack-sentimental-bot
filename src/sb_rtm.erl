%%%-------------------------------------------------------------------
%% @doc Real-Time Messaging
%% @end
%%%-------------------------------------------------------------------

-module(sb_rtm).

-behaviour(websocket_client_handler).

%% API
-export([ start_link/0]).

%% websocket_client_handler callbacks
-export([ init/2
        , websocket_handle/3
        , websocket_info/3
        , websocket_terminate/3]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  Url = gen_server:call(sb_slacker, get_rtm_url),
  websocket_client:start_link(Url, ?MODULE, []).

%%====================================================================
%% websocket_client_handler callbacks
%%====================================================================

init(_Args, _ConnState) ->
  {ok, {}}.

% only written messages are listened
websocket_handle({text, Msg}, _ConnState, State) ->
  Map = jsx:decode(Msg, [return_maps]),
  case maps:find(<<"type">>, Map) of
    {ok, <<"message">>} ->
      gen_server:cast(sb_slacker, {message, Map});
    _Else ->
      ok
  end,
  {ok, State};
websocket_handle(_Msg, _ConnState, State) ->
  {ok, State}.

websocket_info(Msg, _ConnState, State) ->
  lager:error("[~p] unexpected websocket_info: ~p", [?MODULE, Msg]),
  {ok, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
