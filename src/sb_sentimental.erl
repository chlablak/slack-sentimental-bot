%%%-------------------------------------------------------------------
%% @doc sentimental analysis worker.
%% @end
%%%-------------------------------------------------------------------

-module(sb_sentimental).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , to_code/1]).

%% gen_server callbacks
-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

to_code(Sentiments) ->
  Codes = lists:map(fun(S) -> to_code_impl(S) end, Sentiments),
  lists:foldl(fun(A, B) -> string:concat(A, B) end, "", Codes).

%%====================================================================
%% gen_server callbacks
%%====================================================================

code_change(OldVsn, _State, _Extra) ->
  lager:error("[~p] unexpected code_change: ~p", [?MODULE, OldVsn]),
  {error, code_change_unsupported}.

handle_call({message, Msg}, _From, State) ->
  Sentiments = find_sentiment(Msg, State),
  {reply, Sentiments, State}.

handle_cast(Request, State) ->
  lager:error("[~p] unexpected handle_cast: ~p", [?MODULE, Request]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:error("[~p] unexpected handle_info: ~p", [?MODULE, Info]),
  {noreply, State}.

init(_Args) ->
  State = init_state(),
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

init_state() ->
  [ "happy"
  , "sad"].

find_sentiment(Msg, State) ->
  Words = string:tokens(Msg, " "),
  lists:filter(fun(W) -> lists:member(W, State) end, Words).

to_code_impl("happy") -> " :)";
to_code_impl("sad") -> " :(".
