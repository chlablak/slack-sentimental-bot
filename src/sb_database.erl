%%%-------------------------------------------------------------------
%% @doc user's sentiment store worker.
%% @end
%%%-------------------------------------------------------------------

-module(sb_database).

-behaviour(gen_server).

%% API
-export([ start_link/0]).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

code_change(OldVsn, _State, _Extra) ->
  lager:error("[~p] unexpected code_change: ~p", [?MODULE, OldVsn]),
  {error, code_change_unsupported}.

handle_call({get, User}, _From, State) ->
  Sentiment = get_sentiment(User, State),
  {reply, {User, Sentiment}, State};
handle_call({set, User, Sentiment}, _From, State) ->
  NewState = set_sentiment(User, Sentiment, State),
  {reply, ok, NewState};
handle_call(all, _From, State) ->
  Sentiments = get_all(State),
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
  maps:new().

get_sentiment(_User, _State) ->
  todo.

set_sentiment(_User, _Sentiment, _State) ->
  todo.

get_all(_State) ->
  todo.
