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
  Sentiments = get_sentiment(User, State),
  {reply, {User, Sentiments}, State};
handle_call({set, User, Sentiments}, _From, State) ->
  NewState = set_sentiment(User, Sentiments, State),
  {reply, ok, NewState};
handle_call(all, _From, State) ->
  All = get_all(State),
  {reply, All, State}.

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

get_sentiment(User, State) ->
  maps:get(User, State, []).

set_sentiment(User, Sentiments, State) ->
  maps:put(User, Sentiments, State).

get_all(State) ->
  maps:to_list(State).
