%%%-------------------------------------------------------------------
%% @doc Bot logic
%% @end
%%%-------------------------------------------------------------------

-module(sb_bot).

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

handle_call(Request, _From, State) ->
  lager:error("[~p] unexpected handle_call: ~p", [?MODULE, Request]),
  {noreply, State}.

handle_cast({process_message, Channel, User, Text}, State) ->
  Sentiments = gen_server:call(sb_sentimental, {message, Text}),
  if
    Sentiments =/= [] ->
      gen_server:cast(sb_database, {set, User, Sentiments}),
      Message = lists:concat([User, " is", sb_sentimental:to_code(Sentiments)]),
      gen_server:cast(sb_slacker, {post, Channel, Message});
    true ->
      ok
  end,
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
  ok.
