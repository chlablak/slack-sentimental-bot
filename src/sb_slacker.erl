%%%-------------------------------------------------------------------
%% @doc Connect to the Slack team
%% @end
%%%-------------------------------------------------------------------

-module(sb_slacker).

-behaviour(gen_server).

%% API
-export([ start_link/0
        , get_token/0]).

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

get_token() ->
  {ok, Binary} = file:read_file("priv/token.txt"),
  String = unicode:characters_to_list(Binary),
  Exclude = [$", $., $\n],
  lists:filter(fun(C) -> not lists:member(C, Exclude) end, String).

%%====================================================================
%% gen_server callbacks
%%====================================================================

code_change(OldVsn, _State, _Extra) ->
  lager:error("[~p] unexpected code_change: ~p", [?MODULE, OldVsn]),
  {error, code_change_unsupported}.

handle_call(get_rtm_url, _From, State) ->
  Url = get_rtm_url(),
  {reply, Url, State}.

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

get_rtm_url() ->
  Token = get_token(),
  {ok, _, _, Resp} = slacker_rtm:connect(Token),
  {_, Url} = lists:keyfind(<<"url">>, 1, Resp),
  unicode:characters_to_list(Url).

init_state() ->
  todo.
