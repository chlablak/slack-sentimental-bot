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
  case parse_command(Text, State) of
    {ok, Message, NewState} ->
      gen_server:cast(sb_slacker, {post, Channel, Message}),
      {noreply, NewState};
    {ok, NewState} ->
      {noreply, NewState};
    none ->
      case parse_sentiment(User, Text) of
        {ok, Message} ->
          gen_server:cast(sb_slacker, {post, Channel, Message}),
          {noreply, State};
        none ->
          {noreply, State}
      end
  end.

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
  #{name => "sentibot"}.

parse_command(Text, State) ->
  Tokens = string:tokens(string:concat(Text, " _ _ _ _"), " :"),
  lager:info("[~p] tokens: ~p", [?MODULE, Tokens]),
  if Tokens =/= [] ->
    {ok, Name} = maps:find(name, State),
    EqualName = string:equal(Name, lists:nth(1, Tokens)),
    if EqualName ->
      case lists:nth(2, Tokens) of
        "list" ->
          All = gen_server:call(sb_database, all),
          Messages = lists:map(fun({U, S}) ->
            lists:concat([U, " is", sb_sentimental:to_code(S)]) end, All),
          {ok, string:join(Messages, "\n"), State};
        "set" ->
          case lists:nth(3, Tokens) of
            "name" ->
              {ok, #{name => lists:nth(4, Tokens)}}
          end;
        _Else ->
          {ok, "Commands:\n  help\n  list\n  set name NAME", State}
      end;
    true -> none end;
  true -> none end.

parse_sentiment(User, Text) ->
  Sentiments = gen_server:call(sb_sentimental, {message, Text}),
  if Sentiments =/= [] ->
    gen_server:cast(sb_database, {set, User, Sentiments}),
    {ok, lists:concat([User, " is", sb_sentimental:to_code(Sentiments)])};
  true -> none end.
