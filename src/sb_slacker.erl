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
  {ok, Token} = maps:find(token, State),
  {ok, _, _, Resp} = slacker_rtm:connect(Token),
  {_, Value} = lists:keyfind(<<"url">>, 1, Resp),
  Url = unicode:characters_to_list(Value),
  {reply, Url, State}.

handle_cast({message, Msg}, State) ->
  case maps:find(<<"user">>, Msg) of
    {ok, Id} ->
      {ok, Users} = maps:find(users, State),
      {ok, Channel} = maps:find(<<"channel">>, Msg),
      {ok, Text} = maps:find(<<"text">>, Msg),
      {ok, User} = maps:find(Id, Users),
      gen_server:cast(sb_bot, {process_message,
                               unicode:characters_to_list(Channel),
                               unicode:characters_to_list(User),
                               unicode:characters_to_list(Text)});
    error ->
      ok
  end,
  {noreply, State};
handle_cast({post, Channel, Text}, State) ->
  {ok, Token} = maps:find(token, State),
  slacker_chat:post_message(Token, Channel, Text, []),
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
  Token = get_token(),
  Users = init_users(Token),
  #{token => Token, users => Users}.

init_users(Token) ->
  {ok, _, _, [_, {_, Members}, _]} = slacker_user:list(Token, []),
  Users = lists:map(fun(Infos) ->
      lists:filter(fun({K, _}) ->
        (K =:= <<"id">>) or (K =:= <<"real_name">>)
      end, Infos)
    end, Members),
  lists:foldl(fun([{_, Id}, {_, Name}], Acc) ->
    maps:put(Id, unicode:characters_to_list(Name), Acc)
  end, #{}, Users).
