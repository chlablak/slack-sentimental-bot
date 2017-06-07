%%%-------------------------------------------------------------------
%% @doc Connect to the Slack team
%% @end
%%%-------------------------------------------------------------------

-module(sb_slacker).

%-behaviour(supervisor).

%% API
-export([ get_token/0]).

%% supervisor callbacks
-export([]).

%%====================================================================
%% API functions
%%====================================================================

get_token() ->
  {ok, Binary} = file:read_file("priv/token.txt"),
  String = unicode:characters_to_list(Binary),
  Exclude = [$", $., $\n],
  lists:filter(fun(C) -> not lists:member(C, Exclude) end, String).

%%====================================================================
%% supervisor callbacks
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================
