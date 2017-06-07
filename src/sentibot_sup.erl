%%%-------------------------------------------------------------------
%% @doc sentibot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sentibot_sup).

-behaviour(supervisor).

%% API
-export([ start_link/0]).

%% Supervisor callbacks
-export([ init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  Flags = #{strategy => one_for_one
          , intensity => 1
          , period => 5},
  Sentimental = #{id => sb_sentimental
                , start => {sb_sentimental, start_link, []}
                , type => worker},
  Database = #{ id => sb_database
              , start => {sb_database, start_link, []}
              , type => worker},
  Slacker = #{id => sb_slacker
            , start => {sb_slacker, start_link, []}
            , type => worker},
  Rtm = #{id => sb_rtm
        , start => {sb_rtm, start_link, []}
        , type => worker},
  Bot = #{id => sb_bot
      , start => {sb_bot, start_link, []}
      , type => worker},
  Specs = [Sentimental, Database, Slacker, Rtm, Bot],
  {ok, {Flags, Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================
