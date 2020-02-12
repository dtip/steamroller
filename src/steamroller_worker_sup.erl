-module(steamroller_worker_sup).

-behaviour(supervisor).

-export([start_link/1, init/1, terminate_children/0]).

-define(DEFAULT_NUM_WORKERS, 4).

%% API

start_link(Opts) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init(Opts) ->
  NumWorkers = proplists:get_value(j, Opts, ?DEFAULT_NUM_WORKERS),
  RestartStrategy = #{strategy => one_for_one, intensity => 5, period => 60},
  Children = [child(Id) || Id <- lists:seq(1, NumWorkers)],
  {ok, {RestartStrategy, Children}}.

terminate_children() ->
  CountChildren = supervisor:count_children(?MODULE),
  NumWorkers = proplists:get_value(workers, CountChildren),
  [supervisor:terminate_child(?MODULE, id(Id)) || Id <- lists:seq(1, NumWorkers)].

%% Internal

child(Id) -> #{id => id(Id), start => {steamroller_worker, start_link, [Id]}}.

id(Id) -> list_to_atom("steamroller_worker_" ++ integer_to_list(Id)).
