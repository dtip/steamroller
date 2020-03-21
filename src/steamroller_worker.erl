-module(steamroller_worker).

-behaviour(gen_server).

-export(
  [
    % API
    start_link/1,
    format_file/4,
    % Gen Server
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
  ]
).

%% API

start_link(Id) when is_integer(Id) -> gen_server:start_link({local, name(Id)}, ?MODULE, Id, []).

format_file(Id, File, Opts, ReplyPid) ->
  gen_server:cast(name(Id), {format_file, File, Opts, ReplyPid}).

%% Gen Server

init(Id) -> {ok, Id}.

handle_call(_, _From, State) -> {reply, not_implemented, State}.

handle_cast({format_file, File, Opts, ReplyPid}, Id) ->
  ReplyPid ! {steamroll, {worker_id, Id}, catch (steamroller:format_file(File, Opts))},
  {noreply, Id};

handle_cast(_, State) -> {noreply, State}.


handle_info(_, State) -> {noreply, State}.

%% Internal

name(Id) -> list_to_atom("steamroller_worker_" ++ integer_to_list(Id)).
