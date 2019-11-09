-module(steamroll_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, steamroll).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 steamroll"},
            {opts, []},
            {short_desc, "Format that Erlang."},
            {desc, "Format that Erlang."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case format_apps(rebar_state:project_apps(State)) of
        ok -> {ok, State};
        {error, Err} -> {error, format_error(Err)}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("Steamroll Error: ~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

format_apps([App | Rest]) ->
    AppDir = rebar_app_info:dir(App) ++ "/src",
    Files = find_source_files(AppDir),
    case format_files(Files) of
        ok -> format_apps(Rest);
        {error, _} = Err -> Err
    end.

find_source_files(Path) ->
    [list_to_binary(filename:join(Path, Mod)) || Mod <- filelib:wildcard("*.erl", Path)].


format_files([File | Rest]) ->
    case steamroll:format_file(File) of
         ok -> format_files(Rest);
         {error, _} = Err -> Err
    end.

