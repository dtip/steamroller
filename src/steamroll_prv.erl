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
    rebar_api:info("Steamrolling code...", []),
    case format_apps(rebar_state:project_apps(State)) of
        ok -> rebar_api:info("Steamrolling done.", []), {ok, State};
        {error, Err} -> {error, format_error(Err)}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("Steamroll Error: ~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

format_apps([App | Rest]) ->
    SrcDir = rebar_app_info:dir(App) ++ "/src",
    TestDir = rebar_app_info:dir(App) ++ "/test",
    Files = find_source_files(SrcDir) ++ find_source_files(TestDir),
    case format_files(Files) of
        ok -> format_apps(Rest);
        {error, _} = Err -> Err
    end;
format_apps([]) -> ok.

find_source_files(Path) ->
    [list_to_binary(filename:join(Path, Mod)) || Mod <- filelib:wildcard("*.erl", Path)].

format_files([File | Rest]) ->
    case steamroll:format_file(File) of
         ok -> format_files(Rest);
         {error, _} = Err -> Err
    end;
format_files([]) -> ok.
