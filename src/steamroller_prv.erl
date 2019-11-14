-module(steamroller_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, steamroll).
-define(DEPS, [app_discovery]).
-define(KEY, steamroll_file).

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
            {opts, [{?KEY, $f, "file", binary, "File name to format."}]},
            {short_desc, "Format that Erlang."},
            {desc, "Format that Erlang."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Result =
        case rebar_state:command_parsed_args(State) of
            % No idea why a two-element tuple is returned here.
            {[], _} ->
                rebar_api:info("Steamrolling code...", []),
                format_apps(rebar_state:project_apps(State));
            {[{?KEY, File}], _} ->
                rebar_api:info("Steamrolling file: ~s", [File]),
                steamroller:format_file(File)
        end,
    case Result of
        ok -> rebar_api:info("Steamrolling done.", []), {ok, State};
        {error, Err} -> {error, format_error(Err)}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) when is_binary(Reason) ->
    io_lib:format("Steamroller Error: ~s", [Reason]);
format_error(Reason) ->
    io_lib:format("Steamroller Error: ~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

format_apps([App | Rest]) ->
    SrcDir = rebar_app_info:dir(App) ++ "/src",
    TestDir = rebar_app_info:dir(App) ++ "/test",
    Files = [<<"rebar.config">> | find_source_files(SrcDir) ++ find_source_files(TestDir)],
    case format_files(Files) of
        ok -> format_apps(Rest);
        {error, _} = Err -> Err
    end;
format_apps([]) -> ok.

find_source_files(Path) ->
    [list_to_binary(filename:join(Path, Mod)) || Mod <- filelib:wildcard("*.erl", Path)].

format_files([File | Rest]) ->
    case steamroller:format_file(File) of
         ok -> format_files(Rest);
         {error, _} = Err -> Err
    end;
format_files([]) -> ok.
