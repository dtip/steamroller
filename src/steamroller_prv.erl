-module(steamroller_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, steamroll).
-define(DEPS, [app_discovery]).
-define(FILE_KEY, steamroll_file).
-define(DIR_KEY, steamroll_dir).
-define(DEFAULT_INPUTS, ["rebar.config", "{src,test}/**/*.{[he]rl,app.src}"]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create(
            [
                {name, ?PROVIDER},
                {module, ?MODULE},
                {bare, true},
                {deps, ?DEPS},
                {example, "rebar3 steamroll"},
                {
                    opts,
                    [
                        {?FILE_KEY, $f, "file", binary, "File name to format."},
                        {?DIR_KEY, $d, "dir", string, "Dir name to format."},
                        {
                            check,
                            $c,
                            "check",
                            undefined,
                            "Check code formatting without changing anything."
                        }
                    ]
                },
                {short_desc, "An opinionated Erlang code formatter."},
                {desc, "An opinionated Erlang code formatter."}
            ]
        ),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    % No idea why a two-element tuple is returned here.
    {ArgOpts, _} = rebar_state:command_parsed_args(State),
    RebarOpts = rebar_state:opts(State),
    Opts =
        case dict:find(steamroller, RebarOpts) of
            {ok, ConfigOpts} -> ArgOpts ++ ConfigOpts;
            error -> ArgOpts
        end,
    Result =
        case {lists:keyfind(?FILE_KEY, 1, Opts), lists:keyfind(?DIR_KEY, 1, Opts)} of
            {{?FILE_KEY, File}, _} ->
                rebar_api:info("Steamrolling file: ~s", [File]),
                steamroller:format_file(File, Opts);
            {_, {?DIR_KEY, Dir}} ->
                rebar_api:info("Steamrolling dir: ~s", [Dir]),
                Files = find_dir_files(Dir),
                format_files(Files, Opts);
            _ ->
                rebar_api:info("Steamrolling code...", []),
                format_apps(rebar_state:project_apps(State), Opts)
        end,
    case Result of
        ok ->
            rebar_api:info("Steamrolling done.", []),
            {ok, State};
        {error, Err} -> {error, format_error(Err)}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) when is_binary(Reason) -> io_lib:format("Steamroller Error: ~s", [Reason]);
format_error(Reason) -> io_lib:format("Steamroller Error: ~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

format_apps([App | Rest], Opts) ->
    Inputs =
        case proplists:get_value(inputs, Opts) of
            undefined -> ?DEFAULT_INPUTS;
            Custom -> Custom
        end,
    AppDir = rebar_app_info:dir(App),
    Files = lists:flatten(lists:map(fun (Input) -> find_root_files(AppDir, Input) end, Inputs)),
    case format_files(Files, Opts) of
        ok -> format_apps(Rest, Opts);
        {error, _} = Err -> Err
    end;
format_apps([], _) -> ok.

find_root_files(Dir, Input) ->
    [list_to_binary(filename:join(Dir, File)) || File <- filelib:wildcard(Input, Dir)].

find_dir_files(Dir) ->
    [
        list_to_binary(filename:join(Dir, File))
        || File <- filelib:wildcard("./**/*.{[he]rl,app.src}", Dir)
    ].

format_files([File | Rest], Opts) ->
    rebar_api:debug("Steamrolling file: ~s", [File]),
    case steamroller:format_file(File, Opts) of
        ok -> format_files(Rest, Opts);
        {error, {File, {Line, epp, {undefined, Macro, _}}}} ->
            rebar_api:warn(
                "Steamroller Warn: File: ~s: Undefined macro ~p on line ~p. Skipping...",
                [File, Macro, Line]
            ),
            format_files(Rest, Opts);
        {error, {File, {Line, epp, {include, file, IncludeFile}}}} ->
            rebar_api:warn(
                "Steamroller Warn: File: ~s: Undefined include file ~p on line ~p. Skipping...",
                [File, IncludeFile, Line]
            ),
            format_files(Rest, Opts);
        {error, _} = Err -> Err
    end;
format_files([], _) -> ok.
