-module(steamroller_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, steamroll).
-define(DEPS, [app_discovery]).
-define(FILE_KEY, steamroll_file).
-define(DIR_KEY, steamroll_dir).
-define(DEFAULT_INPUTS, ["rebar.config", "{src,test}/**/*.{[he]rl,app.src}"]).
-define(DEFAULT_J_FACTOR, 4).

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
                        {j, $j, undefined, integer, "J Factor."},
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
    Inputs = proplists:get_value(inputs, Opts, ?DEFAULT_INPUTS),
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

format_files(Files, Opts) ->
    J = proplists:get_value(j, Opts, ?DEFAULT_J_FACTOR),
    rebar_api:debug("Steamroller j-factor: ~p", [J]),
    format_files_(J, J, Files, Opts).

format_files_(Spare, J, [File | Rest], Opts) when Spare > 0 ->
    Self = self(),
    rebar_api:debug("Steamrolling file: ~s", [File]),
    spawn(fun () -> Self ! {steamroll, catch steamroller:format_file(File, Opts)} end),
    format_files_(Spare - 1, J, Rest, Opts);
format_files_(J, J, [], _) ->
    % All workers have finished and the queue is empty.
    % Formatting is done.
    ok;
format_files_(Spare0, J, Rest, Opts) ->
    receive
        {steamroll, Result} ->
            Spare1 = Spare0 + 1,
            case Result of
                ok -> format_files_(Spare1, J, Rest, Opts);
                {error, {File, {Line, epp, {undefined, Macro, _}}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: Undefined macro ~p on line ~p. Skipping...",
                        [File, Macro, Line]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, {File, {Line, epp, {include, file, IncludeFile}}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: Undefined include file ~p on line ~p. Skipping...",
                        [File, IncludeFile, Line]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, {File, {Line, epp, Err}}} ->
                    % These errors typically mean that the sorce does not compile.
                    % Not really our problem so we warn instead of erroring.
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: epp error ~p on line ~p. Skipping...",
                        [File, Err, Line]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, {File, {Line, erl_parse, ["syntax error before: ", Str]}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: Syntax error before ~s on line ~p. Skipping...",
                        [File, Str, Line]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, {File, {Line, erl_parse, Err}}} ->
                    % These errors typically mean that the sorce does not compile.
                    % Not really our problem so we warn instead of erroring.
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: erl_parse error ~p on line ~p. Skipping...",
                        [File, Err, Line]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, {File, {Line, file_io_server, Err}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: file_io_server error ~p on line ~p. Skipping...",
                        [File, Err, Line]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, {File, <<"source code is not unicode">>}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: source code is not unicode. Skipping...",
                        [File]
                    ),
                    format_files_(Spare1, J, Rest, Opts);
                {error, _} = Err -> Err;
                {'EXIT', Trace} -> {error, {crash, Trace}}
            end
    end.
