-module(steamroller_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, steamroll).
-define(DEPS, [app_discovery]).
-define(FILE_KEY, steamroll_file).
-define(DIR_KEY, steamroll_dir).
-define(DEFAULT_INPUTS, ["rebar.config", "{src,test}/**/*.{[he]rl,app.src}"]).
-define(DEFAULT_J_FACTOR, 1).

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
        {error, Err} ->
            cleanup_temp_files(),
            {error, format_error(Err)}
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
    {Headers, OtherFiles} = split_header_files(Files),
    {ok, _} = steamroller_worker_sup:start_link(Opts),
    J = proplists:get_value(j, Opts, ?DEFAULT_J_FACTOR),
    rebar_api:debug("Steamroller j-factor: ~p", [J]),
    AvailableWorkers = lists:seq(1, J),
    % We want to process header files first and then the rest.
    % This is to avoid races where we format a header file at the same time as an .erl
    % file which imports it. These races upset erl_parse and cause alleged syntax errors
    % to appear in the .erl file, which results in the `formatter_broke_the_code` error
    % message.
    % There seem to be further races here and they're not especially worth fixing at
    % the moment - parallelism is meant to be a quick implementation to aid development
    % speed.
    % For stable formatting of an entire repo, run with a J factor of 1.
    case format_files_(AvailableWorkers, J, Headers, Opts) of
        ok -> format_files_(AvailableWorkers, J, OtherFiles, Opts);
        Err -> Err
    end.

split_header_files(Files) ->
    Sorted = sort_files(Files),
    SplitFun =
        fun
            (X) ->
                Size = byte_size(X) - 3,
                case X of
                    <<_:Size / binary, "hrl">> -> true;
                    _ -> false
                end
        end,
    lists:splitwith(SplitFun, Sorted).

sort_files(Files) ->
    % Return .hrl files first
    SortFun =
        fun
            (Left, Right) ->
                FileSize = byte_size(Right) - 3,
                PrevSize = byte_size(Left) - 3,
                case {Left, Right} of
                    {<<_:PrevSize / binary, "hrl">>, <<_:FileSize / binary, "erl">>} -> true;
                    {<<_:PrevSize / binary, "erl">>, <<_:FileSize / binary, "hrl">>} -> false;
                    _ -> Left =< Right
                end
        end,
    Sorted = lists:sort(SortFun, Files),
    rebar_api:debug("Sorted=~p", [Sorted]),
    Sorted.

format_files_([Worker | Workers], J, [File | Rest], Opts) ->
    rebar_api:debug("Steamrolling file: ~s", [File]),
    steamroller_worker:format_file(Worker, File, Opts, self()),
    format_files_(Workers, J, Rest, Opts);
format_files_(Workers, J, [], _) when length(Workers) == J ->
    % All workers have finished and the queue is empty.
    % Formatting is done.
    ok;
format_files_(Workers0, J, Files, Opts) ->
    receive
        {steamroll, {worker_id, Id}, Result} ->
            Workers1 = [Id | Workers0],
            case Result of
                ok -> format_files_(Workers1, J, Files, Opts);
                {error, {File, {Line, epp, {undefined, Macro, _}}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: Undefined macro ~p on line ~p. Skipping...",
                        [File, Macro, Line]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, {File, {Line, epp, {include, file, IncludeFile}}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: Undefined include file ~p on line ~p. Skipping...",
                        [File, IncludeFile, Line]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, {File, {Line, epp, Err}}} ->
                    % These errors typically mean that the sorce does not compile.
                    % Not really our problem so we warn instead of erroring.
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: epp error ~p on line ~p. Skipping...",
                        [File, Err, Line]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, {File, {Line, erl_parse, ["syntax error before: ", Str]}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: Syntax error before ~s on line ~p. Skipping...",
                        [File, Str, Line]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, {File, {Line, erl_parse, Err}}} ->
                    % These errors typically mean that the sorce does not compile.
                    % Not really our problem so we warn instead of erroring.
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: erl_parse error ~p on line ~p. Skipping...",
                        [File, Err, Line]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, {File, {Line, file_io_server, Err}}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: file_io_server error ~p on line ~p. Skipping...",
                        [File, Err, Line]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, {File, <<"source code is not unicode">>}} ->
                    rebar_api:warn(
                        "Steamroller Warn: File: ~s: source code is not unicode. Skipping...",
                        [File]
                    ),
                    format_files_(Workers1, J, Files, Opts);
                {error, _} = Err ->
                    steamroller_worker_sup:terminate_children(),
                    Err;
                {'EXIT', Trace} ->
                    steamroller_worker_sup:terminate_children(),
                    {error, {crash, Trace}}
            end
    end.

cleanup_temp_files() -> [file:delete(File) || File <- filelib:wildcard("steamroller_temp_*", ".")].
