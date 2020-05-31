-module(steamroller_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, steamroll).
-define(DEPS, [app_discovery]).
-define(FILE_KEY, steamroll_file).
-define(DIR_KEY, steamroll_dir).
-define(INCLUDES_KEY, steamroll_includes).
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
            {?INCLUDES_KEY, $i, "includes", string, "Wildcard includes path."},
            {j, $j, undefined, integer, "J Factor."},
            {check, $c, "check", undefined, "Check code formatting without changing anything."}
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
  Includes = includes(RebarOpts, State, ArgOpts),
  Macros = macros(RebarOpts),
  rebar_api:debug("Steamroller Includes: ~p", [Includes]),
  rebar_api:debug("Steamroller Macros: ~p", [Macros]),
  % This essentially serves as an integration test to make sure we can format files which
  % include macros which are defined in the rebar.config
  rebar_api:debug("Steamroller Test Macro: ~p", [?TEST_MACRO]),
  Opts0 =
    case dict:find(steamroller, RebarOpts) of
      {ok, ConfigOpts} -> ArgOpts ++ ConfigOpts;
      error -> ArgOpts
    end,
  Opts = Opts0 ++ [{includes, Includes}, {macros, Macros}],
  maybe_set_indent(Opts),
  Files =
    case lists:any(fun (O) -> proplists:is_defined(O, Opts) end,
                   [?FILE_KEY, ?DIR_KEY, ?APP_KEY]) of
        true ->
            Fs = proplists:get_all_values(?FILE_KEY, Opts),
            Ds = proplists:get_all_values(?DIR_KEY, Opts),
            Fs ++ directory_files(Ds, Opts);
        false ->
            Apps = case rebar_state:current_app(State) of
                       undefined ->
                           rebar_state:project_apps(State);
                       AppInfo ->
                           [AppInfo]
                   end,
            application_files(Apps, Opts)
    end,
  case format_files(Files, Opts) of
    ok ->
      rebar_api:info("Steamrolling done.", []),
      {ok, State};

    {error, Err} ->
      cleanup_temp_files(),
      {error, format_error(Err)}
  end.

-spec format_error(any()) -> iolist().
format_error(Reason) when is_binary(Reason) -> io_lib:format("Steamroller Error: ~s", [Reason]);

format_error({File, Reason}) when is_binary(File) andalso is_binary(Reason) ->
  io_lib:format("Steamroller Error: File: ~s: ~s", [File, Reason]);

format_error({complaint, File, Reason}) when is_binary(File) andalso is_binary(Reason) ->
  io_lib:format("Steamroller Error: File: ~s: ~s", [File, Reason]);

format_error({complaint, File, Reason}) when is_binary(File) ->
  io_lib:format("Steamroller Error: File: ~s: ~p", [File, Reason]);

format_error(Reason) -> io_lib:format("Steamroller Error: ~p", [Reason]).

%% ===================================================================
%% Internal
%% ===================================================================

application_files([], _) ->
    [];
application_files([A|As], Opts) ->
    app_files(A, Opts) ++ application_files(As, Opts).

app_files(A, Opts) ->
    Inputs = proplists:get_value(inputs, Opts, ?DEFAULT_INPUTS),
    AppDir = rebar_app_info:dir(A),
    lists:flatmap(fun (Input) -> find_root_files(AppDir, Input) end, Inputs).

find_root_files(Dir, Input) ->
  [list_to_binary(filename:join(Dir, File)) || File <- filelib:wildcard(Input, Dir)].

directory_files([], _Opts) ->
    [];
directory_files([D|Ds], Opts) ->
    find_dir_files(D, Opts) ++ directory_files(Ds, Opts).

find_dir_files(Dir, _Opts) ->
  lists:filtermap(
    fun ("_" ++ _) -> false;
        (File) -> {true, list_to_binary(filename:join(Dir, File))}
    end,
    filelib:wildcard("./**/*.{[he]rl,app.src}", Dir)).

format_files(Files, Opts) ->
  {Headers, OtherFiles} = steamroller_utils:split_header_files(Files),
  steamroller_worker_sup:start_link(Opts),
  J = proplists:get_value(j, Opts, ?DEFAULT_J_FACTOR),
  case J of
    J when J > 1 ->
      rebar_api:warn("Using a J-factor greater than 1 may make autoformatting unstable.", []),
      rebar_api:warn("If you see errors, try using the default J-factor of 1.", []),
      rebar_api:warn(
        "If errors persist, please report them: https://github.com/old-reliable/steamroller",
        []
      );

    _ -> ok
  end,
  rebar_api:debug("Steamroller j-factor: ~p", [J]),
  rebar_api:debug("Steamroller headers: ~p", [Headers]),
  rebar_api:debug("Steamroller others: ~p", [OtherFiles]),
  AvailableWorkers = lists:seq(1, J),
  % We want to process header files first and then the rest.
  % This is to avoid races where we format a header file at the same time as an .erl
  % file which imports it. These races upset erl_parse and cause alleged syntax errors
  % to appear in the .erl file, which results in the `formatter_broke_the_code` error
  % message.
  %
  % There are probably further races here when files import each other but it's not
  % worth investigating at the moment - parallelism is meant to be a quick
  % implementation to aid development speed.
  %
  % For stable formatting of an entire repo, run with a J factor of 1.
  %
  % Format all headers in series to avoid problems where headers include each other.
  [HeaderWorker | _] = AvailableWorkers,
  case format_files_([HeaderWorker], 1, Headers, Opts) of
    ok -> format_files_(AvailableWorkers, J, OtherFiles, Opts);
    Err -> Err
  end.


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

        {error, {File, <<"source code is not unicode">> = Err}} ->
          rebar_api:warn("Steamroller Warn: File: ~s: ~s. Skipping...", [File, Err]),
          format_files_(Workers1, J, Files, Opts);

        {error, {complaint, File, Err}} when is_binary(Err) ->
          rebar_api:warn("Steamroller Warn: File: ~s: ~s Skipping...", [File, Err]),
          format_files_(Workers1, J, Files, Opts);

        {error, {complaint, File, Err}} ->
          rebar_api:warn("Steamroller Warn: File: ~s: ~p. Skipping...", [File, Err]),
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

includes(RebarOpts, State, ArgOpts) ->
  Src = rebar_dir:all_src_dirs(RebarOpts, ["src"], []),
  Deps = rebar_dir:deps_dir(State),
  Root = rebar_dir:root_dir(State),
  RootInclude = filename:join(Root, "include"),
  DepIncludes = filelib:wildcard(filename:join(Deps, "**/include"), Root),
  ExtraIncludes =
    case proplists:get_value(?INCLUDES_KEY, ArgOpts, undefined) of
      undefined -> [];
      WildCard -> filelib:wildcard(WildCard)
    end,
  [RootInclude, Deps | Src] ++ DepIncludes ++ ExtraIncludes.


macros(RebarOpts) ->
  case dict:find(erl_opts, RebarOpts) of
    {ok, ErlOpts} ->
      lists:filtermap(fun ({d, Macro, _}) -> {true, Macro}; (_) -> false end, ErlOpts);

    _ -> []
  end.


maybe_set_indent(Opts) ->
  case proplists:get_value(indent, Opts) of
    undefined ->
      % We'll use the default value
      ok;

    Indent -> application:set_env(steamroller, indent, Indent)
  end.
