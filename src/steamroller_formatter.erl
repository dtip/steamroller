-module(steamroller_formatter).

-export([format/2, format_code/1]).

-include_lib("kernel/include/logger.hrl").

-define(CRASHDUMP, "steamroller.crashdump").

%% API

-spec format(binary(), list(any())) -> ok | {error, any()}.
format(File, Opts) ->
    Check = lists:member(check, Opts),
    case file:read_file(File) of
        {ok, Code} ->
            case format_code(Code, File) of
                {ok, Code} -> ok;
                {ok, FormattedCode} ->
                    case Check of
                        true -> {error, <<"Check failed: code needs to be formatted.">>};
                        false -> file:write_file(File, FormattedCode)
                    end;
                {error, _} = Err -> Err
            end;
        {error, enoent} -> {error, <<"file does not exist">>};
        {error, eisdir} -> {error, <<"that's a directory">>};
        {error, _} = Err -> Err
    end.

-spec format_code(binary()) -> ok | {error, any()}.
format_code(Code) -> format_code(Code, <<"no_file">>).

%% Internal

-spec format_code(binary(), binary()) -> {ok, binary()} | {error, any()}.
format_code(Code, File) ->
    {ok, R} = re:compile("\\.[he]rl$"),
    case re:run(File, R) of
        {match, _} ->
            % Check the AST after formatting for source files.
            case steamroller_ast:ast(Code, File) of
                {ok, OriginalAst} ->
                    Tokens = steamroller_ast:tokens(Code),
                    FormattedCode = steamroller_algebra:format_tokens(Tokens),
                    case steamroller_ast:ast(FormattedCode, ?CRASHDUMP) of
                        {ok, NewAst} ->
                            case steamroller_ast:eq(OriginalAst, NewAst) of
                                true -> {ok, FormattedCode};
                                false ->
                                    handle_formatting_error(
                                        {error, ast_mismatch},
                                        File,
                                        FormattedCode
                                    )
                            end;
                        {error, _} = Err -> handle_formatting_error(Err, File, FormattedCode)
                    end;
                {error, _} = Err -> Err
            end;
        nomatch ->
            % Don't check the AST for config files.
            Tokens = steamroller_ast:tokens(Code),
            {ok, steamroller_algebra:format_tokens(Tokens)}
    end.

handle_formatting_error({error, Msg}, File, FormattedCode) ->
    file:write_file(?CRASHDUMP, FormattedCode),
    {error, {formatter_broke_the_code, {file, File}, {msg, Msg}, {crashdump, ?CRASHDUMP}}}.
