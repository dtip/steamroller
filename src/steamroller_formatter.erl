-module(steamroller_formatter).

-export([format/1, format_code/1]).

-include_lib("kernel/include/logger.hrl").

-define(CRASHDUMP, "steamroller.crashdump").

%% API

-spec format(binary()) -> ok | {error, any()}.
format(File) ->
    case file:read_file(File) of
        {ok, Code} ->
            case format_code(Code, File) of
                {ok, Code} ->
                    ok;
                {ok, FormattedCode} ->
                    file:write_file(File, FormattedCode);
                {error, _} = Err ->
                    Err
            end;
        {error, enoent} ->
            {error, <<"file does not exist">>};
        {error, eisdir} ->
            {error, <<"that's a directory">>};
        {error, _} = Err ->
            Err
    end.

-spec format_code(binary()) -> ok | {error, any()}.
format_code(Code) -> format_code(Code, <<"no_file">>).

%% Internal

-spec format_code(binary(), binary()) -> {ok, binary()} | {error, any()}.
format_code(Code, <<"rebar.config">>) ->
    Tokens = steamroller_ast:tokens(Code),
    {ok, steamroller_algebra:format_tokens(Tokens)};
format_code(Code, File) ->
    case steamroller_ast:ast(Code, File) of
        {ok, OriginalAst} ->
            Tokens = steamroller_ast:tokens(Code),
            FormattedCode = steamroller_algebra:format_tokens(Tokens),
            {ok, NewAst} = steamroller_ast:ast(FormattedCode),
            case steamroller_ast:eq(OriginalAst, NewAst) of
                true ->
                    {ok, FormattedCode};
                false ->
                    file:write_file(?CRASHDUMP, FormattedCode),
                    {
                        error,
                        {
                            formatter_broke_the_code,
                            {file, File},
                            {crashdump, ?CRASHDUMP}
                        }
                    }
            end;
        {error, _} = Err ->
            Err
    end.
