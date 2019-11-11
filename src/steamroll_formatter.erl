-module(steamroll_formatter).

-export([format/1, format_code/1]).

-include_lib("kernel/include/logger.hrl").

%% API

-spec format(binary()) -> ok | {error, any()}.
format(File) ->
    {ok, Code} = file:read_file(File),
    case format_code(Code, File) of
        {ok, Code} -> ok;
        {ok, FormattedCode} -> file:write_file(File, FormattedCode);
        {error, _} = Err -> Err
    end.

-spec format_code(binary()) -> ok | {error, any()}.
format_code(Code) -> format_code(Code, <<"no_file">>).

%% Internal

-spec format_code(binary(), binary()) -> {ok, binary()} | {error, any()}.
format_code(Code, File) ->
    OriginalAst = steamroll_ast:ast(Code),
    Tokens = steamroll_ast:tokens(Code),
    FormattedCode = steamroll_algebra:format_tokens(Tokens),
    NewAst = steamroll_ast:ast(FormattedCode),
    case steamroll_ast:eq(OriginalAst, NewAst) of
        true ->
            {ok, FormattedCode};
        false ->
            {error, {formatter_broke_the_code, {file, File}, {code, Code}, {formatted, FormattedCode}}}
    end.
