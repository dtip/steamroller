-module(steamroll_formatter).

-export([format/1, format_code/1]).

-include_lib("kernel/include/logger.hrl").

%% API

-spec format(binary()) -> ok | {error, any()}.
format(File) ->
    {ok, Code} = file:read_file(File),
    case format_code(Code, File) of
        {ok, FormattedCode} -> file:write_file(File, FormattedCode);
        {error, _} = Err -> Err
    end.

-spec format_code(binary()) -> ok | {error, any()}.
format_code(Code) -> format_code(Code, <<"no_file">>).

%% Internal

-spec format_code(binary(), binary()) -> {ok, binary()} | {error, any()}.
format_code(Code, File) ->
    OriginalAst = steamroll_ast:ast(Code),
    {ok, FormattedCode} = squash_newlines(<<>>, Code),
    NewAst = steamroll_ast:ast(FormattedCode),
    case steamroll_ast:eq(OriginalAst, NewAst) of
        true ->
            {ok, FormattedCode};
        false ->
            {error, {formatter_broke_the_code, {file, File}, {code, Code}, {formatted, FormattedCode}}}
    end.

-spec squash_newlines(binary(), binary()) -> {ok, binary()}.
squash_newlines(<<Code/binary>>, <<"\n\n\n", Rest/binary>>) ->
    squash_newlines(<<Code/binary, "\n\n">>, trim_leading_newlines(Rest));
squash_newlines(<<Code/binary>>, <<"\n\n">>) ->
    {ok, <<Code/binary, "\n">>};
squash_newlines(<<Code/binary>>, <<"\n">>) ->
    {ok, <<Code/binary, "\n">>};
squash_newlines(<<Code/binary>>, <<Char:1/binary, Rest/binary>>) ->
    squash_newlines(<<Code/binary, Char/binary>>, Rest);
squash_newlines(<<Code/binary>>, <<>>) ->
    {ok, <<Code/binary, "\n">>}.

-spec trim_leading_newlines(binary()) -> binary().
trim_leading_newlines(<<"\n", Rest/binary>>) -> trim_leading_newlines(Rest);
trim_leading_newlines(<<Rest/binary>>) -> Rest.
