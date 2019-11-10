-module(steamroll_formatter).

-export([format/1, format_code/1, ast/1]).

-include_lib("kernel/include/logger.hrl").

-define(IS_KNOWN_ATTRIBUTE(Attribute), (Attribute == export orelse Attribute == file orelse Attribute == import orelse Attribute == module orelse Attribute == opaque orelse Attribute == record orelse Attribute == type)).

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

-spec ast(binary()) -> list(erl_parse:abstract_form()).
ast(Code0) ->
    Code1 = <<Code0/binary, "\n">>,
    Lines = binary:split(Code1, <<"\n">>, [global, trim_all]),
    io:fwrite("\nLines=~p", [Lines]),
    WithoutComments = lists:filter(fun is_not_comment/1, Lines),
    io:fwrite("\nWithoutComments=~p", [WithoutComments]),
    Code2 = list_to_binary(lists:join(<<"\n">>, WithoutComments)),
    Code3 = <<Code2/binary, "\n">>,
    Split = binary:split(Code3, <<".\n">>, [global, trim_all]),
    Split2 = lists:filter(fun is_not_whitespace/1, Split),
    Split3 = lists:map(fun(Line) -> <<Line/binary, ".\n">> end, Split2),
    Tokens = lists:map(fun (Line) -> {ok, Token, _} = erl_scan:string(binary_to_list(Line)), Token end, Split3),
    io:fwrite("\nTokens=~p", [Tokens]),
    FilteredTokens = lists:filter(fun ([{'-',1}, {atom, 1, Attribute} | _]) -> ?IS_KNOWN_ATTRIBUTE(Attribute); (_) -> true end, Tokens),
    lists:map(fun (Token) -> io:fwrite("\nToken=~p", [Token]), {ok, Ast} = erl_parse:parse_form(Token), Ast end, FilteredTokens).
    %lists:map(fun (Token) -> io:fwrite("\nToken=~p", [Token]), {ok, Ast} = epp_dodger:parse(Token), Ast end, Split3).

%% Internal

-spec format_code(binary(), binary()) -> ok | {error, any()}.
format_code(Code, File) ->
    OriginalAst = ast(Code),
    {ok, FormattedCode} = squash_newlines(<<>>, Code),
    case ast(FormattedCode) of
        OriginalAst -> {ok, FormattedCode};
        NewAst -> {error, {formatter_broke_the_code, {file, File}, {original, OriginalAst}, {new, NewAst}}}
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

-spec is_not_comment(binary()) -> boolean().
is_not_comment(Binary) -> is_not_comment_(trim_leading_whitespace(Binary)).

-spec is_not_comment_(binary()) -> boolean().
is_not_comment_(<<"%", _/binary>>) -> false;
is_not_comment_(<<_/binary>>) -> true.

-spec is_not_whitespace(binary()) -> boolean().
is_not_whitespace(Binary) -> trim_leading_whitespace(Binary) =/= <<>>.

-spec trim_leading_whitespace(binary()) -> binary().
trim_leading_whitespace(<<"\n", Rest/binary>>) -> trim_leading_whitespace(Rest);
trim_leading_whitespace(<<" ", Rest/binary>>) -> trim_leading_whitespace(Rest);
trim_leading_whitespace(<<"\t", Rest/binary>>) -> trim_leading_whitespace(Rest);
trim_leading_whitespace(<<Rest/binary>>) -> Rest.

-spec trim_leading_newlines(binary()) -> binary().
trim_leading_newlines(<<"\n", Rest/binary>>) -> trim_leading_newlines(Rest);
trim_leading_newlines(<<Rest/binary>>) -> Rest.
