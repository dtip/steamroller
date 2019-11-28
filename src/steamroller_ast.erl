-module(steamroller_ast).

-export([ast/1, ast/2, tokens/1, eq/2]).

-include_lib("kernel/include/logger.hrl").

-define(TEMP_FILE, "steamroller_temp.erl").

-type ast() :: list(erl_parse:abstract_form()).
-type token() :: erl_scan:token().
-type tokens() :: list(token()).

-export_type([token/0, tokens/0]).

%% API

-spec ast(binary()) -> {ok, ast()} | {error, any()}.
ast(Code) -> ast(Code, <<"no_file">>).

-spec ast(binary(), binary() | string()) -> {ok, ast()} | {error, any()}.
ast(Code, File) ->
    file:write_file(?TEMP_FILE, Code),
    {ok, Ast} = epp:parse_file(?TEMP_FILE, []),
    file:delete(?TEMP_FILE),
    case check_for_errors(Ast, File) of
        ok -> {ok, Ast};
        {error, _} = Err -> Err
    end.

-spec tokens(binary()) -> tokens().
tokens(Code) ->
    % Comments are not included in the AST created by epp:parse_file.
    % Neither are most of the attributes (things like `-define(BLAH, blah).`).
    % We'll need them to generate formatted code.
    {ok, Scanned, _} = erl_scan:string(unicode:characters_to_list(Code), 0, [return_comments]),
    Scanned.

-spec eq(ast(), ast()) -> boolean().
eq(Ast0, Ast1) -> lists:foldl(fun eq_/2 , true , lists:zip(Ast0, Ast1)).

%% Internal

% Second argument of the tuples is a line number which we want to ignore.
eq_(_, false) -> false;
eq_({Left, Right}, true) when is_list(Left) andalso is_list(Right) -> eq(Left, Right);
eq_({Element, Element}, true) -> true;
eq_(
    {
        {Type, _, LeftName, LeftValue0, LeftValue1, LeftValue2},
        {Type, _, RightName, RightValue0, RightValue1, RightValue2}
    },
    true
) ->
    % 6-tuples
    R1 = eq_({LeftName, RightName}, true),
    R2 = eq_({LeftValue0, RightValue0}, R1),
    R3 = eq_({LeftValue1, RightValue1}, R2),
    eq_({LeftValue2, RightValue2}, R3);
eq_(
    {{Type, _, LeftName, LeftValue0, LeftValue1}, {Type, _, RightName, RightValue0, RightValue1}},
    true
) ->
    % 5-tuples
    R1 = eq_({LeftName, RightName}, true),
    R2 = eq_({LeftValue0, RightValue0}, R1),
    eq_({LeftValue1, RightValue1}, R2);
eq_({{Type, _, LeftName, LeftValue}, {Type, _, RightName, RightValue}}, true) ->
    % 4-tuples
    R1 = eq_({LeftName, RightName}, true),
    eq_({LeftValue, RightValue}, R1);
eq_({{integer, LeftLine, LeftLine}, {integer, RightLine, RightLine}}, true) ->
    % Not exactly sure what these are but we occasionally get them.
    true;
eq_({{Type, _LeftLine, LeftValue}, {Type, _RightLine, RightValue}}, true) ->
    % 3-tuples
    eq_({LeftValue, RightValue}, true);
eq_({{Type, _}, {Type, _}}, true) ->
    % 2-tuples
    true;
eq_({{{Type, _}, LeftValue}, {{Type, _}, RightValue}}, true) ->
    % special 2-tuples
    eq_(LeftValue, RightValue);
eq_({_Left, _Right}, _) ->
    logger:error("ast_mismatch\nLeft=~p\nRight=~p", [_Left, _Right]),
    false.

check_for_errors([], _) -> ok;
check_for_errors([{error, {LineNum, erl_parse, ["syntax error before: ", Str]}} | _], File) ->
    Err =
        list_to_binary(
            io_lib:format("~s: Error on line ~p: syntax error before ~s", [File, LineNum, Str])
        ),
    {error, Err};
check_for_errors([{error, Msg} | _], File) ->
    Err = list_to_binary(io_lib:format("~s: ~p", [File, Msg])),
    {error, Err};
check_for_errors([_ | Rest], File) -> check_for_errors(Rest, File).
