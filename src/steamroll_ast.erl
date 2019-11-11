-module(steamroll_ast).

-export([ast/1, tokens/1, eq/2]).

-include_lib("kernel/include/logger.hrl").

-define(TEMP_FILE, "steamroll_temp.erl").

-type ast() :: list(erl_parse:abstract_form()).
-type tokens() :: list(erl_scan:token()).

%% API

-spec ast(binary()) -> ast().
ast(Code) ->
    file:write_file(?TEMP_FILE, Code),
    {ok, Ast} = epp:parse_file(?TEMP_FILE, []),
    file:delete(?TEMP_FILE),
    Ast.

-spec tokens(binary()) -> tokens().
tokens(Code) ->
    % Comments are not included in the AST created by epp:parse_file.
    % Neither are most of the attributes (things like `-define(BLAH, blah).`).
    % We'll need them to generate formatted code.
    {ok, Scanned, _} = erl_scan:string(binary_to_list(Code), 0, [return_comments]),
    Scanned.

-spec eq(ast(), ast()) -> boolean().
eq(Ast0, Ast1) ->
    lists:foldl(fun eq_/2, true, lists:zip(Ast0, Ast1)).

%% Internal

% Second argument of the tuples is a line number which we want to ignore.
eq_(_, false) -> false;
eq_({Left, Right}, true) when is_list(Left) andalso is_list(Right) -> eq(Left, Right);
eq_({Element, Element}, true) -> true;
eq_({{Type, _, LeftName, LeftValue0, LeftValue1}, {Type, _, RightName, RightValue0, RightValue1}}, true) ->
    % 5-tuples
    R1 = eq_({LeftName, RightName}, true),
    R2 = eq_({LeftValue0, RightValue0}, R1),
    eq_({LeftValue1, RightValue1}, R2);
eq_({{Type, _, LeftName, LeftValue}, {Type, _, RightName, RightValue}}, true) ->
    % 4-tuples
    R1 = eq_({LeftName, RightName}, true),
    eq_({LeftValue, RightValue}, R1);
eq_({{Type, _, LeftValue}, {Type, _, RightValue}}, true) ->
    % 3-tuples
    eq_({LeftValue, RightValue}, true);
eq_({{Type, _}, {Type, _}}, true) ->
    % 2-tuples
    true;
eq_({{{Type, _}, LeftValue}, {{Type, _}, RightValue}}, true) ->
    % special 2-tuples
    eq_(LeftValue, RightValue);
eq_({_Left, _Right}, _) -> logger:error("ast_mismatch\nLeft=~p\nRight=~p", [_Left, _Right]), false.
