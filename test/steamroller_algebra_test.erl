-module(steamroller_algebra_test).

-include_lib("eunit/include/eunit.hrl").

-define(sp, <<" ">>).
-define(nl, <<"\n">>).

-define(indent, 2).

paper_implementation_test() ->
    [
     ?_assertEqual(<<"if a == b then a << 2 else a + b\n">>, steamroller_algebra:from_the_paper(100, ?indent)),
     ?_assertEqual(<<"if a == b then a << 2 else a + b\n">>, steamroller_algebra:from_the_paper(32, ?indent)),
     ?_assertEqual(<<"if a == b\nthen a << 2\nelse a + b\n">>, steamroller_algebra:from_the_paper(15, ?indent)),
     ?_assertEqual(<<"if a == b\nthen\n  a << 2\nelse a + b\n">>, steamroller_algebra:from_the_paper(10, ?indent)),
     ?_assertEqual(<<"if\n  a == b\nthen\n  a << 2\nelse\n  a + b\n">>, steamroller_algebra:from_the_paper(8, ?indent)),
     ?_assertEqual(<<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a + b\n">>, steamroller_algebra:from_the_paper(7, ?indent)),
     ?_assertEqual(<<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a +\n    b\n">>, steamroller_algebra:from_the_paper(6, ?indent))
    ].

repeat_test_() ->
    [
     ?_assertEqual(<<>>, steamroller_algebra:repeat(?sp, 0)),
     ?_assertEqual(<<" ">>, steamroller_algebra:repeat(?sp, 1)),
     ?_assertEqual(<<"  ">>, steamroller_algebra:repeat(?sp, 2)),
     ?_assertEqual(<<"   ">>, steamroller_algebra:repeat(?sp, 3)),
     ?_assertEqual(<<"    ">>, steamroller_algebra:repeat(?sp, 4)),
     ?_assertEqual(<<"\n">>, steamroller_algebra:repeat(?nl, 1)),
     ?_assertEqual(<<"\n\n">>, steamroller_algebra:repeat(?nl, 2)),
     ?_assertEqual(<<"\n\n\n">>, steamroller_algebra:repeat(?nl, 3)),
     ?_assertEqual(<<"\n\n\n\n">>, steamroller_algebra:repeat(?nl, 4))
    ].

basic_brackets_test_() ->
    Tokens = steamroller_ast:tokens(<<"(Arg1, Arg2)">>),
    Expect0 = <<"(Arg1, Arg2)\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"(\n    Arg1,\n    Arg2\n)\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 1),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1)
    ].

brakets_comment_test_() ->
    Tokens = steamroller_ast:tokens(<<"(\n  init/1,\n  % test\n  thing/0\n)">>),
    Expect = <<"(\n    init/1,\n    % test\n    thing/0\n)\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [
     ?_assertEqual(Expect, Result0),
     ?_assertEqual(Expect, Result1),
     ?_assertEqual(Expect, Result2)
    ].

basic_function_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> ok.">>),
    Expect0 = <<"foo(Arg1, Arg2) -> ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    Expect2 = <<"foo(\n    Arg1,\n    Arg2\n) ->\n    ok.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 1),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

function_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> Arg3 = Arg1 + Arg2, Arg3.">>),
    Expect0 = <<"foo(Arg1, Arg2) ->\n    Arg3 = Arg1 + Arg2,\n    Arg3.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    Arg3 = Arg1 + Arg2,\n    Arg3.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"foo(Arg1, Arg2) ->\n    Arg3 =\n        Arg1 + Arg2,\n    Arg3.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 22),
    Tokens1 = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> foo, bar, baz, ok.">>),
    Expect3 = <<"foo(Arg1, Arg2) ->\n    foo,\n    bar,\n    baz,\n    ok.\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2),
     ?_assertEqual(Expect3, Result3)
    ].

function_clause_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(Arg1, Arg1) -> error; foo(Arg1, Arg2) -> ok.">>),
    Expect0 = <<"foo(Arg1, Arg1) -> error;\nfoo(Arg1, Arg2) -> ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg1) ->\n    error;\nfoo(Arg1, Arg2) ->\n    ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    Expect2 = <<"foo(\n    Arg1,\n    Arg1\n) ->\n    error;\nfoo(\n    Arg1,\n    Arg2\n) ->\n    ok.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 1),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

function_macro_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> ?MACRO.">>),
    Expect0 = <<"foo() -> ?MACRO.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    ?MACRO.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1)
    ].

function_tuple_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> {error, oh_no}.">>),
    Expect0 = <<"foo() -> {error, oh_no}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    {error, oh_no}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    Expect2 = <<"foo() ->\n    {\n        error,\n        oh_no\n    }.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

function_basic_string_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> \"string\".">>),
    Expect0 = <<"foo() -> \"string\".\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    \"string\".\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1)
    ].

function_basic_binary_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> <<\"binary\">>.">>),
    Expect0 = <<"foo() -> <<\"binary\">>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    <<\"binary\">>.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    Expect2 = <<"foo() ->\n    <<\n        \"binary\"\n    >>.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

function_binary_construction_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(A, B) -> <<A/binary, B/binary>>.">>),
    Expect0 = <<"foo(A, B) -> <<A/binary, B/binary>>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(A, B) ->\n    <<A/binary, B/binary>>.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"foo(A, B) ->\n    <<\n        A/binary,\n        B/binary\n    >>.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

function_binary_arg_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(<<A/binary>>) -> A.">>),
    Expect0 = <<"foo(<<A/binary>>) -> A.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(<<A/binary>>) ->\n    A.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    Expect2 = <<"foo(\n    <<A/binary>>\n) ->\n    A.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 16),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

function_complex_binary_arg_test_() ->
    Tokens = steamroller_ast:tokens(<<"foooooo(<<H,B:1/binary, C/binary>>) -> <<B/binary, H/binary,C/binary>>.">>),
    Expect0 = <<"foooooo(<<H, B:1/binary, C/binary>>) -> <<B/binary, H/binary, C/binary>>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foooooo(<<H, B:1/binary, C/binary>>) ->\n    <<B/binary, H/binary, C/binary>>.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    Expect2 = <<"foooooo(\n    <<H, B:1/binary, C/binary>>\n) ->\n    <<\n        B/binary,\n        H/binary,\n        C/binary\n    >>.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 35),
    Expect3 = <<"foooooo(\n    <<\n        H,\n        B:1/binary,\n        C/binary\n    >>\n) ->\n    <<\n        B/binary,\n        H/binary,\n        C/binary\n    >>.\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens, 20),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2),
     ?_assertEqual(Expect3, Result3)
    ].

function_comment_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() ->\n    % Temporary workaround (2010-01-11)\n    {error, oh_no}.">>),
    Expect0 = <<"foo() ->\n    % Temporary workaround (2010-01-11)\n    {error, oh_no}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    % Temporary workaround (2010-01-11)\n    {\n        error,\n        oh_no\n    }.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1)
    ].

function_comment_list_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() ->\n    {error,\n % TODO improve\noh_no}.">>),
    Expect0 = <<"foo() ->\n    {\n        error,\n        % TODO improve\n        oh_no\n    }.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 = <<"foo() ->\n    {\n        error,\n        % TODO improve\n        oh_no\n    }.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 1),
    Tokens1 = steamroller_ast:tokens(<<"foo() ->\n    Error = 1 + 2,\n    {error, % TODO improve\nError}.">>),
    Expect2 = <<"foo() ->\n    Error = 1 + 2,\n    {\n        error,\n        % TODO improve\n        Error\n    }.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens1, 100),
    Expect3 = <<"foo() ->\n    Error =\n        1 + 2,\n    {\n        error,\n        % TODO improve\n        Error\n    }.\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 15),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2),
     ?_assertEqual(Expect3, Result3)
    ].

functions_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> Arg1 + Arg2.\nbar() -> baz.">>),
    Expect0 = <<"foo(Arg1, Arg2) -> Arg1 + Arg2.\n\nbar() -> baz.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    Arg1 + Arg2.\n\nbar() -> baz.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Tokens2 = steamroller_ast:tokens(<<"fooooooooo(Arg1, Arg2) -> Arg1 + Arg2.\nbar() -> baz.">>),
    Expect2 = <<"fooooooooo(\n    Arg1,\n    Arg2\n) ->\n    Arg1 + Arg2.\n\nbar() -> baz.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens2, 16),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

basic_attribute_test_() ->
    Tokens = steamroller_ast:tokens(<<"-module(test).">>),
    Expect0 = <<"-module(test).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-module(\n    test\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 1),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1)
    ].

attribute_test_() ->
    Tokens = steamroller_ast:tokens(<<"-module(test).\n\n-export([start_link/0, init/1]).">>),
    Expect0 = <<"-module(test).\n\n-export([start_link/0, init/1]).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-module(test).\n\n-export(\n    [start_link/0, init/1]\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"-module(test).\n\n-export(\n    [\n        start_link/0,\n        init/1\n    ]\n).\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect1, Result1),
     ?_assertEqual(Expect2, Result2)
    ].

%%
%% TODO change this behaviour. We currently have
%%
%% ```erlang
%% -export(
%%     [
%%         start_link/0,
%%         % Some comment
%%         init/1
%%     ]
%% ).
%% ```
%%
%% which is "fine" by some definition of the word.
%%
%% But really those brackets should be squashed:
%%
%% ```erlang
%% -export([
%%     start_link/0,
%%     % Some comment
%%     init/1
%% ]).
%% ```
%%
attribute_commment_test_() ->
    Tokens = steamroller_ast:tokens(<<"-module(test).\n\n-export([start_link/0,\n    % comment\n    init/1]).">>),
    Expect = <<"-module(test).\n\n-export(\n    [\n        start_link/0,\n        % comment\n        init/1\n    ]\n).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
     ?_assertEqual(Expect, Result0),
     ?_assertEqual(Expect, Result1),
     ?_assertEqual(Expect, Result2)
    ].

comment_test_() ->
    Expect0 = <<"% Hello I am a comment and I don't change length\n">>,
    Tokens0 = steamroller_ast:tokens(Expect0),
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens0, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens0, 10),
    Expect1 = <<"% Hello\n% World\n">>,
    Tokens1 = steamroller_ast:tokens(Expect1),
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    Result4 = steamroller_algebra:format_tokens(Tokens1, 20),
    Result5 = steamroller_algebra:format_tokens(Tokens1, 1),
    [
     ?_assertEqual(Expect0, Result0),
     ?_assertEqual(Expect0, Result1),
     ?_assertEqual(Expect0, Result2),
     ?_assertEqual(Expect1, Result3),
     ?_assertEqual(Expect1, Result4),
     ?_assertEqual(Expect1, Result5)
    ].
