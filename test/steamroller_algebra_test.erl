-module(steamroller_algebra_test).

-include_lib("eunit/include/eunit.hrl").

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(indent, 2).

paper_implementation_test() ->
    [
        ?_assertEqual(
            <<"if a == b then a << 2 else a + b\n">>,
            steamroller_algebra:from_the_paper(100, ?indent)
        ),
        ?_assertEqual(
            <<"if a == b then a << 2 else a + b\n">>,
            steamroller_algebra:from_the_paper(32, ?indent)
        ),
        ?_assertEqual(
            <<"if a == b\nthen a << 2\nelse a + b\n">>,
            steamroller_algebra:from_the_paper(15, ?indent)
        ),
        ?_assertEqual(
            <<"if a == b\nthen\n  a << 2\nelse a + b\n">>,
            steamroller_algebra:from_the_paper(10, ?indent)
        ),
        ?_assertEqual(
            <<"if\n  a == b\nthen\n  a << 2\nelse\n  a + b\n">>,
            steamroller_algebra:from_the_paper(8, ?indent)
        ),
        ?_assertEqual(
            <<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a + b\n">>,
            steamroller_algebra:from_the_paper(7, ?indent)
        ),
        ?_assertEqual(
            <<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a +\n    b\n">>,
            steamroller_algebra:from_the_paper(6, ?indent)
        )
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
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

brackets_comment_test_() ->
    Tokens = steamroller_ast:tokens(<<"(\n  init/1,\n  % test\n  thing/0\n)">>),
    Expect = <<"(\n    init/1,\n    % test\n    thing/0\n)\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1), ?_assertEqual(Expect, Result2)].

brackets_inline_comment_test_() ->
    Tokens = steamroller_ast:tokens(<<"(\n  init/1,\n  thing/0 % test\n)">>),
    Expect = <<"(\n    init/1,\n    % test\n    thing/0\n)\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1), ?_assertEqual(Expect, Result2)].

brackets_multiline_inline_comment_test_() ->
    % Why would you do this, OTP?
    % The comments below the inline could either be a continuation of the inline comment or a fresh
    % comment...
    % Lets use a solution which will upset everyone.
    Tokens = steamroller_ast:tokens(<<"{\nflags % [line1,\n% line2,\n% line3]\n}">>),
    Expect = <<"{\n    % [line1,\n    flags\n    % line2,\n    % line3]\n}\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect, Result0)].

config_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"{erl_opts, [debug_info, {warn_format, 1}, warn_export_all]}.">>),
    Expect0 = <<"{erl_opts, [debug_info, {warn_format, 1}, warn_export_all]}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "{\n    erl_opts,\n    [\n        debug_info,\n        {warn_format, 1},\n        warn_export_all\n    ]\n}.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "{\n    erl_opts,\n    [\n        debug_info,\n        {\n            warn_format,\n            1\n        },\n        warn_export_all\n    ]\n}.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

nested_brackets_test_() ->
    Tokens = steamroller_ast:tokens(<<"{foo(), {error, {oh_no, \"problem\"}}}">>),
    Expect0 = <<"{foo(), {error, {oh_no, \"problem\"}}}\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"{\n    foo(),\n    {error, {oh_no, \"problem\"}}\n}\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 35),
    Expect2 = <<"{\n    foo(),\n    {\n        error,\n        {oh_no, \"problem\"}\n    }\n}\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect3 =
        <<
            "{\n    foo(),\n    {\n        error,\n        {\n            oh_no,\n            \"problem\"\n        }\n    }\n}\n"
        >>,
    Result3 = steamroller_algebra:format_tokens(Tokens, 10),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
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
    Expect1 = <<"foo(Arg1, Arg1) ->\n    error;\nfoo(Arg1, Arg2) -> ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 24),
    Expect2 =
        <<
            "foo(\n    Arg1,\n    Arg1\n) ->\n    error;\nfoo(\n    Arg1,\n    Arg2\n) ->\n    ok.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 1),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

function_long_multiclause_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "my_function(atom_1, X) -> {atom_1, X};\nmy_function(atom_2, {something, X, other_thing}) -> {something, X, yet_another_thing};\nmy_function(atom_2, X) -> X."
            >>
        ),
    Expect0 =
        <<
            "my_function(atom_1, X) -> {atom_1, X};\nmy_function(atom_2, {something, X, other_thing}) -> {something, X, yet_another_thing};\nmy_function(atom_2, X) -> X.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "my_function(atom_1, X) -> {atom_1, X};\nmy_function(atom_2, {something, X, other_thing}) ->\n    {something, X, yet_another_thing};\nmy_function(atom_2, X) -> X.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 70),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

function_macro_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> ?MACRO.">>),
    Expect0 = <<"foo() -> ?MACRO.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    ?MACRO.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

function_macro_args_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(SomeVariable) -> ?MACRO(SomeVariable).">>),
    Expect0 = <<"foo(SomeVariable) -> ?MACRO(SomeVariable).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(SomeVariable) ->\n    ?MACRO(SomeVariable).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

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
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

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
    Tokens =
        steamroller_ast:tokens(
            <<"foooooo(<<H,B:1/binary, C/binary>>) -> <<B/binary, H/binary,C/binary>>.">>
        ),
    Expect0 = <<"foooooo(<<H, B:1/binary, C/binary>>) -> <<B/binary, H/binary, C/binary>>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<"foooooo(<<H, B:1/binary, C/binary>>) ->\n    <<B/binary, H/binary, C/binary>>.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    Expect2 =
        <<
            "foooooo(\n    <<H, B:1/binary, C/binary>>\n) ->\n    <<\n        B/binary,\n        H/binary,\n        C/binary\n    >>.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 35),
    Expect3 =
        <<
            "foooooo(\n    <<\n        H,\n        B:1/binary,\n        C/binary\n    >>\n) ->\n    <<\n        B/binary,\n        H/binary,\n        C/binary\n    >>.\n"
        >>,
    Result3 = steamroller_algebra:format_tokens(Tokens, 20),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

function_comment_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo() ->\n    % Temporary workaround (2010-01-11)\n    {error, oh_no}.">>
        ),
    Expect0 = <<"foo() ->\n    % Temporary workaround (2010-01-11)\n    {error, oh_no}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    % Temporary workaround (2010-01-11)\n    {\n        error,\n        oh_no\n    }.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

function_inline_comment_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo() ->\n    X + Y, % Temporary workaround (2010-01-11)\n    {error, oh_no}.">>
        ),
    Expect0 =
        <<"foo() ->\n    % Temporary workaround (2010-01-11)\n    X + Y,\n    {error, oh_no}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    % Temporary workaround (2010-01-11)\n    X + Y,\n    {\n        error,\n        oh_no\n    }.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

function_comment_list_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() ->\n    {error,\n % TODO improve\noh_no}.">>),
    Expect0 =
        <<"foo() ->\n    {\n        error,\n        % TODO improve\n        oh_no\n    }.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 =
        <<"foo() ->\n    {\n        error,\n        % TODO improve\n        oh_no\n    }.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 1),
    Tokens1 =
        steamroller_ast:tokens(
            <<"foo() ->\n    Error = 1 + 2,\n    {error, \n% TODO improve\nError}.">>
        ),
    Expect2 =
        <<
            "foo() ->\n    Error = 1 + 2,\n    {\n        error,\n        % TODO improve\n        Error\n    }.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens1, 100),
    Expect3 =
        <<
            "foo() ->\n    Error =\n        1 + 2,\n    {\n        error,\n        % TODO improve\n        Error\n    }.\n"
        >>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 15),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

function_clause_comment_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(Arg1, Arg1) -> error;\n% Hello World\nfoo(Arg1, Arg2) -> ok.">>
        ),
    Expect0 = <<"foo(Arg1, Arg1) -> error;\n% Hello World\nfoo(Arg1, Arg2) -> ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg1) ->\n    error;\n% Hello World\nfoo(Arg1, Arg2) ->\n    ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    Expect2 =
        <<
            "foo(\n    Arg1,\n    Arg1\n) ->\n    error;\n% Hello World\nfoo(\n    Arg1,\n    Arg2\n) ->\n    ok.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 1),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

multi_function_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(X) -> Y = bar(X), baz(Y).">>),
    Expect0 = <<"foo(X) ->\n    Y = bar(X),\n    baz(Y).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"foo(X) ->\n    Y =\n        bar(X),\n    baz(Y).\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 14),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect0, Result1),
        ?_assertEqual(Expect2, Result2)
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
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

attribute_test_() ->
    Tokens = steamroller_ast:tokens(<<"-module(test).\n\n-export([start_link/0, init/1]).">>),
    Expect0 = <<"-module(test).\n\n-export([start_link/0, init/1]).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-module(test).\n\n-export(\n    [start_link/0, init/1]\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 =
        <<"-module(test).\n\n-export(\n    [\n        start_link/0,\n        init/1\n    ]\n).\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

attribute_comment_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"-module(test).\n\n% Comment\n-export([start_link/0, init/1]).">>),
    Expect0 = <<"-module(test).\n\n% Comment\n-export([start_link/0, init/1]).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-module(test).\n\n% Comment\n-export(\n    [start_link/0, init/1]\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 =
        <<
            "-module(test).\n\n% Comment\n-export(\n    [\n        start_link/0,\n        init/1\n    ]\n).\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

%
% TODO change this behaviour. We currently have
%
% ```erlang
% -export(
%     [
%         start_link/0,
%         % Some comment
%         init/1
%     ]
% ).
% ```
%
% which is "fine" by some definition of the word.
%
% But really those brackets should be squashed:
%
% ```erlang
% -export([
%     start_link/0,
%     % Some comment
%     init/1
% ]).
% ```
%
attribute_commment_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"-module(test).\n\n-export([start_link/0,\n    % comment\n    init/1]).">>
        ),
    Expect =
        <<
            "-module(test).\n\n-export(\n    [\n        start_link/0,\n        % comment\n        init/1\n    ]\n).\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1), ?_assertEqual(Expect, Result2)].

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

basic_spec_test_() ->
    Tokens = steamroller_ast:tokens(<<"-spec test(some_type()) -> other_type().\n">>),
    Expect0 = <<"-spec test(some_type()) -> other_type().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-spec test(some_type()) -> other_type().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"-spec test(some_type()) ->\n    other_type().\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

spec_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"-spec test(some_type()) -> other_type() | {error, atom()}.\n">>),
    Expect0 = <<"-spec test(some_type()) -> other_type() | {error, atom()}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-spec test(some_type()) ->\n    other_type() | {error, atom()}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"-spec test(some_type()) ->\n    other_type()\n    | {error, atom()}.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

spec_comment_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"% My first spec\n\n\n-spec test(some_type()) -> other_type().\n">>
        ),
    Expect0 = <<"% My first spec\n-spec test(some_type()) -> other_type().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"% My first spec\n-spec test(some_type()) -> other_type().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"% My first spec\n-spec test(some_type()) ->\n    other_type().\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

spec_bracket_removal_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"-spec(test(some_type()) -> other_type() | {error, atom()}).\n">>),
    Expect0 = <<"-spec test(some_type()) -> other_type() | {error, atom()}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-spec test(some_type()) ->\n    other_type() | {error, atom()}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"-spec test(some_type()) ->\n    other_type()\n    | {error, atom()}.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

specced_function_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"-spec test(some_type()) -> other_type().\ntest(A) -> A + 1.\n">>),
    Expect0 = <<"-spec test(some_type()) -> other_type().\ntest(A) -> A + 1.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-spec test(some_type()) -> other_type().\ntest(A) -> A + 1.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"-spec test(some_type()) ->\n    other_type().\ntest(A) -> A + 1.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

basic_case_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(X) -> case bar(X) of {ok, Something} -> Something; {error, _} -> oh_no end.">>
        ),
    Expect =
        <<
            "foo(X) ->\n    case bar(X) of\n        {ok, Something} -> Something;\n        {error, _} -> oh_no\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1)].

case_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "foo(X) -> case bar(X) of {ok, Y} -> Z = baz(Y), foo(Z); {error, oops} -> oops; {error, _} -> oh_no end."
            >>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    case bar(X) of\n        {ok, Y} ->\n            Z = baz(Y),\n            foo(Z);\n        {error, oops} -> oops;\n        {error, _} -> oh_no\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

matched_case_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foooooooooo(X) -> Ret = case X of some_value -> great end, Ret.">>
        ),
    Expect0 = <<"foooooooooo(X) ->\n    Ret = case X of some_value -> great end,\n    Ret.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foooooooooo(X) ->\n    Ret =\n        case X of\n            some_value -> great\n        end,\n    Ret.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

commented_case_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "foo(X) -> {Y, Z} = case X of\n    some_value ->\n        % Comment\n        {1, 2}\n    end."
            >>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    {Y, Z} =\n        case X of\n            some_value ->\n                % Comment\n                {1, 2}\n        end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

guarded_case_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(X) -> case X of X when X == test orelse X == other -> ok; _ -> error end">>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    case X of\n        X when X == test orelse X == other -> ok;\n        _ -> error\n    end\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo(X) ->\n    case X of\n        X when X == test orelse X == other ->\n            ok;\n        _ -> error\n    end\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 45),
    Expect2 =
        <<
            "foo(X) ->\n    case X of\n        X\n        when X == test orelse X == other ->\n            ok;\n        _ -> error\n    end\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

guarded_case_equation_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "foo(X) -> Y = case X of X when X == test orelse X == other -> ok; _ -> error end, Y."
            >>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    Y =\n        case X of\n            X when X == test orelse X == other -> ok;\n            _ -> error\n        end,\n    Y.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

simple_module_function_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(X) -> module:bar().">>),
    Expect0 = <<"foo(X) -> module:bar().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X) ->\n    module:bar().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

function_head_pattern_match_test_() ->
    Tokens = steamroller_ast:tokens(<<"long_foo({X, _} = Y) -> {bar(X), baz(Y)}.">>),
    Expect0 = <<"long_foo({X, _} = Y) -> {bar(X), baz(Y)}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<"long_foo(\n    {X, _} = Y\n) ->\n    {\n        bar(X),\n        baz(Y)\n    }.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 15),
    Expect2 =
        <<
            "long_foo(\n    {X, _} =\n        Y\n) ->\n    {\n        bar(\n            X\n        ),\n        baz(\n            Y\n        )\n    }.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 12),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

case_pattern_match_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo() -> case bar() of ok -> ok; {error, _} = Err -> Err end.">>),
    Expect =
        <<
            "foo() ->\n    case bar() of\n        ok -> ok;\n        {error, _} = Err -> Err\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1)].

slash_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() -> \"\\\"some string\\\"\".">>),
    Expect0 = <<"foo() -> \"\\\"some string\\\"\".\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Tokens1 = steamroller_ast:tokens(<<"foo() -> re:compile(\"\\\\.[he]rl$\").">>),
    Expect1 = <<"foo() -> re:compile(\"\\\\.[he]rl$\").\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    Tokens2 = steamroller_ast:tokens(<<"foo() -> re:compile(\"\\.[he]rl$\").">>),
    Expect2 = <<"foo() -> re:compile(\".[he]rl$\").\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens2, 100),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

if_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo(A, B) -> if A == B -> great; true -> oh_no end.">>),
    Expect0 =
        <<"foo(A, B) ->\n    if\n        A == B -> great;\n        true -> oh_no\n    end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 = Expect0,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 50),
    Expect2 =
        <<
            "foo(A, B) ->\n    if\n        A == B ->\n            great;\n        true ->\n            oh_no\n    end.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens0, 20),
    Tokens1 = steamroller_ast:tokens(<<"foo(A) -> if length(A) > 1 -> great; true -> oh_no end.">>),
    Expect3 =
        <<"foo(A) ->\n    if\n        length(A) > 1 -> great;\n        true -> oh_no\n    end.\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    Expect4 = Expect3,
    Result4 = steamroller_algebra:format_tokens(Tokens1, 50),
    Expect5 =
        <<
            "foo(A) ->\n    if\n        length(A) > 1 ->\n            great;\n        true -> oh_no\n    end.\n"
        >>,
    Result5 = steamroller_algebra:format_tokens(Tokens1, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3),
        ?_assertEqual(Expect4, Result4),
        ?_assertEqual(Expect5, Result5)
    ].

nested_case_if_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo(A, B) -> case A of test -> if B == 5 -> great end end.">>),
    Expect0 = <<"foo(A, B) -> case A of test -> if B == 5 -> great end end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(A, B) ->\n    case A of test -> if B == 5 -> great end end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 = <<"foo(A, B) ->\n    case A of\n        test -> if B == 5 -> great end\n    end.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    Expect3 =
        <<
            "foo(A, B) ->\n    case A of\n        test ->\n            if\n                B == 5 ->\n                    great\n            end\n    end.\n"
        >>,
    Result3 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

nested_if_case_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo(A, B) -> if B == 5 -> case A of test -> great end end.">>),
    Expect0 = <<"foo(A, B) -> if B == 5 -> case A of test -> great end end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(A, B) ->\n    if B == 5 -> case A of test -> great end end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo(A, B) ->\n    if\n        B == 5 ->\n            case A of test -> great end\n    end.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    Expect3 =
        <<
            "foo(A, B) ->\n    if\n        B == 5 ->\n            case A of\n                test -> great\n            end\n    end.\n"
        >>,
    Result3 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

nested_fun_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo(X) -> fun() -> case X of 1 -> ok; 0 -> error end end.">>),
    Expect0 =
        <<
            "foo(X) ->\n    fun\n        () ->\n            case X of\n                1 -> ok;\n                0 -> error\n            end\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

when_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo(X) when is_atom(X) -> atom; foo(X) when X =< 10 -> ok.">>),
    Expect0 = <<"foo(X) when is_atom(X) -> atom;\nfoo(X) when X =< 10 -> ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X) when is_atom(X) ->\n    atom;\nfoo(X) when X =< 10 -> ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"foo(X)\nwhen is_atom(X) ->\n    atom;\nfoo(X) when X =< 10 ->\n    ok.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

guard_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(X) when is_atom(X), X == test -> ok.">>),
    Expect0 = <<"foo(X) when is_atom(X), X == test -> ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X)\nwhen is_atom(X), X == test ->\n    ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"foo(X)\nwhen is_atom(X),\n     X == test ->\n    ok.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

guard_sequence_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(X) when X == test; X == other -> ok.">>),
    Expect0 = <<"foo(X) when X == test; X == other -> ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X)\nwhen X == test; X == other ->\n    ok.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"foo(X)\nwhen X == test;\n     X == other ->\n    ok.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 20),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

if_guard_sequence_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(X) -> if X =:= test; X =:= not_test -> ok; true -> error end.">>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    if\n        X =:= test; X =:= not_test -> ok;\n        true -> error\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo(X) ->\n    if\n        X =:= test;\n        X =:= not_test ->\n            ok;\n        true -> error\n    end.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

multi_guard_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(X) when is_integer(X) andalso X > 200 andalso X < 500 -> big_integer.">>
        ),
    Expect0 = <<"foo(X) when is_integer(X) andalso X > 200 andalso X < 500 -> big_integer.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<"foo(X)\nwhen is_integer(X) andalso X > 200 andalso X < 500 ->\n    big_integer.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo(X)\nwhen is_integer(X)\n     andalso X > 200\n     andalso X < 500 ->\n    big_integer.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

grouped_multi_guard_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(X) when is_integer(X) andalso (X > 200 orelse X < 0) -> maybe_negative.">>
        ),
    Expect0 = <<"foo(X) when is_integer(X) andalso (X > 200 orelse X < 0) -> maybe_negative.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<"foo(X)\nwhen is_integer(X) andalso (X > 200 orelse X < 0) ->\n    maybe_negative.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo(X)\nwhen is_integer(X)\n     andalso (X > 200 orelse X < 0) ->\n    maybe_negative.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

type_test_() ->
    Tokens = steamroller_ast:tokens(<<"-type my_type() :: something | {something_else, atom()}.">>),
    Expect0 = <<"-type my_type() :: something | {something_else, atom()}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-type my_type() :: something\n                 | {something_else, atom()}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

long_type_test_() ->
    Tokens = steamroller_ast:tokens(<<"-type my_type() :: one | two | three | four | five.">>),
    Expect0 = <<"-type my_type() :: one | two | three | four | five.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-type my_type() :: one\n                 | two\n                 | three\n                 | four\n                 | five.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

type_bracket_removal_test_() ->
    Tokens = steamroller_ast:tokens(<<"-type(my_type() :: atom()).">>),
    Expect0 = <<"-type my_type() :: atom().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-type my_type() :: atom().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

atom_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> '{'.">>),
    Expect0 = <<"foo() -> '{'.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

dot_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() -> dot.">>),
    Expect0 = <<"foo() -> dot.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Tokens1 = steamroller_ast:tokens(<<"foo() -> 'dot'.">>),
    Expect1 = <<"foo() -> dot.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

anon_function_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foooooooooo(X) -> lists:any(fun (Y) -> Y == thing end, X).">>),
    Expect0 = <<"foooooooooo(X) -> lists:any(fun (Y) -> Y == thing end, X).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foooooooooo(X) ->\n    lists:any(fun (Y) -> Y == thing end, X).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foooooooooo(X) ->\n    lists:any(\n        fun (Y) -> Y == thing end,\n        X\n    ).\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

anon_function_multicase_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> fun (test) -> ok; (_) -> error end.">>),
    Expect0 = <<"foo() -> fun (test) -> ok; (_) -> error end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    fun (test) -> ok; (_) -> error end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    Expect2 = <<"foo() ->\n    fun\n        (test) -> ok;\n        (_) -> error\n    end.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

dynamic_function_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foooooooooo(X) -> lists:any(fun local/1, X).">>),
    Expect0 = <<"foooooooooo(X) -> lists:any(fun local/1, X).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 = <<"foooooooooo(X) ->\n    lists:any(fun local/1, X).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 30),
    Expect2 = <<"foooooooooo(X) ->\n    lists:any(\n        fun local/1,\n        X\n    ).\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens0, 20),
    Tokens1 = steamroller_ast:tokens(<<"foooooooooo(X) -> lists:any(fun module:func/1, X).">>),
    Expect3 = <<"foooooooooo(X) -> lists:any(fun module:func/1, X).\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    Expect4 = <<"foooooooooo(X) ->\n    lists:any(fun module:func/1, X).\n">>,
    Result4 = steamroller_algebra:format_tokens(Tokens1, 40),
    Expect5 =
        <<"foooooooooo(X) ->\n    lists:any(\n        fun module:func/1,\n        X\n    ).\n">>,
    Result5 = steamroller_algebra:format_tokens(Tokens1, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3),
        ?_assertEqual(Expect4, Result4),
        ?_assertEqual(Expect5, Result5)
    ].

empty_string_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() -> <<\"\">>.">>),
    Expect0 = <<"foo() -> <<\"\">>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Tokens1 = steamroller_ast:tokens(<<"foo() -> \"\".">>),
    Expect1 = <<"foo() -> \"\".\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

group_type_test_() ->
    Tokens = steamroller_ast:tokens(<<"-type a() :: atom().\n\n-type b() :: atom().">>),
    Expect0 = <<"-type a() :: atom().\n-type b() :: atom().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

group_define_test_() ->
    Tokens = steamroller_ast:tokens(<<"-define(a, a).\n\n-define(b, b).">>),
    Expect0 = <<"-define(a, a).\n-define(b, b).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

define_whitespace_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "-define(MY_MACRO(Arg), (Arg == 5 orelse Arg == 6 orelse Arg == 7 orelse Arg == 8))."
            >>
        ),
    Expect0 =
        <<"-define(MY_MACRO(Arg), (Arg == 5 orelse Arg == 6 orelse Arg == 7 orelse Arg == 8)).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-define(\n    MY_MACRO(Arg),\n    (Arg == 5 orelse Arg == 6 orelse Arg == 7 orelse Arg == 8)\n).\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 70),
    Expect2 =
        <<
            "-define(\n    MY_MACRO(Arg),\n    (\n        Arg == 5\n        orelse Arg == 6\n        orelse Arg == 7\n        orelse Arg == 8\n    )\n).\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

bracketless_define_whitespace_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"-define(MY_MACRO(Arg), Arg == 5 orelse Arg == 6 orelse Arg == 7 orelse Arg == 8).">>
        ),
    Expect0 =
        <<"-define(MY_MACRO(Arg), Arg == 5 orelse Arg == 6 orelse Arg == 7 orelse Arg == 8).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-define(\n    MY_MACRO(Arg),\n    Arg == 5 orelse Arg == 6 orelse Arg == 7 orelse Arg == 8\n).\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 70),
    Expect2 =
        <<
            "-define(\n    MY_MACRO(Arg),\n    Arg == 5\n    orelse Arg == 6\n    orelse Arg == 7\n    orelse Arg == 8\n).\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

char_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() -> $f.">>),
    Expect0 = <<"foo() -> $f.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 = <<"foo() -> $f.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 100),
    Tokens1 = steamroller_ast:tokens(<<"foo() -> [$a, $b, $c].">>),
    Expect2 = <<"foo() -> [$a, $b, $c].\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens1, 100),
    Expect3 = <<"foo() -> [$a, $b, $c].\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

list_concat_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(One, Two, Three, Four) -> {[One] ++ lists:reverse(Two) ++ [Three], Four}.">>
        ),
    Expect0 = <<"foo(One, Two, Three, Four) -> {[One] ++ lists:reverse(Two) ++ [Three], Four}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<"foo(One, Two, Three, Four) ->\n    {[One] ++ lists:reverse(Two) ++ [Three], Four}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo(One, Two, Three, Four) ->\n    {\n        [One] ++ lists:reverse(Two) ++ [Three],\n        Four\n    }.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 48),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

list_comprehension_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"fooooooooo(X) -> [some_other_module:bar(Y) || Y <- baz(X)].">>),
    Expect0 = <<"fooooooooo(X) -> [some_other_module:bar(Y) || Y <- baz(X)].\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"fooooooooo(X) ->\n    [some_other_module:bar(Y) || Y <- baz(X)].\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "fooooooooo(X) ->\n    [\n        some_other_module:bar(Y)\n        || Y <- baz(X)\n    ].\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

map_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo() -> X = #{test => 123, other => 456}, X#{\"test\" => 789}.">>
        ),
    Expect0 = <<"foo() ->\n    X = #{test => 123, other => 456},\n    X#{\"test\" => 789}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    X = #{test => 123, other => 456},\n    X#{\"test\" => 789}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo() ->\n    X =\n        #{\n            test => 123,\n            other => 456\n        },\n    X#{\"test\" => 789}.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

record_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo() -> State = #state{test=123}, State#state{other=456}.">>),
    Expect0 = <<"foo() ->\n    State = #state{test = 123},\n    State#state{other = 456}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    State = #state{test = 123},\n    State#state{other = 456}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo() ->\n    State =\n        #state{\n            test = 123\n        },\n    State#state{\n        other = 456\n    }.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 25),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

ifdef_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"-ifdef(something).\n-define(x, 1).\n-else.\n-define(x, 2).\n-endif.">>
        ),
    Expect0 = <<"-ifdef(something).\n-define(x, 1).\n-else.\n-define(x, 2).\n-endif.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

begin_test_() ->
    Tokens = steamroller_ast:tokens(<<"-define(macro(Thing),\n begin\n (Thing + 1)\n end).">>),
    Expect0 = <<"-define(macro(Thing), begin (Thing + 1) end).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-define(\n    macro(Thing),\n    begin\n        (Thing + 1)\n    end\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

begin_list_comprehension_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<"foo(Pids) -> [begin unlink(Pid), exit(Pid, kill) end || Pid <- Pids].">>
        ),
    Expect0 = <<"foo(Pids) -> [begin unlink(Pid), exit(Pid, kill) end || Pid <- Pids].\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo(Pids) ->\n    [\n        begin unlink(Pid), exit(Pid, kill) end\n        || Pid <- Pids\n    ].\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo(Pids) ->\n    [\n        begin\n            unlink(Pid),\n            exit(Pid, kill)\n        end\n        || Pid <- Pids\n    ].\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

send_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo(Pid) -> Pid ! message.">>),
    Expect0 = <<"foo(Pid) -> Pid ! message.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Pid) ->\n    Pid ! message.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

receive_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "foo() -> receive\n  X when is_integer(X) -> X + 1;\n  Y -> {ok, Y}\n  after\n  100 -> error\n end"
            >>
        ),
    Expect0 =
        <<
            "foo() ->\n    receive\n        X when is_integer(X) -> X + 1;\n        Y -> {ok, Y}\n    after\n        100 -> error\n    end\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    receive\n        X when is_integer(X) ->\n            X + 1;\n        Y -> {ok, Y}\n    after\n        100 -> error\n    end\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

empty_receive_test_() ->
    % Who knew this is valid erlang!
    Tokens = steamroller_ast:tokens(<<"foo() -> receive\n  after X -> timeout\n end.">>),
    Expect0 = <<"foo() -> receive after X -> timeout end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    receive after X -> timeout end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 36),
    Expect2 = <<"foo() ->\n    receive\n    after\n        X -> timeout\n    end.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

nested_receive_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo() -> receive message -> receive after 0 -> ok end end">>),
    Expect0 = <<"foo() -> receive message -> receive after 0 -> ok end end\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    receive\n        message -> receive after 0 -> ok end\n    end\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "foo() ->\n    receive\n        message ->\n            receive\n            after\n                0 -> ok\n            end\n    end\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

float_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> 0.333333.">>),
    Expect0 = <<"foo() -> 0.333333.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

% FIXME
%
% number_base_test_ and number_e_test_ show that we convert allowed erlang number syntax into
% the number literals (...apart from 2.3e3 because that's apparently allowed).
%
% This behaviour comes from erl_scan:string which we use to generate our source code tokens.
%
% I think the fix is to pass the `text` option to erl_scan:string and scan the tokens and the
% raw text at the same time. Use the text instead of the parsed tokens for these number
% representations.
%
% This seems like a faff so it's getting left for later.
%
number_base_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> 16#1f.">>),
    %Expect0 = <<"foo() -> 16#1f.\n">>,
    Expect0 = <<"foo() -> 31.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

number_e_test_() ->
    Tokens0 = steamroller_ast:tokens(<<"foo() -> 2.3e3.">>),
    Expect0 = <<"foo() -> 2.3e3.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Tokens1 = steamroller_ast:tokens(<<"foo() -> 2.3e-3.">>),
    %Expect1 = <<"foo() -> 2.3e-3.\n">>,
    Expect1 = <<"foo() -> 0.0023.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

try_catch_test_() ->
    Tokens = steamroller_ast:tokens(<<"foo() -> try bar(), baz() catch Y -> {caught, Y} end">>),
    Expect0 =
        <<
            "foo() ->\n    try\n        bar(),\n        baz()\n    catch\n        Y -> {caught, Y}\n    end\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    try\n        bar(),\n        baz()\n    catch\n        Y -> {caught, Y}\n    end\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

try_of_test_() ->
    Tokens =
        steamroller_ast:tokens(<<"foo() -> try bar() of X -> {ok, X} catch Y -> {caught, Y} end">>),
    Expect0 = <<"foo() -> try bar() of X -> {ok, X} catch Y -> {caught, Y} end\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    try bar() of\n        X -> {ok, X}\n    catch\n        Y -> {caught, Y}\n    end\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

try_case_test_() ->
    Tokens =
        steamroller_ast:tokens(
            <<
                "foo() -> try case bar() of test -> ok; other -> error end catch _:Y -> throw(Y) end."
            >>
        ),
    Expect0 =
        <<
            "foo() ->\n    try\n        case bar() of\n            test -> ok;\n            other -> error\n        end\n    catch\n        _ : Y -> throw(Y)\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].
