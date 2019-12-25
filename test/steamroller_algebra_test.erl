-module(steamroller_algebra_test).

-include_lib("eunit/include/eunit.hrl").

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(indent, 2).

%%
%% Atom
%%

atom_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> '{'.">>),
    Expect0 = <<"foo() -> '{'.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

dot_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> dot.">>),
    Expect0 = <<"foo() -> dot.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo() -> 'dot'.">>),
    Expect1 = <<"foo() -> dot.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

%%
%% String
%%

basic_string_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> \"string\".">>),
    Expect0 = <<"foo() -> \"string\".\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    \"string\".\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

empty_string_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> \"\".">>),
    Expect0 = <<"foo() -> \"\".\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% List
%%

list_concat_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} =
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

%%
%% Char
%%

char_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> $f.">>),
    Expect0 = <<"foo() -> $f.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 = <<"foo() -> $f.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo() -> [$a, $b, $c].">>),
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

%%
%% Binary
%%

basic_binary_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> <<\"binary\">>.">>),
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

empty_binary_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> <<\"\">>.">>),
    Expect0 = <<"foo() -> <<\"\">>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    [?_assertEqual(Expect0, Result0)].

binary_construction_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(A, B) -> <<A/binary, B/binary>>.">>),
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

binary_literal_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> <<\"test\"/unicode>>.">>),
    Expect0 = <<"foo() -> <<\"test\"/unicode>>.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    <<\"test\"/unicode>>.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 25),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

binary_arg_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(<<A/binary>>) -> A.">>),
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

complex_binary_arg_test_() ->
    {ok, Tokens} =
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

%%
%% Escape
%%

slash_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> \"\\\"some string\\\"\".">>),
    Expect0 = <<"foo() -> \"\\\"some string\\\"\".\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo() -> re:compile(\"\\\\.[he]rl$\").">>),
    Expect1 = <<"foo() -> re:compile(\"\\\\.[he]rl$\").\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    {ok, Tokens2} = steamroller_ast:tokens(<<"foo() -> re:compile(\"\\.[he]rl$\").">>),
    Expect2 = <<"foo() -> re:compile(\".[he]rl$\").\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens2, 100),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

%%
%% Boolean
%%

bool_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo(X) -> true = bar(X) =:= (X > 0 andalso X < 10).">>),
    Expect0 = <<"foo(X) -> true = bar(X) =:= (X > 0 andalso X < 10).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X) ->\n    true = bar(X) =:= (X > 0 andalso X < 10).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<"foo(X) ->\n    true =\n        bar(X) =:=\n            (X > 0 andalso X < 10).\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

%%
%% Float
%%

float_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> 0.333333.">>),
    Expect0 = <<"foo() -> 0.333333.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Number Syntax
%%

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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> 16#1f.">>),
    %Expect0 = <<"foo() -> 16#1f.\n">>,
    Expect0 = <<"foo() -> 31.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

number_e_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> 2.3e3.">>),
    Expect0 = <<"foo() -> 2.3e3.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo() -> 2.3e-3.">>),
    %Expect1 = <<"foo() -> 2.3e-3.\n">>,
    Expect1 = <<"foo() -> 0.0023.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

%%
%% Unicode
%%

unicode_test_() ->
    {ok, Tokens0} =
        steamroller_ast:tokens(
            <<"foo() -> 'こんにちは' = bar(\"こんにちは\"), ok."/utf8>>
        ),
    Expect0 = <<"foo() ->\n    'こんにちは' = bar(\"こんにちは\"),\n    ok.\n"/utf8>>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo() -> $ん."/utf8>>),
    Expect1 = <<"foo() -> $ん.\n"/utf8>>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

%%
%% Brackets
%%

basic_brackets_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"(Arg1, Arg2).">>),
    Expect0 = <<"(Arg1, Arg2).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"(\n    Arg1,\n    Arg2\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 1),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

brackets_comment_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"(\n  init/1,\n  % test\n  thing/0\n).">>),
    Expect = <<"(\n    init/1,\n    % test\n    thing/0\n).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1), ?_assertEqual(Expect, Result2)].

brackets_inline_comment_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"(\n  init/1,\n  thing/0 % test\n).">>),
    Expect = <<"(\n    init/1,\n    % test\n    thing/0\n).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1), ?_assertEqual(Expect, Result2)].

brackets_multiline_inline_comment_test_() ->
    % Why would you do this, OTP?
    % This is not perfect, but equally this is a silly way to comment.
    {ok, Tokens} = steamroller_ast:tokens(<<"{\nflags % [line1,\n% line2,\n% line3]\n}.">>),
    Expect = <<"{\n    % [line1,\n    flags\n    % line2,\n    % line3]\n}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect, Result0)].

config_test_() ->
    {ok, Tokens} =
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

config_whitespace_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<
                "{erl_opts, [debug_info, {warn_format, 1}, warn_export_all]}.\n{plugins, [steamroller]}."
            >>
        ),
    Expect0 =
        <<
            "{erl_opts, [debug_info, {warn_format, 1}, warn_export_all]}.\n\n{plugins, [steamroller]}.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "{\n    erl_opts,\n    [\n        debug_info,\n        {warn_format, 1},\n        warn_export_all\n    ]\n}.\n\n{plugins, [steamroller]}.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<
            "{\n    erl_opts,\n    [\n        debug_info,\n        {\n            warn_format,\n            1\n        },\n        warn_export_all\n    ]\n}.\n\n{\n    plugins,\n    [\n        steamroller\n    ]\n}.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 10),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

nested_brackets_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"{foo(), {error, {oh_no, \"problem\"}}}.">>),
    Expect0 = <<"{foo(), {error, {oh_no, \"problem\"}}}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"{\n    foo(),\n    {error, {oh_no, \"problem\"}}\n}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 35),
    Expect2 = <<"{\n    foo(),\n    {\n        error,\n        {oh_no, \"problem\"}\n    }\n}.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect3 =
        <<
            "{\n    foo(),\n    {\n        error,\n        {\n            oh_no,\n            \"problem\"\n        }\n    }\n}.\n"
        >>,
    Result3 = steamroller_algebra:format_tokens(Tokens, 10),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

%%
%% Function
%%

basic_function_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> ok.">>),
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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> Arg3 = Arg1 + Arg2, Arg3.">>),
    Expect0 = <<"foo(Arg1, Arg2) ->\n    Arg3 = Arg1 + Arg2,\n    Arg3.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    Arg3 = Arg1 + Arg2,\n    Arg3.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    Expect2 = <<"foo(Arg1, Arg2) ->\n    Arg3 =\n        Arg1 + Arg2,\n    Arg3.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 22),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> foo, bar, baz, ok.">>),
    Expect3 = <<"foo(Arg1, Arg2) ->\n    foo,\n    bar,\n    baz,\n    ok.\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

function_clause_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(Arg1, Arg1) -> error; foo(Arg1, Arg2) -> ok.">>),
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
    {ok, Tokens} =
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

function_tuple_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> {error, oh_no}.">>),
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

multi_function_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X) -> Y = bar(X), baz(Y).">>),
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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(Arg1, Arg2) -> Arg1 + Arg2.\nbar() -> baz.">>),
    Expect0 = <<"foo(Arg1, Arg2) -> Arg1 + Arg2.\n\nbar() -> baz.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    Arg1 + Arg2.\n\nbar() -> baz.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    {ok, Tokens2} =
        steamroller_ast:tokens(<<"fooooooooo(Arg1, Arg2) -> Arg1 + Arg2.\nbar() -> baz.">>),
    Expect2 = <<"fooooooooo(\n    Arg1,\n    Arg2\n) ->\n    Arg1 + Arg2.\n\nbar() -> baz.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens2, 16),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

simple_module_function_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X) -> module:bar().">>),
    Expect0 = <<"foo(X) -> module:bar().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X) ->\n    module:bar().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

function_head_pattern_match_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"long_foo({X, _} = Y) -> {bar(X), baz(Y)}.">>),
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

%%
%% Attribute
%%

basic_attribute_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-module(test).">>),
    Expect0 = <<"-module(test).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-module(\n    test\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 1),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

attribute_bracketless_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-module test.">>),
    Expect0 = <<"-module(test).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-module(\n    test\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 1),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

attribute_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-module(test).\n\n-export([start_link/0, init/1]).">>),
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

%%
%% Type
%%

type_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"-type my_type() :: something | {something_else, atom()}.">>),
    Expect0 = <<"-type my_type() :: something | {something_else, atom()}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-type my_type() :: something\n                 | {something_else, atom()}.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

long_type_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"-type my_type() :: one | two | three | four | five.">>),
    Expect0 = <<"-type my_type() :: one | two | three | four | five.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-type my_type() :: one\n                 | two\n                 | three\n                 | four\n                 | five.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

type_var_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"-type x() :: fun((Y :: other_type()) -> ok) | fun((Y :: type2()) -> ok | error).">>
        ),
    Expect0 =
        <<"-type x() :: fun((Y :: other_type()) -> ok) | fun((Y :: type2()) -> ok | error).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-type x() :: fun((Y :: other_type()) -> ok)\n           | fun((Y :: type2()) -> ok | error).\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 60),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

type_bracket_removal_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-type(my_type() :: atom()).">>),
    Expect0 = <<"-type my_type() :: atom().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-type my_type() :: atom().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

type_grouping_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-type a() :: atom().\n\n-type b() :: atom().">>),
    Expect0 = <<"-type a() :: atom().\n-type b() :: atom().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

type_fun_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"-type x() :: fun().">>),
    Expect0 = <<"-type x() :: fun().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"-type x() :: fun() | map().">>),
    Expect1 = <<"-type x() :: fun() | map().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    Expect2 = <<"-type x() :: fun()\n           | map().\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens1, 20),
    {ok, Tokens2} = steamroller_ast:tokens(<<"-type x() :: fun().\n-type y() :: foo().">>),
    Expect3 = <<"-type x() :: fun().\n-type y() :: foo().\n">>,
    Result3 = steamroller_algebra:format_tokens(Tokens2, 100),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2),
        ?_assertEqual(Expect3, Result3)
    ].

type_fun_brackets_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"-type foo() :: {fun()}.">>),
    Expect0 = <<"-type foo() :: {fun()}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"-spec foo() -> fun(() -> fun()).">>),
    Expect1 = <<"-spec foo() -> fun(() -> fun()).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

opaque_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-opaque my_type() :: other_type().">>),
    Expect0 = <<"-opaque my_type() :: other_type().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-opaque my_type() :: other_type().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

%%
%% Spec
%%

basic_spec_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-spec test(some_type()) -> other_type().\n">>),
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
    {ok, Tokens} =
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
    {ok, Tokens} =
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
    {ok, Tokens} =
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

spec_var_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"-spec test(X, Y) -> type() when X :: integer(), Y :: [string()].">>
        ),
    Expect0 = <<"-spec test(X, Y) -> type() when X :: integer(), Y :: [string()].\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-spec test(X, Y) ->\n    type() when X :: integer(), Y :: [string()].\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    Expect2 =
        <<"-spec test(X, Y) ->\n    type()\n    when X :: integer(),\n         Y :: [string()].\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

spec_var_fun_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"-spec foo(X) -> type() when X :: integer().\nfoo(X) when is_integer(X) -> bar().">>
        ),
    Expect0 =
        <<"-spec foo(X) -> type() when X :: integer().\nfoo(X) when is_integer(X) -> bar().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-spec foo(X) ->\n    type() when X :: integer().\nfoo(X) when is_integer(X) -> bar().\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    Expect2 =
        <<
            "-spec foo(X) ->\n    type()\n    when X :: integer().\nfoo(X) when is_integer(X) ->\n    bar().\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

specced_function_test_() ->
    {ok, Tokens} =
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

multiclause_spec_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<
                "-spec foo(bar, X) -> Y when X :: type(), Y :: type();\n    ({baz, term()}, term()) -> term();\n    (baz, term()) -> term()."
            >>
        ),
    Expect0 =
        <<
            "-spec foo(bar, X) -> Y when X :: type(), Y :: type();\n         ({baz, term()}, term()) -> term();\n         (baz, term()) -> term().\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-spec foo(bar, X) ->\n             Y when X :: type(), Y :: type();\n         ({baz, term()}, term()) -> term();\n         (baz, term()) -> term().\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    % TODO This could be better
    Expect2 =
        <<
            "-spec foo(bar, X) ->\n             Y\n             when X :: type(),\n                  Y :: type();\n         ({baz, term()}, term()) ->\n             term();\n         (baz, term()) -> term().\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 40),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

%%
%% Callback
%%

callback_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"-callback foo(X :: bar()) -> thing().">>),
    Expect0 = <<"-callback foo(X :: bar()) -> thing().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"-callback(foo(X :: bar()) -> thing()).">>),
    Expect1 = <<"-callback foo(X :: bar()) -> thing().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

%%
%% Define
%%

define_grouping_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-define(a, a).\n\n-define(b, b).">>),
    Expect0 = <<"-define(a, a).\n-define(b, b).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

define_whitespace_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} =
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

define_variable_function_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-define(MACRO(X), {X, fun X/1}).">>),
    Expect0 = <<"-define(MACRO(X), {X, fun X/1}).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Ifdef
%%

ifdef_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"-ifdef(something).\n-define(x, 1).\n-else.\n-define(x, 2).\n-endif.">>
        ),
    Expect0 = <<"-ifdef(something).\n-define(x, 1).\n-else.\n-define(x, 2).\n-endif.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

if_attribute_test_() ->
    % Apparently you can have an attribute just called `-if`.
    % It appears to behave in the same was as `ifdef` so is probably legacy.
    % It upsets the AST generator because `if` is a keyword so we have to handle it separately.
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"-if(something).\n-define(x, 1).\n-else.\n-define(x, 2).\n-endif.">>
        ),
    Expect0 = <<"-if(something).\n-define(x, 1).\n-else.\n-define(x, 2).\n-endif.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Case
%%

basic_case_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} =
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
    {ok, Tokens} =
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

case_pattern_match_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> case bar() of ok -> ok; {error, _} = Err -> Err end.">>),
    Expect =
        <<
            "foo() ->\n    case bar() of\n        ok -> ok;\n        {error, _} = Err -> Err\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect, Result0), ?_assertEqual(Expect, Result1)].

case_fun_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<
                "foo(X) -> case X of 0 -> fun baz/2; 1 -> fun other:bee/2; 2 -> fun ?MODULE:bop/2 end."
            >>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    case X of\n        0 -> fun baz/2;\n        1 -> fun other:bee/2;\n        2 -> fun ?MODULE:bop/2\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo(X) ->\n    case X of\n        0 -> fun baz/2;\n        1 -> fun other:bee/2;\n        2 -> fun ?MODULE:bop/2\n    end.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

case_fun_macro_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> case X of 1 -> fun ?BAR:bar/2 end.">>),
    Expect0 = <<"foo() -> case X of 1 -> fun ?BAR:bar/2 end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    case X of\n        1 ->\n            fun ?BAR:bar/2\n    end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

case_fun_var_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> case X of 1 -> fun X:bar/2 end.">>),
    Expect0 = <<"foo() -> case X of 1 -> fun X:bar/2 end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    case X of\n        1 ->\n            fun X:bar/2\n    end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

case_fun_var_2_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> case X of 1 -> fun X() -> ok, X() end end.">>),
    Expect0 =
        <<
            "foo() ->\n    case X of\n        1 ->\n            fun\n                X() ->\n                    ok,\n                    X()\n            end\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% If
%%

if_test_() ->
    {ok, Tokens0} =
        steamroller_ast:tokens(<<"foo(A, B) -> if A == B -> great; true -> oh_no end.">>),
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
    {ok, Tokens1} =
        steamroller_ast:tokens(<<"foo(A) -> if length(A) > 1 -> great; true -> oh_no end.">>),
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
    {ok, Tokens} =
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
    {ok, Tokens} =
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

%%
%% Fun
%%

anon_function_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> fun (test) -> ok; (_) -> error end.">>),
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

nested_fun_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo(X) -> fun() -> case X of 1 -> ok; 0 -> error end end.">>),
    Expect0 =
        <<
            "foo(X) ->\n    fun\n        () ->\n            case X of\n                1 -> ok;\n                0 -> error\n            end\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

dynamic_function_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foooooooooo(X) -> lists:any(fun local/1, X).">>),
    Expect0 = <<"foooooooooo(X) -> lists:any(fun local/1, X).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 = <<"foooooooooo(X) ->\n    lists:any(fun local/1, X).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 30),
    Expect2 = <<"foooooooooo(X) ->\n    lists:any(\n        fun local/1,\n        X\n    ).\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens0, 20),
    {ok, Tokens1} =
        steamroller_ast:tokens(<<"foooooooooo(X) -> lists:any(fun module:func/1, X).">>),
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

variable_arity_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo(M, F, A) -> fun M:F/A.">>),
    Expect0 = <<"foo(M, F, A) -> fun M:F/A.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Guard
%%

when_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X) when is_atom(X), X == test -> ok.">>),
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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X) when X == test; X == other -> ok.">>),
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

guarded_case_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} =
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

if_guard_sequence_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} =
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
    {ok, Tokens} =
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

%%
%% Begin
%%

begin_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"-define(macro(Thing),\n begin\n (Thing + 1)\n end).">>),
    Expect0 = <<"-define(macro(Thing), begin (Thing + 1) end).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"-define(\n    macro(Thing),\n    begin\n        (Thing + 1)\n    end\n).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

begin_list_comprehension_test_() ->
    {ok, Tokens} =
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

begin_case_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo(X) -> begin case X of test -> ok; other -> error end end.">>),
    Expect0 =
        <<
            "foo(X) ->\n    begin\n        case X of\n            test -> ok;\n            other -> error\n        end\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Send/Receive
%%

send_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(Pid) -> Pid ! message.">>),
    Expect0 = <<"foo(Pid) -> Pid ! message.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(Pid) ->\n    Pid ! message.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 20),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

receive_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> receive\n  after X -> timeout\n end.">>),
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
    {ok, Tokens} =
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

%%
%% Try/Catch
%%

try_catch_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> try bar(), baz() catch Y -> {caught, Y} end">>),
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

catch_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> catch bar(), catch baz(), 5 + 5.">>),
    Expect0 = <<"foo() ->\n    catch bar(),\n    catch baz(),\n    5 + 5.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

try_internal_catch_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> try X = (catch bar()), 2 div 0 after baz() end">>),
    Expect0 =
        <<
            "foo() ->\n    try\n        X = (catch bar()),\n        2 div 0\n    after\n        baz()\n    end\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

try_of_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> try bar() of X -> {ok, X} catch Y -> {caught, Y} end">>),
    Expect0 = <<"foo() -> try bar() of X -> {ok, X} catch Y -> {caught, Y} end\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    try bar() of\n        X -> {ok, X}\n    catch\n        Y -> {caught, Y}\n    end\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

try_of_multiexpression_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"foo() -> try true = bar(), baz() of baz -> ok catch Err -> Err end.">>
        ),
    Expect0 = <<"foo() -> try true = bar(), baz() of baz -> ok catch Err -> Err end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo() ->\n    try true = bar(), baz() of\n        baz -> ok\n    catch\n        Err -> Err\n    end.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

try_case_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<
                "foo() -> try case bar() of test -> ok; other -> error end catch _:Y -> throw(Y) end."
            >>
        ),
    Expect0 =
        <<
            "foo() ->\n    try\n        case bar() of\n            test -> ok;\n            other -> error\n        end\n    catch\n        _:Y -> throw(Y)\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

nested_try_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"foo() -> try\n try bar(), baz() catch X -> raise(X) end\ncatch Y -> oh_no\nend.">>
        ),
    Expect0 =
        <<
            "foo() ->\n    try\n        try\n            bar(),\n            baz()\n        catch\n            X -> raise(X)\n        end\n    catch\n        Y -> oh_no\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

try_after_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> try\n bar()\nafter\n baz(), ok\nend">>),
    Expect0 =
        <<"foo() ->\n    try\n        bar()\n    after\n        baz(),\n        ok\n    end\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

try_of_after_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"foo() -> try bar() of ok -> ok; error -> oh_no\nafter\n baz(), ok\nend">>
        ),
    Expect0 =
        <<
            "foo() ->\n    try bar() of\n        ok -> ok;\n        error -> oh_no\n    after\n        baz(),\n        ok\n    end\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

try_receive_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<
                "foo() -> try receive ok -> ok; oh_no -> throw(oh_no) after 0 -> ok end catch _ -> ok end"
            >>
        ),
    Expect0 =
        <<
            "foo() ->\n    try\n        receive\n            ok -> ok;\n            oh_no -> throw(oh_no)\n        after\n            0 -> ok\n        end\n    catch\n        _ -> ok\n    end\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

after_case_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> receive after case {x} of {x} -> 0 end -> ok end.">>),
    Expect0 = <<"foo() -> receive after case {x} of {x} -> 0 end -> ok end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

try_fun_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"foo(X) -> try\n Y = bar(fun () -> X:do() end), Y()\n after ok = baz() end.">>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    try\n        Y = bar(fun () -> X:do() end),\n        Y()\n    after\n        ok = baz()\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Macro
%%

macro_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> ?MACRO.">>),
    Expect0 = <<"foo() -> ?MACRO.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo() ->\n    ?MACRO.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 10),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

macro_args_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(SomeVariable) -> ?MACRO(SomeVariable).">>),
    Expect0 = <<"foo(SomeVariable) -> ?MACRO(SomeVariable).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(SomeVariable) ->\n    ?MACRO(SomeVariable).\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

macro_no_comma_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X, Y) -> ?MACRO(X, Y) bar().">>),
    Expect0 = <<"foo(X, Y) -> ?MACRO(X, Y) bar().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_try_catch_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo(X) -> try bar(X) catch ?Macro(X) handle_error(X) end.">>),
    Expect0 = <<"foo(X) -> try bar(X) catch ?Macro(X) handle_error(X) end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "foo(X) ->\n    try\n        bar(X)\n    catch\n        ?Macro(X) handle_error(X)\n    end.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 50),
    % FIXME This is not perfect but shouldn't happen often
    % There should be an indent after the macro.
    Expect2 =
        <<
            "foo(X) ->\n    try\n        bar(X)\n    catch\n        ?Macro(X)\n        handle_error(X)\n    end.\n"
        >>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 30),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

macro_case_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X) -> case bar(X) of ?Macro(X) ok end.">>),
    Expect0 = <<"foo(X) -> case bar(X) of ?Macro(X) ok end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 = <<"foo(X) ->\n    case bar(X) of\n        ?Macro(X) ok\n    end.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    % FIXME This is not perfect but shouldn't happen often.
    % There should be an indent after the macro.
    Expect2 = <<"foo(X) ->\n    case bar(X) of\n        ?Macro(X)\n        ok\n    end.\n">>,
    Result2 = steamroller_algebra:format_tokens(Tokens, 19),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect1, Result1),
        ?_assertEqual(Expect2, Result2)
    ].

top_level_macro_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"?MACRO(X).">>),
    Expect0 = <<"?MACRO(X).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_export_test_() ->
    % FIXME remove spaces
    {ok, Tokens} = steamroller_ast:tokens(<<"-export([?MACRO/0]).">>),
    Expect0 = <<"-export([?MACRO / 0]).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_multiexpr_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"?MACRO() -> foo(x), ok.">>),
    Expect0 = <<"?MACRO() -> foo(x), ok.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_module_function_call_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> ?MODULE:bar().">>),
    Expect0 = <<"foo() -> ?MODULE:bar().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo() -> ?MODULE:?F().">>),
    Expect1 = <<"foo() -> ?MODULE:?F().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

macro_function_call_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> ?F().">>),
    Expect0 = <<"foo() -> ?F().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_spec_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"-spec ?MODULE:foo() -> bar().">>),
    Expect0 = <<"-spec ?MODULE:foo() -> bar().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"-spec ?MODULE:?F() -> bar().">>),
    Expect1 = <<"-spec ?MODULE:?F() -> bar().\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

macro_record_spec_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"-spec foo(#?MODULE{}) -> {ok, #?MODULE{}}.">>),
    Expect0 = <<"-spec foo(#?MODULE{}) -> {ok, #?MODULE{}}.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_record_element_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo(X) -> X#?MODULE.x.">>),
    Expect0 = <<"foo(X) -> X#?MODULE.x.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

macro_multiclause_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<
                "foo(X, Y) -> case bar(X) of ?MACRO(Y) when length(Y) >= X -> ok;\n    ?MACRO(_) -> error\nend."
            >>
        ),
    Expect0 =
        <<
            "foo(X, Y) ->\n    case bar(X) of\n         ?MACRO(Y) when length(Y) >= X -> ok;\n         ?MACRO(_) -> error\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% Record
%%

record_test_() ->
    {ok, Tokens} =
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

record_element_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo(X) -> X#rec.id.">>),
    Expect0 = <<"foo(X) -> X#rec.id.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo(X, Id) -> X#rec.Id.">>),
    Expect1 = <<"foo(X, Id) -> X#rec.Id.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

record_key_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> #rec.id.">>),
    Expect0 = <<"foo() -> #rec.id.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} = steamroller_ast:tokens(<<"foo(Id) -> #rec.Id.">>),
    Expect1 = <<"foo(Id) -> #rec.Id.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

record_definition_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"-record(state, {one :: integer(), two=2 :: integer()}).">>),
    Expect0 = <<"-record(state, {one :: integer(), two = 2 :: integer()}).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-record(\n    state,\n    {\n        one :: integer(),\n        two = 2 :: integer()\n    }\n).\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 40),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

record_definition_fun_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"-record(state, {one :: fun(), two=2 :: integer()}).">>),
    Expect0 = <<"-record(state, {one :: fun(), two = 2 :: integer()}).\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    Expect1 =
        <<
            "-record(\n    state,\n    {\n        one :: fun(),\n        two = 2 :: integer()\n    }\n).\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens, 30),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

%%
%% Map
%%

map_test_() ->
    {ok, Tokens} =
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

%%
%% Comment
%%

comment_test_() ->
    Expect0 = <<"% Hello I am a comment and I don't change length\n">>,
    {ok, Tokens0} = steamroller_ast:tokens(Expect0),
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Result1 = steamroller_algebra:format_tokens(Tokens0, 50),
    Result2 = steamroller_algebra:format_tokens(Tokens0, 10),
    Expect1 = <<"% Hello\n% World\n">>,
    {ok, Tokens1} = steamroller_ast:tokens(Expect1),
    Result3 = steamroller_algebra:format_tokens(Tokens1, 100),
    Result4 = steamroller_algebra:format_tokens(Tokens1, 20),
    Result5 = steamroller_algebra:format_tokens(Tokens1, 1),
    {ok, Tokens2} = steamroller_ast:tokens(<<"% Comment   \n">>),
    Expect2 = <<"% Comment\n">>,
    Result6 = steamroller_algebra:format_tokens(Tokens2, 100),
    Result7 = steamroller_algebra:format_tokens(Tokens2, 20),
    Result8 = steamroller_algebra:format_tokens(Tokens2, 1),
    [
        ?_assertEqual(Expect0, Result0),
        ?_assertEqual(Expect0, Result1),
        ?_assertEqual(Expect0, Result2),
        ?_assertEqual(Expect1, Result3),
        ?_assertEqual(Expect1, Result4),
        ?_assertEqual(Expect1, Result5),
        ?_assertEqual(Expect2, Result6),
        ?_assertEqual(Expect2, Result7),
        ?_assertEqual(Expect2, Result8)
    ].

attribute_inbetween_comment_test_() ->
    {ok, Tokens} =
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
attribute_comment_test_() ->
    {ok, Tokens} =
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

function_comment_test_() ->
    {ok, Tokens} =
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
    {ok, Tokens} =
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

comment_list_test_() ->
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() ->\n    {error,\n % TODO improve\noh_no}.">>),
    Expect0 =
        <<"foo() ->\n    {\n        error,\n        % TODO improve\n        oh_no\n    }.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    Expect1 =
        <<"foo() ->\n    {\n        error,\n        % TODO improve\n        oh_no\n    }.\n">>,
    Result1 = steamroller_algebra:format_tokens(Tokens0, 1),
    {ok, Tokens1} =
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

comment_list_final_element_test_() ->
    % Test that we do not change the order of lists when the final element is commented out.
    % Otherwise we annoy people who temporarily comment things out and autoformat on save.
    {ok, Tokens0} = steamroller_ast:tokens(<<"foo() -> [bar\n %baz\n].">>),
    Expect0 = <<"foo() ->\n    [\n        bar\n        %baz\n    ].\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    [?_assertEqual(Expect0, Result0)].

function_clause_comment_test_() ->
    {ok, Tokens} =
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

commented_case_test_() ->
    {ok, Tokens} =
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

empty_receive_comment_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(<<"foo() -> receive\n  % comment\n  after X -> timeout\n end.">>),
    Expect0 =
        <<"foo() ->\n    receive\n        % comment\n    after\n        X -> timeout\n    end.\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

comment_sadness_test_() ->
    {ok, Tokens} = steamroller_ast:tokens(<<"foo() -> bar()\n% why\n% WHY\n, baz().">>),
    Expect0 = <<"foo() ->\n    % why\n    % WHY\n    bar(),\n    baz().\n">>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

case_comment_sadness_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"foo(X) -> case X of hello -> world\n% A very special comment\nend.">>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    case X of\n        hello -> world\n        % A very special comment\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

case_arg_comment_test_() ->
    {ok, Tokens0} =
        steamroller_ast:tokens(<<"foo(X) -> case % Comment\n X of hello -> world end.">>),
    Expect0 =
        <<
            "foo(X) ->\n    case\n        % Comment\n        X\n    of\n        hello -> world\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens0, 100),
    {ok, Tokens1} =
        steamroller_ast:tokens(<<"foo(X) -> case X\n  % Comment\n of hello -> world end.">>),
    Expect1 =
        <<
            "foo(X) ->\n    case\n        X\n        % Comment\n    of\n        hello -> world\n    end.\n"
        >>,
    Result1 = steamroller_algebra:format_tokens(Tokens1, 100),
    [?_assertEqual(Expect0, Result0), ?_assertEqual(Expect1, Result1)].

try_comment_sadness_test_() ->
    {ok, Tokens} =
        steamroller_ast:tokens(
            <<"foo(X) -> try bar(X)\n% Please no more\ncatch _ -> error\n% Oh no not again\nend.">>
        ),
    Expect0 =
        <<
            "foo(X) ->\n    try\n        bar(X)\n        % Please no more\n    catch\n        _ -> error\n        % Oh no not again\n    end.\n"
        >>,
    Result0 = steamroller_algebra:format_tokens(Tokens, 100),
    [?_assertEqual(Expect0, Result0)].

%%
%% From the paper
%%

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
