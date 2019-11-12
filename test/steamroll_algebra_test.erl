-module(steamroll_algebra_test).

-include_lib("eunit/include/eunit.hrl").

-define(sp, <<" ">>).
-define(nl, <<"\n">>).

-define(indent, 2).

sanity_test() ->
    ?assertEqual(<<"if a == b then a << 2 else a + b\n">>, steamroll_algebra:from_the_paper(100, ?indent)),
    ?assertEqual(<<"if a == b then a << 2 else a + b\n">>, steamroll_algebra:from_the_paper(32, ?indent)),
    ?assertEqual(<<"if a == b\nthen a << 2\nelse a + b\n">>, steamroll_algebra:from_the_paper(15, ?indent)),
    ?assertEqual(<<"if a == b\nthen\n  a << 2\nelse a + b\n">>, steamroll_algebra:from_the_paper(10, ?indent)),
    ?assertEqual(<<"if\n  a == b\nthen\n  a << 2\nelse\n  a + b\n">>, steamroll_algebra:from_the_paper(8, ?indent)),
    ?assertEqual(<<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a + b\n">>, steamroll_algebra:from_the_paper(7, ?indent)),
    ?assertEqual(<<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a +\n    b\n">>, steamroll_algebra:from_the_paper(6, ?indent)).

repeat_test() ->
    ?assertEqual(<<>>, steamroll_algebra:repeat(?sp, 0)),
    ?assertEqual(<<" ">>, steamroll_algebra:repeat(?sp, 1)),
    ?assertEqual(<<"  ">>, steamroll_algebra:repeat(?sp, 2)),
    ?assertEqual(<<"   ">>, steamroll_algebra:repeat(?sp, 3)),
    ?assertEqual(<<"    ">>, steamroll_algebra:repeat(?sp, 4)),
    ?assertEqual(<<"\n">>, steamroll_algebra:repeat(?nl, 1)),
    ?assertEqual(<<"\n\n">>, steamroll_algebra:repeat(?nl, 2)),
    ?assertEqual(<<"\n\n\n">>, steamroll_algebra:repeat(?nl, 3)),
    ?assertEqual(<<"\n\n\n\n">>, steamroll_algebra:repeat(?nl, 4)).

basic_brackets_test() ->
    Tokens = steamroll_ast:tokens(<<"foo(Arg1, Arg2)">>),
    Expect0 = <<"foo(Arg1, Arg2)\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 100),
    ?assertEqual(Expect0, Result0),
    Expect1 = <<"foo(\n    Arg1,\n    Arg2\n)\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 1),
    ?assertEqual(Expect1, Result1).

basic_function_test() ->
    Tokens = steamroll_ast:tokens(<<"foo(Arg1, Arg2) -> ok.">>),
    Expect0 = <<"foo(Arg1, Arg2) -> ok.\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 100),
    ?assertEqual(Expect0, Result0),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    ok.\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 20),
    ?assertEqual(Expect1, Result1),
    Expect2 = <<"foo(\n    Arg1,\n    Arg2\n) ->\n    ok.\n">>,
    Result2 = steamroll_algebra:format_tokens(Tokens, 1),
    ?assertEqual(Expect2, Result2).

function_test() ->
    Tokens = steamroll_ast:tokens(<<"foo(Arg1, Arg2) -> Arg3 = Arg1 + Arg2, Arg3.">>),
    Expect0 = <<"foo(Arg1, Arg2) ->\n    Arg3 = Arg1 + Arg2,\n    Arg3.\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 30),
    ?assertEqual(Expect0, Result0),
    Expect1 = <<"foo(Arg1, Arg2) ->\n    Arg3 =\n        Arg1 + Arg2,\n    Arg3.\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 20),
    ?assertEqual(Expect1, Result1).

function_clause_test() ->
    Tokens = steamroll_ast:tokens(<<"foo(Arg1, Arg1) -> error; foo(Arg1, Arg2) -> ok.">>),
    Expect0 = <<"foo(Arg1, Arg1) -> error;\nfoo(Arg1, Arg2) -> ok.\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 100),
    ?assertEqual(Expect0, Result0),
    Expect1 = <<"foo(Arg1, Arg1) ->\n    error;\nfoo(Arg1, Arg2) ->\n    ok.\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 20),
    ?assertEqual(Expect1, Result1),
    Expect2 = <<"foo(\n    Arg1,\n    Arg1\n) ->\n    error;\nfoo(\n    Arg1,\n    Arg2\n) ->\n    ok.\n">>,
    Result2 = steamroll_algebra:format_tokens(Tokens, 1),
    ?assertEqual(Expect2, Result2).

basic_attribute_test() ->
    Tokens = steamroll_ast:tokens(<<"-module(test).">>),
    Expect0 = <<"-module(test).\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 100),
    ?assertEqual(Expect0, Result0),
    Expect1 = <<"-module(\n    test\n).\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 1),
    ?assertEqual(Expect1, Result1).

