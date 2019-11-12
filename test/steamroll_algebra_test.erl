-module(steamroll_algebra_test).

-include_lib("eunit/include/eunit.hrl").

-define(sp, <<" ">>).
-define(nl, <<"\n">>).

-define(indent, 2).

sanity_test() ->
    ?assert(<<"if a == b then a << 2 else a + b\n">> == steamroll_algebra:from_the_paper(100, ?indent)),
    ?assert(<<"if a == b then a << 2 else a + b\n">> == steamroll_algebra:from_the_paper(32, ?indent)),
    ?assert(<<"if a == b\nthen a << 2\nelse a + b\n">> == steamroll_algebra:from_the_paper(15, ?indent)),
    ?assert(<<"if a == b\nthen\n  a << 2\nelse a + b\n">> == steamroll_algebra:from_the_paper(10, ?indent)),
    ?assert(<<"if\n  a == b\nthen\n  a << 2\nelse\n  a + b\n">> == steamroll_algebra:from_the_paper(8, ?indent)),
    ?assert(<<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a + b\n">> == steamroll_algebra:from_the_paper(7, ?indent)),
    ?assert(<<"if\n  a ==\n    b\nthen\n  a <<\n    2\nelse\n  a +\n    b\n">> == steamroll_algebra:from_the_paper(6, ?indent)).

repeat_test() ->
    ?assert(<<>> == steamroll_algebra:repeat(?sp, 0)),
    ?assert(<<" ">> == steamroll_algebra:repeat(?sp, 1)),
    ?assert(<<"  ">> == steamroll_algebra:repeat(?sp, 2)),
    ?assert(<<"   ">> == steamroll_algebra:repeat(?sp, 3)),
    ?assert(<<"    ">> == steamroll_algebra:repeat(?sp, 4)),
    ?assert(<<"\n">> == steamroll_algebra:repeat(?nl, 1)),
    ?assert(<<"\n\n">> == steamroll_algebra:repeat(?nl, 2)),
    ?assert(<<"\n\n\n">> == steamroll_algebra:repeat(?nl, 3)),
    ?assert(<<"\n\n\n\n">> == steamroll_algebra:repeat(?nl, 4)).

basic_brackets_test() ->
    Tokens = steamroll_ast:tokens(<<"foo(Arg1, Arg2)">>),
    Expect0 = <<"foo(Arg1, Arg2)\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 100),
    io:fwrite("\nResult0=~p", [Result0]),
    ?assert(Expect0 == Result0),
    Expect1 = <<"foo(\n    Arg1,\n    Arg2\n)\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 1),
    io:fwrite("\nResult1=~p", [Result1]),
    ?assert(Expect1 == Result1).

basic_function_test() ->
    Tokens = steamroll_ast:tokens(<<"foo(Arg1, Arg2) -> ok.">>),
    Expect0 = <<"foo(Arg1, Arg2) -> ok.\n">>,
    Result0 = steamroll_algebra:format_tokens(Tokens, 100),
    io:fwrite("\nResult0=~p", [Result0]),
    ?assert(Expect0 == Result0),
    Expect1 = <<"foo(Arg1, Arg2) ->\nok.\n">>,
    Result1 = steamroll_algebra:format_tokens(Tokens, 20),
    io:fwrite("\nResult1=~p", [Result1]),
    ?assert(Expect1 == Result1),
    Expect2 = <<"foo(\n    Arg1,\n    Arg2\n) ->\nok.\n">>,
    Result2 = steamroll_algebra:format_tokens(Tokens, 1),
    io:fwrite("\nResult2=~p", [Result2]),
    ?assert(Expect2 == Result2).

