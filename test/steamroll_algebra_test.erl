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
