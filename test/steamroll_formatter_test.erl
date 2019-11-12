-module(steamroll_formatter_test).

-include_lib("eunit/include/eunit.hrl").

basic_boilerplate_test_() ->
    Expect = {ok, <<"-module(test).\n\n-export([init/1]).\n">>},
    [
     ?_assertEqual(Expect, steamroll_formatter:format_code(<<"-module(test).\n\n-export([init/1]).\n">>)),
     ?_assertEqual(Expect, steamroll_formatter:format_code(<<"-module(test).\n\n\n\n-export([init/1]).\n">>)),
     ?_assertEqual(Expect, steamroll_formatter:format_code(<<"-module(test).\n\n-export([init/1]).">>))
    ].

function_test_() ->
    Expect = {ok, <<"-module(test).\n\n-export([run/1]).\nrun(foo) -> ok;\nrun(bar) -> error.\n">>},
    [
     ?_assertEqual(Expect, steamroll_formatter:format_code(<<"-module(test).\n\n-export([run/1]).\nrun(foo) -> ok;\nrun(bar) -> error.\n">>))
    ].

define_test_() ->
    Expect = {ok, <<"-module(test).\n\n-define(SOMETHING, some_atom).\n">>},
    [
     ?_assertEqual(Expect, steamroll_formatter:format_code(<<"-module(test).\n\n-define(SOMETHING, some_atom).\n">>))
    ].
