-module(steamroller_formatter_test).

-include_lib("eunit/include/eunit.hrl").

-define(RES_DIR, "./test/steamroller_formatter/resources/").

basic_boilerplate_test_() ->
    Expect = {ok, <<"-module(test).\n\n-export([init/1]).\n">>},
    [
     ?_assertEqual(Expect, steamroller_formatter:format_code(<<"-module(test).\n\n-export([init/1]).\n">>)),
     ?_assertEqual(Expect, steamroller_formatter:format_code(<<"-module(test).\n\n\n\n-export([init/1]).\n">>)),
     ?_assertEqual(Expect, steamroller_formatter:format_code(<<"-module(test).\n\n-export([init/1]).">>))
    ].

function_test_() ->
    Expect = {ok, <<"-module(test).\n\n-export([run/1]).\n\nrun(foo) -> ok;\nrun(bar) -> error.\n">>},
    [
     ?_assertEqual(Expect, steamroller_formatter:format_code(<<"-module(test).\n\n-export([run/1]).\nrun(foo) -> ok;\nrun(bar) -> error.\n">>))
    ].

define_test_() ->
    Expect = {ok, <<"-module(test).\n\n-define(SOMETHING, some_atom).\n">>},
    [
     ?_assertEqual(Expect, steamroller_formatter:format_code(<<"-module(test).\n\n-define(SOMETHING, some_atom).\n">>))
    ].

simple_module_test_() ->
    Expect = {ok, Correct} = file:read_file(?RES_DIR ++ "simple_module/correct.sterl"),
    {ok, NotEnoughWhitespace} = file:read_file(?RES_DIR ++ "simple_module/not_enough_whitespace.sterl"),
    {ok, TooMuchWhitespace} = file:read_file(?RES_DIR ++ "simple_module/too_much_whitespace.sterl"),
    [
     ?_assertEqual(Expect, steamroller_formatter:format_code(Correct)),
     ?_assertEqual(Expect, steamroller_formatter:format_code(NotEnoughWhitespace)),
     ?_assertEqual(Expect, steamroller_formatter:format_code(TooMuchWhitespace))
    ].

