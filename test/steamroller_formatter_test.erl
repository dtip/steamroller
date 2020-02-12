-module(steamroller_formatter_test).

-include_lib("eunit/include/eunit.hrl").

-define(FILE_DIR, "./test/steamroller_formatter/").

basic_boilerplate_test_() ->
  Expect = {ok, <<"-module(test).\n\n-export([init/1]).\n">>},
  [
    ?_assertEqual(
      Expect,
      steamroller_formatter:test_format(<<"-module(test).\n\n-export([init/1]).\n">>)
    ),
    ?_assertEqual(
      Expect,
      steamroller_formatter:test_format(<<"-module(test).\n\n\n\n-export([init/1]).\n">>)
    ),
    ?_assertEqual(
      Expect,
      steamroller_formatter:test_format(<<"-module(test).\n\n-export([init/1]).">>)
    )
  ].

function_test_() ->
  Expect = {ok, <<"-module(test).\n\n-export([run/1]).\n\nrun(foo) -> ok;\nrun(bar) -> error.\n">>},
  [
    ?_assertEqual(
      Expect,
      steamroller_formatter:test_format(
        <<"-module(test).\n\n-export([run/1]).\nrun(foo) -> ok;\nrun(bar) -> error.\n">>
      )
    )
  ].

define_test_() ->
  Expect = {ok, <<"-module(test).\n\n-define(SOMETHING, some_atom).\n">>},
  [
    ?_assertEqual(
      Expect,
      steamroller_formatter:test_format(<<"-module(test).\n\n-define(SOMETHING, some_atom).\n">>)
    )
  ].

simple_module_test_() ->
  Expect = {ok, Correct} = file:read_file(?FILE_DIR ++ "simple_module/correct.sterl"),
  {ok, NotEnoughWhitespace} =
    file:read_file(?FILE_DIR ++ "simple_module/not_enough_whitespace.sterl"),
  {ok, TooMuchWhitespace} = file:read_file(?FILE_DIR ++ "simple_module/too_much_whitespace.sterl"),
  [
    ?_assertEqual(Expect, steamroller_formatter:test_format(Correct)),
    ?_assertEqual(Expect, steamroller_formatter:test_format(NotEnoughWhitespace)),
    ?_assertEqual(Expect, steamroller_formatter:test_format(TooMuchWhitespace))
  ].

specced_module_test_() ->
  Expect = {ok, Correct} = file:read_file(?FILE_DIR ++ "specced_module/correct.sterl"),
  {ok, Grim1} = file:read_file(?FILE_DIR ++ "specced_module/grim1.sterl"),
  {ok, Grim2} = file:read_file(?FILE_DIR ++ "specced_module/grim2.sterl"),
  [
    ?_assertEqual(Expect, steamroller_formatter:test_format(Correct)),
    ?_assertEqual(Expect, steamroller_formatter:test_format(Grim1)),
    ?_assertEqual(Expect, steamroller_formatter:test_format(Grim2))
  ].

commented_module_test_() ->
  Expect = {ok, Correct} = file:read_file(?FILE_DIR ++ "commented_module/correct.sterl"),
  {ok, Grim1} = file:read_file(?FILE_DIR ++ "commented_module/grim1.sterl"),
  [
    ?_assertEqual(Expect, steamroller_formatter:test_format(Correct)),
    ?_assertEqual(Expect, steamroller_formatter:test_format(Grim1))
  ].

unicode_test_() ->
  Expect = {ok, Correct} = file:read_file(?FILE_DIR ++ "unicode/correct.sterl"),
  [?_assertEqual(Expect, steamroller_formatter:test_format(Correct))].
