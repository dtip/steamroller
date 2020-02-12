-module(steamroller_ast_test).

-include_lib("eunit/include/eunit.hrl").

ast_test_() ->
  {ok, Expect} = steamroller_ast:ast(<<"-module(test).\n\n-export([init/1]).\n">>),
  {ok, Result1} = steamroller_ast:ast(<<"-module(test).\n\n-export([init/1]).\n\n">>),
  {ok, Result2} = steamroller_ast:ast(<<"-module(test).\n\n%% Comment\n-export([init/1]).\n\n">>),
  {ok, Result3} = steamroller_ast:ast(<<"-module(test).\n\n% Comment\n-export([init/1]).\n\n">>),
  {ok, Result4} = steamroller_ast:ast(<<"-module(test).\n\n%%% Comment\n-export([init/1]).\n\n">>),
  {ok, Result5} =
    steamroller_ast:ast(<<"-module(test).\n\n%% Comment %%\n-export([init/1]).\n\n">>),
  {ok, Result6} =
    steamroller_ast:ast(<<"-module(test).\n\n%% === Comment === %%\n-export([init/1]).\n\n">>),
  {ok, Result7} =
    steamroller_ast:ast(<<"-module(test).\n\n%% Comment\n-export([init/1]).\n%% Comment\n">>),
  {ok, Result8} =
    steamroller_ast:ast(<<"-module(test).\n\n-define(MACRO, macro).\n-export([init/1]).\n\n">>),
  {ok, Result9} = steamroller_ast:ast(<<"-module(test).\n\n-export([init/1]).">>),
  [
    ?_assert(steamroller_ast:eq(Expect, Result1)),
    ?_assert(steamroller_ast:eq(Expect, Result2)),
    ?_assert(steamroller_ast:eq(Expect, Result3)),
    ?_assert(steamroller_ast:eq(Expect, Result4)),
    ?_assert(steamroller_ast:eq(Expect, Result5)),
    ?_assert(steamroller_ast:eq(Expect, Result6)),
    ?_assert(steamroller_ast:eq(Expect, Result7)),
    ?_assert(steamroller_ast:eq(Expect, Result8)),
    ?_assert(steamroller_ast:eq(Expect, Result9))
  ].

comment_tokens_test_() ->
  [
    ?_assertEqual({ok, [{comment, 0, "% Comment"}]}, steamroller_ast:tokens(<<"% Comment\n">>)),
    ?_assertEqual({ok, [{comment, 0, "% Comment"}]}, steamroller_ast:tokens(<<"% Comment">>)),
    ?_assertEqual(
      {ok, [{comment, 0, "% Comment"}, {comment, 1, "%% API"}]},
      steamroller_ast:tokens(<<"% Comment\n%% API\n">>)
    ),
    ?_assertEqual(
      {ok, [{comment, 0, "% Comment"}, {comment, 1, "%% API"}]},
      steamroller_ast:tokens(<<"% Comment\n%% API">>)
    )
  ].

module_attribute_tokens_test_() ->
  [
    ?_assertEqual(
      {ok, [{'-', 0}, {atom, 0, module}, {'(', 0}, {atom, 0, test}, {')', 0}, {dot, 0}]},
      steamroller_ast:tokens(<<"-module(test).\n">>)
    ),
    ?_assertEqual(
      {
        ok,
        [
          {'-', 0},
          {atom, 0, include_lib},
          {'(', 0},
          {string, 0, "eunit/include/eunit.hrl"},
          {')', 0},
          {dot, 0}
        ]
      },
      steamroller_ast:tokens(<<"-include_lib(\"eunit/include/eunit.hrl\").\n">>)
    ),
    ?_assertEqual(
      {
        ok,
        [
          {'-', 0},
          {atom, 0, define},
          {'(', 0},
          {var, 0, 'MACRO'},
          {',', 0},
          {atom, 0, some_atom},
          {')', 0},
          {dot, 0}
        ]
      },
      steamroller_ast:tokens(<<"-define(MACRO, some_atom).\n">>)
    ),
    ?_assertEqual(
      {
        ok,
        [
          {'-', 0},
          {atom, 0, define},
          {'(', 0},
          {var, 0, 'MACRO'},
          {',', 0},
          {string, 0, "some_string"},
          {')', 0},
          {dot, 0}
        ]
      },
      steamroller_ast:tokens(<<"-define(MACRO, \"some_string\").\n">>)
    )
  ].

function_tokens_test_() ->
  [
    ?_assertEqual(
      {
        ok,
        [
          {atom, 0, foo},
          {'(', 0},
          {var, 0, 'Arg1'},
          {',', 0},
          {var, 0, 'Arg1'},
          {')', 0},
          {'->', 0},
          {atom, 0, error},
          {';', 0},
          {atom, 0, foo},
          {'(', 0},
          {var, 0, 'Arg1'},
          {',', 0},
          {var, 0, 'Arg2'},
          {')', 0},
          {'->', 0},
          {atom, 0, ok},
          {dot, 0}
        ]
      },
      steamroller_ast:tokens(<<"foo(Arg1, Arg1) -> error; foo(Arg1, Arg2) -> ok.">>)
    )
  ].
