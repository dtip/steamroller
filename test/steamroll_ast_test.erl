-module(steamroll_ast_test).

-include_lib("eunit/include/eunit.hrl").

ast_test() ->
    Ast = steamroll_ast:ast(<<"-module(test).\n\n-export([init/1]).\n">>),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n%% Comment\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n% Comment\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n%%% Comment\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n%% Comment %%\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n%% === Comment === %%\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n%% Comment\n-export([init/1]).\n%% Comment\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n-define(MACRO, macro).\n-export([init/1]).\n\n">>))),
    ?assert(steamroll_ast:eq(Ast, steamroll_ast:ast(<<"-module(test).\n\n-export([init/1]).">>))).
