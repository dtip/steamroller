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

comment_tokens_test() ->
    [{comment, 0, "% Comment"}] = steamroll_ast:tokens(<<"% Comment\n">>),
    [{comment, 0, "% Comment"}] = steamroll_ast:tokens(<<"% Comment">>),
    [{comment, 0, "% Comment"},
     {comment, 1, "%% API"}] = steamroll_ast:tokens(<<"% Comment\n%% API\n">>),
    [{comment, 0, "% Comment"},
     {comment, 1, "%% API"}] = steamroll_ast:tokens(<<"% Comment\n%% API">>).

module_attribute_tokens_test() ->
     [{'-',0}, {atom,0,module}, {'(',0}, {atom,0,test}, {')',0}, {dot,0}] = steamroll_ast:tokens(<<"-module(test).\n">>),
     [{'-',0}, {atom,0,include_lib}, {'(',0}, {string,0,"eunit/include/eunit.hrl"}, {')',0}, {dot,0}] = steamroll_ast:tokens(<<"-include_lib(\"eunit/include/eunit.hrl\").\n">>),
     [{'-',0}, {atom,0,define}, {'(',0}, {var,0,'MACRO'}, {',',0}, {atom,0,some_atom}, {')',0}, {dot,0}] = steamroll_ast:tokens(<<"-define(MACRO, some_atom).\n">>),
     [{'-',0}, {atom,0,define}, {'(',0}, {var,0,'MACRO'}, {',',0}, {string,0,"some_string"}, {')',0}, {dot,0}] = steamroll_ast:tokens(<<"-define(MACRO, \"some_string\").\n">>).

function_tokens_test() ->
    [{atom,0,foo},
     {'(',0},
     {var,0,'Arg1'},
     {',',0},
     {var,0,'Arg1'},
     {')',0},
     {'->',0},
     {atom,0,error},
     {';',0},
     {atom,0,foo},
     {'(',0},
     {var,0,'Arg1'},
     {',',0},
     {var,0,'Arg2'},
     {')',0},
     {'->',0},
     {atom,0,ok},
     {dot,0}] = steamroll_ast:tokens(<<"foo(Arg1, Arg1) -> error; foo(Arg1, Arg2) -> ok.">>).


