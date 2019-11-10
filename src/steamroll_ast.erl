-module(steamroll_ast).

-export([ast/1, eq/2]).

-include_lib("kernel/include/logger.hrl").

-define(TEMP_FILE, "steamroll_temp.erl").

-type ast() :: list(erl_parse:abstract_form()).

%% API

-spec ast(binary()) -> ast().
ast(Code0) ->
    file:write_file(?TEMP_FILE, Code0),
    {ok, Ast} = epp:parse_file(?TEMP_FILE, []),
    file:delete(?TEMP_FILE),
    Ast.

-spec eq(ast(), ast()) -> boolean().
eq(Ast0, Ast1) ->
    lists:foldl(fun eq_/2, true, lists:zip(Ast0, Ast1)).

%% Internal

% Second argument of the tuples is a line number which we want to ignore.
eq_(_, false) -> false;
eq_({{Type, _, Name, Value}, {Type, _, Name, Value}}, true) -> true;
eq_({{Type, _, Name, LeftChild}, {Type, _, Name, RightChild}}, true)
  when is_list(LeftChild) andalso is_list(RightChild) -> eq(LeftChild, RightChild);
eq_({{function, _, Name, Arity, LeftChild}, {function, _, Name, Arity, RightChild}}, true) -> eq(LeftChild, RightChild);
eq_({{clause, _, Arg, Guard, Value}, {clause, _, Arg, Guard, Value}}, true) -> true;
eq_({{clause, _, Arg, Guard, LeftChild}, {clause, _, Arg, Guard, RightChild}}, true)
  when is_list(LeftChild) andalso is_list(RightChild) -> eq(LeftChild, RightChild);
eq_({{match, _, {var, _, Var}, {tuple, _, Value}}, {match, _, {var, _, Var}, {tuple, _, Value}}}, true) -> true;
eq_({{match, _, {var, _, Var}, {tuple, _, LeftChild}}, {match, _, {var, _, Var}, {tuple, _, RightChild}}}, true)
  when is_list(LeftChild) andalso is_list(RightChild) -> eq(LeftChild, RightChild);
eq_({{match, _, {var, _, Var}, {call, _, {remote, _, {atom, _, M}, {atom, _, F}}, LeftA}}, {match, _, {var, _, Var}, {call, _, {remote, _, {atom, _, M}, {atom, _, F}}, RightA}}}, true)
  when is_list(LeftA) andalso is_list(RightA) -> eq(LeftA, RightA);
eq_({{Type, _, Value}, {Type, _, Value}}, true) -> true;
eq_({{Type, _, LeftChild}, {Type, _, RightChild}}, true)
  when is_list(LeftChild) andalso is_list(RightChild) -> eq(LeftChild, RightChild);
eq_({{bin_element, _, {string, _, String}, default, default}, {bin_element, _, {string, _, String}, default, default}}, true) -> true;
eq_({{eof, _}, {eof, _}}, true) -> true;
eq_({_Left, _Right}, _) -> logger:error("ast_mismatch Left=~p\nRight=~p", [_Left, _Right]), false.
