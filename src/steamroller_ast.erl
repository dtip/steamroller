-module(steamroller_ast).

-export([ast/1, ast/4, tokens/1, eq/2]).

-type ast() :: list(erl_parse:abstract_form()).
-type token() :: erl_scan:token().
-type tokens() :: list(token()).
-type macros() :: list(atom() | {atom(), term()}).

-export_type([token/0, tokens/0, macros/0]).

%% API

-spec ast(binary()) -> {ok, ast()} | {error, any()}.
ast(Code) -> ast(Code, <<"no_file">>, [], []).

-spec ast(binary(), binary() | string(), list(file:name_all()), macros()) ->
  {ok, ast()} | {error, any()}.
ast(Code, File, Includes, Macros) ->
  TempFile = temp_file(File),
  file:write_file(TempFile, Code),
  % Add the file dir to includes so we can find necessary headers.
  Dir = filename:dirname(File),
  {ok, Ast} = epp:parse_file(TempFile, [{includes, [Dir | Includes]}, {macros, Macros}]),
  file:delete(TempFile),
  case check_for_errors(Ast, File) of
    ok -> {ok, Ast};
    {error, _} = Err -> Err
  end.

-spec tokens(binary()) -> {ok, tokens()} | {error, binary()}.
tokens(Code) ->
  case unicode:characters_to_list(Code) of
    {error, _, _} -> {error, <<"source code is not unicode">>};

    Unicode ->
      % Comments are not included in the AST created by epp:parse_file.
      % Neither are most of the attributes (things like `-define(BLAH, blah).`).
      % We'll need them to generate formatted code.
      {ok, Scanned, _} = erl_scan:string(Unicode, 0, [return_comments]),
      {ok, Scanned}
  end.

-spec eq(ast(), ast()) -> boolean().
eq(Ast0, Ast1) -> lists:foldl(fun eq_/2, true, lists:zip(Ast0, Ast1)).

%% Internal

% Second argument of the tuples is a line number which we want to ignore.
eq_(_, false) -> false;
eq_({Left, Right}, true) when is_list(Left) andalso is_list(Right) -> eq(Left, Right);
eq_({Element, Element}, true) -> true;

eq_(
  {
    {Type, _, LeftName, LeftValue0, LeftValue1, LeftValue2},
    {Type, _, RightName, RightValue0, RightValue1, RightValue2}
  },
  true
) ->
  % 6-tuples
  R1 = eq_({LeftName, RightName}, true),
  R2 = eq_({LeftValue0, RightValue0}, R1),
  R3 = eq_({LeftValue1, RightValue1}, R2),
  eq_({LeftValue2, RightValue2}, R3);

eq_(
  {{Type, _, LeftName, LeftValue0, LeftValue1}, {Type, _, RightName, RightValue0, RightValue1}},
  true
) ->
  % 5-tuples
  R1 = eq_({LeftName, RightName}, true),
  R2 = eq_({LeftValue0, RightValue0}, R1),
  eq_({LeftValue1, RightValue1}, R2);

eq_({{Type, _, LeftName, LeftValue}, {Type, _, RightName, RightValue}}, true) ->
  % 4-tuples
  R1 = eq_({LeftName, RightName}, true),
  eq_({LeftValue, RightValue}, R1);

eq_({{integer, LeftLine, LeftLine}, {integer, RightLine, RightLine}}, true) ->
  % Not exactly sure what these are but we occasionally get them.
  true;

eq_({{Type, _LeftLine, LeftValue}, {Type, _RightLine, RightValue}}, true) ->
  % 3-tuples
  eq_({LeftValue, RightValue}, true);

eq_({{Type, _}, {Type, _}}, true) ->
  % 2-tuples
  true;

eq_({{{Type, _}, LeftValue}, {{Type, _}, RightValue}}, true) ->
  % special 2-tuples
  eq_(LeftValue, RightValue);

eq_({_Left, _Right}, _) ->
  io:format("[error] ast_mismatch\nLeft= ~p\nRight=~p~n", [_Left, _Right]),
  false.

check_for_errors([], _) -> ok;
check_for_errors([{error, Msg} | _], File) -> {error, {File, Msg}};
check_for_errors([_ | Rest], File) -> check_for_errors(Rest, File).

temp_file(File) ->
  Temp = "steamroller_temp_" ++ pid_to_list(self()) ++ "_" ++ binary_to_list(File),
  lists:map(
    fun
      (X)
      when X >= $a andalso X =< $z;
           X >= $A andalso X =< $Z;
           X >= $0 andalso X =< $9;
           X =:= $_;
           X =:= $-;
           X =:= $. ->
        X;
      (_) -> $_
    end,
    Temp
  ).
