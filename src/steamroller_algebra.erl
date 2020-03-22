%%
%% @doc An implementation of "Strictly Pretty" (2000) by Christian Lindig [0].
%%
%% Inspired by the Elixir implementation of the same paper in Inspect.Algebra. Thanks to the core team for their hard work!
%%
%% Includes plenty of Erlang-specific additions.
%%
%% [0] [https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200]
%%

-module(steamroller_algebra).

-export([format_tokens/2]).

% Testing
-export([repeat/2, from_the_paper/2]).

-type doc() :: doc_nil
             | {doc_cons, doc(), doc()}
             | {doc_text, binary()}
             | {doc_nest, integer(), doc()}
             | {doc_underneath, integer(), doc()}
             | {doc_break, binary()}
             | {doc_group, doc(), inherit()}
             | {doc_pad_group, doc(), padding(), inherit()}
             | {doc_force_break, doc()}.
-type sdoc() :: s_nil | {s_text, binary(), sdoc()} | {s_line, binary(), sdoc()}.
-type mode() :: flat | break.
-type padding() :: top | bottom | both.
-type inherit() :: self | inherit.
-type continue() :: continue | done.
-type force_break() :: force_break | no_force_break.
-type token() :: steamroller_ast:token().
-type tokens() :: steamroller_ast:tokens().
-type previous_term() :: new_file
                       | {attribute, atom()}
                       | spec
                       | type
                       | list
                       | function
                       | module_comment
                       | function_comment
                       | expr.

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(two_nl, <<"\n\n">>).
-define(dot, <<".">>).
-define(indent, application:get_env(steamroller, indent, 2)).
-define(IS_LIST_OPEN_CHAR(C), (C == '(' orelse C == '{' orelse C == '[' orelse C == '<<')).
-define(IS_LIST_CLOSE_CHAR(C), (C == ')' orelse C == '}' orelse C == ']' orelse C == '>>')).
-define(IS_EQUALS(C), (C == '=' orelse C == '==' orelse C == '=:=' orelse C == '=/=')).
-define(IS_BOOL_CONCATENATOR(C), (C == 'andalso' orelse C == 'orelse')).
-define(IS_NUMBER(C), (C == integer orelse C == float)).
-define(
  IS_TERMINATED_KEYWORD(C),
  % 'fun' is not always terminated with 'end'.
  % Make sure to handle that separately if you use this macro.
  (
    C == 'case'
    orelse C == 'if'
    orelse C == 'fun'
    orelse C == 'receive'
    orelse C == 'try'
    orelse C == 'begin'
  )
).
-define(IS_OF_KEYWORD(C), (C == 'case' orelse C == 'try')).
-define(IS_VALID_MODULE_TYPE(T), (T == atom orelse T == var)).
-define(IS_VALID_FUNCTION_TYPE(T), (T == atom orelse T == var)).
-define(IS_VALID_ARITY_TYPE(T), (T == integer orelse T == var)).
-define(
  IS_VALID_MFA(M, F, A),
  (?IS_VALID_MODULE_TYPE(M) andalso ?IS_VALID_FUNCTION_TYPE(F) andalso ?IS_VALID_ARITY_TYPE(A))
).
-define(IS_VALID_MF(M, F), (?IS_VALID_MODULE_TYPE(M) andalso ?IS_VALID_FUNCTION_TYPE(F))).
-define(IS_VALID_FA(F, A), (?IS_VALID_FUNCTION_TYPE(F) andalso ?IS_VALID_ARITY_TYPE(A))).
-define(IS_VALID_RECORD_KEY(C), (C == atom orelse C == var)).
-define(IS_VALID_BINARY_SIZE(S), (S == integer orelse S == var)).

%%
%% API
%%

-spec format_tokens(tokens(), integer()) -> binary().
format_tokens(Tokens, Width) ->
  Doc = generate_doc(Tokens),
  pretty(Doc, Width).


% Used for testing.
-spec from_the_paper(integer(), integer()) -> binary().
from_the_paper(Width, Indent) ->
  C = test_binop(<<"a">>, <<"==">>, <<"b">>, Indent),
  E1 = test_binop(<<"a">>, <<"<<">>, <<"2">>, Indent),
  E2 = test_binop(<<"a">>, <<"+">>, <<"b">>, Indent),
  Doc = test_ifthen(C, E1, E2, Indent),
  pretty(Doc, Width).

%%
%% Internal
%%

-spec generate_doc(tokens()) -> doc().
generate_doc(Tokens) -> generate_doc_(Tokens, empty(), new_file).

-spec pretty(doc(), integer()) -> binary().
pretty(Doc, Width) ->
  SDoc = format(Width, 0, [{0, flat, group(Doc)}]),
  String = sdoc_to_string(SDoc),
  <<String/binary, "\n">>.

%%
%% Constructor Functions
%%

-spec cons(doc(), doc()) -> doc().
cons(X, Y) -> {doc_cons, X, Y}.

-spec cons(list(doc())) -> doc().
cons([X]) -> X;
cons([X, Y]) -> cons(X, Y);
cons([X | Rest]) -> cons(X, cons(Rest)).

-spec empty() -> doc().
empty() -> doc_nil.

-spec text(binary()) -> doc().
text(S) -> {doc_text, S}.

-spec nest(integer(), doc()) -> doc().
nest(I, X) -> {doc_nest, I, X}.

-spec underneath(integer(), doc()) -> doc().
underneath(Offset, X) -> {doc_underneath, Offset, X}.

-spec break(binary()) -> doc().
break(S) -> {doc_break, S}.

-spec force_break(force_break(), doc()) -> doc().
force_break(force_break, X) -> {doc_force_break, X};
force_break(no_force_break, X) -> X.

-spec group(doc()) -> doc().
group(D) -> {doc_group, D, self}.

% Group inheritance is lifted from the Elixir algebra implementation.
-spec group(doc(), inherit()) -> doc().
group(doc_nil, _) -> doc_nil;
group(D, Inherit) -> {doc_group, D, Inherit}.

-spec pad_group(doc(), inherit()) -> doc().
pad_group(D, Inherit) -> {doc_pad_group, D, both, Inherit}.

-spec pad_group_top(doc(), inherit()) -> doc().
pad_group_top(D, Inherit) -> {doc_pad_group, D, top, Inherit}.

-spec pad_group_bottom(doc(), inherit()) -> doc().
pad_group_bottom(D, Inherit) -> {doc_pad_group, D, bottom, Inherit}.

%%
%% Operators
%%

-spec space(doc(), doc()) -> doc().
space(X, Y) -> concat(X, Y, ?sp).

-spec space(list(doc())) -> doc().
space([X]) -> X;
space([X, Y]) -> space(X, Y);
space([X | Rest]) -> space(X, space(Rest)).

-spec stick(doc(), doc()) -> doc().
stick(X, Y) -> concat(X, Y, <<>>).

-spec newline(doc(), doc()) -> doc().
newline(X, Y) -> concat(X, Y, ?nl).

-spec newline(list(doc())) -> doc().
newline([X]) -> X;
newline([X, Y]) -> newline(X, Y);
newline([X | Rest]) -> newline(X, newline(Rest)).

-spec newlines(doc(), doc()) -> doc().
newlines(X, Y) -> concat(X, Y, ?two_nl).

-spec concat(doc(), doc(), binary()) -> doc().
concat(doc_nil, Y, _) -> Y;
concat(X, doc_nil, _) -> X;
concat(X, Y, Break) -> cons(X, cons(break(Break), Y)).

%%
%% Token Consumption
%%

-spec generate_doc_(tokens(), doc(), previous_term()) -> doc().
generate_doc_([], Doc, _) -> Doc;

generate_doc_([{'-', _} = H0, {atom, _, Spec} = H1, {'(', _} | Rest0], Doc, PrevTerm)
when Spec == spec orelse Spec == callback ->
  % Remove brackets from Specs and Callbacks
  Rest1 = remove_matching('(', ')', Rest0),
  generate_doc_([H0, H1 | Rest1], Doc, PrevTerm);

generate_doc_([{'-', _}, {atom, _, Spec} | Tokens], Doc0, PrevTerm)
when Spec == spec orelse Spec == callback ->
  % Spec
  {Group, Rest} = spec(Tokens),
  Doc = cons([text(<<"-">>), text(a2b(Spec)), text(<<" ">>), Group]),
  Doc1 =
    case PrevTerm of
      function_comment -> newline(Doc0, Doc);
      broken_function -> newlines(Doc0, newline(text(<<>>), Doc));
      _ -> newlines(Doc0, Doc)
    end,
  generate_doc_(Rest, Doc1, spec);

generate_doc_([{'-', _} = H0, {atom, _, Type} = H1, {'(', _} | Rest0], Doc, PrevTerm)
when Type == type orelse Type == opaque ->
  % Remove brackets from Types
  Rest1 = remove_matching('(', ')', Rest0),
  generate_doc_([H0, H1 | Rest1], Doc, PrevTerm);

generate_doc_([{'-', _}, {atom, _, Type} | Tokens], Doc0, PrevTerm)
when Type == type orelse Type == opaque ->
  % Type
  % Re-use the function code because the syntax is identical.
  {_ForceBreak, Group, Rest} = function(Tokens),
  Spec = cons([text(<<"-">>), text(a2b(Type)), text(<<" ">>), Group]),
  Doc1 =
    case PrevTerm of
      PrevTerm when PrevTerm == function_comment orelse PrevTerm == type -> newline(Doc0, Spec);
      _ -> newlines(Doc0, Spec)
    end,
  generate_doc_(Rest, Doc1, type);

generate_doc_([{'-', _}, {atom, _, Atom} | Tokens], Doc0, PrevTerm) ->
  % Module Attribute
  {Group, Rest} = attribute(Atom, Tokens),
  Doc1 =
    case {Atom, PrevTerm} of
      {_, function_comment} -> newline(Doc0, Group);
      {Atom, {attribute, Atom}} -> newline(Doc0, Group);

      {define, {attribute, IfDef}} when IfDef == ifdef orelse IfDef == else orelse IfDef == 'if' ->
        newline(Doc0, Group);

      {IfDef, {attribute, define}} when IfDef == else orelse IfDef == endif -> newline(Doc0, Group);
      _ -> newlines(Doc0, Group)
    end,
  generate_doc_(Rest, Doc1, {attribute, Atom});

generate_doc_([{'-', _}, {'if' = Atom, _} | Tokens], Doc0, _) ->
  % :'(
  {Group, Rest} = attribute(Atom, Tokens),
  generate_doc_(Rest, newlines(Doc0, Group), {attribute, Atom});

generate_doc_([{atom, _, _Atom} | _] = Tokens, Doc0, PrevTerm) ->
  % Function
  {ForceBreak, Group, Rest} = function(Tokens),
  Term =
    case ForceBreak of
      force_break -> broken_function;
      no_force_break -> function
    end,
  Doc1 =
    case PrevTerm of
      PrevTerm when PrevTerm == function_comment orelse PrevTerm == spec -> newline(Doc0, Group);
      broken_function -> newlines(Doc0, newline(text(<<>>), Group));
      _ -> newlines(Doc0, Group)
    end,
  generate_doc_(Rest, Doc1, Term);

generate_doc_([{comment, _, "%%" ++ _ = CommentText} | Rest], Doc0, PrevTerm) ->
  % Module Comment
  Comment = comment(CommentText),
  Doc1 =
    case PrevTerm of
      new_file -> cons(Doc0, Comment);
      PrevTerm when PrevTerm == module_comment -> newline(Doc0, Comment);
      _ -> newlines(Doc0, Comment)
    end,
  generate_doc_(Rest, Doc1, module_comment);

generate_doc_([{comment, _, CommentText} | Rest], Doc0, PrevTerm) ->
  % Function Comment
  Comment = comment(CommentText),
  Doc1 =
    case PrevTerm of
      new_file -> cons(Doc0, Comment);
      PrevTerm when PrevTerm == function_comment orelse PrevTerm == spec -> newline(Doc0, Comment);
      broken_function -> newlines(Doc0, newline(text(<<>>), Comment));
      _ -> newlines(Doc0, Comment)
    end,
  generate_doc_(Rest, Doc1, function_comment);

generate_doc_(Tokens, Doc0, PrevTerm) ->
  % Anything unhandled gets treated as an expression.
  % Things which fall through to here:
  % - Macros: can appear at the top level
  {_End, ForceBreak, Exprs, Rest} = exprs(Tokens),
  Group = force_break(ForceBreak, group(space(Exprs), inherit)),
  Doc1 =
    case PrevTerm of
      function_comment -> newline(Doc0, Group);
      _ -> newlines(Doc0, Group)
    end,
  generate_doc_(Rest, Doc1, expr).

%%
%% Erlang Source Elements
%%

-spec attribute(atom(), tokens()) -> {doc(), tokens()}.
attribute(Att, [{dot, _} | Rest]) ->
  % Handle attributes without brackets.
  % -else.
  % -endif.
  Attribute = group(cons([text(<<"-">>), text(a2b(Att)), text(?dot)])),
  {Attribute, Rest};

attribute('if', [{'(', _} | _] = Tokens) ->
  % Easiest to handle this here...
  % a2b/1 will print 'if' with the single quotes.
  % op2b/1 will print `dot` as `.`.
  % so pattern match the 'if' and convert it to <<"if">>.
  {_ForceBreak, Expr, [{dot, _} | Rest]} = list_group(Tokens),
  Attribute = group(cons([text(<<"-">>), text(<<"if">>), Expr, text(?dot)])),
  {Attribute, Rest};

attribute(Att, [{'(', _} | _] = Tokens) ->
  {_ForceBreak, Expr, [{dot, _} | Rest]} = list_group(Tokens),
  Attribute = group(cons([text(<<"-">>), text(a2b(Att)), Expr, text(?dot)])),
  {Attribute, Rest};

attribute(Att, Tokens0) ->
  % Put brackets around attributes which are missing them.
  {AttTokens, Rest, End} = get_until(dot, Tokens0),
  Tokens1 = [{'(', 0} | AttTokens] ++ [{')', 0}, End | Rest],
  attribute(Att, Tokens1).


-spec function(tokens()) -> {force_break(), doc(), tokens()}.
function(Tokens0) ->
  {FunctionTokens0, Tokens1, Token} = get_until(dot, Tokens0),
  FunctionTokens1 = FunctionTokens0 ++ [Token],
  {ForceBreak, Doc} =
    case {get_until('->', FunctionTokens1), get_until('::', FunctionTokens1)} of
      {{_, _, {dot, _}}, {_, _, {dot, _}}} ->
        % This is not actually a function with clauses.
        % We can get here if there is funny code inside an ifdef.
        % Process it as a normal expression.
        {_End, ForceBreak0, Exprs, []} = exprs(FunctionTokens1),
        {ForceBreak0, force_break(ForceBreak0, group(space(Exprs), inherit))};

      _ ->
        {ForceBreak0, Clauses, []} = clauses(FunctionTokens1),
        {ForceBreak0, newline(Clauses)}
    end,
  {ForceBreak, Doc, Tokens1}.


-spec spec(tokens()) -> {doc(), tokens()}.
spec(Tokens0) ->
  {FunctionNameTokens, Tokens1, Token} = get_until('(', Tokens0),
  Tokens2 = [Token | Tokens1],
  {empty, _, FunctionName, []} = expr(FunctionNameTokens, no_force_break),
  {ClauseForceBreak, Clauses, Rest} = clauses(Tokens2),
  Doc =
    case length(Clauses) > 1 of
      true ->
        % If we have multiple clauses in a spec we want them to be properly spaced underneath each other:
        % -spec foo(type()) -> ok;
        %          (other()) -> error.
        force_break(
          force_break,
          group(cons(FunctionName, underneath(0, group(space(Clauses), inherit))), inherit)
        );

      false ->
        % If we have multiple clauses in a spec we want to indent instead of going underneath.
        force_break(
          ClauseForceBreak,
          group(cons(FunctionName, group(space(Clauses), inherit)), inherit)
        )
    end,
  {Doc, Rest}.


-spec case_(tokens()) -> {force_break(), doc(), tokens()}.
case_([{'case', _} | Tokens]) ->
  {CaseArg, CaseArgForceBreak, Rest0} = case_arg(Tokens),
  {{CaseForceBreak, Clauses}, Rest1} =
    case get_until_end(Rest0) of
      {CaseClauseTokens, Rest, {'end', _}} ->
        {handle_trailing_comments(clauses(CaseClauseTokens)), Rest};

      {_, _, _} ->
        % Our 'case' is missing and 'end'. This is likely because someone has put
        % invalid syntax inside an unused macro.
        throw({complaint, partial_case_statement})
    end,
  ForceBreak =
    case length(Clauses) > 1 of
      true -> force_break;
      false -> resolve_force_break([CaseArgForceBreak, CaseForceBreak])
    end,
  GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
  CaseBody =
    case CaseArgForceBreak of
      % Nesting and grouping are not playing nicely here.
      % The way indentation works means that we have to use `indent()` on the doc
      % before the one we want to be indented.
      % The grouping we require does not appear to allow this.
      % There is probably a smarter way of doing what we want but the solution here works
      % so it's fine for now.
      force_break ->
        group(
          force_break(
            CaseArgForceBreak,
            group(
              space(
                nest(?indent, space(text(<<"case">>), CaseArg)),
                group(
                  nest(
                    ?indent,
                    force_break(ForceBreak, group(space(text(<<"of">>), GroupedClauses), inherit))
                  )
                )
              ),
              inherit
            )
          )
        );

      no_force_break ->
        cons(
          group(space(text(<<"case">>), CaseArg)),
          nest(?indent, space(text(<<" of">>), GroupedClauses))
        )
    end,
  Doc = force_break(ForceBreak, group(space(CaseBody, text(<<"end">>)), inherit)),
  {ForceBreak, Doc, Rest1}.


-spec case_arg(tokens()) -> {doc(), force_break(), tokens()}.
case_arg(Tokens) ->
  {CaseArgTokens, Rest, _} = get_until_of(Tokens),
  {CaseArg, ForceBreak} = case_arg_(CaseArgTokens, no_force_break, empty()),
  {CaseArg, ForceBreak, Rest}.


-spec case_arg_(tokens(), force_break(), doc()) -> {doc(), force_break()}.
case_arg_(CaseArgTokens, ForceBreak, Doc0) ->
  case expr(CaseArgTokens, no_force_break) of
    {comment, _, CaseArg, Rest} -> case_arg_(Rest, force_break, space(Doc0, CaseArg));
    {empty, _, CaseArg, []} -> {group(space(Doc0, CaseArg), inherit), ForceBreak};

    {empty, _, CaseArg, Rest} ->
      Comments = comments(Rest),
      {group(space([Doc0, CaseArg | Comments]), inherit), force_break}
  end.


-spec if_(tokens()) -> {force_break(), doc(), tokens()}.
if_([{'if', _} | Tokens]) ->
  {IfClauseTokens, Rest1, _} = get_until_end(Tokens),
  {IfForceBreak, Clauses} = handle_trailing_comments(clauses(IfClauseTokens)),
  ForceBreak =
    case length(Clauses) > 1 of
      true -> force_break;
      false -> IfForceBreak
    end,
  GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
  Doc =
    force_break(
      ForceBreak,
      group(space(nest(?indent, space(text(<<"if">>), GroupedClauses)), text(<<"end">>)), inherit)
    ),
  {ForceBreak, Doc, Rest1}.


-spec receive_(tokens()) -> {force_break(), doc(), tokens()}.
receive_([{'receive', _} | Tokens]) ->
  {ReceiveClauseTokens0, Rest, _} = get_until_end(Tokens),
  {ReceiveClauseTokens1, AfterClauseTokens} =
    case get_until('after', ReceiveClauseTokens0) of
      {_, [], _} -> {ReceiveClauseTokens0, []};
      {R, A, Token} -> {R, [Token | A]}
    end,
  {ReceiveForceBreak, Clauses} = handle_trailing_comments(clauses(ReceiveClauseTokens1)),
  {AfterForceBreak, After} = after_(AfterClauseTokens),
  ForceBreak =
    case length(Clauses) > 1 of
      true -> force_break;
      false -> resolve_force_break([ReceiveForceBreak, AfterForceBreak])
    end,
  GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
  Doc =
    force_break(
      ForceBreak,
      group(
        space([nest(?indent, space(text(<<"receive">>), GroupedClauses)), After, text(<<"end">>)]),
        inherit
      )
    ),
  {ForceBreak, Doc, Rest}.


-spec after_(tokens()) -> {force_break(), doc()}.
after_([]) -> {no_force_break, empty()};

after_([{'after', _} | Tokens]) ->
  {AfterForceBreak, Clauses} = handle_trailing_comments(clauses(Tokens)),
  ForceBreak =
    case length(Clauses) > 1 of
      true -> force_break;
      false -> AfterForceBreak
    end,
  GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
  Doc =
    force_break(ForceBreak, group(nest(?indent, space(text(<<"after">>), GroupedClauses)), inherit)),
  {ForceBreak, Doc}.


% 'try' can have an 'of' followed by heads and clauses or it can have no 'of' and instead
% be followed by expressions.
% TODO This is messy and should be refactored.
-spec try_(tokens()) -> {force_break(), doc(), tokens()}.
try_([{'try', _} | Tokens]) ->
  {TryTokens, Rest, _} = get_until_end(Tokens),
  {TryType, TryDoc, Rest1} =
    case get_until_of(TryTokens) of
      {_, [], _} ->
        % There is no 'of' for this 'try'
        {exprs, text(<<"try">>), TryTokens};

      {TryArgTokens, Rest0, _} ->
        {empty, _, TryArgs, []} = exprs(TryArgTokens),
        {clauses, group(space(text(<<"try">>), space(TryArgs))), Rest0}
    end,
  {TryTokens1, AfterClauseTokens} =
    case get_until('after', Rest1) of
      {Tokens0, [], EndToken0} ->
        % No 'after'
        {Tokens0 ++ [EndToken0], []};

      {Tokens0, Rest2, EndToken0} ->
        % Found 'after'. EndToken is the after token.
        {Tokens0, [EndToken0 | Rest2]}
    end,
  {TryTokens2, CatchClauseTokens} =
    case get_until('catch', TryTokens1) of
      {Tokens1, [], EndToken1} ->
        % No 'catch'
        {Tokens1 ++ [EndToken1], []};

      {Tokens1, Rest3, EndToken1} ->
        % Found 'catch'. EndToken is the catch token.
        {Tokens1, [EndToken1 | Rest3]}
    end,
  Catch = try_catch_(CatchClauseTokens),
  {AfterForceBreak, After} = try_after_(AfterClauseTokens),
  {ForceBreak0, Doc0} =
    case TryType of
      exprs ->
        {empty, TryForceBreak, Exprs} = handle_trailing_expr_comments(exprs(TryTokens2)),
        ForceBreak =
          case length(Exprs) > 1 of
            true -> force_break;
            false -> resolve_force_break([TryForceBreak, AfterForceBreak])
          end,
        GroupedExprs = force_break(ForceBreak, group(space(Exprs), inherit)),
        Doc =
          force_break(
            ForceBreak,
            group(
              space([nest(?indent, space(TryDoc, GroupedExprs)), Catch, After, text(<<"end">>)]),
              inherit
            )
          ),
        {ForceBreak, Doc};

      clauses ->
        {TryForceBreak, Clauses} = handle_trailing_comments(clauses(TryTokens2)),
        ForceBreak =
          case length(Clauses) > 1 of
            true -> force_break;
            false -> resolve_force_break([TryForceBreak, AfterForceBreak])
          end,
        GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
        Doc =
          force_break(
            ForceBreak,
            group(
              space(
                [
                  cons(TryDoc, nest(?indent, space(text(<<" of">>), GroupedClauses))),
                  Catch,
                  After,
                  text(<<"end">>)
                ]
              ),
              inherit
            )
          ),
        {ForceBreak, Doc}
    end,
  {ForceBreak0, Doc0, Rest}.


-spec try_catch_(tokens()) -> doc().
try_catch_([]) -> empty();

try_catch_([{'catch', _} | Tokens]) ->
  {CatchForceBreak, Clauses} = handle_trailing_comments(clauses(Tokens)),
  ForceBreak =
    case length(Clauses) > 1 of
      true -> force_break;
      false -> CatchForceBreak
    end,
  GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
  force_break(ForceBreak, group(nest(?indent, space(text(<<"catch">>), GroupedClauses)), inherit)).


-spec try_after_(tokens()) -> {force_break(), doc()}.
try_after_([]) -> {no_force_break, empty()};

try_after_([{'after', _} | Tokens]) ->
  {empty, AfterForceBreak, Exprs, []} = exprs(Tokens),
  ForceBreak =
    case length(Exprs) > 1 of
      true -> force_break;
      false -> AfterForceBreak
    end,
  GroupedExprs = force_break(ForceBreak, group(space(Exprs), inherit)),
  Doc =
    force_break(ForceBreak, group(nest(?indent, space(text(<<"after">>), GroupedExprs)), inherit)),
  {ForceBreak, Doc}.


-spec fun_(tokens()) -> {force_break(), doc(), tokens()}.
fun_([{'fun', _} | Tokens]) ->
  {ForceBreak, Doc, Rest} =
    case get_until_end(Tokens) of
      {[{'(', _}, {')', _}], [], {dot, _} = End} ->
        % This case can happen in typedefs if the type looks like
        % -type x() :: fun().
        % Put the dot back so that we pick it up as the end token of the
        % expression. Otherwise we thing the expression is `empty`.
        {no_force_break, text(<<"fun()">>), [End]};

      {[{'(', _}, {')', _}, {'|', _} | _] = Tokens0, Rest0, End} ->
        % This case can happen in typedefs if the type looks like
        % -type x() :: fun() | map().
        [_, _ | Tokens1] = Tokens0,
        Rest1 = Tokens1 ++ [End] ++ Rest0,
        {no_force_break, text(<<"fun()">>), Rest1};

      {[{'(', _}], [], {')', _}} ->
        % This case can happen in typedefs if the type looks like
        % -type x() :: {fun()}.
        {no_force_break, text(<<"fun()">>), []};

      {[{'(', _}, {')', _}], Rest0, {',', _} = End} ->
        % This case can happen in records and specs:
        % -record(x, {y :: fun(), z}).
        % or
        % -spec x(X) -> Y() when X :: fun(), Y :: any().
        % Put the comma back so that we pick it up as the end token of the
        % expression. Otherwise we thing the expression is `empty`.
        {no_force_break, text(<<"fun()">>), [End | Rest0]};

      {ClauseTokens, Rest0, _} ->
        {ForceBreak0, Clauses} = handle_trailing_comments(clauses(ClauseTokens)),
        Doc0 =
          force_break(
            ForceBreak0,
            group(
              space(nest(?indent, space([text(<<"fun">>) | Clauses])), text(<<"end">>)),
              inherit
            )
          ),
        {ForceBreak0, Doc0, Rest0}
    end,
  {ForceBreak, Doc, Rest}.


-spec mfa_module(token()) -> doc().
mfa_module({atom, _, M}) -> text(a2b(M));
mfa_module({var, _, M}) -> text(v2b(M)).

-spec mfa_function(token()) -> doc().
mfa_function({atom, _, F}) -> text(a2b(F));
mfa_function({var, _, F}) -> text(v2b(F)).

-spec mfa_arity(token()) -> doc().
mfa_arity({integer, _, A}) -> text(i2b(A));
mfa_arity({var, _, A}) -> text(v2b(A)).

-spec record_key(token()) -> doc().
record_key({atom, _, K}) -> text(a2b(K));
record_key({var, _, K}) -> text(v2b(K)).

-spec binary_size(token()) -> doc().
binary_size({integer, _, S}) -> text(i2b(S));
binary_size({var, _, S}) -> text(v2b(S)).

-spec begin_(tokens()) -> {force_break(), doc(), tokens()}.
begin_([{'begin', _} | Tokens]) ->
  {BeginTokens, Rest, _} = get_until_end(Tokens),
  {empty, ForceBreak, Exprs, []} = exprs(BeginTokens),
  GroupedExprs = force_break(ForceBreak, group(space(Exprs), inherit)),
  Doc =
    force_break(
      ForceBreak,
      group(space(nest(?indent, space(text(<<"begin">>), GroupedExprs)), text(<<"end">>)), inherit)
    ),
  {ForceBreak, group(Doc), Rest}.


-spec when_(tokens()) -> {dot | ';' | empty, force_break(), doc()}.
when_([{'when', _} | Tokens]) ->
  case is_attribute(Tokens) of
    true -> attribute_when_(Tokens, no_force_break, []);
    false -> function_when_(Tokens, no_force_break, [])
  end.


-spec attribute_when_(tokens(), force_break(), list(doc())) ->
  {dot | ';' | empty, force_break(), doc()}.
attribute_when_(Tokens, ForceBreak0, Acc) ->
  {End, ForceBreak, Exprs, []} = exprs(Tokens),
  Expr = space(Acc ++ Exprs),
  ForceBreak1 = resolve_force_break([ForceBreak, ForceBreak0]),
  Doc = group(cons(text(<<"when ">>), underneath(0, group(Expr, inherit)))),
  {End, ForceBreak1, Doc}.


-spec function_when_(tokens(), force_break(), list(doc())) ->
  {dot | ';' | empty, force_break(), doc()}.
function_when_(Tokens, ForceBreak0, Acc) ->
  case exprs(Tokens) of
    {';', ForceBreak, Exprs, Rest} ->
      ForceBreak1 = resolve_force_break([ForceBreak, ForceBreak0]),
      function_when_(Rest, ForceBreak1, Acc ++ Exprs);

    {End, ForceBreak, Exprs, []} ->
      Expr = space(Acc ++ Exprs),
      Doc = group(cons(text(<<"when ">>), underneath(0, group(Expr, inherit)))),
      ForceBreak1 = resolve_force_break([ForceBreak, ForceBreak0]),
      {End, ForceBreak1, Doc}
  end.


-spec is_attribute(tokens()) -> boolean().
is_attribute([]) -> false;
is_attribute([{'::', _} | _]) -> true;
is_attribute([{';', _} | _]) -> false;
is_attribute([_ | Rest]) -> is_attribute(Rest).

-spec comment(string()) -> doc().
comment(Comment) -> text(string:trim(unicode:characters_to_binary(Comment))).

-spec comments(tokens()) -> list(doc()).
comments(Comments) -> lists:map(fun ({comment, _, Text}) -> comment(Text) end, Comments).

-spec handle_trailing_comments({force_break(), list(doc()), tokens()}) ->
  {force_break(), list(doc())}.
handle_trailing_comments({ForceBreak, Clauses, []}) -> {ForceBreak, Clauses};

handle_trailing_comments({_, Clauses, Comments}) ->
  CommentDocs = comments(Comments),
  {force_break, Clauses ++ CommentDocs}.


-spec handle_trailing_expr_comments({empty, force_break(), list(doc()), tokens()}) ->
  {empty, force_break(), list(doc())}.
handle_trailing_expr_comments({empty, ForceBreak, Exprs, []}) -> {empty, ForceBreak, Exprs};

handle_trailing_expr_comments({empty, _, Exprs, Comments}) ->
  CommentDocs = comments(Comments),
  {empty, force_break, Exprs ++ CommentDocs}.

%%
%% Generic Erlang Expressions
%%

-spec equation(doc(), doc(), doc(), force_break()) -> doc().
equation(Doc0, Equals, Expr, ForceBreak) ->
  Equation =
    group(nest(?indent, force_break(ForceBreak, group(space(Equals, group(Expr)), inherit)))),
  case Doc0 of
    doc_nil -> Equation;
    _ -> group(cons([Doc0, text(?sp), Equation]))
  end.


-spec list_group(tokens()) -> {force_break(), doc(), tokens()}.
list_group([{Open, _} | Rest0]) when ?IS_LIST_OPEN_CHAR(Open) ->
  Close = close_bracket(Open),
  case get_until(Close, Rest0) of
    {Tokens, Rest1, {Close, _}} ->
      {ForceBreak, ListGroup} = brackets(Tokens, op2b(Open), op2b(Close)),
      {ForceBreak, ListGroup, Rest1};

    {_Tokens, _Rest, {dot, _}} ->
      % This is probably due to a funky macro.
      % We could handle it properly but it's more effort than it's worth right now.
      throw({complaint, reached_dot_before_closing_bracket})
  end.


-spec brackets(tokens(), binary(), binary()) -> {force_break(), doc()}.
brackets([], Open, Close) -> {no_force_break, group(cons(text(Open), text(Close)))};

brackets(Tokens, Open, Close) ->
  {ForceBreak, ListElements} = list_elements(Tokens),
  Doc =
    group(
      force_break(ForceBreak, stick(nest(?indent, stick(text(Open), ListElements)), text(Close)))
    ),
  {ForceBreak, Doc}.


-spec list_elements(tokens()) -> {force_break(), doc()}.
list_elements(Tokens) -> list_elements(Tokens, empty(), no_force_break).

-spec list_elements(tokens(), doc(), force_break()) -> {force_break(), doc()}.
list_elements([], Doc, ForceBreak) -> {ForceBreak, Doc};

list_elements(Tokens, Doc, ForceBreak0) ->
  {_End, ForceBreak1, Expr, Rest} = expr(Tokens, ForceBreak0),
  list_elements(Rest, space(Doc, group(Expr)), ForceBreak1).


-spec clauses(tokens()) -> {force_break(), list(doc()), tokens()}.
clauses([]) -> {no_force_break, [empty()], []};
clauses(Tokens) -> clauses(Tokens, [], []).

-spec clauses(tokens(), list(doc()), list(force_break())) -> {force_break(), list(doc()), tokens()}.
clauses(Tokens, Acc0, ForceBreaks0) ->
  {Continue, ClauseForceBreak, Clause, Rest0} = head_and_clause(Tokens, Acc0),
  Acc1 = [Clause | Acc0],
  ForceBreaks1 = [ClauseForceBreak | ForceBreaks0],
  case Continue of
    continue -> clauses(Rest0, Acc1, ForceBreaks1);

    done ->
      ForceBreak = resolve_force_break(ForceBreaks1),
      {ForceBreak, lists:reverse(Acc1), Rest0}
  end.


-spec head_and_clause(tokens(), list(doc())) -> {continue(), force_break(), doc(), tokens()}.
head_and_clause([], _) -> {done, force_break, empty(), []};
head_and_clause(Tokens, PrevClauses) -> head_and_clause(Tokens, PrevClauses, empty()).

-spec head_and_clause(tokens(), list(doc()), doc()) -> {continue(), force_break(), doc(), tokens()}.
head_and_clause([{'?', _} | _] = Tokens, PrevClauses, doc_nil) ->
  % Macro
  case lists:any(fun ({'->', _}) -> true; (_) -> false end, Tokens) of
    true ->
      % This is a hack so the doc_nil above no longer matches.
      head_and_clause(Tokens, PrevClauses, cons(empty(), empty()));

    false -> clause(Tokens)
  end;

head_and_clause([{atom, _, Name} | Rest], PrevClauses, Doc) ->
  % Name
  head_and_clause(Rest, PrevClauses, cons(Doc, text(a2b(Name))));

head_and_clause([{C, _} | _] = Tokens, PrevClauses, Doc) when ?IS_LIST_OPEN_CHAR(C) ->
  % Args
  {_ForceBreak, Group, Rest} = list_group(Tokens),
  head_and_clause(Rest, PrevClauses, cons(Doc, Group));

head_and_clause([{comment, _, Comment} | Rest], _PrevClauses, Doc0) ->
  % Handle any comments between function clauses.
  {Continue, ForceBreak, Doc1, Tokens} = head_and_clause(Rest, []),
  Doc2 = newline([Doc0, comment(Comment), Doc1]),
  {Continue, ForceBreak, Doc2, Tokens};

head_and_clause([{'->', _} | Rest0], PrevClauses, Doc0) ->
  % End
  {Continue, ForceBreak, Clause, Rest1} = clause(Rest0),
  ClauseGroup =
    force_break(ForceBreak, nest(?indent, group(space(text(<<" ->">>), Clause), inherit))),
  Doc1 =
    case {Continue, PrevClauses} of
      {continue, []} ->
        force_break(ForceBreak, pad_group_bottom(cons(group(Doc0), ClauseGroup), inherit));

      {done, []} -> cons(group(Doc0), ClauseGroup);
      {done, _} -> force_break(ForceBreak, pad_group_top(cons(group(Doc0), ClauseGroup), inherit));
      _ -> force_break(ForceBreak, pad_group(cons(group(Doc0), ClauseGroup), inherit))
    end,
  {Continue, ForceBreak, group(Doc1), Rest1};

head_and_clause([{'::', _} | Rest0], _PrevClauses, Doc) ->
  % Altenative End (for Type definitions)
  {Continue, ForceBreak, Clause, Rest1} = clause(Rest0),
  Doc1 =
    cons([Doc, text(<<" :: ">>), force_break(ForceBreak, underneath(-2, group(Clause, inherit)))]),
  Doc1 =
    cons([Doc, text(<<" :: ">>), force_break(ForceBreak, underneath(-2, group(Clause, inherit)))]),
  {Continue, ForceBreak, Doc1, Rest1};

head_and_clause(Rest0, PrevClauses, Doc0) ->
  {Tokens, Rest1, Token} = get_until('->', Rest0),
  % We expect this to come back with either:
  % End = empty and Rest = []
  % End = ';' and Rest = list(tokens())
  %
  % The first case happens most of the time. The second case happens when we have
  % things like:
  % if X =:= test; X =:= other ->
  {_End, ForceBreak, Exprs, Leftovers} = exprs(Tokens),
  Group = force_break(ForceBreak, group(space(Exprs), inherit)),
  head_and_clause(Leftovers ++ [Token | Rest1], PrevClauses, space(Doc0, Group)).


-spec clause(tokens()) -> {continue(), force_break(), doc(), tokens()}.
clause(Tokens) ->
  {End, ForceBreak, Exprs, Rest} = exprs(Tokens),
  Continue =
    case End of
      dot -> done;
      empty -> done;
      ';' -> continue
    end,
  case Exprs of
    [Expr] -> {Continue, ForceBreak, Expr, Rest};

    _ ->
      % Force indentation for multi-expression clauses
      {Continue, force_break, space(Exprs), Rest}
  end.


% Get a list of expressions.
-spec exprs(tokens()) -> {dot | ';' | empty, force_break(), list(doc()), tokens()}.
exprs(Tokens) -> exprs(Tokens, [], no_force_break).

-spec exprs(tokens(), list(doc()), force_break()) ->
  {dot | ';' | empty, force_break(), list(doc()), tokens()}.
exprs(Tokens, Acc0, ForceBreak0) ->
  {End, ForceBreak1, Expr, Rest} = expr(Tokens, ForceBreak0),
  Acc1 = [Expr | Acc0],
  case End of
    End when End == ',' orelse End == comment -> exprs(Rest, Acc1, ForceBreak1);
    _ -> {End, ForceBreak1, lists:reverse(Acc1), Rest}
  end.


-spec expr(tokens(), force_break()) ->
  {dot | ';' | ',' | empty | comment, force_break(), doc(), tokens()}.
expr(Tokens, ForceBreak0) ->
  {ExprTokens, Rest} = get_end_of_expr(Tokens),
  {End, ForceBreak1, Expr} = expr_(ExprTokens, empty(), ForceBreak0),
  {End, ForceBreak1, group(Expr), Rest}.


-spec expr_(tokens(), doc(), force_break()) ->
  {dot | ';' | ',' | empty | comment, force_break(), doc()}.
expr_([], Doc, ForceBreak) -> {empty, ForceBreak, Doc};

expr_([{'?', _} | Rest0], Doc, ForceBreak0) ->
  % Handle macros
  {End, ForceBreak1, Expr, []} = expr(Rest0, ForceBreak0),
  {End, ForceBreak1, space(Doc, cons(text(<<"?">>), Expr))};

expr_([{var, LineNum, MacroName}, {'(', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
  % Handle macros which take arguments
  Tokens1 = tl(Tokens0),
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Macro = space(Doc, cons(text(v2b(MacroName)), ListGroup)),
  expr_(Rest, Macro, ForceBreak1);

expr_([{'case', _} | _] = Tokens, Doc, ForceBreak0) ->
  {GroupForceBreak, Group, Rest} = case_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  expr_(Rest, space(Doc, Group), ForceBreak1);

expr_([{'if', _} | _] = Tokens, Doc, ForceBreak0) ->
  {GroupForceBreak, Group, Rest} = if_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  expr_(Rest, space(Doc, Group), ForceBreak1);

expr_([{'receive', _} | _] = Tokens, Doc, ForceBreak0) ->
  {GroupForceBreak, Group, Rest} = receive_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  expr_(Rest, space(Doc, Group), ForceBreak1);

expr_([{'try', _} | _] = Tokens, Doc, ForceBreak0) ->
  {GroupForceBreak, Group, Rest} = try_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  expr_(Rest, space(Doc, Group), ForceBreak1);

expr_([{'begin', _} | _] = Tokens, Doc, ForceBreak0) ->
  {GroupForceBreak, Group, Rest} = begin_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  expr_(Rest, space(Doc, Group), ForceBreak1);

expr_([{'when', _} | _] = Tokens, Doc, ForceBreak0) ->
  {End, GroupForceBreak, Group} = when_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  {End, ForceBreak1, space(Doc, Group)};

expr_([{'#', LineNum}, {atom, LineNum, Atom}, {'{', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
  % Handle records
  % #record_name{key => value}
  [_, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Record = group(cons([text(<<"#">>), text(a2b(Atom)), ListGroup])),
  expr_(Rest, space(Doc, Record), ForceBreak1);

expr_(
  [{'#', LineNum}, {'?', LineNum}, {var, LineNum, Var}, {'{', LineNum} | _] = Tokens0,
  Doc,
  ForceBreak0
) ->
  % Handle macro records
  % #?MACRO{key => value}
  [_, _, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Record = group(cons([text(<<"#">>), text(<<"?">>), text(v2b(Var)), ListGroup])),
  expr_(Rest, space(Doc, Record), ForceBreak1);

expr_(
  [
    {var, LineNum, Var},
    {'#', LineNum},
    {atom, LineNum, Rec},
    {'.', LineNum},
    {K, LineNum, _} = Key | Rest
  ],
  Doc,
  ForceBreak
)
when ?IS_VALID_RECORD_KEY(K) ->
  % Handle record element lookup
  % X#record_name.key
  Record =
    group(cons([text(v2b(Var)), text(<<"#">>), text(a2b(Rec)), text(<<".">>), record_key(Key)])),
  expr_(Rest, space(Doc, Record), ForceBreak);

expr_(
  [
    {var, LineNum, Var},
    {'#', LineNum},
    {'?', LineNum},
    {var, LineNum, Macro},
    {'.', LineNum},
    {K, LineNum, _} = Key | Rest
  ],
  Doc,
  ForceBreak
)
when ?IS_VALID_RECORD_KEY(K) ->
  % Handle macro record element lookup
  % X#?MACRO.key
  Record =
    group(
      cons(
        [
          text(v2b(Var)),
          text(<<"#">>),
          text(<<"?">>),
          text(v2b(Macro)),
          text(<<".">>),
          record_key(Key)
        ]
      )
    ),
  expr_(Rest, space(Doc, Record), ForceBreak);

expr_(
  [{'#', LineNum}, {atom, LineNum, Rec}, {'.', LineNum}, {K, LineNum, _} = Key | Rest],
  Doc,
  ForceBreak
)
when ?IS_VALID_RECORD_KEY(K) ->
  % Handle record key
  % #record_name.key
  Record = group(cons([text(<<"#">>), text(a2b(Rec)), text(<<".">>), record_key(Key)])),
  expr_(Rest, space(Doc, Record), ForceBreak);

expr_(
  [
    {'#', LineNum},
    {'?', LineNum},
    {var, LineNum, Var},
    {'.', LineNum},
    {K, LineNum, _} = Key | Rest
  ],
  Doc,
  ForceBreak
)
when ?IS_VALID_RECORD_KEY(K) ->
  % Handle macro record key
  % #?MACRO.key
  Record =
    group(cons([text(<<"#">>), text(<<"?">>), text(v2b(Var)), text(<<".">>), record_key(Key)])),
  expr_(Rest, space(Doc, Record), ForceBreak);

expr_(
  [{var, LineNum, Var}, {'#', LineNum}, {atom, LineNum, Atom}, {'{', LineNum} | _] = Tokens0,
  Doc,
  ForceBreak0
) ->
  % Handle record updates
  % Record#record_name{key => value}
  [_, _, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Record = group(cons([text(v2b(Var)), text(<<"#">>), text(a2b(Atom)), ListGroup])),
  expr_(Rest, space(Doc, Record), ForceBreak1);

expr_(
  [{var, LineNum, Var}, {'#', LineNum}, {'?', LineNum}, {var, LineNum, Macro}, {'{', LineNum} | _] =
    Tokens0,
  Doc,
  ForceBreak0
) ->
  % Handle macro record updates
  % Record#?MACRO{key => value}
  [_, _, _, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Record = group(cons([text(v2b(Var)), text(<<"#">>), text(<<"?">>), text(v2b(Macro)), ListGroup])),
  expr_(Rest, space(Doc, Record), ForceBreak1);

expr_([{'#', LineNum}, {'{', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
  % Handle maps
  % #{key => value}
  Tokens1 = tl(Tokens0),
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Map = group(cons(text(<<"#">>), ListGroup)),
  expr_(Rest, space(Doc, Map), ForceBreak1);

expr_([{var, LineNum, Var}, {'#', LineNum}, {'{', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
  % Handle map updates
  % X#{key => value}
  [_, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Map = group(cons([text(v2b(Var)), text(<<"#">>), ListGroup])),
  expr_(Rest, space(Doc, Map), ForceBreak1);

expr_([{var, _, Var}, {'/', _}, {atom, _, Atom} | Rest], Doc, ForceBreak) ->
  % Handle binary matching
  % <<Thing/binary>>
  TermDoc = cons([text(v2b(Var)), text(<<"/">>), text(a2b(Atom))]),
  expr_(Rest, space(Doc, TermDoc), ForceBreak);

expr_([{string, _, String}, {'/', _}, {atom, _, Atom} | Rest], Doc, ForceBreak) ->
  % Handle binary literals
  % <<"hello"/utf8>>
  TermDoc = cons([text(s2b(String)), text(<<"/">>), text(a2b(Atom))]),
  expr_(Rest, space(Doc, TermDoc), ForceBreak);

expr_(
  [{var, _, Var}, {':', _}, {S, _, _} = Size, {'/', _}, {atom, _, Atom} | Rest],
  Doc,
  ForceBreak
)
when ?IS_VALID_BINARY_SIZE(S) ->
  % Handle more binary matching
  % <<Thing:1/binary, Rest/binary>>
  % or
  % <<Thing:Size/binary, Rest/binary>>
  TermDoc =
    cons([text(v2b(Var)), text(<<":">>), binary_size(Size), text(<<"/">>), text(a2b(Atom))]),
  expr_(Rest, space(Doc, TermDoc), ForceBreak);

expr_([{'fun', _}, {'(', _}, {'(', _} | _] = Tokens0, Doc, ForceBreak0) ->
  % Handle 'fun((Arg :: type()) -> other_type())'
  Tokens1 = tl(Tokens0),
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Fun = group(cons(text(<<"fun">>), ListGroup)),
  expr_(Rest, space(Doc, Fun), ForceBreak1);

expr_(
  [
    {'fun', _},
    {M, LineNum, _} = Module,
    {':', LineNum},
    {F, LineNum, _} = Function,
    {'/', LineNum},
    {A, LineNum, _} = Arity | Rest
  ],
  Doc,
  ForceBreak
)
when ?IS_VALID_MFA(M, F, A) ->
  % Handle `fun module:function/arity`
  Fun =
    cons(
      [
        text(<<"fun ">>),
        mfa_module(Module),
        text(<<":">>),
        mfa_function(Function),
        text(<<"/">>),
        mfa_arity(Arity)
      ]
    ),
  expr_(Rest, space(Doc, Fun), ForceBreak);

expr_(
  [
    {'fun', _},
    {'?', LineNum},
    {var, LineNum, MacroName},
    {':', LineNum},
    {F, LineNum, _} = Function,
    {'/', LineNum},
    {A, LineNum, _} = Arity | Rest
  ],
  Doc,
  ForceBreak
)
when ?IS_VALID_FA(F, A) ->
  % Handle `fun ?MACRO:function/arity`
  Fun =
    cons(
      [
        text(<<"fun ">>),
        text(<<"?">>),
        text(v2b(MacroName)),
        text(<<":">>),
        mfa_function(Function),
        text(<<"/">>),
        mfa_arity(Arity)
      ]
    ),
  expr_(Rest, space(Doc, Fun), ForceBreak);

expr_(
  [{'fun', _}, {F, LineNum, _} = Function, {'/', LineNum}, {A, LineNum, _} = Arity | Rest],
  Doc,
  ForceBreak
)
when ?IS_VALID_FA(F, A) ->
  % Handle `fun function/arity`
  Fun = cons([text(<<"fun ">>), mfa_function(Function), text(<<"/">>), mfa_arity(Arity)]),
  expr_(Rest, space(Doc, Fun), ForceBreak);

expr_([{'fun', _} | _] = Tokens, Doc, ForceBreak0) ->
  % Handle any 'fun' expressions which have an 'end' and a few other special cases
  % without an `end`.
  {GroupForceBreak, Group, Rest} = fun_(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
  expr_(Rest, space(Doc, Group), ForceBreak1);

expr_(
  [{M, LineNum, _} = Module, {':', LineNum}, {F, LineNum, _} = Function, {'(', LineNum} | _] =
    Tokens0,
  Doc0,
  ForceBreak0
)
when ?IS_VALID_MF(M, F) ->
  % Handle function calls to other modules
  % `module:function(Args)`
  [_, _, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Doc1 = space(Doc0, cons([mfa_module(Module), text(<<":">>), mfa_function(Function), ListGroup])),
  expr_(Rest, Doc1, ForceBreak1);

expr_(
  [
    {M, LineNum, _} = Module,
    {':', LineNum},
    {'?', LineNum},
    {F, LineNum, _} = Function,
    {'(', LineNum} | _
  ] = Tokens0,
  Doc0,
  ForceBreak0
)
when ?IS_VALID_MF(M, F) ->
  % As above but the function name can be a macro.
  % `module:?F(Args)`
  [_, _, _, _ | Tokens1] = Tokens0,
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Doc1 =
    space(
      Doc0,
      cons([mfa_module(Module), text(<<":">>), text(<<"?">>), mfa_function(Function), ListGroup])
    ),
  expr_(Rest, Doc1, ForceBreak1);

expr_(
  [{M, LineNum, _} = Module, {':', LineNum}, {F, LineNum, _} = Function | _] = Tokens,
  Doc0,
  ForceBreak
)
when ?IS_VALID_MF(M, F) ->
  % Handle function calls to other modules without brackets.
  % We can get here when creating specs due to the way we split tokens in order to perform
  % underneath alignment.
  % We can also get here from catch syntax.
  % `module:fuction`
  % or
  % `X:Y`
  [_, _, _ | Rest] = Tokens,
  Doc1 = space(Doc0, cons([mfa_module(Module), text(<<":">>), mfa_function(Function)])),
  expr_(Rest, Doc1, ForceBreak);

expr_(
  [{M, LineNum, _} = Module, {':', LineNum}, {'?', LineNum}, {F, LineNum, _} = Function | _] =
    Tokens,
  Doc0,
  ForceBreak
)
when ?IS_VALID_MF(M, F) ->
  % As above but the function name can be a macro.
  [_, _, _, _ | Rest] = Tokens,
  Doc1 =
    space(Doc0, cons([mfa_module(Module), text(<<":">>), text(<<"?">>), mfa_function(Function)])),
  expr_(Rest, Doc1, ForceBreak);

expr_([{F, LineNum, _} = Function, {'(', LineNum} | _] = Tokens0, Doc0, ForceBreak0)
when ?IS_VALID_FUNCTION_TYPE(F) ->
  % Handle local function calls
  % `function(Args)`
  Tokens1 = tl(Tokens0),
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  Doc1 = space(Doc0, cons(mfa_function(Function), ListGroup)),
  expr_(Rest, Doc1, ForceBreak1);

expr_([{C, _} | _] = Tokens, Doc, ForceBreak0) when ?IS_LIST_OPEN_CHAR(C) ->
  % Handle lists
  {ListForceBreak, ListGroup, Rest} = list_group(Tokens),
  ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
  expr_(Rest, space(Doc, ListGroup), ForceBreak1);

expr_([{C, _} | Rest0], Doc0, ForceBreak0) when ?IS_EQUALS(C) ->
  % Handle things like
  % Arg3 =
  %     Arg1 + Arg2,
  Equals = text(op2b(C)),
  case is_bool_list(Rest0) of
    true ->
      case get_until_any(['andalso', 'orelse'], Rest0) of
        {_Tokens, [], not_found} ->
          % This should never happen
          throw(token_not_found);

        {Tokens, Rest1, EndToken} ->
          % If we have a list of boolean terms
          %   e.g. Arg1 == 5 andalso Arg2 == 6
          % then we want to group each term individually rather than grouping everything
          % after the `==`.
          {_End, RestForceBreak, Expr} = expr_(Tokens, empty(), no_force_break),
          Equation = equation(Doc0, Equals, Expr, RestForceBreak),
          ForceBreak1 = resolve_force_break([ForceBreak0, RestForceBreak]),
          expr_([EndToken | Rest1], Equation, ForceBreak1)
      end;

    false ->
      {End, RestForceBreak, Expr} = expr_(Rest0, empty(), no_force_break),
      Equation = equation(Doc0, Equals, Expr, RestForceBreak),
      ForceBreak1 = resolve_force_break([ForceBreak0, RestForceBreak]),
      {End, ForceBreak1, Equation}
  end;

expr_([{End, _}], Doc, ForceBreak) ->
  % Handle the expression end character
  {End, ForceBreak, cons(Doc, text(op2b(End)))};

expr_([{Type0, _, _} = Num0, {'-', _}, {Type1, _, _} = Num1 | Rest], Doc0, ForceBreak)
when ?IS_NUMBER(Type0) andalso ?IS_NUMBER(Type1) ->
  % Subtraction: numbers
  Doc1 = space([Doc0, text(n2b(Num0)), text(op2b('-')), text(n2b(Num1))]),
  expr_(Rest, Doc1, ForceBreak);

expr_([{var, _, Var}, {'-', _}, {Type, _, _} = Num | Rest], Doc0, ForceBreak) when ?IS_NUMBER(Type) ->
  % Subtraction: variables
  Doc1 = space([Doc0, text(v2b(Var)), text(op2b('-')), text(n2b(Num))]),
  expr_(Rest, Doc1, ForceBreak);

expr_([{Op, _}, {'-', _}, {Type, _, _} = Num | Rest], Doc0, ForceBreak) when ?IS_NUMBER(Type) ->
  % Negative number
  Bin = n2b(Num),
  Doc1 = space([Doc0, text(op2b(Op)), text(<<"-", Bin/binary>>)]),
  expr_(Rest, Doc1, ForceBreak);

expr_([{'-', _}, {Type, _, _} = Num | Rest], doc_nil = Doc0, ForceBreak) when ?IS_NUMBER(Type) ->
  % Negative number
  Bin = n2b(Num),
  Doc1 = space(Doc0, text(<<"-", Bin/binary>>)),
  expr_(Rest, Doc1, ForceBreak);

expr_([{'-', _}, {Type, _, _} = Num | Rest], Doc0, ForceBreak) when ?IS_NUMBER(Type) ->
  % Subtraction: functions
  Doc1 = space([Doc0, text(op2b('-')), text(n2b(Num))]),
  expr_(Rest, Doc1, ForceBreak);

expr_([{atom, _, Atom}, {'/', _}, {integer, _, Int} | Rest], Doc, ForceBreak) ->
  % Handle function arity expressions
  % some_fun/1
  FunctionDoc = cons([text(a2b(Atom)), text(<<"/">>), text(i2b(Int))]),
  expr_(Rest, space(Doc, FunctionDoc), ForceBreak);

expr_([{atom, _, Atom} | Rest], Doc, ForceBreak) ->
  expr_(Rest, space(Doc, text(a2b(Atom))), ForceBreak);

expr_([{var, _, Var} | Rest], Doc, ForceBreak) ->
  expr_(Rest, space(Doc, text(v2b(Var))), ForceBreak);

expr_([{integer, _, Integer} | Rest], Doc, ForceBreak) ->
  expr_(Rest, space(Doc, text(i2b(Integer))), ForceBreak);

expr_([{float, _, Float} | Rest], Doc, ForceBreak) ->
  expr_(Rest, space(Doc, text(f2b(Float))), ForceBreak);

expr_([{string, _, Var} | Rest], Doc, ForceBreak) ->
  expr_(Rest, space(Doc, text(s2b(Var))), ForceBreak);

expr_([{BoolOp, _} | Rest0], Doc, ForceBreak0) when ?IS_BOOL_CONCATENATOR(BoolOp) ->
  case get_until_any(['andalso', 'orelse'], Rest0) of
    {Tokens, [], not_found} ->
      {End, ForceBreak1, Expr} = expr_(Tokens, empty(), ForceBreak0),
      {End, ForceBreak1, space(Doc, group(space(text(op2b(BoolOp)), Expr)))};

    {Tokens, Rest1, EndToken} ->
      {_End, ForceBreak1, Expr} = expr_(Tokens, empty(), ForceBreak0),
      expr_([EndToken | Rest1], space(Doc, group(space(text(op2b(BoolOp)), Expr))), ForceBreak1)
  end;

expr_([{'|' = Op, _} | Rest0], Doc, ForceBreak0) ->
  case get_until_any(['|'], Rest0) of
    {Tokens, [], not_found} ->
      {End, ForceBreak1, Expr} = expr_(Tokens, empty(), ForceBreak0),
      {End, ForceBreak1, space(Doc, group(space(text(op2b(Op)), Expr)))};

    {Tokens, Rest1, EndToken} ->
      {_End, ForceBreak1, Expr} = expr_(Tokens, empty(), ForceBreak0),
      expr_([EndToken | Rest1], space(Doc, group(space(text(op2b(Op)), Expr))), ForceBreak1)
  end;

expr_([{comment, _, Comment}], Doc, _ForceBreak) ->
  {comment, force_break, space(Doc, comment(Comment))};

expr_([{comment, _, Comment} | Rest], Doc, _ForceBreak) ->
  expr_(Rest, space(Doc, comment(Comment)), force_break);

expr_([{'||', _} | Rest], Doc, ForceBreak0) ->
  % Handle list comprehensions
  {End, ForceBreak1, Expr} = expr_(Rest, text(<<"||">>), ForceBreak0),
  {End, ForceBreak1, group(space(Doc, group(Expr)))};

expr_([{Op, _} | Rest], Doc0, ForceBreak) ->
  Doc1 = space(Doc0, text(op2b(Op))),
  expr_(Rest, Doc1, ForceBreak);

expr_([{char, _, Char} | Rest], Doc, ForceBreak) ->
  expr_(Rest, space(Doc, text(c2b(Char))), ForceBreak).

%%
%% Document Formatting
%%

-spec format(integer(), integer(), list({integer(), mode(), doc()})) -> sdoc().
format(_, _, []) -> s_nil;
format(W, K, [{_, _, doc_nil} | Rest]) -> format(W, K, Rest);
format(W, K, [{I, M, {doc_cons, X, Y}} | Rest]) -> format(W, K, [{I, M, X}, {I, M, Y} | Rest]);
format(W, K, [{_, M, {doc_underneath, J, X}} | Rest]) -> format(W, K, [{K + J, M, X} | Rest]);
format(W, K, [{I, M, {doc_nest, J, X}} | Rest]) -> format(W, K, [{I + J, M, X} | Rest]);
format(W, K, [{_, _, {doc_text, S}} | Rest]) -> {s_text, S, format(W, K + byte_size(S), Rest)};
format(W, _, [{I, flat, {doc_break, ?nl}} | Rest]) -> {s_text, ?nl, format(W, I, Rest)};
format(W, _, [{I, flat, {doc_break, ?two_nl}} | Rest]) -> {s_text, ?two_nl, format(W, I, Rest)};
format(W, K, [{_, flat, {doc_break, S}} | Rest]) -> {s_text, S, format(W, K + byte_size(S), Rest)};

format(W, _, [{I, break, {doc_break, ?two_nl}} | Rest]) ->
  {s_line, 0, {s_line, I, format(W, I, Rest)}};

format(W, _, [{I, break, {doc_break, _}} | Rest]) -> {s_line, I, format(W, I, Rest)};
%
% Lookahead.
% If this group has bottom padding and doesn't fit and the next group has top padding and doesn't
% fit, we want the skip the bottom padding for this group else we'll get double padding.
format(
  W,
  K,
  [
    {I, _, {doc_group, {doc_pad_group, X, Padding, _}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_cons, {doc_group, {doc_pad_group, Y, LookaheadPadding, _}, self}, _}
      }
    } = Next | Rest0
  ]
)
when (Padding == bottom orelse Padding == both)
     andalso (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  fiddle_padding(W, K, I, X, Padding, Y, Rest);

format(
  W,
  K,
  [
    {I, _, {doc_group, {doc_pad_group, X, Padding, _}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_cons, {doc_group, {doc_force_break, {doc_pad_group, _, LookaheadPadding, _}}, self}, _}
      }
    } = Next | Rest0
  ]
)
when (Padding == bottom orelse Padding == both)
     andalso (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  case fits(W - K, [{I, flat, X}]) of
    true -> format(W, K, [{I, flat, X} | Rest]);

    false ->
      case Padding of
        both -> format(W, K, pad(I, X, top) ++ Rest);
        bottom -> format(W, K, [{I, break, X} | Rest])
      end
  end;

format(
  W,
  K,
  [
    {I, _, {doc_group, {doc_pad_group, X, Padding, _}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_group, {doc_force_break, {doc_pad_group, _, LookaheadPadding, _}}, self}
      }
    } = Next | Rest0
  ]
)
when (Padding == bottom orelse Padding == both)
     andalso (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  case fits(W - K, [{I, flat, X}]) of
    true -> format(W, K, [{I, flat, X} | Rest]);

    false ->
      case Padding of
        both -> format(W, K, pad(I, X, top) ++ Rest);
        bottom -> format(W, K, [{I, break, X} | Rest])
      end
  end;

% More Lookahead
format(
  W,
  K,
  [
    {I, _, {doc_group, {doc_pad_group, X, Padding, _}, self}},
    {_, _, {doc_cons, {doc_break, _}, {doc_group, {doc_pad_group, Y, LookaheadPadding, _}, self}}} =
      Next | Rest0
  ]
)
when (Padding == bottom orelse Padding == both)
     andalso (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  fiddle_padding(W, K, I, X, Padding, Y, Rest);

% ForceBreak Lookahead
format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, bottom, Inherit}}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_group, {doc_force_break, {doc_pad_group, _, LookaheadPadding, _}}, self}
      }
    } = Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  % Both broken, so skip bottom padding for this one.
  format(W, K, [{I, M, {doc_force_break, {doc_group, X, Inherit}}}, Next | Rest0]);

format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, bottom, Inherit}}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_cons, {doc_group, {doc_force_break, {doc_pad_group, _, LookaheadPadding, _}}, self}, _}
      }
    } = Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  % Both broken, so skip bottom padding for this one.
  format(W, K, [{I, M, {doc_force_break, {doc_group, X, Inherit}}}, Next | Rest0]);

% ForceBreak Lookahead
format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, both, Inherit}}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_group, {doc_force_break, {doc_pad_group, _, LookaheadPadding, _}}, self}
      }
    } = Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  % Both broken, so skip bottom padding for this one.
  format(W, K, [{I, M, {doc_force_break, {doc_pad_group, X, top, Inherit}}}, Next | Rest0]);

format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, both, Inherit}}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_cons, {doc_group, {doc_force_break, {doc_pad_group, _, LookaheadPadding, _}}, self}, _}
      }
    } = Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  % Both broken, so skip bottom padding for this one.
  format(W, K, [{I, M, {doc_force_break, {doc_pad_group, X, top, Inherit}}}, Next | Rest0]);

% More ForceBreak Lookahead
format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, bottom, Inherit}}, self}},
    {_, _, {doc_cons, {doc_break, _}, {doc_group, {doc_pad_group, Y, LookaheadPadding, _}, self}}} =
      Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  case fits(W - K, [{I, flat, Y}]) of
    true ->
      % Keep padding
      format(W, K, pad(I, X, bottom) ++ Rest);

    false ->
      % Remove bottom padding
      format(W, K, [{I, M, {doc_force_break, {doc_group, X, Inherit}}} | Rest])
  end;

% More ForceBreak Lookahead
format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, both, Inherit}}, self}},
    {_, _, {doc_cons, {doc_break, _}, {doc_group, {doc_pad_group, Y, LookaheadPadding, _}, self}}} =
      Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  case fits(W - K, [{I, flat, Y}]) of
    true ->
      % Keep padding
      format(W, K, pad(I, X, both) ++ Rest);

    false ->
      % Remove bottom padding
      format(W, K, [{I, M, {doc_force_break, {doc_pad_group, X, top, Inherit}}} | Rest])
  end;

% Even More ForceBreak Lookahead
format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, bottom, Inherit}}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_cons, {doc_group, {doc_pad_group, Y, LookaheadPadding, _}, self}, _}
      }
    } = Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  case fits(W - K, [{I, flat, Y}]) of
    true ->
      % Keep padding
      format(W, K, pad(I, X, bottom) ++ Rest);

    false ->
      % Remove bottom padding
      format(W, K, [{I, M, {doc_force_break, {doc_group, X, Inherit}}} | Rest])
  end;

% Even More ForceBreak Lookahead
format(
  W,
  K,
  [
    {I, M, {doc_group, {doc_force_break, {doc_pad_group, X, both, Inherit}}, self}},
    {
      _,
      _,
      {
        doc_cons,
        {doc_break, _},
        {doc_cons, {doc_group, {doc_pad_group, Y, LookaheadPadding, _}, self}, _}
      }
    } = Next | Rest0
  ]
)
when (LookaheadPadding == top orelse LookaheadPadding == both) ->
  Rest = [Next | Rest0],
  case fits(W - K, [{I, flat, Y}]) of
    true ->
      % Keep padding
      format(W, K, pad(I, X, both) ++ Rest);

    false ->
      % Remove bottom padding
      format(W, K, [{I, M, {doc_force_break, {doc_pad_group, X, top, Inherit}}} | Rest])
  end;

format(W, K, [{I, _, {doc_force_break, X}} | Rest]) -> format(W, K, [{I, break, X} | Rest]);
format(W, K, [{I, break, {doc_group, X, inherit}} | Rest]) -> format(W, K, [{I, break, X} | Rest]);

format(W, K, [{I, _, {doc_group, X, _}} | Rest]) ->
  case fits(W - K, [{I, flat, X}]) of
    true -> format(W, K, [{I, flat, X} | Rest]);
    false -> format(W, K, [{I, break, X} | Rest])
  end;

format(W, K, [{I, break, {doc_pad_group, X, Padding, inherit}} | Rest]) ->
  format(W, K, pad(I, X, Padding) ++ Rest);

format(W, K, [{I, _, {doc_pad_group, X, Padding, _}} | Rest]) ->
  case fits(W - K, [{I, flat, X}]) of
    true -> format(W, K, [{I, flat, X} | Rest]);
    false -> format(W, K, pad(I, X, Padding) ++ Rest)
  end.


-spec pad(integer(), doc(), padding()) -> list({integer(), mode(), doc()}).
pad(I, X, top) -> [{I, break, {doc_break, ?nl}}, {I, break, X}];
pad(I, X, bottom) -> [{I, break, X}, {I, flat, {doc_break, ?nl}}];
pad(I, X, both) -> [{I, break, {doc_break, ?nl}}, {I, break, X}, {I, flat, {doc_break, ?nl}}].

fiddle_padding(W, K, I, X, Padding, Y, Rest) ->
  case {fits(W - K, [{I, flat, X}]), fits(W - K, [{I, flat, Y}])} of
    {true, _} -> format(W, K, [{I, flat, X} | Rest]);
    {false, true} -> format(W, K, pad(I, X, Padding) ++ Rest);

    {false, false} ->
      case Padding of
        both -> format(W, K, pad(I, X, top) ++ Rest);
        bottom -> format(W, K, [{I, break, X} | Rest])
      end
  end.


-spec fits(integer(), list({integer(), mode(), doc()})) -> boolean().
fits(W, _) when W < 0 -> false;
fits(_, []) -> true;
fits(W, [{_, _, doc_nil} | Rest]) -> fits(W, Rest);
fits(W, [{I, M, {doc_cons, X, Y}} | Rest]) -> fits(W, [{I, M, X}, {I, M, Y} | Rest]);
fits(W, [{I, M, {doc_underneath, J, X}} | Rest]) -> fits(W, [{I + J, M, X} | Rest]);
fits(W, [{I, M, {doc_nest, J, X}} | Rest]) -> fits(W, [{I + J, M, X} | Rest]);
fits(W, [{_, _, {doc_text, S}} | Rest]) -> fits(W - byte_size(S), Rest);
fits(_, [{_, flat, {doc_break, ?nl}} | _]) -> true;
fits(W, [{_, flat, {doc_break, S}} | Rest]) -> fits(W - byte_size(S), Rest);
% This clause is impossible according to the research paper and dialyzer agrees.
%fits(_, [{_, break, {doc_break, _}} | _Rest]) -> throw(impossible);
fits(_, [{_, _, {doc_force_break, _}} | _]) -> true;
fits(W, [{I, _, {doc_group, X, _}} | Rest]) -> fits(W, [{I, flat, X} | Rest]);
fits(W, [{I, _, {doc_pad_group, X, _, _}} | Rest]) -> fits(W, [{I, flat, X} | Rest]).

-spec sdoc_to_string(sdoc()) -> binary().
sdoc_to_string(s_nil) -> <<"">>;

sdoc_to_string({s_text, String, Doc}) ->
  DocString = sdoc_to_string(Doc),
  <<String/binary, DocString/binary>>;

sdoc_to_string({s_line, _Indent, {s_text, <<"">>, {s_line, _, _} = Doc}}) ->
  DocString = sdoc_to_string(Doc),
  <<"\n", DocString/binary>>;

sdoc_to_string({s_line, _Indent, {s_line, _, _} = Doc}) ->
  DocString = sdoc_to_string(Doc),
  <<"\n", DocString/binary>>;

sdoc_to_string({s_line, Indent, Doc}) ->
  Prefix = repeat(?sp, Indent),
  DocString = sdoc_to_string(Doc),
  <<"\n", Prefix/binary, DocString/binary>>.

%%
%% Binary Conversion
%%

-spec op2b(atom()) -> binary().
op2b(dot) -> ?dot;
op2b(Atom) -> list_to_binary(atom_to_list(Atom)).

-spec v2b(atom()) -> binary().
v2b(Atom) -> list_to_binary(atom_to_list(Atom)).

-spec a2b(atom()) -> binary().
% Escape atoms so that atoms such as '{' are converted correctly to binary.
a2b(Atom) -> unicode:characters_to_binary(io_lib:write_atom(Atom)).

-spec i2b(integer()) -> binary().
i2b(Integer) -> integer_to_binary(Integer).

-spec f2b(float()) -> binary().
f2b(Float) -> list_to_binary(io_lib:format("~p", [Float])).

-spec n2b(token()) -> binary().
n2b({integer, _, Integer}) -> i2b(Integer);
n2b({float, _, Float}) -> f2b(Float).

-spec c2b(integer()) -> binary().
c2b(Char) -> unicode:characters_to_binary(io_lib:write_char(Char)).

-spec s2b(string()) -> binary().
s2b("") -> <<"\"\"">>;
s2b(String) -> unicode:characters_to_binary(io_lib:write_string(String)).

%%
%% Utils
%%

-spec repeat(binary(), integer()) -> binary().
repeat(Bin, Times) when Times >= 0 -> repeat_(<<>>, Bin, Times).

-spec repeat_(binary(), binary(), integer()) -> binary().
repeat_(Acc, _, 0) -> Acc;
repeat_(Acc, Bin, Times) -> repeat_(<<Acc/binary, Bin/binary>>, Bin, Times - 1).

-spec get_until_end(tokens()) -> {tokens(), tokens(), token()}.
get_until_end(Tokens) -> get_until('end', Tokens).

-spec get_until(atom(), tokens()) -> {tokens(), tokens(), token()}.
get_until(End, Tokens) -> get_until(End, Tokens, [], [], []).

-spec get_until(atom(), tokens(), tokens(), list(atom()), list(atom())) ->
  {tokens(), tokens(), token()}.
get_until(_End, [Token], Acc, _BracketStack, _KeywordStack) -> {lists:reverse(Acc), [], Token};

get_until(_End, [{dot, _} = Token | Rest], Acc, _, _) ->
  % An unexpected dot probably means there's an unused macro which has invalid syntax.
  {lists:reverse(Acc), Rest, Token};

get_until(End, [{End, _} = Token | Rest], Acc, [], _KeywordStack) when ?IS_LIST_CLOSE_CHAR(End) ->
  % The keyword stack is not necessarily empty here.
  % If it's not empty then we probably have some funny macro syntax.
  % That's dealt with elsewhere.
  {lists:reverse(Acc), Rest, Token};

get_until(End, [{End, _} = Token | Rest], Acc, [], []) -> {lists:reverse(Acc), Rest, Token};

get_until(End, [{'end', _} = Token | Rest], Acc, BracketStack, [Keyword | KeywordStack])
when ?IS_TERMINATED_KEYWORD(Keyword) ->
  get_until(End, Rest, [Token | Acc], BracketStack, KeywordStack);

get_until(End, [{C, _} = Token | Rest], Acc, [C | BracketStack], KeywordStack)
when ?IS_LIST_CLOSE_CHAR(C) ->
  % Close bracket
  get_until(End, Rest, [Token | Acc], BracketStack, KeywordStack);

get_until(End, [{C, _} = Token | Rest], Acc, BracketStack, KeywordStack) when ?IS_LIST_OPEN_CHAR(C) ->
  % If we hit an open bracket we ignore until the close bracket
  get_until(End, Rest, [Token | Acc], [close_bracket(C) | BracketStack], KeywordStack);

get_until(
  End,
  [{'fun', _} = Token, {'(', _}, {')', _}, {Op, _} | _] = Rest0,
  Acc,
  BracketStack,
  KeywordStack
)
when Op == '|' orelse Op == dot orelse Op == ',' orelse Op == '}' orelse Op == ')' ->
  % 'fun' without 'end'
  % -type x() :: fun().
  % or
  % -type x() :: fun() | y().
  % or
  % -record(x, {y :: fun(), z}).
  % or
  % -type x() :: {y(), fun()}.
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(
  End,
  [{'fun', _} = Token, {'(', _}, {'(', _} | _] = Rest0,
  Acc,
  BracketStack,
  KeywordStack
) ->
  % 'fun' without 'end'
  % fun((Arg :: type) -> other_type())
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(End, [{'fun', _} = Token, {atom, _, _} | _] = Rest0, Acc, BracketStack, KeywordStack) ->
  % 'fun' without 'end'
  % fun local/1
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(
  End,
  [{'fun', _} = Token, {var, _, _}, {'/', _} | _] = Rest0,
  Acc,
  BracketStack,
  KeywordStack
) ->
  % 'fun' without 'end'
  % fun X/1
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(
  End,
  [{'fun', _} = Token, {'?', _}, {var, _, _}, {':', _} | _] = Rest0,
  Acc,
  BracketStack,
  KeywordStack
) ->
  % 'fun' without 'end'
  % fun ?MACRO:x/1
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(
  End,
  [{'fun', _} = Token, {'?', _}, {var, _, _}, {'/', _} | _] = Rest0,
  Acc,
  BracketStack,
  KeywordStack
) ->
  % 'fun' without 'end'
  % fun ?MACRO/1
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(
  End,
  [{'fun', _} = Token, {var, _, _}, {':', _} | _] = Rest0,
  Acc,
  BracketStack,
  KeywordStack
) ->
  % 'fun' without 'end'
  % fun Var:x/1
  Rest1 = tl(Rest0),
  get_until(End, Rest1, [Token | Acc], BracketStack, KeywordStack);

get_until(End, [{Keyword, _} = Token | Rest], Acc, BracketStack, KeywordStack)
when ?IS_TERMINATED_KEYWORD(Keyword) ->
  % If a 'fun' keyword gets through to here then we expect it to have an end.
  % We need to be a bit careful, something like this is valid syntax:
  %
  % `fun F() -> do_stuff end`
  %
  get_until(End, Rest, [Token | Acc], BracketStack, [Keyword | KeywordStack]);

get_until(End, [Token | Rest], Acc, BracketStack, KeywordStack) ->
  get_until(End, Rest, [Token | Acc], BracketStack, KeywordStack).


-spec get_until_of(tokens()) -> {tokens(), tokens(), token()}.
get_until_of(Tokens) -> get_until_of(Tokens, [], []).

-spec get_until_of(tokens(), tokens(), list(atom())) -> {tokens(), tokens(), token()}.
get_until_of([], Acc, []) -> {lists:reverse(Acc), [], not_found};
get_until_of([{'of', _} = Token | Rest], Acc, []) -> {lists:reverse(Acc), Rest, Token};

get_until_of([{'of', _} = Token | Rest], Acc, ['try' | _] = Stack) ->
  get_until_of(Rest, [Token | Acc], Stack);

get_until_of([{'of', _} = Token | Rest], Acc, Stack) ->
  get_until_of(Rest, [Token | Acc], tl(Stack));

get_until_of([{Op, _} = Token | Rest], Acc, ['try' | Stack]) when Op == 'catch' orelse Op == 'after' ->
  % Only a 'catch' or an 'after' can remove the 'try' from the stack.
  get_until_of(Rest, [Token | Acc], Stack);

get_until_of([{Keyword, _} = Token | Rest], Acc, Stack) when ?IS_OF_KEYWORD(Keyword) ->
  get_until_of(Rest, [Token | Acc], [Keyword | Stack]);

get_until_of([Token | Rest], Acc, Stack) -> get_until_of(Rest, [Token | Acc], Stack).


-spec get_until_any(list(atom()), tokens()) -> {tokens(), tokens(), token() | not_found}.
get_until_any(Ends, Tokens) -> get_until_any(Ends, Tokens, [], []).

-spec get_until_any(list(atom()), tokens(), tokens(), list(atom())) ->
  {tokens(), tokens(), token() | not_found}.
get_until_any(_Ends, [], Acc, []) -> {lists:reverse(Acc), [], not_found};

get_until_any(Ends, [{Open, _} = Token | Rest], Acc, Stack) when ?IS_LIST_OPEN_CHAR(Open) ->
  Close = close_bracket(Open),
  get_until_any(Ends, Rest, [Token | Acc], [Close | Stack]);

get_until_any(Ends, [{CloseBracket, _} = Token | Rest], Acc, [CloseBracket | Stack]) ->
  get_until_any(Ends, Rest, [Token | Acc], Stack);

get_until_any(Ends, [{MaybeEnd, _} = Token | Rest], Acc, [] = Stack) ->
  case lists:member(MaybeEnd, Ends) of
    true -> {lists:reverse(Acc), Rest, Token};
    false -> get_until_any(Ends, Rest, [Token | Acc], Stack)
  end;

get_until_any(Ends, [Token | Rest], Acc, Stack) -> get_until_any(Ends, Rest, [Token | Acc], Stack).


-spec remove_matching(atom(), atom(), tokens()) -> tokens().
remove_matching(Start, End, Tokens) -> remove_matching(Start, End, Tokens, [], 0).

-spec remove_matching(atom(), atom(), tokens(), tokens(), integer()) -> tokens().
remove_matching(Start, End, [{Start, _} = Token | Rest], Acc, Stack) ->
  remove_matching(Start, End, Rest, [Token | Acc], Stack + 1);

remove_matching(_Start, End, [{End, _} | Rest], Acc, 0) -> lists:reverse(Acc) ++ Rest;

remove_matching(Start, End, [{End, _} = Token | Rest], Acc, Stack) ->
  remove_matching(Start, End, Rest, [Token | Acc], Stack - 1);

remove_matching(Start, End, [Token | Rest], Acc, Stack) ->
  remove_matching(Start, End, Rest, [Token | Acc], Stack).

-spec get_end_of_expr(tokens()) -> {tokens(), tokens()}.
get_end_of_expr(Tokens) -> get_end_of_expr(Tokens, [], 0, [], no_when).

% Dialyzer gets upset if we use integer() for the third arg here but that's what it is.
-spec get_end_of_expr(tokens(), tokens(), any(), list(atom()), no_when | when_guard | when_type) ->
  {tokens(), tokens()}.
% We consider the end of an expression to be any ',', ';', or 'dot'.
% If we find an end-terminated keyword, we put it onto the keyword stack and continue until
% we find the matching 'end'.
% If we find a 'when', we ignore any commas and semicolons until the corresponding '->',
% unless we find a '::', which means the 'when' is part of a type spec and so is terminated by
% a ';' or a dot.
get_end_of_expr([], Acc, _LineNum, _KeywordStack, _When) -> {lists:reverse(Acc), []};

get_end_of_expr([{comment, _, _} = Comment | Rest], [], _LineNum, _KeywordStack, _) ->
  {[Comment], Rest};

get_end_of_expr([{comment, LineNum, _} = Comment | Rest], Acc, LineNum, [], _) ->
  % Inline comment - naughty naughty
  % Return the comment and put the acc back.
  % We must only do this when the keyword stack is empty otherwise we'll put the comment
  % in the wrong place.
  {[Comment], lists:reverse(Acc) ++ Rest};

get_end_of_expr([{comment, _, _} = Comment | Rest] = Tokens, Acc, _, [], _) ->
  case only_comments_left(Rest) of
    true ->
      % If there are only comments left we leave them alone. Trailing comments
      % are handled elsewhere.
      {lists:reverse(Acc), Tokens};

    false ->
      % This can happen if you have something ludicrous like:
      %
      % ```
      % X = 1
      % % great comment
      % ,
      % ```
      %
      % Return the comment and put the acc back.
      % We must only do this when the keyword stack is empty otherwise we'll put the comment
      % in the wrong place.
      {[Comment], lists:reverse(Acc) ++ Rest}
  end;

get_end_of_expr([{End, LineNum} = Token, {comment, LineNum, _} = Comment | Rest], Acc, _, [], _)
when End == ',' orelse End == ';' orelse End == dot ->
  % Inline comment - naughty naughty
  % Return the comment and put the acc back.
  % We must only do this when the keyword stack is empty otherwise we'll put the comment
  % in the wrong place.
  {[Comment], lists:reverse([Token | Acc]) ++ Rest};

get_end_of_expr([{End, _} = Token | Rest], Acc, _, [], no_when)
when End == ',' orelse End == ';' orelse End == dot ->
  {lists:reverse([Token | Acc]), Rest};

get_end_of_expr([{dot, _} = Token | Rest], Acc, _, [], when_guard) ->
  {lists:reverse([Token | Acc]), Rest};

get_end_of_expr([{End, _} = Token | Rest], Acc, _, [], when_type) when End == ';' orelse End == dot ->
  {lists:reverse([Token | Acc]), Rest};

get_end_of_expr([{'end', _} = Token | Rest], Acc, _LineNum, [], _) ->
  {lists:reverse([Token | Acc]), Rest};

get_end_of_expr([{'when', LineNum} = Token | Rest], Acc, _, KeywordStack, no_when) ->
  get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack, when_guard);

get_end_of_expr([{'->', LineNum} = Token | Rest], Acc, _, KeywordStack, when_guard) ->
  get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack, no_when);

get_end_of_expr([{'::', LineNum} = Token | Rest], Acc, _, KeywordStack, when_guard) ->
  get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack, when_type);

get_end_of_expr([{'end', _} = Token | Rest], Acc, LineNum, KeywordStack, Guard) ->
  get_end_of_expr(Rest, [Token | Acc], LineNum, tl(KeywordStack), Guard);

get_end_of_expr(
  [{'fun', LineNum} = Token, {'(', _}, {'(', _} | _] = Rest0,
  Acc,
  _,
  KeywordStack,
  Guard
) ->
  % 'fun' without 'end'
  % fun((Arg :: type) -> other_type())
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr(
  [{'fun', LineNum} = Token, {'(', _}, {')', _}, {Op, _} | _] = Rest0,
  Acc,
  _,
  KeywordStack,
  Guard
)
when Op == dot orelse Op == '|' orelse Op == ',' orelse Op == '}' orelse Op == ')' ->
  % 'fun' without 'end'
  % -type x() :: fun().
  % or
  % -type x() :: fun() | y().
  % or
  % -record(x, {y :: fun(), z}).
  % or
  % -type x() :: {y(), fun()}.
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr([{'fun', LineNum} = Token, {atom, _, _} | _] = Rest0, Acc, _, KeywordStack, Guard) ->
  % 'fun' without 'end'
  % fun local/1
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr(
  [{'fun', LineNum} = Token, {var, _, _}, {'/', _} | _] = Rest0,
  Acc,
  _,
  KeywordStack,
  Guard
) ->
  % 'fun' without 'end'
  % fun X/1
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr(
  [{'fun', LineNum} = Token, {'?', _}, {var, _, _}, {':', _} | _] = Rest0,
  Acc,
  _,
  KeywordStack,
  Guard
) ->
  % 'fun' without 'end'
  % fun ?MACRO:x/1
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr(
  [{'fun', LineNum} = Token, {'?', _}, {var, _, _}, {'/', _} | _] = Rest0,
  Acc,
  _,
  KeywordStack,
  Guard
) ->
  % 'fun' without 'end'
  % fun ?MACRO/1
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr(
  [{'fun', LineNum} = Token, {var, _, _}, {':', _} | _] = Rest0,
  Acc,
  _,
  KeywordStack,
  Guard
) ->
  % 'fun' without 'end'
  % fun Var:x/1
  Rest1 = tl(Rest0),
  get_end_of_expr(Rest1, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr([{Keyword, LineNum} = Token | Rest], Acc, _, KeywordStack, Guard)
when ?IS_TERMINATED_KEYWORD(Keyword) ->
  % If a 'fun' keyword gets through to here then we expect it to have an end.
  % We need to be a bit careful, something like this is valid syntax:
  %
  % `fun F() -> do_stuff end`
  %
  get_end_of_expr(Rest, [Token | Acc], LineNum, [Keyword | KeywordStack], Guard);

get_end_of_expr([{Open, _} = Token | Rest0], Acc, _, KeywordStack, Guard)
when ?IS_LIST_OPEN_CHAR(Open) ->
  Close = close_bracket(Open),
  {Tokens, Rest1, {Close, LineNum} = EndToken} = get_until(Close, Rest0),
  get_end_of_expr(
    Rest1,
    [EndToken] ++ lists:reverse(Tokens) ++ [Token | Acc],
    LineNum,
    KeywordStack,
    Guard
  );

get_end_of_expr([{_, LineNum} = Token | Rest], Acc, _, KeywordStack, Guard) ->
  get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack, Guard);

get_end_of_expr([{_, LineNum, _} = Token | Rest], Acc, _, KeywordStack, Guard) ->
  get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack, Guard).


-spec resolve_force_break(list(force_break())) -> force_break().
resolve_force_break(Args) ->
  case lists:any(fun (X) -> X == force_break end, Args) of
    true -> force_break;
    false -> no_force_break
  end.


-spec is_bool_list(tokens()) -> boolean().
is_bool_list(Tokens) -> is_bool_list(Tokens, []).

-spec is_bool_list(tokens(), list(atom())) -> boolean().
is_bool_list([], []) -> false;
is_bool_list([{Op, _} | _], []) when ?IS_TERMINATED_KEYWORD(Op) -> false;
is_bool_list([{Op, _} | _], []) when ?IS_BOOL_CONCATENATOR(Op) -> true;

is_bool_list([{C, _} | Rest], Stack) when ?IS_LIST_OPEN_CHAR(C) ->
  is_bool_list(Rest, [close_bracket(C) | Stack]);

is_bool_list([{C, _} | Rest], [C | Stack]) when ?IS_LIST_CLOSE_CHAR(C) -> is_bool_list(Rest, Stack);
is_bool_list([_ | Rest], Stack) -> is_bool_list(Rest, Stack).

-spec close_bracket(atom()) -> atom().
close_bracket('(') -> ')';
close_bracket('[') -> ']';
close_bracket('{') -> '}';
close_bracket('<<') -> '>>'.

-spec only_comments_left(tokens()) -> boolean().
only_comments_left([]) -> true;
only_comments_left([{comment, _, _} | Rest]) -> only_comments_left(Rest);
only_comments_left(_) -> false.

%%
%% Testing
%%

test_binop(Left, Op, Right, Indent) ->
  group(nest(Indent, space(group(space(text(Left), text(Op))), text(Right)))).

test_ifthen(C, E1, E2, Indent) ->
  group(
    space(
      [
        group(nest(Indent, space(text(<<"if">>), C))),
        group(nest(Indent, space(text(<<"then">>), E1))),
        group(nest(Indent, space(text(<<"else">>), E2)))
      ]
    )
  ).
