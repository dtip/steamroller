%%
%% @doc An implementation of ["Strictly Pretty" (2000) by Christian Lindig][0].
%%
%% Inspired by the Elixir implementation of the same paper in Inspect.Algebra. Thanks to the core team for their hard work!
%%
%% [0] https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
%%
-module(steamroll_algebra).

-export([format_tokens/1, format_tokens/2, generate_doc/1, pretty/1, pretty/2]).
% Testing
-export([repeat/2, from_the_paper/2]).

-type doc() :: doc_nil
            | {doc_cons, doc(), doc()}
            | {doc_text, binary()}
            | {doc_nest, integer(), doc()}
            | {doc_break, binary()}
            | {doc_group, doc(), inherit()}.

-type sdoc() :: s_nil | {s_text, binary(), sdoc()} | {s_line, binary(), sdoc()}.
-type mode() :: flat | break.
-type inherit() :: self | inherit.
-type tokens() :: steamroll_ast:tokens().

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(two_nl, <<"\n\n">>).
-define(dot, <<".">>).
-define(max_width, 100).
-define(indent, 4).

-define(IS_LIST_CHAR(C), (C == '(' orelse C == '{' orelse C == '[')).

%% API

-spec format_tokens(tokens()) -> binary().
format_tokens(Tokens) -> format_tokens(Tokens, ?max_width).

-spec format_tokens(tokens(), integer()) -> binary().
format_tokens(Tokens, Width) ->
    Doc = generate_doc(Tokens),
    pretty(Doc, Width).

-spec generate_doc(tokens()) -> doc().
generate_doc(Tokens) -> generate_doc_(Tokens, empty()).

-spec pretty(doc()) -> binary().
pretty(Doc) ->
    SDoc = format(?max_width, 0, [{0, flat, group(Doc)}]),
    String = sdoc_to_string(SDoc),
    <<String/binary, "\n">>.

-spec pretty(doc(), integer()) -> binary().
pretty(Doc, Width) ->
    SDoc = format(Width, 0, [{0, flat, group(Doc)}]),
    String = sdoc_to_string(SDoc),
    <<String/binary, "\n">>.

% Used for testing.
-spec from_the_paper(integer(), integer()) -> binary().
from_the_paper(Width, Indent) ->
    C = test_binop(<<"a">>, <<"==">>, <<"b">>, Indent),
    E1 = test_binop(<<"a">>, <<"<<">>, <<"2">>, Indent),
    E2 = test_binop(<<"a">>, <<"+">>, <<"b">>, Indent),
    Doc = test_ifthen(C, E1, E2, Indent),
    pretty(Doc, Width).

%% Constructor Functions

-spec cons(doc(), doc()) -> doc().
cons(X, Y) -> {doc_cons, X, Y}.

-spec empty() -> doc().
empty() -> doc_nil.

-spec text(binary()) -> doc().
text(S) -> {doc_text, S}.

-spec nest(integer(), doc()) -> doc().
nest(I, X) -> {doc_nest, I, X}.

%-spec break() -> doc().
%break() -> {doc_break, ?sp}.

-spec break(binary()) -> doc().
break(S) -> {doc_break, S}.

-spec group(doc()) -> doc().
group(D) -> {doc_group, D, self}.

%% Group inheritance is lifted from the Elixir algebra implementation.
-spec group(doc(), inherit()) -> doc().
group(D, Inherit) -> {doc_group, D, Inherit}.

%% Operators

-spec space(doc(), doc()) -> doc().
space(X, Y) -> concat(X, Y, ?sp).

-spec space(list(doc())) -> doc().
space([X]) -> X;
space([X, Y]) -> space(X, Y);
space([X | Rest]) -> space(X, space(Rest)).

-spec stick(doc(), doc()) -> doc().
stick(X, Y) -> concat(X, Y, <<>>).

%-spec stick(list(doc())) -> doc().
%stick([X, Y]) -> stick(X, Y);
%stick([X | Rest]) -> stick(X, stick(Rest)).

-spec newline(doc(), doc()) -> doc().
newline(X, Y) -> concat(X, Y, ?nl).

-spec newlines(doc(), doc()) -> doc().
newlines(X, Y) -> concat(X, Y, ?two_nl).

%-spec nested_newline(doc(), doc()) -> doc().
%nested_newline(X, Y) -> concat(X, nest(?indent, Y), ?nl).

-spec concat(doc(), doc(), binary()) -> doc().
concat(doc_nil, Y, _) -> Y;
concat(X, doc_nil, _) -> X;
concat(X, Y, Break) -> cons(X, cons(break(Break), Y)).

%% Erlang Source Elements

-spec attribute(atom(), tokens()) -> {doc(), tokens()}.
attribute(Att, Tokens) ->
    {Expr, [{dot, _} | Rest]} = list_group(Tokens),
    {group(cons(text(<<"-">>), cons(text(a2b(Att)), cons(Expr, text(?dot))))), Rest}.

-spec list_group(tokens()) -> {doc(), tokens()}.
list_group([{'(', _} | Rest0]) ->
    {Tokens, Rest1} = get_until(')', Rest0),
    {brackets(Tokens, <<"(">>, <<")">>), Rest1};
list_group([{'{', _} | Rest0]) ->
    {Tokens, Rest1} = get_until('}', Rest0),
    {brackets(Tokens, <<"{">>, <<"}">>), Rest1};
list_group([{'[', _} | Rest0]) ->
    {Tokens, Rest1} = get_until(']', Rest0),
    {brackets(Tokens, <<"[">>, <<"]">>), Rest1}.


-spec brackets(tokens(), binary(), binary()) -> doc().
brackets([], Open, Close) ->
    group(cons(text(Open), text(Close)));
brackets(Tokens, Open, Close) ->
    group(
      stick(
        nest(
          ?indent,
          stick(
            text(Open),
            space(
              list_elements(Tokens)
            )
          )
        ),
      text(Close)
      )
    ).

-spec list_elements(tokens()) -> list(doc()).
list_elements(Tokens) -> list_elements(Tokens, []).

-spec list_elements(tokens(), list(doc())) -> list(doc()).
list_elements([], Acc) -> lists:reverse(Acc);
list_elements([{C, _} | _] = Tokens, Acc) when ?IS_LIST_CHAR(C) ->
    {Group, Rest} = list_group(Tokens),
    list_elements(Rest, [Group | Acc]);
list_elements(Tokens, Acc) ->
    {Expr, Rest} = expr(Tokens),
    list_elements(Rest, [Expr | Acc]).

-spec clause(tokens()) -> {doc(), tokens()}.
clause(Tokens) ->
    {Doc, Rest} = clause(Tokens, empty()),
    {group(Doc, inherit), Rest}.

clause([], Doc) -> {Doc, []};
clause([{var, _, Var}, {'=', _} | Rest0], Doc0) ->
    Equals = group(space(text(a2b(Var)), text(<<"=">>))),
    {Expr, Rest1} = expr(Rest0),
    Equation = group(nest(?indent, (space(Equals, Expr)))),
    {Doc1, Rest2} = clause(Rest1),
    {space(newline(Doc0, Equation), Doc1), Rest2};
clause([{Token, _, Var}, {';', _} | Rest], Doc0) when Token == var orelse Token == atom ->
    Line = cons(text(a2b(Var)), text(<<";">>)),
    Doc1 = nest(?indent, space(Doc0, Line)),
    {Doc1, Rest};
clause([{Token, _, Var}, {dot, _} | Rest], Doc) when Token == var orelse Token == atom ->
    {space(Doc, cons(text(a2b(Var)), text(?dot))), Rest}.

-spec expr(tokens()) -> {doc(), tokens()}.
expr(Tokens) ->
    {ExprTokens, Rest} = get_end_of_expr(Tokens),
    {group(expr(ExprTokens, empty())), Rest}.

expr([], Doc) -> Doc;
expr([{End, _}], Doc) when End == ',' orelse End == 'dot' ->
    cons(Doc, text(a2b(End)));
expr([{var, _, Var}, {Op, _} | Rest], Doc0) when Op == '+' orelse Op == '-' ->
    Doc1 = space(Doc0, space(text(a2b(Var)), text(a2b(Op)))),
    expr(Rest, Doc1);
expr([{atom, _, Atom}, {'/', _}, {integer, _, Int} | Rest], Doc) ->
    FunctionDoc = cons(text(a2b(Atom)), cons(text(<<"/">>), text(i2b(Int)))),
    expr(Rest, space(Doc, FunctionDoc));
expr([{Token, _, Var} | Rest], Doc) when Token == var orelse Token == atom ->
    expr(Rest, space(Doc, text(a2b(Var)))).



%% Internal

-spec format(integer(), integer(), list({integer(), mode(), doc()})) -> sdoc().
format(_, _, []) -> s_nil;
format(W, K, [{_, _, doc_nil} | Rest]) -> format(W, K, Rest);
format(W, K, [{I, M, {doc_cons, X, Y}} | Rest]) -> format(W, K, [{I, M, X}, {I, M, Y} | Rest]);
format(W, K, [{I, M, {doc_nest, J, X}} | Rest]) -> format(W, K, [{I + J, M, X} | Rest]);
format(W, K, [{_, _, {doc_text, S}} | Rest]) -> {s_text, S, format(W, K + byte_size(S), Rest)};
format(W, K, [{_, flat, {doc_break, S}} | Rest]) -> {s_text, S, format(W, K + byte_size(S), Rest)};
format(W, _, [{I, break, {doc_break, ?two_nl}} | Rest]) -> {s_line, 0, {s_line, I, format(W, I, Rest)}};
format(W, _, [{I, break, {doc_break, _}} | Rest]) -> {s_line, I, format(W, I, Rest)};
format(W, K, [{I, M, {doc_group, X, inherit}} | Rest]) -> format(W, K, [{I, M, X} | Rest]);
format(W, K, [{I, _, {doc_group, X, self}} | Rest]) ->
    case fits(W - K, [{I, flat, X}]) of
        true ->
            format(W, K, [{I, flat, X} | Rest]);
        false ->
            format(W, K, [{I, break, X} | Rest])
    end.

-spec fits(integer(), list({integer(), mode(), doc()})) -> boolean().
fits(W, _) when W < 0 -> false;
fits(_, []) -> true;
fits(W, [{_, _, doc_nil} | Rest]) -> fits(W, Rest);
fits(W, [{I, M, {doc_cons, X, Y}} | Rest]) -> fits(W, [{I, M, X}, {I, M, Y} | Rest]);
fits(W, [{I, M, {doc_nest, J, X}} | Rest]) -> fits(W, [{I + J, M, X} | Rest]);
fits(W, [{_, _, {doc_text, S}} | Rest]) -> fits(W - byte_size(S), Rest);
fits(W, [{_, flat, {doc_break, S}} | Rest]) -> fits(W - byte_size(S), Rest);
% This clause is impossible according to the research paper and dialyzer agrees.
%fits(_, [{_, break, {doc_break, _}} | _Rest]) -> throw(impossible);
fits(W, [{I, M, {doc_group, X, inherit}} | Rest]) -> fits(W, [{I, M, X} | Rest]);
fits(W, [{I, _, {doc_group, X, self}} | Rest]) -> fits(W, [{I, flat, X} | Rest]).

-spec sdoc_to_string(sdoc()) -> binary().
sdoc_to_string(s_nil) -> <<"">>;
sdoc_to_string({s_text, String, Doc}) ->
    DocString = sdoc_to_string(Doc),
    <<String/binary, DocString/binary>>;
sdoc_to_string({s_line, Indent, Doc}) ->
    Prefix = repeat(?sp, Indent),
    DocString = sdoc_to_string(Doc),
    <<"\n", Prefix/binary, DocString/binary>>.

%% Token Consumption

-spec generate_doc_(tokens(), doc()) -> doc().
generate_doc_([], Doc) -> Doc;
generate_doc_([{'-', _}, {atom, _, Atom} | Tokens], Doc) ->
    % Module Attribute
    {Group, Rest} = attribute(Atom, Tokens),
    % Put a line gap between module attributes.
    generate_doc_(Rest, newlines(Doc, Group));
generate_doc_([{atom, _, Atom} | Rest], Doc) ->
    % Function
    generate_doc_(Rest, newline(Doc, text(a2b(Atom))));
generate_doc_([{C, _} | _] = Tokens, Doc) when ?IS_LIST_CHAR(C) ->
    {Group, Rest} = list_group(Tokens),
    generate_doc_(Rest, cons(Doc, Group));
generate_doc_([{'->', _} | Rest0], Doc) ->
    {Clause, Rest1} = clause(Rest0),
    generate_doc_(Rest1, cons(Doc, nest(?indent, space(text(<<" ->">>), Clause)))).

%% Utils

-spec repeat(binary(), integer()) -> binary().
repeat(Bin, Times) when Times >= 0 -> repeat_(<<>>, Bin, Times).

-spec repeat_(binary(), binary(), integer()) -> binary().
repeat_(Acc, _, 0) -> Acc;
repeat_(Acc, Bin, Times) -> repeat_(<<Acc/binary, Bin/binary>>, Bin, Times - 1).

-spec a2b(atom()) -> binary().
a2b(dot) -> ?dot;
a2b(Atom) -> list_to_binary(atom_to_list(Atom)).

-spec i2b(integer()) -> binary().
i2b(Integer) -> integer_to_binary(Integer).

-spec get_until(atom(), tokens()) -> {tokens(), tokens()}.
get_until(Char, Tokens) -> get_until(Char, Tokens, []).
get_until(Char, [{Char, _} = _Token | Rest], Acc) ->
    % No need to return the token because we already know what it is.
    {lists:reverse(Acc), Rest};
get_until(Char, [Token | Rest], Acc) ->
    get_until(Char, Rest, [Token | Acc]).

-spec get_end_of_expr(tokens()) -> {tokens(), tokens()}.
get_end_of_expr(Tokens) -> get_end_of_expr(Tokens, []).

get_end_of_expr([], Acc) ->
    {lists:reverse(Acc), []};
get_end_of_expr([{End, _} = Token | Rest], Acc) when End == ',' orelse End == dot ->
    {lists:reverse([Token | Acc]), Rest};
get_end_of_expr([{'(', _} = Token | Rest0], Acc) ->
    {Tokens, Rest1} = get_until(')', Rest0),
    get_end_of_expr(Rest1, [{')', 0}] ++ lists:reverse(Tokens) ++ [Token | Acc]);
get_end_of_expr([{'{', _} = Token | Rest0], Acc) ->
    {Tokens, Rest1} = get_until('}', Rest0),
    get_end_of_expr(Rest1, [{'}', 0}] ++ lists:reverse(Tokens) ++ [Token | Acc]);
get_end_of_expr([{'[', _} = Token | Rest0], Acc) ->
    {Tokens, Rest1} = get_until(']', Rest0),
    get_end_of_expr(Rest1, [{']', 0}] ++ lists:reverse(Tokens) ++ [Token | Acc]);
get_end_of_expr([Token | Rest], Acc) ->
    get_end_of_expr(Rest, [Token | Acc]).

%% Testing

test_binop(Left, Op, Right, Indent) ->
    group(
      nest(
        Indent,
        space(
          group(
            space(
              text(Left),
              text(Op)
            )
          ),
          text(Right)
        )
      )
    ).

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

