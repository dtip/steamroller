%%
%% @doc An implementation of ["Strictly Pretty" (2000) by Christian Lindig][0].
%%
%% Inspired by the Elixir implementation of the same paper in Inspect.Algebra. Thanks to the core team for their hard work!
%%
%% [0] https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
%%
-module(steamroll_algebra).

-export([pretty/1, pretty/2]).
% Testing
-export([repeat/2, from_the_paper/2]).

-type doc() :: doc_nil
            | {doc_cons, doc(), doc()}
            | {doc_text, binary()}
            | {doc_nest, integer(), doc()}
            | {doc_break, binary()}
            | {doc_group, doc()}.

-type sdoc() :: s_nil | {s_text, binary(), sdoc()} | {s_line, binary(), sdoc()}.

-type mode() :: flat | break.

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(max_width, 100).
-define(indent, 4).

%% API

-spec pretty(doc()) -> binary().
pretty(Doc) ->
    SDoc = format(?max_width, 0, [{0, flat, {doc_group, Doc}}]),
    String = sdoc_to_string(SDoc),
    <<String/binary, "\n">>.

-spec pretty(doc(), integer()) -> binary().
pretty(Doc, Width) ->
    SDoc = format(Width, 0, [{0, flat, {doc_group, Doc}}]),
    String = sdoc_to_string(SDoc),
    <<String/binary, "\n">>.

% Used for testing.
-spec from_the_paper(integer(), integer()) -> binary().
from_the_paper(Width, Indent) ->
    C = binop(<<"a">>, <<"==">>, <<"b">>, Indent),
    E1 = binop(<<"a">>, <<"<<">>, <<"2">>, Indent),
    E2 = binop(<<"a">>, <<"+">>, <<"b">>, Indent),
    Doc = ifthen(C, E1, E2, Indent),
    pretty(Doc, Width).

%% Constructor Functions

-spec cons(doc(), doc()) -> doc().
cons(X, Y) -> {doc_cons, X, Y}.

%-spec empty() -> doc().
%empty() -> doc_nil.

-spec text(binary()) -> doc().
text(S) -> {doc_text, S}.

-spec nest(integer(), doc()) -> doc().
nest(I, X) -> {doc_nest, I, X}.

-spec break() -> doc().
break() -> {doc_break, ?sp}.

%-spec break(doc()) -> doc().
%break(S) -> {doc_break, S}.

-spec group(doc()) -> doc().
group(D) -> {doc_group, D}.

%% Operators

space(doc_nil, Y) -> Y;
space(X, doc_nil) -> X;
space(X, Y) -> cons(X, cons(break(), Y)).

%binop(Left, Op, Right) -> binop(Left, Op, Right, ?indent).

binop(Left, Op, Right, Indent) ->
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

%ifthen(C, E1, E2) -> ifthen(C, E1, E2, ?indent).

ifthen(C, E1, E2, Indent) ->
    group(
      space(
        space(
          group(nest(Indent, space(text(<<"if">>), C))),
          group(nest(Indent, space(text(<<"then">>), E1)))
        ),
        group(nest(Indent, space(text(<<"else">>), E2)))
      )
    ).

%% Internal

-spec format(integer(), integer(), list({integer(), mode(), doc()})) -> sdoc().
format(_, _, []) -> s_nil;
format(W, K, [{_, _, doc_nil} | Rest]) -> format(W, K, Rest);
format(W, K, [{I, M, {doc_cons, X, Y}} | Rest]) -> format(W, K, [{I, M, X}, {I, M, Y} | Rest]);
format(W, K, [{I, M, {doc_nest, J, X}} | Rest]) -> format(W, K, [{I + J, M, X} | Rest]);
format(W, K, [{_, _, {doc_text, S}} | Rest]) -> {s_text, S, format(W, K + byte_size(S), Rest)};
format(W, K, [{_, flat, {doc_break, S}} | Rest]) -> {s_text, S, format(W, K + byte_size(S), Rest)};
format(W, _, [{I, break, {doc_break, _}} | Rest]) -> {s_line, I, format(W, I, Rest)};
format(W, K, [{I, _, {doc_group, X}} | Rest]) ->
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
fits(_, [{_, break, {doc_break, _}} | _Rest]) -> throw(impossible);
fits(W, [{I, _, {doc_group, X}} | Rest]) -> fits(W, [{I, flat, X} | Rest]).

-spec sdoc_to_string(sdoc()) -> binary().
sdoc_to_string(s_nil) -> <<"">>;
sdoc_to_string({s_text, String, Doc}) ->
    DocString = sdoc_to_string(Doc),
    <<String/binary, DocString/binary>>;
sdoc_to_string({s_line, Indent, Doc}) ->
    Prefix = repeat(?sp, Indent),
    DocString = sdoc_to_string(Doc),
    <<"\n", Prefix/binary, DocString/binary>>.

%% Utils

-spec repeat(binary(), integer()) -> binary().
repeat(Bin, Times) when Times >= 0 -> repeat_(<<>>, Bin, Times).

-spec repeat_(binary(), binary(), integer()) -> binary().
repeat_(Acc, _, 0) -> Acc;
repeat_(Acc, Bin, Times) -> repeat_(<<Acc/binary, Bin/binary>>, Bin, Times - 1).
