%%
%% @doc An implementation of ["Strictly Pretty" (2000) by Christian Lindig][0].
%%
%% Inspired by the Elixir implementation of the same paper in Inspect.Algebra. Thanks to the core team for their hard work!
%%
%% [0] https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
%%
-module(steamroller_algebra).

-export([format_tokens/1, format_tokens/2, generate_doc/1, pretty/1, pretty/2]).
% Testing
-export([repeat/2, from_the_paper/2]).

-type doc() :: doc_nil
            | {doc_cons, doc(), doc()}
            | {doc_text, binary()}
            | {doc_nest, integer(), doc()}
            | {doc_break, binary()}
            | {doc_group, doc(), inherit()}
            | {doc_force_break, doc()}.

-type sdoc() :: s_nil | {s_text, binary(), sdoc()} | {s_line, binary(), sdoc()}.
-type mode() :: flat | break.
-type inherit() :: self | inherit.
-type continue() :: continue | done.
-type force_break() :: force_break | no_force_break.
-type token() :: steamroller_ast:token().
-type tokens() :: steamroller_ast:tokens().
-type previous_term() :: new_file | attribute | spec | list | function | module_comment | function_comment | dot.

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(two_nl, <<"\n\n">>).
-define(dot, <<".">>).
-define(max_width, 100).
-define(indent, 4).

-define(IS_LIST_CHAR(C), (C == '(' orelse C == '{' orelse C == '[' orelse C == '<<')).
-define(IS_OPERATOR(C), (C == '+' orelse C == '-' orelse C == '*' orelse C == '/' orelse C == 'div')).
-define(IS_EQUALS(C), (C == '=' orelse C == '==')).

%% API

-spec format_tokens(tokens()) -> binary().
format_tokens(Tokens) -> format_tokens(Tokens, ?max_width).

-spec format_tokens(tokens(), integer()) -> binary().
format_tokens(Tokens, Width) ->
    Doc = generate_doc(Tokens),
    pretty(Doc, Width).

-spec generate_doc(tokens()) -> doc().
generate_doc(Tokens) -> generate_doc_(Tokens, empty(), new_file).

-spec pretty(doc()) -> binary().
pretty(Doc) -> pretty(Doc, ?max_width).

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

-spec break(binary()) -> doc().
break(S) -> {doc_break, S}.

-spec force_break(force_break(), doc()) -> doc().
force_break(force_break, X) -> {doc_force_break, X};
force_break(no_force_break, X) -> X.

-spec group(doc()) -> doc().
group(D) -> {doc_group, D, self}.

% Group inheritance is lifted from the Elixir algebra implementation.
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

%% Token Consumption

-spec generate_doc_(tokens(), doc(), previous_term()) -> doc().
generate_doc_([], Doc, _) -> Doc;
generate_doc_([{'-', _} = H0, {atom, _, spec} = H1, {'(', _} | Rest0], Doc, PrevTerm) ->
    % Remove brackets from Specs
    Rest1 = remove_matching('(', ')', Rest0),
    generate_doc_([H0, H1 | Rest1], Doc, PrevTerm);
generate_doc_([{'-', _}, {atom, _, spec} | Tokens], Doc0, PrevTerm) ->
    % Spec
    % Re-use the function code because the syntax is identical.
    {Group, Rest} = function(Tokens),
    Spec = cons(text(<<"-spec ">>), Group),
    Doc1 =
        case PrevTerm of
            function_comment -> newline(Doc0, Spec);
            _ -> newlines(Doc0, Spec)
        end,
    generate_doc_(Rest, Doc1, spec);
generate_doc_([{'-', _}, {atom, _, Atom} | Tokens], Doc0, PrevTerm) ->
    % Module Attribute
    {Group, Rest} = attribute(Atom, Tokens),
    Doc1 =
        case PrevTerm of
            function_comment -> newline(Doc0, Group);
            _ -> newlines(Doc0, Group)
        end,
    generate_doc_(Rest, Doc1, attribute);
generate_doc_([{atom, _, _Atom} | _] = Tokens, Doc0, PrevTerm) ->
    % Function
    {Group, Rest} = function(Tokens),
    Doc1 =
        case PrevTerm of
            PrevTerm when PrevTerm == function_comment orelse PrevTerm == spec ->
                newline(Doc0, Group);
            _ ->
                newlines(Doc0, Group)
        end,
    generate_doc_(Rest, Doc1, function);
generate_doc_([{C, _} | _] = Tokens, Doc0, PrevTerm) when ?IS_LIST_CHAR(C) ->
    % List -> if this is at the top level this is probably a config file
    {ForceBreak, Group0, Rest} = list_group(Tokens),
    Group1 = force_break(ForceBreak, Group0),
    Doc1 =
        case PrevTerm of
            function_comment -> newline(Doc0, Group1);
            _ -> newlines(Doc0, Group1)
        end,
    generate_doc_(Rest, Doc1, list);
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
            _ -> newlines(Doc0, Comment)
        end,
    generate_doc_(Rest, Doc1, function_comment);
generate_doc_([{dot, _} | Rest], Doc, _PrevTerm) ->
    % Any undhandled dots, for example at the end of terms in config files.
    generate_doc_(Rest, cons(Doc, text(?dot)), dot).

%% Erlang Source Elements

-spec attribute(atom(), tokens()) -> {doc(), tokens()}.
attribute(Att, Tokens) ->
    {_ForceBreak, Expr, [{dot, _} | Rest]} = list_group(Tokens),
    Attribute =
          group(
            cons(
              [
               text(<<"-">>),
               text(a2b(Att)),
               Expr,
               text(?dot)
              ]
             )
           ),
    {Attribute, Rest}.

-spec function(tokens()) -> {doc(), tokens()}.
function(Tokens) ->
    {_ForceBreak, Clauses, Rest} = clauses(Tokens),
    {group(newline(Clauses)), Rest}.

-spec case_(tokens()) -> {force_break(), doc(), tokens()}.
case_([{'case', _} | Tokens]) ->
    {CaseArgTokens, Rest0, _} = get_from_until('case', 'of', Tokens),
    {empty, _ForceBreak, CaseArg, []} = expr(CaseArgTokens, no_force_break),
    {CaseClauseTokens, Rest1, _} = get_from_until('case', 'end', Rest0),
    {ForceBreak, Clauses, []} = clauses(CaseClauseTokens),
    Doc =
        force_break(
          ForceBreak,
          group(
            space(
              cons(
                group(
                  space(
                    text(<<"case">>),
                    CaseArg
                   )
                 ),
                nest(
                  ?indent,
                  space([text(<<" of">>) | Clauses])
                 )
               ),
              text(<<"end">>)
             ),
            inherit
           )
         ),
    {ForceBreak, Doc, Rest1}.

-spec if_(tokens()) -> {force_break(), doc(), tokens()}.
if_([{'if', _} | Tokens]) ->
    {IfClauseTokens, Rest1, _} = get_from_until('if', 'end', Tokens),
    {ForceBreak, Clauses, []} = clauses(IfClauseTokens),
    Doc =
        force_break(
          ForceBreak,
          group(
            space(
              nest(
                ?indent,
                space([text(<<"if">>) | Clauses])
               ),
              text(<<"end">>)
             ),
            inherit
           )
         ),
    {ForceBreak, Doc, Rest1}.

-spec list_group(tokens()) -> {force_break(), doc(), tokens()}.
list_group([{'(', _} | Rest0]) ->
    {Tokens, Rest1, _} = get_from_until('(', ')', Rest0),
    {ForceBreak, ListGroup} = brackets(Tokens, <<"(">>, <<")">>),
    {ForceBreak, ListGroup, Rest1};
list_group([{'{', _} | Rest0]) ->
    {Tokens, Rest1, _} = get_from_until('{', '}', Rest0),
    {ForceBreak, ListGroup} = brackets(Tokens, <<"{">>, <<"}">>),
    {ForceBreak, ListGroup, Rest1};
list_group([{'[', _} | Rest0]) ->
    {Tokens, Rest1, _} = get_from_until('[', ']', Rest0),
    {ForceBreak, ListGroup} = brackets(Tokens, <<"[">>, <<"]">>),
    {ForceBreak, ListGroup, Rest1};
list_group([{'<<', _} | Rest0]) ->
    {Tokens, Rest1, _} = get_from_until('<<', '>>', Rest0),
    {ForceBreak, ListGroup} = brackets(Tokens, <<"<<">>, <<">>">>),
    {ForceBreak, ListGroup, Rest1}.

-spec brackets(tokens(), binary(), binary()) -> {force_break(), doc()}.
brackets([], Open, Close) ->
    {no_force_break, group(cons(text(Open), text(Close)))};
brackets(Tokens, Open, Close) ->
    {ForceBreak, ListElements} = list_elements(Tokens),

    Doc =
        group(
          force_break(
            ForceBreak,
            stick(
              nest(
                ?indent,
                stick(
                  text(Open),
                  ListElements
                 )
               ),
              text(Close)
             )
           )
         ),
    {ForceBreak, Doc}.

-spec list_elements(tokens()) -> {force_break(), doc()}.
list_elements(Tokens) -> list_elements(Tokens, empty(), no_force_break).

-spec list_elements(tokens(), doc(), force_break()) -> {force_break(), doc()}.
list_elements([], Doc, ForceBreak) -> {ForceBreak, Doc};
list_elements([{C, _} | _] = Tokens, Doc0, ForceBreak0) when ?IS_LIST_CHAR(C) ->
    {ListGroupForceBreak, Group0, Rest0} = list_group(Tokens),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListGroupForceBreak]),
    {Group1, Rest1} =
        case Rest0 of
            [{End, _} | Rest] when End == ',' ->
                % If a list element is a list followed by a comma we want to capture the
                % comma and attach it to the list group.
                {cons(Group0, text(a2b(End))), Rest};
            _ -> {Group0, Rest0}
        end,
    Doc1 = space(Doc0, Group1),
    list_elements(Rest1, Doc1, ForceBreak1);
list_elements(Tokens, Doc0, ForceBreak0) ->
    {_End, ForceBreak1, Doc1, Rest} = expr(Tokens, ForceBreak0, Doc0),
    list_elements(Rest, Doc1, ForceBreak1).

-spec clauses(tokens()) -> {force_break(), list(doc()), tokens()}.
clauses(Tokens) -> clauses(Tokens, [], []).

-spec clauses(tokens(), list(doc()), list(force_break())) -> {force_break(), list(doc()), tokens()}.
clauses(Tokens, Acc0, ForceBreaks0) ->
    {Continue, ForceBreak, Clause, Rest0} = head_and_clause(Tokens),
    Acc1 = [Clause | Acc0],
    ForceBreaks1 = [ForceBreak | ForceBreaks0],
    case Continue of
        continue -> clauses(Rest0, Acc1, ForceBreaks1);
        done -> {resolve_force_break(ForceBreaks1), lists:reverse(Acc1), Rest0}
    end.

-spec head_and_clause(tokens()) -> {continue(), force_break(), doc(), tokens()}.
head_and_clause(Tokens) -> head_and_clause(Tokens, empty()).

-spec head_and_clause(tokens(), doc()) -> {continue(), force_break(), doc(), tokens()}.
head_and_clause([{atom, _, Name} | Rest], Doc) ->
    % Name
    head_and_clause(Rest, cons(Doc, text(a2b(Name))));
head_and_clause([{C, _} | _] = Tokens, Doc) when ?IS_LIST_CHAR(C) ->
    % Args
    {_ForceBreak, Group, Rest} = list_group(Tokens),
    head_and_clause(Rest, cons(Doc, Group));
head_and_clause([{comment, _, Comment} | Rest], Doc0) ->
    % Handle any comments between function clauses.
    {Continue, ForceBreak, Doc1, Tokens} = head_and_clause(Rest),
    Doc2 =
        newline(
              [
               Doc0,
               comment(Comment),
               Doc1
              ]
             ),
    {Continue, ForceBreak, Doc2, Tokens};
head_and_clause([{'->', _} | Rest0], Doc) ->
    % End
    {Continue, ForceBreak, Body, Rest1} = clause(Rest0),
    {Continue, ForceBreak, cons(Doc, force_break(ForceBreak, nest(?indent, group(space(text(<<" ->">>), Body), inherit)))), Rest1};
head_and_clause(Rest0, Doc0) ->
    {Tokens, Rest1, Token} = get_until('->', Rest0),
    {empty, _ForceBreak, Doc1, []} = expr(Tokens, no_force_break, Doc0),
    head_and_clause([Token | Rest1], Doc1).

-spec clause(tokens()) -> {continue(), force_break(), doc(), tokens()}.
clause(Tokens) ->
    {End, ForceBreak, Exprs, Rest} = exprs(Tokens),
    Continue = case End of dot -> done; empty -> done; ';' -> continue end,
    if length(Exprs) > 1 ->
           % Force indentation for multi-line clauses
           {Continue, force_break, space(Exprs), Rest};
       true ->
           [Expr] = Exprs,
           {Continue, ForceBreak, Expr, Rest}
    end.

-spec exprs(tokens()) -> {dot | ';' | empty, force_break(), list(doc()), tokens()}.
exprs(Tokens) -> exprs(Tokens, [], no_force_break).

-spec exprs(tokens(), list(doc()), force_break()) -> {dot | ';' | empty, force_break(), list(doc()), tokens()}.
exprs(Tokens, Acc0, ForceBreak0) ->
    {End, ForceBreak1, Expr, Rest} = expr(Tokens, ForceBreak0),
    Acc1 = [Expr | Acc0],
    case End of
        End when End == ',' orelse End == comment -> exprs(Rest, Acc1, ForceBreak1);
        _ -> {End, ForceBreak1, lists:reverse(Acc1), Rest}
    end.

-spec expr(tokens(), force_break()) -> {dot | ';' | ',' | empty | comment, force_break(), doc(), tokens()}.
expr(Tokens, ForceBreak0) ->
    {ExprTokens, Rest} = get_end_of_expr(Tokens),
    {End, ForceBreak1, Expr} = expr_(ExprTokens, empty(), ForceBreak0),
    {End, ForceBreak1, group(Expr), Rest}.

-spec expr(tokens(), force_break(), doc()) -> {dot | ';' | ',' | empty | comment, force_break(), doc(), tokens()}.
expr(Tokens, ForceBreak0, Doc) ->
    {ExprTokens, Rest} = get_end_of_expr(Tokens),
    {End, ForceBreak1, Expr} = expr_(ExprTokens, Doc, ForceBreak0),
    {End, ForceBreak1, group(Expr, inherit), Rest}.

-spec expr_(tokens(), doc(), force_break()) -> {dot | ';' | ',' | empty | comment, force_break(), doc()}.
expr_([], Doc, ForceBreak) -> {empty, ForceBreak, Doc};
expr_([{'?', _} | Rest0], Doc, ForceBreak0) ->
    % Handle macros
    {End, ForceBreak1, Expr, []} = expr(Rest0, ForceBreak0),
    {End, ForceBreak1, space(Doc, cons(text(<<"?">>), Expr))};
expr_([{'case', _} | _] = Tokens, Doc, ForceBreak0) ->
    {CaseForceBreak, CaseGroup, Rest} = case_(Tokens),
    ForceBreak1 = resolve_force_break([ForceBreak0, CaseForceBreak]),
    expr_(Rest, space(Doc, CaseGroup), ForceBreak1);
expr_([{'if', _} | _] = Tokens, Doc, ForceBreak0) ->
    io:fwrite("\nTokens=~p", [Tokens]),
    {IfForceBreak, CaseGroup, Rest} = if_(Tokens),
    ForceBreak1 = resolve_force_break([ForceBreak0, IfForceBreak]),
    expr_(Rest, space(Doc, CaseGroup), ForceBreak1);
expr_([{atom, LineNum, ModuleName}, {':', LineNum}, {atom, LineNum, FunctionName}, {'(', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
    % Handle function calls to other modules
    [_, _, _ | Tokens1] = Tokens0,
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    Function =
        space(
          Doc,
          cons(
            [
             text(a2b(ModuleName)),
             text(<<":">>),
             text(a2b(FunctionName)),
             ListGroup
            ]
           )
        ),
    expr_(Rest, Function, ForceBreak1);
expr_([{atom, LineNum, FunctionName}, {'(', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
    % Handle local function calls
    Tokens1 = tl(Tokens0),
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    Function =
        space(
          Doc,
          cons(
            text(a2b(FunctionName)),
            ListGroup
          )
        ),
    expr_(Rest, Function, ForceBreak1);
expr_([{C, _} | _] = Tokens, Doc, ForceBreak0) when ?IS_LIST_CHAR(C) ->
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    expr_(Rest, space(Doc, ListGroup), ForceBreak1);
expr_([{C, _} | Rest], Doc0, ForceBreak0) when ?IS_EQUALS(C) ->
    % Handle pattern matching in e.g. function heads
    % foo({X, _} = Y) -> ...
    Equals = group(space(Doc0, text(a2b(C)))),
    {End, ForceBreak1, Expr} = expr_(Rest, empty(), ForceBreak0),
    Equation = group(nest(?indent, space(Equals, group(Expr)))),
    {End, ForceBreak1, Equation};
expr_([{var, _, Var}, {C, _} | Rest], Doc, ForceBreak0) when ?IS_EQUALS(C) ->
    % Handle equations
    % Arg3 =
    %     Arg1 + Arg2,
    Equals = group(space(text(a2b(Var)), text(a2b(C)))),
    {End, ForceBreak1, Expr} = expr_(Rest, empty(), ForceBreak0),
    Equation = group(nest(?indent, space(Equals, group(Expr)))),
    {End, ForceBreak1, space(Doc, Equation)};
expr_([{End, _}], Doc, ForceBreak) ->
    {End, ForceBreak, cons(Doc, text(a2b(End)))};
expr_([{atom, _, Atom}, {'/', _}, {integer, _, Int} | Rest], Doc, ForceBreak) ->
    % Handle function arity expressions
    % some_fun/1
    FunctionDoc = cons(
                    [
                    text(a2b(Atom)),
                    text(<<"/">>),
                    text(i2b(Int))
                    ]
                   ),
    expr_(Rest, space(Doc, FunctionDoc), ForceBreak);
expr_([{var, _, Var}, {'/', _}, {atom, _, Atom} | Rest], Doc, ForceBreak) ->
    % Handle binary matching
    % <<Thing/binary>>
    TermDoc = cons([
                    text(a2b(Var)),
                    text(<<"/">>),
                    text(a2b(Atom))
                   ]),
    expr_(Rest, space(Doc, TermDoc), ForceBreak);
expr_([{var, _, Var}, {':', _}, {integer, _, Integer}, {'/', _}, {atom, _, Atom} | Rest], Doc, ForceBreak) ->
    % Handle more binary matching
    % <<Thing:1/binary, Rest/binary>>
    TermDoc =
        cons(
          [
           text(a2b(Var)),
           text(<<":">>),
           text(i2b(Integer)),
           text(<<"/">>),
           text(a2b(Atom))
          ]
        ),
    expr_(Rest, space(Doc, TermDoc), ForceBreak);
expr_([{var, _, Var}, {Op, _} | Rest], Doc0, ForceBreak) when ?IS_OPERATOR(Op) ->
    Doc1 = space(Doc0, space(text(a2b(Var)), text(a2b(Op)))),
    expr_(Rest, Doc1, ForceBreak);
expr_([{integer, _, Integer}, {Op, _} | Rest], Doc0, ForceBreak) when ?IS_OPERATOR(Op) ->
    Doc1 = space(Doc0, space(text(i2b(Integer)), text(a2b(Op)))),
    expr_(Rest, Doc1, ForceBreak);
expr_([{Token, _, Var} | Rest], Doc, ForceBreak) when Token == var orelse Token == atom ->
    expr_(Rest, space(Doc, text(a2b(Var))), ForceBreak);
expr_([{integer, _, Integer} | Rest], Doc, ForceBreak) ->
    expr_(Rest, space(Doc, text(i2b(Integer))), ForceBreak);
expr_([{string, _, Var} | Rest], Doc, ForceBreak) ->
    expr_(Rest, space(Doc, text(s2b(Var))), ForceBreak);
expr_([{comment, _, Comment}], Doc, _ForceBreak) ->
    {comment, force_break, space(Doc, comment(Comment))};
expr_([{'|', _} | Rest0], Doc, ForceBreak0) ->
    {End, ForceBreak1, Expr} = expr_(Rest0, empty(), ForceBreak0),
    Group = group(cons(text(<<"| ">>), group(Expr))),
    {End, ForceBreak1, space(Doc, Group)}.

-spec comment(string()) -> doc().
comment(Comment) -> text(list_to_binary(Comment)).

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
format(W, K, [{I, _, {doc_force_break, X}} | Rest]) -> format(W, K, [{I, break, X} | Rest]);
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
fits(W, [{_, _, {doc_force_break, _}} | Rest]) -> fits(W, Rest);
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

-spec s2b(string()) -> binary().
s2b(String) -> list_to_binary(escape(String)).

-spec escape(string()) -> string().
escape(String) -> io_lib:format("~p", [String]).

-spec get_from_until(atom(), atom(), tokens()) -> {tokens(), tokens(), token()}.
get_from_until(Start, End, Tokens) -> get_from_until(Start, End, Tokens, [], 0).

-spec get_from_until(atom(), atom(), tokens(), tokens(), integer()) -> {tokens(), tokens(), token()}.
get_from_until(Start, End, [{Start, _} = Token | Rest], Acc, Stack) ->
    get_from_until(Start, End, Rest, [Token | Acc], Stack + 1);
get_from_until(_Start, End, [{End, _} = Token | Rest], Acc, 0) ->
    {lists:reverse(Acc), Rest, Token};
get_from_until(Start, End, [{End, _} = Token | Rest], Acc, Stack) ->
    get_from_until(Start, End, Rest, [Token | Acc], Stack - 1);
get_from_until(Start, End, [Token | Rest], Acc, Stack) ->
    get_from_until(Start, End, Rest, [Token | Acc], Stack).

-spec get_until(atom(), tokens()) -> {tokens(), tokens(), token()}.
get_until(End, Tokens) -> get_until(End, Tokens, []).

-spec get_until(atom(), tokens(), tokens()) -> {tokens(), tokens(), token()}.
get_until(End, [{End, _} = Token | Rest], Acc) ->
    {lists:reverse(Acc), Rest, Token};
get_until(End, [Token | Rest], Acc) ->
    get_until(End, Rest, [Token | Acc]).

-spec remove_matching(atom(), atom(), tokens()) -> tokens().
remove_matching(Start, End, Tokens) -> remove_matching(Start, End, Tokens, [], 0).

-spec remove_matching(atom(), atom(), tokens(), tokens(), integer()) -> tokens().
remove_matching(Start, End, [{Start, _} = Token | Rest], Acc, Stack) ->
    remove_matching(Start, End, Rest, [Token | Acc], Stack + 1);
remove_matching(_Start, End, [{End, _} | Rest], Acc, 0) ->
    lists:reverse(Acc) ++ Rest;
remove_matching(Start, End, [{End, _} = Token | Rest], Acc, Stack) ->
    remove_matching(Start, End, Rest, [Token | Acc], Stack - 1);
remove_matching(Start, End, [Token | Rest], Acc, Stack) ->
    remove_matching(Start, End, Rest, [Token | Acc], Stack).

-spec get_end_of_expr(tokens()) -> {tokens(), tokens()}.
get_end_of_expr(Tokens) -> get_end_of_expr(Tokens, [], 0, []).

% Dialyzer gets upset if we use integer() for the third arg here but that's what it is.
-spec get_end_of_expr(tokens(), tokens(), any(), list(atom())) -> {tokens(), tokens()}.
get_end_of_expr([], Acc, _LineNum, _KeywordStack) ->
    {lists:reverse(Acc), []};
get_end_of_expr([{comment, _, _} = Comment | Rest], [], _LineNum, _KeywordStack) ->
    {[Comment], Rest};
get_end_of_expr([{comment, LineNum, _} = Comment | Rest], Acc, LineNum, _KeywordStack) ->
    % Inline comment - naughty naughty
    % Return the comment and put the acc back.
    {[Comment], lists:reverse(Acc) ++ Rest};
get_end_of_expr([{comment, _, _} | _] = Rest, Acc, _LineNum, _KeywordStack) ->
    {lists:reverse(Acc), Rest};
get_end_of_expr([{End, LineNum} = Token, {comment, LineNum, _} = Comment | Rest], Acc, _, _)
  when End == ',' orelse End == ';' orelse End == dot ->
    % Inline comment - naughty naughty
    % Return the comment and put the acc back.
    {[Comment], lists:reverse([Token | Acc]) ++ Rest};
get_end_of_expr([{End, _} = Token | Rest], Acc, _, []) when End == ',' orelse End == ';' orelse End == dot ->
    {lists:reverse([Token | Acc]), Rest};
get_end_of_expr([{'end', _} = Token | Rest], Acc, _LineNum, []) ->
    {lists:reverse([Token | Acc]), Rest};
get_end_of_expr([{'end', _} = Token | Rest], Acc, LineNum, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, tl(KeywordStack));
get_end_of_expr([{'case', _} = Token | Rest], Acc, LineNum, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, ['case' | KeywordStack]);
get_end_of_expr([{'if', _} = Token | Rest], Acc, LineNum, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, ['if' | KeywordStack]);
get_end_of_expr([{'(', _} = Token | Rest0], Acc, _, KeywordStack) ->
    {Tokens, Rest1, {')', LineNum} = EndToken} = get_from_until('(', ')', Rest0),
    get_end_of_expr(Rest1, [EndToken] ++ lists:reverse(Tokens) ++ [Token | Acc], LineNum, KeywordStack);
get_end_of_expr([{'{', _} = Token | Rest0], Acc, _, KeywordStack) ->
    {Tokens, Rest1, {'}', LineNum} = EndToken} = get_from_until('{', '}', Rest0),
    get_end_of_expr(Rest1, [EndToken] ++ lists:reverse(Tokens) ++ [Token | Acc], LineNum, KeywordStack);
get_end_of_expr([{'[', _} = Token | Rest0], Acc, _, KeywordStack) ->
    {Tokens, Rest1, {']', LineNum} = EndToken} = get_from_until('[', ']', Rest0),
    get_end_of_expr(Rest1, [EndToken] ++ lists:reverse(Tokens) ++ [Token | Acc], LineNum, KeywordStack);
get_end_of_expr([{'<<', _} = Token | Rest0], Acc, _, KeywordStack) ->
    {Tokens, Rest1, {'>>', LineNum} = EndToken} = get_from_until('<<', '>>', Rest0),
    get_end_of_expr(Rest1, [EndToken] ++ lists:reverse(Tokens) ++ [Token | Acc], LineNum, KeywordStack);
get_end_of_expr([{_, LineNum} = Token | Rest], Acc, _, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack);
get_end_of_expr([{_, LineNum, _} = Token | Rest], Acc, _, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack).

-spec resolve_force_break(list(force_break())) -> force_break().
resolve_force_break(Args) ->
    case lists:any(fun(X) -> X == force_break end, Args) of
        true -> force_break;
        false -> no_force_break
    end.

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

