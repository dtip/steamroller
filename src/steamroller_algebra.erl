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

-export([format_tokens/2, generate_doc/1, pretty/2]).

% Testing
-export([repeat/2, from_the_paper/2]).

-type doc() :: doc_nil
             | {doc_cons, doc(), doc()}
             | {doc_text, binary()}
             | {doc_nest, integer(), doc()}
             | {doc_underneath, integer(), doc()}
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
-type previous_term() :: new_file
                       | {attribute, atom()}
                       | spec
                       | type
                       | list
                       | function
                       | module_comment
                       | function_comment
                       | dot.

-define(sp, <<" ">>).
-define(nl, <<"\n">>).
-define(two_nl, <<"\n\n">>).
-define(dot, <<".">>).
-define(indent, 4).
-define(IS_LIST_CHAR(C), (C == '(' orelse C == '{' orelse C == '[' orelse C == '<<')).
-define(IS_EQUALS(C), (C == '=' orelse C == '==')).
-define(IS_BOOL_CONCATENATOR(C), (C == 'andalso' orelse C == 'orelse')).
-define(
    IS_TERMINATED_KEYWORD(C),
    % 'fun' is not always terminated - there is probably a bug here.
    (C == 'case' orelse C == 'if' orelse C == 'fun' orelse C == 'receive' orelse C == 'try' orelse C == 'begin')
).

%%
%% API
%%

-spec format_tokens(tokens(), integer()) -> binary().
format_tokens(Tokens, Width) ->
    Doc = generate_doc(Tokens),
    pretty(Doc, Width).

-spec generate_doc(tokens()) -> doc().
generate_doc(Tokens) -> generate_doc_(Tokens, empty(), new_file).

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
generate_doc_([{'-', _} = H0, {atom, _, type} = H1, {'(', _} | Rest0], Doc, PrevTerm) ->
    % Remove brackets from Types
    Rest1 = remove_matching('(', ')', Rest0),
    generate_doc_([H0, H1 | Rest1], Doc, PrevTerm);
generate_doc_([{'-', _}, {atom, _, type} | Tokens], Doc0, PrevTerm) ->
    % Type
    % Re-use the function code because the syntax is identical.
    {Group, Rest} = function(Tokens),
    Spec = cons(text(<<"-type ">>), Group),
    Doc1 =
        case PrevTerm of
            PrevTerm when PrevTerm == function_comment orelse PrevTerm == type ->
                newline(Doc0, Spec);
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
            {define, {attribute, IfDef}} when IfDef == ifdef orelse IfDef == else ->
                newline(Doc0, Group);
            {IfDef, {attribute, define}} when IfDef == else orelse IfDef == endif ->
                newline(Doc0, Group);
            _ -> newlines(Doc0, Group)
        end,
    generate_doc_(Rest, Doc1, {attribute, Atom});
generate_doc_([{atom, _, _Atom} | _] = Tokens, Doc0, PrevTerm) ->
    % Function
    {Group, Rest} = function(Tokens),
    Doc1 =
        case PrevTerm of
            PrevTerm when PrevTerm == function_comment orelse PrevTerm == spec ->
                newline(Doc0, Group);
            _ -> newlines(Doc0, Group)
        end,
    generate_doc_(Rest, Doc1, function);
generate_doc_([{C, _} | _] = Tokens, Doc0, PrevTerm) when ?IS_LIST_CHAR(C) ->
    % List
    % If this is at the top level this is probably a config file
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
            PrevTerm when PrevTerm == function_comment orelse PrevTerm == spec ->
                newline(Doc0, Comment);
            _ -> newlines(Doc0, Comment)
        end,
    generate_doc_(Rest, Doc1, function_comment);
generate_doc_([{dot, _} | Rest], Doc, _PrevTerm) ->
    % Any undhandled dots, for example at the end of terms in config files.
    generate_doc_(Rest, cons(Doc, text(?dot)), dot).

%%
%% Erlang Source Elements
%%

-spec attribute(atom(), tokens()) -> {doc(), tokens()}.
attribute(Att, [{dot, _} | Rest]) ->
    % Handle attributes without brackets
    % -else.
    % -endif.
    Attribute = group(cons([text(<<"-">>), text(a2b(Att)), text(?dot)])),
    {Attribute, Rest};
attribute(Att, Tokens) ->
    {_ForceBreak, Expr, [{dot, _} | Rest]} = list_group(Tokens),
    Attribute = group(cons([text(<<"-">>), text(a2b(Att)), Expr, text(?dot)])),
    {Attribute, Rest}.

-spec function(tokens()) -> {doc(), tokens()}.
function(Tokens) ->
    {_ForceBreak, Clauses, Rest} = clauses(Tokens),
    {newline(Clauses), Rest}.

-spec case_(tokens()) -> {force_break(), doc(), tokens()}.
case_([{'case', _} | Tokens]) ->
    {CaseArgTokens, Rest0, _} = get_from_until('case', 'of', Tokens),
    {empty, _, CaseArg, []} = expr(CaseArgTokens, no_force_break),
    {CaseClauseTokens, Rest1, _} = get_until_end(Rest0),
    {CaseForceBreak, Clauses, []} = clauses(CaseClauseTokens),
    ForceBreak =
        case length(Clauses) > 1 of
            true -> force_break;
            false -> CaseForceBreak
        end,
    GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
    Doc =
        force_break(
            ForceBreak,
            group(
                space(
                    cons(
                        group(space(text(<<"case">>), CaseArg)),
                        nest(?indent, space(text(<<" of">>), GroupedClauses))
                    ),
                    text(<<"end">>)
                ),
                inherit
            )
        ),
    {ForceBreak, Doc, Rest1}.

-spec if_(tokens()) -> {force_break(), doc(), tokens()}.
if_([{'if', _} | Tokens]) ->
    {IfClauseTokens, Rest1, _} = get_until_end(Tokens),
    {IfForceBreak, Clauses, []} = clauses(IfClauseTokens),
    ForceBreak =
        case length(Clauses) > 1 of
            true -> force_break;
            false -> IfForceBreak
        end,
    GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
    Doc =
        force_break(
            ForceBreak,
            group(
                space(nest(?indent, space(text(<<"if">>), GroupedClauses)), text(<<"end">>)),
                inherit
            )
        ),
    {ForceBreak, Doc, Rest1}.

-spec receive_(tokens()) -> {force_break(), doc(), tokens()}.
receive_([{'receive', _} | Tokens]) ->
    {ReceiveClauseTokens0, Rest, _} = get_until_end(Tokens),
    {ReceiveClauseTokens1, AfterClauseTokens} =
        case get_until_any(['after'], ReceiveClauseTokens0) of
            {_Tokens, [], not_found} -> {ReceiveClauseTokens0, []};
            {R, A, Token} -> {R, [Token | A]}
        end,
    {ReceiveForceBreak, Clauses, []} = clauses(ReceiveClauseTokens1),
    After = after_(AfterClauseTokens),
    ForceBreak =
        case length(Clauses) > 1 of
            true -> force_break;
            false -> ReceiveForceBreak
        end,
    GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
    Doc =
        force_break(
            ForceBreak,
            group(
                space(
                    [
                        nest(?indent, space(text(<<"receive">>), GroupedClauses)),
                        After,
                        text(<<"end">>)
                    ]
                ),
                inherit
            )
        ),
    {ForceBreak, Doc, Rest}.

-spec after_(tokens()) -> doc().
after_([]) -> empty();
after_([{'after', _} | Tokens]) ->
    {AfterForceBreak, Clauses, []} = clauses(Tokens),
    ForceBreak =
        case length(Clauses) > 1 of
            true -> force_break;
            false -> AfterForceBreak
        end,
    GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
    Doc =
        force_break(
            ForceBreak,
            group(nest(?indent, space(text(<<"after">>), GroupedClauses)), inherit)
        ),
    Doc.

-spec try_(tokens()) -> {force_break(), doc(), tokens()}.
try_([{'try', _} | Tokens]) ->
    {TryArgTokens, Rest0, _} = get_from_until('try', 'of', Tokens),
    {empty, _, TryArg, []} = expr(TryArgTokens, no_force_break),
    {TryClauseTokens0, Rest1, _} = get_until_end(Rest0),
    {TryClauseTokens1, CatchClauseTokens} =
        case get_until_any(['catch'], TryClauseTokens0) of
            {_Tokens, [], not_found} -> {TryClauseTokens0, []};
            {R, A, Token} -> {R, [Token | A]}
        end,
    {TryForceBreak, Clauses, []} = clauses(TryClauseTokens1),
    Catch = catch_(CatchClauseTokens),
    ForceBreak =
        case length(Clauses) > 1 of
            true -> force_break;
            false -> TryForceBreak
        end,
    GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
    Doc =
        force_break(
            ForceBreak,
            group(
                space(
                  [
                    cons(
                        group(space(text(<<"try">>), TryArg)),
                        nest(?indent, space(text(<<" of">>), GroupedClauses))
                    ),
                    Catch,
                    text(<<"end">>)
                  ]
                ),
                inherit
            )
        ),
    {ForceBreak, Doc, Rest1}.

-spec catch_(tokens()) -> doc().
catch_([]) -> empty();
catch_([{'catch', _} | Tokens]) ->
    {CatchForceBreak, Clauses, []} = clauses(Tokens),
    ForceBreak =
        case length(Clauses) > 1 of
            true -> force_break;
            false -> CatchForceBreak
        end,
    GroupedClauses = force_break(ForceBreak, group(space(Clauses), inherit)),
    Doc =
        force_break(
            ForceBreak,
            group(nest(?indent, space(text(<<"catch">>), GroupedClauses)), inherit)
        ),
    Doc.

-spec fun_(tokens()) -> {force_break(), doc(), tokens()}.
fun_([{'fun', _} | Tokens]) ->
    {ClauseTokens, Rest1, _} = get_until_end(Tokens),
    {ForceBreak, Clauses, []} = clauses(ClauseTokens),
    Doc = group(space(nest(?indent, space([text(<<"fun">>) | Clauses])), text(<<"end">>))),
    {ForceBreak, Doc, Rest1}.

-spec begin_(tokens()) -> {force_break(), doc(), tokens()}.
begin_([{'begin', _} | Tokens]) ->
    {BeginTokens, Rest, _} = get_until_end(Tokens),
    {empty, ForceBreak, Exprs, []} = exprs(BeginTokens),
    GroupedExprs = force_break(ForceBreak, group(space(Exprs), inherit)),
    Doc = group(space(nest(?indent, space(text(<<"begin">>), GroupedExprs)), text(<<"end">>))),
    {ForceBreak, Doc, Rest}.

-spec comment(string()) -> doc().
comment(Comment) -> text(list_to_binary(Comment)).

%%
%% Generic Erlang Terms
%%

-spec equation(doc(), doc(), force_break()) -> doc().
equation(Equals, Expr, ForceBreak) ->
    group(nest(?indent, force_break(ForceBreak, group(space(Equals, group(Expr)), inherit)))).

-spec list_group(tokens()) -> {force_break(), doc(), tokens()}.
list_group([{Open, _} | Rest0]) when ?IS_LIST_CHAR(Open) ->
    Close = close_bracket(Open),
    {Tokens, Rest1, _} = get_from_until(Open, Close, Rest0),
    {ForceBreak, ListGroup} = brackets(Tokens, op2b(Open), op2b(Close)),
    {ForceBreak, ListGroup, Rest1}.

-spec brackets(tokens(), binary(), binary()) -> {force_break(), doc()}.
brackets([], Open, Close) -> {no_force_break, group(cons(text(Open), text(Close)))};
brackets(Tokens, Open, Close) ->
    {ForceBreak, ListElements} = list_elements(Tokens),
    Doc =
        group(
            force_break(
                ForceBreak,
                stick(nest(?indent, stick(text(Open), ListElements)), text(Close))
            )
        ),
    {ForceBreak, Doc}.

-spec list_elements(tokens()) -> {force_break(), doc()}.
list_elements(Tokens) -> list_elements(Tokens, empty(), no_force_break).

-spec list_elements(tokens(), doc(), force_break()) -> {force_break(), doc()}.
list_elements([], Doc, ForceBreak) -> {ForceBreak, Doc};
list_elements(Tokens, Doc, ForceBreak0) ->
    {_End, ForceBreak1, Expr, Rest} = expr(Tokens, ForceBreak0, empty()),
    list_elements(Rest, space(Doc, group(Expr)), ForceBreak1).

-spec clauses(tokens()) -> {force_break(), list(doc()), tokens()}.
clauses([]) -> {no_force_break, [empty()], []};
clauses(Tokens) -> clauses(Tokens, [], []).

-spec clauses(tokens(), list(doc()), list(force_break())) -> {force_break(), list(doc()), tokens()}.
clauses(Tokens, Acc0, ForceBreaks0) ->
    {Continue, ClauseForceBreak, Clause, Rest0} = head_and_clause(Tokens),
    Acc1 = [Clause | Acc0],
    ForceBreaks1 = [ClauseForceBreak | ForceBreaks0],
    case Continue of
        continue -> clauses(Rest0, Acc1, ForceBreaks1);
        done ->
            ForceBreak = resolve_force_break(ForceBreaks1),
            {ForceBreak, lists:reverse(Acc1), Rest0}
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
    Doc2 = newline([Doc0, comment(Comment), Doc1]),
    {Continue, ForceBreak, Doc2, Tokens};
head_and_clause([{'->', _} | Rest0], Doc0) ->
    % End
    {Continue, ForceBreak, Clause, Rest1} = clause(Rest0),
    ClauseGroup =
        force_break(ForceBreak, nest(?indent, group(space(text(<<" ->">>), Clause), inherit))),
    Doc1 = group(cons(group(Doc0), ClauseGroup)),
    {Continue, ForceBreak, Doc1, Rest1};
head_and_clause([{'::', _} | Rest0], Doc) ->
    % Altenative End (for Type definitions)
    {Continue, ForceBreak, Clause, Rest1} = clause(Rest0),
    Doc1 =
        cons(
            [
                Doc,
                text(<<" :: ">>),
                force_break(ForceBreak, underneath(- 2, group(Clause, inherit)))
            ]
        ),
    {Continue, ForceBreak, Doc1, Rest1};
head_and_clause(Rest0, Doc0) ->
    {Tokens, Rest1, Token} = get_until('->', Rest0),
    {empty, _ForceBreak, Doc1, []} = expr(Tokens, no_force_break, Doc0),
    head_and_clause([Token | Rest1], Doc1).

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
expr(Tokens, ForceBreak) -> expr(Tokens, ForceBreak, empty()).

-spec expr(tokens(), force_break(), doc()) ->
    {dot | ';' | ',' | empty | comment, force_break(), doc(), tokens()}.
expr(Tokens, ForceBreak0, Doc) ->
    {ExprTokens, Rest} = get_end_of_expr(Tokens),
    {End, ForceBreak1, Expr} = expr_(ExprTokens, Doc, ForceBreak0),
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
expr_([{'#', LineNum}, {atom, LineNum, Atom}, {'{', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
    % Handle records
    % #record_name{key => value}
    [_, _ | Tokens1] = Tokens0,
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    Record = group(cons([text(<<"#">>), text(a2b(Atom)), ListGroup])),
    expr_(Rest, space(Doc, Record), ForceBreak1);
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
expr_(
    [
        {'fun', _},
        {atom, LineNum, ModuleName},
        {':', LineNum},
        {atom, LineNum, FunctionName},
        {'/', LineNum},
        {integer, LineNum, Arity} | Rest
    ],
    Doc,
    ForceBreak
) ->
    % Handle `fun module:function/arity`
    Fun =
        cons(
            [
                text(<<"fun ">>),
                text(a2b(ModuleName)),
                text(<<":">>),
                text(a2b(FunctionName)),
                text(<<"/">>),
                text(i2b(Arity))
            ]
        ),
    expr_(Rest, space(Doc, Fun), ForceBreak);
expr_(
    [{'fun', _}, {atom, LineNum, FunctionName}, {'/', LineNum}, {integer, LineNum, Arity} | Rest],
    Doc,
    ForceBreak
) ->
    % Handle `fun function/arity`
    Fun = cons([text(<<"fun ">>), text(a2b(FunctionName)), text(<<"/">>), text(i2b(Arity))]),
    expr_(Rest, space(Doc, Fun), ForceBreak);
expr_([{'fun', _} | _] = Tokens, Doc, ForceBreak0) ->
    {GroupForceBreak, Group, Rest} = fun_(Tokens),
    ForceBreak1 = resolve_force_break([ForceBreak0, GroupForceBreak]),
    expr_(Rest, space(Doc, Group), ForceBreak1);
expr_(
    [{atom, LineNum, ModuleName}, {':', LineNum}, {atom, LineNum, FunctionName}, {'(', LineNum} | _]
        =
        Tokens0,
    Doc,
    ForceBreak0
) ->
    % Handle function calls to other modules
    [_, _, _ | Tokens1] = Tokens0,
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    Function =
        space(Doc, cons([text(a2b(ModuleName)), text(<<":">>), text(a2b(FunctionName)), ListGroup])),
    expr_(Rest, Function, ForceBreak1);
expr_([{atom, LineNum, FunctionName}, {'(', LineNum} | _] = Tokens0, Doc, ForceBreak0) ->
    % Handle local function calls
    Tokens1 = tl(Tokens0),
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens1),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    Function = space(Doc, cons(text(a2b(FunctionName)), ListGroup)),
    expr_(Rest, Function, ForceBreak1);
expr_([{C, _} | _] = Tokens, Doc, ForceBreak0) when ?IS_LIST_CHAR(C) ->
    % Handle lists
    {ListForceBreak, ListGroup, Rest} = list_group(Tokens),
    ForceBreak1 = resolve_force_break([ForceBreak0, ListForceBreak]),
    expr_(Rest, space(Doc, ListGroup), ForceBreak1);
expr_([{C, _} | Rest0], Doc0, ForceBreak0) when ?IS_EQUALS(C) ->
    % Handle things like
    % Arg3 =
    %     Arg1 + Arg2,
    Equals = group(space(Doc0, text(op2b(C)))),
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
                    Equation = equation(Equals, Expr, RestForceBreak),
                    ForceBreak1 = resolve_force_break([ForceBreak0, RestForceBreak]),
                    expr_([EndToken | Rest1], Equation, ForceBreak1)
            end;
        false ->
            {End, RestForceBreak, Expr} = expr_(Rest0, empty(), no_force_break),
            Equation = equation(Equals, Expr, RestForceBreak),
            ForceBreak1 = resolve_force_break([ForceBreak0, RestForceBreak]),
            {End, ForceBreak1, Equation}
    end;
expr_([{End, _}], Doc, ForceBreak) ->
    % Handle the expression end character
    {End, ForceBreak, cons(Doc, text(op2b(End)))};
expr_([{atom, _, Atom}, {'/', _}, {integer, _, Int} | Rest], Doc, ForceBreak) ->
    % Handle function arity expressions
    % some_fun/1
    FunctionDoc = cons([text(a2b(Atom)), text(<<"/">>), text(i2b(Int))]),
    expr_(Rest, space(Doc, FunctionDoc), ForceBreak);
expr_([{var, _, Var}, {'/', _}, {atom, _, Atom} | Rest], Doc, ForceBreak) ->
    % Handle binary matching
    % <<Thing/binary>>
    TermDoc = cons([text(v2b(Var)), text(<<"/">>), text(a2b(Atom))]),
    expr_(Rest, space(Doc, TermDoc), ForceBreak);
expr_(
    [{var, _, Var}, {':', _}, {integer, _, Integer}, {'/', _}, {atom, _, Atom} | Rest],
    Doc,
    ForceBreak
) ->
    % Handle more binary matching
    % <<Thing:1/binary, Rest/binary>>
    TermDoc =
        cons([text(v2b(Var)), text(<<":">>), text(i2b(Integer)), text(<<"/">>), text(a2b(Atom))]),
    expr_(Rest, space(Doc, TermDoc), ForceBreak);
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
            expr_(
                [EndToken | Rest1],
                space(Doc, group(space(text(op2b(BoolOp)), Expr))),
                ForceBreak1
            )
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
expr_([{'when', _} | Rest0], Doc, ForceBreak0) ->
    {End, ForceBreak1, Expr} = expr_(Rest0, empty(), ForceBreak0),
    Group = group(nest(?indent, space(text(<<"when">>), Expr))),
    {End, ForceBreak1, group(space(Doc, group(Group)))};
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
format(W, K, [{I, _, {doc_force_break, X}} | Rest]) -> format(W, K, [{I, break, X} | Rest]);
format(W, K, [{I, break, {doc_group, X, inherit}} | Rest]) -> format(W, K, [{I, break, X} | Rest]);
format(W, K, [{I, _, {doc_group, X, _}} | Rest]) ->
    case fits(W - K, [{I, flat, X}]) of
        true -> format(W, K, [{I, flat, X} | Rest]);
        false -> format(W, K, [{I, break, X} | Rest])
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
fits(W, [{I, _, {doc_group, X, _}} | Rest]) -> fits(W, [{I, flat, X} | Rest]).

-spec sdoc_to_string(sdoc()) -> binary().
sdoc_to_string(s_nil) -> <<"">>;
sdoc_to_string({s_text, String, Doc}) ->
    DocString = sdoc_to_string(Doc),
    <<String/binary, DocString/binary>>;
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
a2b(Atom) -> list_to_binary(escape(Atom)).

-spec i2b(integer()) -> binary().
i2b(Integer) -> integer_to_binary(Integer).

-spec f2b(float()) -> binary().
f2b(Float) -> list_to_binary(io_lib:format("~p", [Float])).

-spec c2b(integer()) -> binary().
c2b(Char) -> list_to_binary(io_lib:format("$~c", [Char])).

-spec s2b(string()) -> binary().
s2b("") -> <<"\"\"">>;
s2b(String) -> list_to_binary(escape(String)).

-spec escape(string() | atom()) -> string().
escape(Term) -> io_lib:format("~p", [Term]).

%%
%% Utils
%%

-spec repeat(binary(), integer()) -> binary().
repeat(Bin, Times) when Times >= 0 -> repeat_(<<>>, Bin, Times).

-spec repeat_(binary(), binary(), integer()) -> binary().
repeat_(Acc, _, 0) -> Acc;
repeat_(Acc, Bin, Times) -> repeat_(<<Acc/binary, Bin/binary>>, Bin, Times - 1).

-spec get_from_until(atom(), atom(), tokens()) -> {tokens(), tokens(), token()}.
get_from_until(Start, End, Tokens) -> get_from_until(Start, End, Tokens, [], 0).

-spec get_from_until(atom(), atom(), tokens(), tokens(), integer()) ->
    {tokens(), tokens(), token()}.
get_from_until(Start, End, [{Start, _} = Token | Rest], Acc, Stack) ->
    get_from_until(Start, End, Rest, [Token | Acc], Stack + 1);
get_from_until(_Start, End, [{End, _} = Token | Rest], Acc, 0) -> {lists:reverse(Acc), Rest, Token};
get_from_until(Start, End, [{End, _} = Token | Rest], Acc, Stack) ->
    get_from_until(Start, End, Rest, [Token | Acc], Stack - 1);
get_from_until(Start, End, [Token | Rest], Acc, Stack) ->
    get_from_until(Start, End, Rest, [Token | Acc], Stack).

-spec get_until(atom(), tokens()) -> {tokens(), tokens(), token()}.
get_until(End, Tokens) -> get_until(End, Tokens, []).

-spec get_until(atom(), tokens(), tokens()) -> {tokens(), tokens(), token()}.
get_until(End, [{End, _} = Token | Rest], Acc) -> {lists:reverse(Acc), Rest, Token};
get_until(End, [Token | Rest], Acc) -> get_until(End, Rest, [Token | Acc]).

-spec get_until_end(tokens()) -> {tokens(), tokens(), token()}.
get_until_end(Tokens) -> get_until_end(Tokens, [], []).

-spec get_until_end(tokens(), tokens(), list(atom())) -> {tokens(), tokens(), token()}.
get_until_end([{'end', _} = Token | Rest], Acc, []) -> {lists:reverse(Acc), Rest, Token};
get_until_end([{'end', _} = Token | Rest], Acc, Stack) ->
    get_until_end(Rest, [Token | Acc], tl(Stack));
get_until_end([{Keyword, _} = Token | Rest], Acc, Stack) when ?IS_TERMINATED_KEYWORD(Keyword) ->
    get_until_end(Rest, [Token | Acc], [Keyword | Stack]);
get_until_end([Token | Rest], Acc, Stack) -> get_until_end(Rest, [Token | Acc], Stack).

-spec get_until_any(list(atom()), tokens()) -> {tokens(), tokens(), token() | not_found}.
get_until_any(Ends, Tokens) -> get_until_any(Ends, Tokens, [], []).

-spec get_until_any(list(atom()), tokens(), tokens(), list(atom())) ->
    {tokens(), tokens(), token() | not_found}.
get_until_any(_Ends, [], Acc, []) -> {lists:reverse(Acc), [], not_found};
get_until_any(Ends, [{Open, _} = Token | Rest], Acc, Stack) when ?IS_LIST_CHAR(Open) ->
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
get_end_of_expr(Tokens) -> get_end_of_expr(Tokens, [], 0, []).

% Dialyzer gets upset if we use integer() for the third arg here but that's what it is.
-spec get_end_of_expr(tokens(), tokens(), any(), list(atom())) -> {tokens(), tokens()}.
get_end_of_expr([], Acc, _LineNum, _KeywordStack) -> {lists:reverse(Acc), []};
get_end_of_expr([{comment, _, _} = Comment | Rest], [], _LineNum, _KeywordStack) ->
    {[Comment], Rest};
get_end_of_expr([{comment, LineNum, _} = Comment | Rest], Acc, LineNum, []) ->
    % Inline comment - naughty naughty
    % Return the comment and put the acc back.
    % We must only do this when the keyword stack is empty otherwise we'll put the comment
    % in the wrong place.
    {[Comment], lists:reverse(Acc) ++ Rest};
get_end_of_expr([{End, LineNum} = Token, {comment, LineNum, _} = Comment | Rest], Acc, _, [])
when End == ',' orelse End == ';' orelse End == dot ->
    % Inline comment - naughty naughty
    % Return the comment and put the acc back.
    % We must only do this when the keyword stack is empty otherwise we'll put the comment
    % in the wrong place.
    {[Comment], lists:reverse([Token | Acc]) ++ Rest};
get_end_of_expr([{comment, _, _} | _] = Rest, Acc, _LineNum, []) ->
    % This can happen if there are comments after the final element in a list
    {lists:reverse(Acc), Rest};
get_end_of_expr([{End, _} = Token | Rest], Acc, _, [])
when End == ',' orelse End == ';' orelse End == dot ->
    {lists:reverse([Token | Acc]), Rest};
get_end_of_expr([{'end', _} = Token | Rest], Acc, _LineNum, []) ->
    {lists:reverse([Token | Acc]), Rest};
get_end_of_expr([{'end', _} = Token | Rest], Acc, LineNum, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, tl(KeywordStack));
get_end_of_expr([{Keyword, _} = Token | Rest], Acc, LineNum, KeywordStack)
when Keyword == 'case' orelse Keyword == 'if' orelse Keyword == 'receive' orelse Keyword == 'try' orelse Keyword == 'begin' ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, [Keyword | KeywordStack]);
get_end_of_expr([{'fun', _} = Token, {'(', _} | _] = Tokens, Acc, LineNum, KeywordStack) ->
    % We only expect an 'end' if this is an anon function and not pointing to another function:
    % ignore:     `fun local/1`
    % care about: `fun (X) -> X + 1 end`
    Rest = tl(Tokens),
    get_end_of_expr(Rest, [Token | Acc], LineNum, ['fun' | KeywordStack]);
get_end_of_expr([{Open, _} = Token | Rest0], Acc, _, KeywordStack) when ?IS_LIST_CHAR(Open) ->
    Close = close_bracket(Open),
    {Tokens, Rest1, {Close, LineNum} = EndToken} = get_from_until(Open, Close, Rest0),
    get_end_of_expr(
        Rest1,
        [EndToken] ++ lists:reverse(Tokens) ++ [Token | Acc],
        LineNum,
        KeywordStack
    );
get_end_of_expr([{_, LineNum} = Token | Rest], Acc, _, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack);
get_end_of_expr([{_, LineNum, _} = Token | Rest], Acc, _, KeywordStack) ->
    get_end_of_expr(Rest, [Token | Acc], LineNum, KeywordStack).

-spec resolve_force_break(list(force_break())) -> force_break().
resolve_force_break(Args) ->
    case lists:any(fun (X) -> X == force_break end, Args) of
        true -> force_break;
        false -> no_force_break
    end.

-spec is_bool_list(tokens()) -> boolean().
is_bool_list([]) -> false;
is_bool_list([{Op, _} | _]) when ?IS_BOOL_CONCATENATOR(Op) -> true;
is_bool_list([_ | Rest]) -> is_bool_list(Rest).

-spec close_bracket(atom()) -> atom().
close_bracket('(') -> ')';
close_bracket('[') -> ']';
close_bracket('{') -> '}';
close_bracket('<<') -> '>>'.

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
