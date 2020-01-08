-module(steamroller_formatter).

-export([format/2, format_code/1, test_format/1]).

-define(CRASHDUMP, "steamroller.crashdump").
-define(default_line_length, 100).
-define(default_includes, []).

%% API

-spec format(binary(), list(any())) -> ok | {error, any()}.
format(File, Opts) ->
    Check = lists:member(check, Opts),
    LineLength = proplists:get_value(line_length, Opts, ?default_line_length),
    Includes = proplists:get_value(includes, Opts, ?default_includes),
    case file:read_file(File) of
        {ok, Code} ->
            try
                case format_code(Code, File, LineLength, Includes) of
                    {ok, Code} -> ok;
                    {ok, FormattedCode} ->
                        case Check of
                            true -> {error, <<"Check failed: code needs to be formatted.">>};
                            false -> file:write_file(File, FormattedCode)
                        end;
                    {error, _} = Err -> Err
                end
            catch
                {complaint, partial_case_statement} ->
                    {
                        error,
                        {
                            complaint,
                            File,
                            <<
                                "There seems to be a partial case statement in this file. ",
                                "Probably within an unused macro."
                            >>
                        }
                    };
                {complaint, reached_dot_before_closing_bracket} ->
                    {
                        error,
                        {
                            complaint,
                            File,
                            <<
                                "There seems to be unbalanced brackets in this file. ",
                                "Probably within a macro. ",
                                "Steamroller currently does not support this."
                            >>
                        }
                    };
                {complaint, Reason} -> {error, {complaint, File, Reason}}
            end;
        {error, enoent} -> {error, <<"file does not exist">>};
        {error, eisdir} -> {error, <<"that's a directory">>};
        {error, _} = Err -> Err
    end.

-spec format_code(binary()) -> ok | {error, any()}.
format_code(Code) -> format_code(Code, <<"no_file">>, ?default_line_length, ?default_includes).

% For testing.
% We give the file a proper name so that we compare the ASTs.
-spec test_format(binary()) -> ok | {error, any()}.
test_format(Code) -> format_code(Code, <<"test.erl">>, ?default_line_length, ?default_includes).

%% Internal

-spec format_code(binary(), binary(), integer(), list(file:name_all())) ->
    {ok, binary()} | {error, any()}.
format_code(Code, File, LineLength, Includes) ->
    {ok, R} = re:compile("\\.[he]rl$"),
    % This is a last resort for unhappy files. It's an awful solution.
    ASTCheckSkipFiles =
        [
            % This file contains the line
            % `-l(?LINE)`
            % and this line is moved by the autoformatter.
            % But the ?LINE macro is special and is converted into an integer by `epp`.
            % We end up with
            % `{attribute,20,l,20},`
            % in the pre-formatted AST and
            % `{attribute,21,l,21},`
            % in the post-formatted AST.
            % Pretty sure this is fine so lets skip the AST check.
            <<"erlang/otp/lib/stdlib/test/epp_SUITE_data/mac3.erl">>
        ],
    NoMatch = fun (IgnoreFile) -> nomatch == binary:match(File, IgnoreFile) end,
    case {re:run(File, R), lists:all(NoMatch, ASTCheckSkipFiles)} of
        {{match, _}, true} ->
            % Check the AST after formatting for source files.
            case steamroller_ast:ast(Code, File, Includes) of
                {ok, OriginalAst} ->
                    case steamroller_ast:tokens(Code) of
                        {ok, Tokens} ->
                            FormattedCode = steamroller_algebra:format_tokens(Tokens, LineLength),
                            case steamroller_ast:ast(FormattedCode, File, Includes) of
                                {ok, NewAst} ->
                                    case steamroller_ast:eq(OriginalAst, NewAst) of
                                        true -> {ok, FormattedCode};
                                        false ->
                                            handle_formatting_error(
                                                {error, ast_mismatch},
                                                File,
                                                FormattedCode
                                            )
                                    end;
                                {error, _} = Err ->
                                    handle_formatting_error(Err, File, FormattedCode)
                            end;
                        {error, Msg} -> {error, {File, Msg}}
                    end;
                {error, _} = Err -> Err
            end;
        _ ->
            % Don't check the AST for config files and for files in our ignore list.
            case steamroller_ast:tokens(Code) of
                {ok, Tokens} -> {ok, steamroller_algebra:format_tokens(Tokens, LineLength)};
                {error, Msg} -> {error, {File, Msg}}
            end
    end.

handle_formatting_error({error, _} = Err, File, FormattedCode) ->
    file:write_file(?CRASHDUMP, FormattedCode),
    {error, {formatter_broke_the_code, {file, File}, Err, {crashdump, ?CRASHDUMP}}}.
