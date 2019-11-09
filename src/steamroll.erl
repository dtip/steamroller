-module(steamroll).

-export([init/1, format_file/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = steamroll_prv:init(State),
    {ok, State1}.

-spec format_file(binary()) -> ok | {error, any()}.
format_file(File) when is_binary(File) ->
    steamroll_formatter:format(File).
