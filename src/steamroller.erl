-module(steamroller).

-export([init/1, format_file/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = steamroller_prv:init(State),
    {ok, State1}.

-spec format_file(binary()) -> ok | {error, any()}.
format_file(File) when is_binary(File) -> steamroller_formatter:format(File).
