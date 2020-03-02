%%
%% @doc
%%
%% == Steamroller ==
%%
%% Steamroller is an opinionated Erlang code formatter.
%%
%% See the <a href="https://github.com/old-reliable/steamroller">Github README</a> for more info.
%%
%% === Use ===
%%
%% Add steamroller to your rebar config:
%%
%% ```
%% {plugins, [steamroller]}.
%% '''
%%
%% Then ask it to steamroll your code directly in an existing application:
%%
%% ```
%% $ rebar3 steamroll
%% ===> Fetching steamroller
%% ===> Compiling steamroller
%% <Steamroller Output>
%% '''
%%
%% === CI ===
%%
%% You can use steamroller to check that code is properly formatted as part of your CI:
%%
%% ```
%% $ rebar3 steamroll --check
%% '''
%%
%% The exit code will be non-zero if the code has not been formatted before being committed.
%%
%% === Configure ===
%%
%% See the <a href="https://github.com/old-reliable/steamroller">Github README</a> for the latest configuration options.
%%
%% @end
%%

-module(steamroller).

-export([init/1, format_file/1, format_file/2]).

%% ===================================================================
%% Public API
%% ===================================================================
%% @doc Initialises the rebar3 plugin.

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, State1} = steamroller_prv:init(State),
  {ok, State1}.

%% @doc Format a file.

-spec format_file(binary()) -> ok | {error, any()}.
format_file(File) -> format_file(File, []).

%% @doc Format a file with options.

-spec format_file(binary(), list(any())) -> ok | {error, any()}.
format_file(File, Opts) when is_binary(File) -> steamroller_formatter:format(File, Opts).
