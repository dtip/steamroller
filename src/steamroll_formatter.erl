-module(steamroll_formatter).

-export([format/1]).

%% API


-spec format(binary()) -> ok | {error, any()}.
format(_File) ->
    {error, not_implemented}.

