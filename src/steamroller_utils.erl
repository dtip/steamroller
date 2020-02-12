-module(steamroller_utils).

-export([split_header_files/1]).

-spec split_header_files(list(binary())) -> {list(binary()), list(binary())}.
split_header_files(Files) -> split_header_files(Files, [], []).

split_header_files([File | Rest], Headers, Others) ->
  Size = byte_size(File) - 3,
  case File of
    <<_:Size/binary, "hrl">> -> split_header_files(Rest, [File | Headers], Others);
    _ -> split_header_files(Rest, Headers, [File | Others])
  end;
split_header_files([], Headers, Others) ->
  % The reversal here is only so the files are processed in alphabetical order.
  {lists:reverse(Headers), lists:reverse(Others)}.
