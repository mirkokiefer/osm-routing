-module(xml_parser).

-export([parse_file/2]).

-define(chunk, 10000).

parse_file(File, Fun) ->
  G = fun continue_file/2,
  {ok, Handle} = file:open(File, [read, raw, binary]),
  Position = 0,
  CState = {Handle, Position, ?chunk},
  SaxCallbackState = undefined,
  
  erlsom:parse_sax(<<>>, SaxCallbackState, Fun, [{continuation_function, G, CState}]).
  
continue_file(Tail, {Handle, Offset, Chunk}) ->
  %% read the next chunk
  case file:pread(Handle, Offset, Chunk) of
    {ok, Data} ->
      {<<Tail/binary, Data/binary>>, {Handle, Offset + Chunk, Chunk}};
    eof ->
      {Tail, {Handle, Offset, Chunk}}
  end.