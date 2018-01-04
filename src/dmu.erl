%%%===================================================================
%%%
%%%===================================================================
-module(dmu).
-export([replay_record/0, replay_record/1, replay_record/2]).
-include("zfs.hrl").
-include_lib("eunit/include/eunit.hrl").
-ifdef(debug).
-compile(export_all).
-endif.

%%%===================================================================
%%% API SECTION - EXPORTED FUNCTION
%%%===================================================================
-spec replay_record() -> list().
replay_record() -> 
  [drr_type, drr_payloadlen, drr_begin].

-spec replay_record(bitstring()) -> {ok, map(), bitstring()}.
replay_record(Bitstring) ->
  replay_record(Bitstring, []).

-spec replay_record(bitstring(), list()) -> {ok, map(), bitstring()}.
replay_record(<<0:32, PayloadLen:32, Rest/bitstring>>, _Opts) -> 
  << Begin:(304*8)/bitstring, _/bitstring>> = Rest,
  {ok, Struct, <<>>} = drr:struct_begin(Begin),
  Init = #{ drr_type => 'begin'
          , drr_payloadlen => PayloadLen
          , content => Struct },
  {ok, Init, Rest};
replay_record(<<1:32, PayloadLen:32, Rest/bitstring>>, _Opts) ->
  ok.

%%%===================================================================
%%% PRIVATE SECTION 
%%%===================================================================
-spec drr_payloadlen(bitstring()) -> {ok, map(), bitstring()}.
drr_payloadlen(<<PayloadLen:32/little, Rest/bitstring>>) -> 
  {ok, #{ payloadlen => PayloadLen }, Rest}.

drr_payloadlen_test() ->
  [ drr_payloadlen_test(X) || X <- lists:seq(1,1) ].

drr_payloadlen_test(1) ->
  IN = <<1,0,0,0>>,
  OUT = {ok, #{ payloadlen => 1 }, <<>>},
  ?assertEqual(OUT, drr_payloadlen(IN)).

