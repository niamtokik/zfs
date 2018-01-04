%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2017
%%% @title Fletcher Checksums Implementation
%%% @doc fletcher checksum naive implementation. All this code
%%%      is not optimized and was made only for testing purpose.
%%% @end
%%% @see sys/cddl/contrib/opensolaris/common/zfs/zfs_fletcher.c
%%%===================================================================
-module(fletcher).
-export([fletcher16/1]).
-export([fletcher32/1]).
-export([fletcher64/1]).
-export([adler32/1]).
-include_lib("eunit/include/eunit.hrl").
-define(ENDIANESS, little).

%%--------------------------------------------------------------------
%% @doc naive fletcher16 implementation
%% @end
%%--------------------------------------------------------------------
-spec fletcher16(bitstring()) -> integer().
fletcher16(Bitstring) -> 
  fletcher16(Bitstring, {0, 0}).

-spec fletcher16(bitstring(), {integer(), integer()}) -> integer().
fletcher16(<<>>, {Sum1, Sum2}) ->
  (Sum2 bsl 8) bor Sum1;
fletcher16(<<Char:8, Rest/bitstring>>, {Sum1, Sum2}) -> 
  S1 = (Sum1 + Char) rem 255,
  S2 = (Sum2 + S1) rem 255,
  fletcher16(Rest, {S1, S2}).

fletcher16_test() ->
  [ fletcher16_test(X) || X <- lists:seq(1,3) ].

fletcher16_test(1) ->
  IN = <<"abcde">>,
  OUT = 51440,
  ?assertEqual(OUT, fletcher16(IN));
fletcher16_test(2) ->
  IN = <<"abcdef">>,
  OUT = 8279,
  ?assertEqual(OUT, fletcher16(IN));
fletcher16_test(3) ->
  IN = <<"abcdefgh">>,
  OUT = 1575,
  ?assertEqual(OUT, fletcher16(IN)).

%%--------------------------------------------------------------------
%% @doc naive fletcher32 implementation
%% @end
%%--------------------------------------------------------------------
fletcher32(Bitstring) -> 
  fletcher32(Bitstring, {0, 0}).

fletcher32(<<>>, {Sum1, Sum2}) ->
  S1 = Sum1 rem 65535,
  S2 = Sum2 rem 65535,
  (S2 bsl 16) bor S1; 
fletcher32(<<Char:16/?ENDIANESS, Rest/bitstring>>, {Sum1, Sum2}) ->
  S1 = (Sum1 + Char),
  S2 = (Sum2 + S1),
  fletcher32(Rest, {S1, S2});
fletcher32(<<Char:8/?ENDIANESS, Rest/bitstring>>, {Sum1, Sum2}) ->
  S1 = (Sum1 + Char),
  S2 = (Sum2 + S1),
  fletcher32(Rest, {S1, S2}).

fletcher32_test() ->
  [ fletcher32_test(X) || X <- lists:seq(1,3) ].

fletcher32_test(1) ->
  IN = <<"abcde">>,
  OUT = 4031760169,
  ?assertEqual(OUT, fletcher32(IN));
fletcher32_test(2) ->
  IN = <<"abcdef">>,
  OUT = 1448095018,
  ?assertEqual(OUT, fletcher32(IN));
fletcher32_test(3) ->
  IN = <<"abcdefgh">>,
  OUT = 3957429649,
  ?assertEqual(OUT, fletcher32(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
fletcher64(Bitstring) ->
  fletcher64(Bitstring, {0, 0}).

fletcher64(<<>>, {Sum1, Sum2}) ->
  S1 = Sum1 rem 4294967295,
  S2 = Sum2 rem 4294967295,
  (S2 bsl 32) bor S1;
fletcher64(<<Char:32/?ENDIANESS, Rest/bitstring>>, {Sum1, Sum2}) ->
  S1 = Sum1 + Char,
  S2 = Sum2 + S1,
  fletcher64(Rest, {S1, S2}); 
fletcher64(<<Char:16/?ENDIANESS, Rest/bitstring>>, {Sum1, Sum2}) ->
  S1 = Sum1 + Char,
  S2 = Sum2 + S1,
  fletcher64(Rest, {S1, S2}); 
fletcher64(<<Char:8/?ENDIANESS, Rest/bitstring>>, {Sum1, Sum2}) ->
  S1 = Sum1 + Char,
  S2 = Sum2 + S1,
  fletcher64(Rest, {S1, S2}).

fletcher64_test() ->
  [ fletcher64_test(X) || X <- lists:seq(1,3) ].

fletcher64_test(1) ->
  IN = <<"abcde">>,
  OUT = 14467467625952928454,
  ?assertEqual(OUT, fletcher64(IN));
fletcher64_test(2) ->
  IN = <<"abcdef">>,
  OUT = 14467579776138987718,
  ?assertEqual(OUT, fletcher64(IN));
fletcher64_test(3) ->
  IN = <<"abcdefgh">>,
  OUT = 3543817411021686982,
  ?assertEqual(OUT, fletcher64(IN)).

%%--------------------------------------------------------------------
%% @doc naive adler32 implementation. Erlang is delivered with its
%%      own adler implementation, see erlang:adler32/1 function.
%% @end
%%--------------------------------------------------------------------
adler32(Bitstring) -> 
  adler32(Bitstring, {1, 0}).

adler32(<<>>, {C1, C2}) ->
  (C2 bsl 16) bor C1;
adler32(<<Char:8, Rest/bitstring>>, {C1, C2}) ->
  S1 = C1 + Char rem 65521,
  S2 = C2 + S1 rem 65521,
  adler32(Rest, {S1, S2}).

adler32_test() ->
  [ adler32_test(X) || X <- lists:seq(1,3) ].
adler32_test(1) ->
  IN = <<"abcde">>,
  OUT = 16#05C801F0,
  ?assertEqual(OUT, adler32(IN));
adler32_test(2) ->
  IN = <<"abcdef">>,
  OUT = 16#081E0256,
  ?assertEqual(OUT, adler32(IN));
adler32_test(3) ->
  IN = <<"abcdefgh">>,
  OUT = 16#0E000325,
  ?assertEqual(OUT, adler32(IN)).
