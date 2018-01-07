%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_fromguid data structure 
%%% @doc this structure is used in multiple data structure and is 
%%%      mainly used for alignment. This module is currently not
%%%      finish (yet), its API isn't conform to other API.
%%% @end
%%%===================================================================
-module(drr_pad).
-export([struct/0]).
-export([parse/1, parse/2, parse/3]).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct() -> list().
struct() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring()) -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(), list()) 
	   -> {ok, map(), bitstring()}.
parse(Bitstring, _Opts) -> 
    parse(Bitstring, 1, _Opts).

%%--------------------------------------------------------------------
%% @doc drr_pad and drr_pad2 are "dynamic" and could have multiple
%%      size. In zfs_ioctl.h, we can find multiple definition for this
%%      data_structure:
%%        - uint32
%%        - uint64
%%        - uint8
%%        - unit8[6].
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(), integer(), list()) 
	   -> {ok, map(), bitstring()}.
parse(Bitstring, Padding, _Opts) ->
    Value = Padding*8,
    <<Pad:Value/bitstring, Rest/bitstring>> = Bitstring,
    ?debugFmt("pad (~p): ~p~n", [Value, Pad]),
    {ok, #{ pad => Pad }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<0,0,0,0>>,
    OUT = {ok, #{pad => <<0>>}, <<0,0,0>>},
    ?assertEqual(OUT, parse(IN)).
parse_0002_test() ->
    IN = <<1,2,3,4,5,6,7,8>>,
    OUT = {ok, #{pad => <<1,2,3>>}, <<4,5,6,7,8>>},
    ?assertEqual(OUT, parse(IN, 3, [])).
