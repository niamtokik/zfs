%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_magic data structure 
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_magic).
-export([struct/0, size/0]).
-export([parse/1, parse/2]).
-include("zfs_drr.hrl").
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
-spec size() -> non_neg_integer().
size() ->
    ?DRR_MAGIC_SIZE.

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
parse(<<16#2F5bacbac:?DRR_MAGIC_SIZE/little, Rest/bitstring>>, _Opts) -> 
  ?debugFmt("drr_magic: ok", []),
  {ok, #{ magic => <<16#2F5bacbac:64/little>> }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
  IN = <<172,203,186,245,2,0,0,0>>,
  OUT = {ok, #{magic => <<16#2F5bacbac:64/little>>}, <<>>},
  ?assertEqual(OUT, parse(IN)).
parse_0002_test() ->
  IN = <<1,2,3,4, 5,6,7,8>>,
  ?assertException(error, function_clause, parse(IN)).
