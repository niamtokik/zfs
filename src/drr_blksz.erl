%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_blksz data structure 
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_blksz).
-export([struct/0]).
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
-spec parse(bitstring()) -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(), list()) 
        -> {ok, map(), bitstring()}.
parse(<<BlkSz:?DRR_BLKSZ_SIZE/little, Rest/bitstring>>, _Opts) -> 
    ?debugFmt("drr_blksz: ~p", [BlkSz]),
    {ok, #{ blksz => BlkSz }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    error.
