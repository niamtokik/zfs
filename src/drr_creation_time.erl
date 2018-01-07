%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_creation_time data structure 
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_creation_time).
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
    ?DRR_CREATIONTIME_SIZE.

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
parse(<<CreationTime:?DRR_CREATIONTIME_SIZE/little, Rest/bitstring>>
     , _Opts) -> 
    ?debugFmt("drr_creation_time: ~p", [CreationTime]),
    {ok, #{ creation_time => CreationTime }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<16#66, 16#2f, 16#29, 16#5a,0,0,0,0>>,
    OUT = {ok, #{ creation_time => 16#5a292f66 }, <<>>},
    ?assertEqual(OUT, parse(IN)).
