%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_toguid data structure
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_toguid).
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
    ?DRR_TOGUID_SIZE.

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
parse(<<ToGuid:?DRR_TOGUID_SIZE/bitstring, Rest/bitstring>>, _Opts) -> 
    ?debugFmt("toguid: ~p", [ToGuid]),
    {ok, #{ toguid => ToGuid }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
  IN = <<16#36, 16#50, 16#0f, 16#21, 16#f9, 16#72, 16#e9, 16#87>>,
  OUT = {ok, #{ toguid => <<54,80,15,33,249,114,233,135>>}, <<>>},
  ?assertEqual(OUT, parse(IN)).
