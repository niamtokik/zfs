%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_fromguid data structure 
%%% @doc this structure is used in drr_begin.
%%% @end
%%%===================================================================
-module(drr_fromguid).
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
    ?DRR_FROMGUID_SIZE.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring()) -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc drr_fromguid is defined as 64bits unsigned integer (uint64),
%%      you can find its definition here: 
%%      sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#135
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(), list()) -> {ok, map(), bitstring()}.
parse(<<FromGuid:?DRR_FROMGUID_SIZE/bitstring, Rest/bitstring>>
     ,_Opts) -> 
    ?debugFmt("drr_fromguid: ~p", [FromGuid]),
    {ok, #{ fromguid => FromGuid }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<0:64>>,
    OUT = {ok, #{fromguid => <<0:64>>}, <<>>},
    ?assertEqual(OUT, parse(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0002_test() ->
    IN = <<1,0,0,0
	  ,0,0,0,0>>,
    OUT = {ok, #{fromguid => IN}, <<>>},
    ?assertEqual(OUT, parse(IN)).
