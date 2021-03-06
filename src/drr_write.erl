%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_write data structure
%%% @doc
%%% @end
%%%===================================================================
-module(drr_write).
-export([struct/0]).
-export([parse/1, parse/2]).
-include("zfs_drr.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec struct() -> list().
struct() ->
     [drr_type, drr_pad, drr_offset, drr_length
     ,drr_toguid, drr_checksumtype, drr_checksumflags
     ,{drr_pad, parse, 6}, drr_key].

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
-spec parse(bitstring(), list()) -> {ok, map(), bitstring()}.
parse(<<DRR_OBJECT:?DRR_OBJECT_SIZE, Rest/bitstring>>
	, _Opts) ->
    struct_parser:do(Rest, struct()).
