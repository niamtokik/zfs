%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_key data structure 
%%% @doc 
%%% @end
%%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/spa.h#133
%%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/ddt.h#64
%%%===================================================================
-module(drr_key).
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
    [zio_cksum, ddk_prop].

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
parse(Bitstring, _Opts) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    error.

