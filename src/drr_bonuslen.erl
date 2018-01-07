%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_bonustype data structure 
%%% @doc 
%%% @end
%%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#149
%%%===================================================================
-module(drr_bonuslen).
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
-spec parse(bitstring()) 
        -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(), list()) 
        -> {ok, map(), bitstring()}.
parse(<<BonusLen:?DRR_BONUSLEN_SIZE/little, Rest/bitstring>>
     , _Opts) ->
    ?debugFmt("drr_bonuslen: ~p", [BonusLen]),
    {ok, #{bonuslen => BonusLen}, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    error.
