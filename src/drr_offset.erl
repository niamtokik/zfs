%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_offset data structure 
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_offset).
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
parse(<<Offset:?DRR_OFFSET_SIZE/little, Rest/bitstring>>, _Opts) -> 
    ?debugFmt("offset: ~p", [Offset]),
    {ok, #{ offset => Offset }, Rest}.

