%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @doc
%%% @end
%%%===================================================================
-module(drr_checksumflags).
-export([struct/0]).
-export([parse/1, parse/2]).
-include("zfs_drr.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec struct() -> list().
struct() ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @todo check data checksum
%%--------------------------------------------------------------------
-spec parse(bitstring()) -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(),list()) 
	   -> {ok, bitstring(), bitstring}.
parse(<<ChecksumFlags:?DRR_CHECKSUMFLAGS_SIZE, Rest/bitstring>>
     ,_Opts) -> 
  ?debugFmt("drr_checksumflags: ~p", [ChecksumFlags]),
  {ok, ChecksumFlags, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    ok.
