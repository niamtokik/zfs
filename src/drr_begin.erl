%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_begin data structure
%%% @doc
%%% @end
%%%===================================================================
-module(drr_begin).
-export([struct/0, size/0]).
-export([parse/1, parse/2]).
-include("zfs_drr.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc list of all data structure composing drr_begin data structure
%% @end
%%--------------------------------------------------------------------
-spec struct() -> list().
struct() ->
    [drr_magic, drr_versioninfo, drr_creation_time
    ,drr_type, drr_flags, drr_toguid, drr_fromguid
    ,drr_toname].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec size() -> non_neg_integer().
size() ->
    lists:sum([ Module:size() || Module <- struct() ]).

%%--------------------------------------------------------------------
%% @doc parse/1 use default options.
%% @end
%% @param Bitstring a raw data.
%% @return tuple containing tag, parsed structure and rest of the raw
%%         bitstring.
%%--------------------------------------------------------------------
-spec parse(bitstring()) -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc drr_begin structure is composed of multiple sub-structure
%%      found in struct/0 function. 
%% @end
%% @param Bitstring a raw data as bitstring.
%% @param _Opts a proplist containing optional features.
%%--------------------------------------------------------------------
-spec parse(bitstring(), list()) -> {ok, map(), bitstring()}.
parse(Bitstring, _Opts) ->        
    struct_parser:do(Bitstring, struct()).

%%--------------------------------------------------------------------
%% @doc parse_0001_test/0 parse a simple drr_begin raw data structure
%%      extract from zfs send. This data structure doesn't contain
%%      dmu_replay_record header, and has a fixed size of 312bytes.
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
  IN = <<172, 203, 186, 245, 2, 0, 0, 0, 17, 0, 0, 0, 0, 0, 0, 0, 102,
         47, 41, 90, 0, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 54, 80, 15,
         33, 249, 114, 233, 135, 0, 0, 0, 0, 0, 0, 0, 0, 112, 111,
         111, 108, 64, 116, 101, 115, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 >>,
  OUT = {ok
        , #{ magic => <<16#2F5bacbac:64/little>>
           , creation_time => 16#5a292f66
           , type => zfs
           , featuresflags => 4
           , flags => <<4,0,0,0>>
           , hdrtype => substream
           , toguid => <<54,80,15,33,249,114,233,135>>
           , fromguid => <<0,0,0,0,0,0,0,0>>
           , toname => <<"pool@test">> 
           % , versioninfo => <<17,0,0,0,0,0,0,0>> 
           }
         , <<>>
         },
  ?assertEqual(OUT, parse(IN)).

    
