%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title drr data structure implementation
%%% @doc
%%% @end
%%%===================================================================
-module(drr).
-export([struct_begin/0, struct_begin/1, struct_begin/2]).
-export([struct_end/0, struct_end/1, struct_end/2]).
-export([struct_object/0]).
-export([struct_freeobjects/0]).
-export([struct_write/0]).
-export([struct_free/0]).
-export([struct_write_byref/0]).
-export([struct_spill/0]).
-include("zfs.hrl").
-include_lib("eunit/include/eunit.hrl").

-ifdef(debug).
-compile(export_all).
-endif.

%%%===================================================================
%%% API SECTION - EXPORTED FUNCTION
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_begin() -> list().
struct_begin() -> 
  [ drr_magic, drr_versioninfo, drr_creation_time, drr_type
  , drr_flags, drr_toguid, drr_fromguid, drr_toname ].

-spec struct_begin(bitstring()) 
  -> {ok, map()} | 
     {ok, map(), bitstring()} |
     {error, term()}.
struct_begin(Bitstring) ->
  struct_begin(Bitstring, []).

-spec struct_begin(bitstring(), list()) 
  -> {ok, map()} |
     {ok, map(), bitstring()} |
     {error, term()}.
struct_begin(Bitstring, _Opts) ->
  Return = #{},

  {ok, Magic, MagicRest} = drr_magic(Bitstring),
  Return1 = maps:merge(Return, Magic),

  {ok, VersionInfo, VersionInfoRest} = drr_versioninfo(MagicRest),
  Return2 = maps:merge(Return1, VersionInfo),

  {ok, CreationTime, CreationTimeRest} = drr_creation_time(VersionInfoRest),
  Return3 = maps:merge(Return2, CreationTime),

  {ok, Type, TypeRest} = objset_type(CreationTimeRest),
  Return4 = maps:merge(Return3, Type),

  {ok, Flags, FlagsRest} = drr_flags(TypeRest),
  Return5 = maps:merge(Return4, Flags),

  {ok, ToGuid, ToGuidRest} = drr_toguid(FlagsRest),
  Return6 = maps:merge(Return5, ToGuid),

  {ok, FromGuid, FromGuidRest} = drr_fromguid(ToGuidRest),
  Return7 = maps:merge(Return6, FromGuid),

  {ok, ToName, ToNameRest} = drr_toname(FromGuidRest),
  Return8 = maps:merge(Return7, ToName),

  {ok, Return8, ToNameRest}.

struct_begin_0001_test() ->
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
  ?assertEqual(OUT, struct_begin(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_end() -> list().
struct_end() -> 
  [ drr_checksum, drr_toguid ].

-spec struct_end(bitstring())
   -> {ok, map()} |
      {ok, map(), bitstring()} |
      {error, term()}.
struct_end(Bitstring) ->
  struct_end(Bitstring, []).

-spec struct_end(bitstring(), list())
  -> {ok, map()} |
     {ok, map(), bitstring()} |
     {error, term()}.
struct_end(Bitstring, _Opts) ->
  Init = #{},
  
  {ok, Checksum, ChecksumRest} = drr_checksum(Bitstring),
  Return1 = maps:merge(Checksum, Init),

  {ok, ToGuid, ToGuidRest} = drr_toguid(ChecksumRest),
  Return2 = maps:merge(ToGuid, Return1),

  {ok, Return2, ToGuidRest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_object() -> list().
struct_object() -> 
  [ drr_object, drr_type, drr_bonustype, drr_blksz, drr_bonuslen
  , drr_checksumtype, drr_compress, {drr_pad, 6}, drr_toguid ].

% -spec object(bitstring())
%   -> {ok, drr_object()} |
%      {ok, drr_object(), bitstring()} |
%      {error, term()}.
% -spec object(bitstring(), list())
%   -> {ok, drr_object()} |
%      {ok, drr_object(), bitstring()} |
%      {error, term()}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_freeobjects() -> list().
struct_freeobjects() -> 
  [ drr_firstobj, drr_numobjs, drr_toguid ].

-spec struct_freeobjects(bitstring())
   -> {ok, map()} |
      {ok, map(), bitstring()} |
      {error, term()}.
struct_freeobjects(Bitstring) ->
  struct_freeobjects(Bitstring, []).

-spec struct_freeobjects(bitstring(), list())
   -> {ok, map()} |
      {ok, map(), bitstring()} |
      {error, term()}.
struct_freeobjects(Bitstring, _Opts) ->
  {wip, Bitstring}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_write() -> list().
struct_write() -> 
  [ drr_object, drr_type, {drr_pad, 4}, drr_offset, drr_length
  , drr_toguid, drr_checksumtype, drr_checksumflags, {drr_pad, 2}
  , drr_key ].

% -spec write(bitstring())
%   -> {ok, drr_write()} |
%      {ok, drr_write(), bitstring()} |
%      {error, term()}.
% -spec write(bitstring(), list())
%   -> {ok, drr_write()} |
%      {ok, drr_write(), bitstring()} |
%      {error, term()}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_free() -> list().
struct_free() -> 
  [ drr_object, drr_offset, drr_length, drr_toguid ].

% -spec free(bitstring())
%   -> {ok, drr_free()} |
%      {ok, drr_free(), bitstring()} |
%      {error, term()}.
% -spec free(bitstring(), list())
%   -> {ok, drr_free()} |
%      {ok, drr_free(), bitstring()} |
%      {error, term()}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_write_byref() -> list().
struct_write_byref() -> 
  [ drr_object, drr_offset, drr_length, drr_toguid, drr_refguid
  , drr_refobject, drr_refoffset, drr_checksumtype, drr_checksumflags
  , {drr_pad, 6}, drr_key ].

% -spec byref(bitstring())
%   -> {ok, drr_byref()} |
%      {ok, drr_byref(), bitstring()} |
%      {error, term()}.
% -spec byref(bitstring(), list())
%   -> {ok, drr_byref()} |
%      {ok, drr_byref(), bitstring()} |
%      {error, term()}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec struct_spill() -> list().
struct_spill() ->
  [ drr_object, drr_length, drr_toguid, {drr_pad, 4} ].

% -spec struct_spill(bitstring())
%   -> {ok, map()} |
%      {ok, map(), bitstring()} |
%      {error, term()}.
% -spec struct_spill(bitstring(), list())
%   -> {ok, map()} |
%      {ok, map(), bitstring()} |
%      {error, term()}.

%%%===================================================================
%%% INTERNAL SECTION - PRIVATE FUNCTION
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_blksz(bitstring()) 
        -> {ok, map(), bitstring()}.
drr_blksz(<<BlkSz:32/little, Rest/bitstring>>) -> 
  {ok, #{ blksz => BlkSz }, Rest}.

drr_blksz_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_bonuslen(bitstring()) 
        -> {ok, map(), bitstring()}.
drr_bonuslen(<<BonusLen:32/little, Rest/bitstring>>) ->
  {ok, #{bonuslen => BonusLen}, Rest}.

drr_bonuslen_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_bonustype(bitstring()) 
        -> {ok, atom(), bitstring()}.
drr_bonustype(<<BonusType:64/little, Rest/bitstring>>) ->
  {ok, bonustype(BonusType), Rest/bitstring}.

drr_bonustype_0001_test() ->
    ok.

% http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/dmu.h#126
-define(BONUSTYPE(X,A), bonustype(X) when is_integer(X) -> A;
                        bonustype(A) when is_atom(A) -> X).
?BONUSTYPE(0, none);
?BONUSTYPE(1, object_directory);
?BONUSTYPE(2, object_array);
?BONUSTYPE(3, packed_nvlist);
?BONUSTYPE(4, packed_nvlist_size);
?BONUSTYPE(5, pbobj);
?BONUSTYPE(6, pbobj_hdr);
?BONUSTYPE(7, space_map_header);
?BONUSTYPE(8, space_map);
?BONUSTYPE(9, intent_log);
?BONUSTYPE(10, dnode);
?BONUSTYPE(11, objset);
?BONUSTYPE(12, dsl_dir);
?BONUSTYPE(13, dsl_dir_child_map);
?BONUSTYPE(14, dsl_ds_snap_map);
?BONUSTYPE(15, dsl_ds_props);
?BONUSTYPE(16, dsl_dataset);
?BONUSTYPE(17, znode);
?BONUSTYPE(18, oldacl);
?BONUSTYPE(19, plain_file_contents);
?BONUSTYPE(20, directory_contents);
?BONUSTYPE(21, master_node);
?BONUSTYPE(22, unlinked_set);
?BONUSTYPE(23, zvol);
?BONUSTYPE(25, zvol_prop);
?BONUSTYPE(26, plain_other);
?BONUSTYPE(27, uint64_other);
?BONUSTYPE(28, zap_other);
?BONUSTYPE(29, error_log);
?BONUSTYPE(30, spa_history);
?BONUSTYPE(31, spa_history_offsets);
?BONUSTYPE(32, pool_props);
?BONUSTYPE(33, dsl_perms);
?BONUSTYPE(34, acl);
?BONUSTYPE(35, sysacl);
?BONUSTYPE(36, fuid);
?BONUSTYPE(37, fuid_size);
?BONUSTYPE(38, next_clones);
?BONUSTYPE(39, scan_queue);
?BONUSTYPE(40, usergroup_used);
?BONUSTYPE(41, usergroup_quota);
?BONUSTYPE(42, userrefs);
?BONUSTYPE(43, ddt_zap);
?BONUSTYPE(44, ddt_stats);
?BONUSTYPE(45, sa);
?BONUSTYPE(46, sa_master_node);
?BONUSTYPE(47, sa_attr_registration);
?BONUSTYPE(48, sa_attr_layouts);
?BONUSTYPE(49, scan_xlate);
?BONUSTYPE(50, dedup);
?BONUSTYPE(51, deadlist);
?BONUSTYPE(52, deadlist_hdr);
?BONUSTYPE(53, dsl_clones);
?BONUSTYPE(54, bpobj_subobj);
?BONUSTYPE(55, numtypes).
% DMU_OTN_UINT8_DATA = DMU_OT(DMU_BSWAP_UINT8, B_FALSE),
% DMU_OTN_UINT8_METADATA = DMU_OT(DMU_BSWAP_UINT8, B_TRUE),
% DMU_OTN_UINT16_DATA = DMU_OT(DMU_BSWAP_UINT16, B_FALSE),
% DMU_OTN_UINT16_METADATA = DMU_OT(DMU_BSWAP_UINT16, B_TRUE),
% DMU_OTN_UINT32_DATA = DMU_OT(DMU_BSWAP_UINT32, B_FALSE),
% DMU_OTN_UINT32_METADATA = DMU_OT(DMU_BSWAP_UINT32, B_TRUE),
% DMU_OTN_UINT64_DATA = DMU_OT(DMU_BSWAP_UINT64, B_FALSE),
% DMU_OTN_UINT64_METADATA = DMU_OT(DMU_BSWAP_UINT64, B_TRUE),
% DMU_OTN_ZAP_DATA = DMU_OT(DMU_BSWAP_ZAP, B_FALSE),
% DMU_OTN_ZAP_METADATA = DMU_OT(DMU_BSWAP_ZAP, B_TRUE),

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @todo check data checksum
%%--------------------------------------------------------------------
-spec drr_checksum(bitstring) 
        -> {ok, bitstring(), bitstring}.
drr_checksum(<<Checksum:256/bitstring, Rest/bitstring>>) -> 
  ?debugFmt("drr_checksum: ~p", [Checksum]),
  {ok, Checksum, Rest}.

drr_checksum_0001_test() -> 
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_checksumflags(bitstring()) -> {ok, bitstring(), bitstring}.
drr_checksumflags(<<ChecksumFlags:8, Rest/bitstring>>) -> 
  ?debugFmt("drr_checksumflags: ~p", [ChecksumFlags]),
  {ok, ChecksumFlags, Rest}.

drr_checksumflags_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_checksumtype(bitstring()) -> {ok, atom(), bitstring()}.
drr_checksumtype(<<ChecksumType:8/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_checksumtype: ~p", [ChecksumType]),
  {ok, ChecksumType, Rest}.

drr_checksumtype_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_compress(bitstring()) -> {ok, atom(), bitstring()}.
drr_compress(<<Compress:8/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_compress: ~p", [Compress]),
  {ok, Compress, Rest}.

drr_compress_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_creation_time(bitstring()) 
        -> {ok, map(), bitstring()}.
drr_creation_time(<<CreationTime:64/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_creation_time: ~p", [CreationTime]),
  {ok, #{ creation_time => CreationTime }, Rest}.

drr_creation_time_0001_test() ->
  IN = <<16#66, 16#2f, 16#29, 16#5a,0,0,0,0>>,
  OUT = {ok, #{ creation_time => 16#5a292f66 }, <<>>},
  ?assertEqual(OUT, drr_creation_time(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_firstobj(bitstring()) 
        -> {ok, bitstring(), bitstring()}.
drr_firstobj(<<FirstObj:64/bitstring, Rest/bitstring>>) -> 
  ?debugFmt("drr_firstobj: ~p", [FirstObj]),
  {ok, FirstObj, Rest}.

drr_firstobj_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_flags(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_flags(<<Flags:32/bitstring, Rest/bitstring>>) -> 
  ?debugFmt("drr_flags: ~p", [Flags]),
  {ok, #{ flags => Flags }, Rest}.

drr_flags_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_fromguid(bitstring()) -> {ok, map(), bitstring()}.
drr_fromguid(<<FromGuid:64/bitstring, Rest/bitstring>>) -> 
  ?debugFmt("drr_fromguid: ~p", [FromGuid]),
  {ok, #{ fromguid => FromGuid }, Rest}.

drr_fromguid_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_key(bitstring()) -> {ok, map(), bitstring()}.
drr_key(<<CkSum:256/bitstring, Prop:64/bitstring, Rest/bitstring>>) -> 
  ?debugFmt("drr_key: ~p, ~p", [CkSum, Prop]),
  {ok, #{cksum => CkSum, prop => Prop}, Rest}.

drr_key_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_length(bitstring()) -> {ok, map(), bitstring()}.
drr_length(<<Length:64/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_length: ~p", [Length]),
  {ok, #{ length => Length }, Rest}.

drr_length_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_magic(bitstring()) 
        -> {ok, map(), bitstring()} | 
           {error, term()}.
drr_magic(<<16#2F5bacbac:64/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_magic: ok", []),
  {ok, #{ magic => <<16#2F5bacbac:64/little>> }, Rest};
drr_magic(<<_:64, _/bitstring>>) ->
  {error, magic}.

drr_magic_0001_test() ->
  IN = <<172,203,186,245,2,0,0,0>>,
  OUT = {ok, #{magic => <<16#2F5bacbac:64/little>>}, <<>>},
  ?assertEqual(OUT, drr_magic(IN)).
drr_magic_0002_test() ->
  IN = <<1,2,3,4, 5,6,7,8>>,
  OUT = {error, magic},
  ?assertEqual(OUT, drr_magic(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_numobjs(bitstring()) -> {ok, non_neg_integer(), bitstring()}.
drr_numobjs(<<NumObjs:64/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_numobjs: ~p", [NumObjs]),
  {ok, NumObjs, Rest}.

drr_numobjs_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_object(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_object(<<Object:64/bitstring, Rest/bitstring>>) -> 
  {ok, Object, Rest}.

drr_object_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_offset(bitstring()) -> {ok, non_neg_integer(), bitstring()}.
drr_offset(<<Offset:64/little, Rest/bitstring>>) -> 
  {ok, Offset, Rest}.

drr_offset_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_pad(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_pad(<<Pad:32, Rest/bitstring>>) -> 
  {ok, Pad, Rest}.

drr_pad_0001_test() ->
    ok.

-spec drr_pad2(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_pad2(<<Pad:8, Rest/bitstring>>) ->
  {ok, Pad, Rest}.

drr_pad2_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
-spec drr_refguid(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_refguid(<<RefGuid:64/bitstring, Rest/bitstring>>) -> 
  {ok, RefGuid, Rest}.

drr_refguid_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
-spec drr_refobject(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_refobject(<<RefObject:64/bitstring, Rest/bitstring>>) -> 
  {ok, RefObject, Rest}.

drr_refobject_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
-spec drr_refoffset(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_refoffset(<<RefOffset:64/bitstring, Rest/bitstring>>) -> 
  {ok, RefOffset, Rest}.

drr_refoffset_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%
%%--------------------------------------------------------------------
-spec drr_toguid(bitstring()) 
        -> {ok, map(), bitstring()}.
drr_toguid(<<ToGuid:64/bitstring, Rest/bitstring>>) -> 
  {ok, #{ toguid => ToGuid }, Rest}.

drr_toguid_0001_test() ->
  IN = <<16#36, 16#50, 16#0f, 16#21, 16#f9, 16#72, 16#e9, 16#87>>,
  OUT = {ok, #{ toguid => <<54,80,15,33,249,114,233,135>>}, <<>>},
  ?assertEqual(OUT, drr_toguid(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @todo check snapshot name.
%%--------------------------------------------------------------------
-spec drr_toname(bitstring()) 
        -> {ok, map(), bitstring()}.
drr_toname(<<ToName:(256*8)/bitstring, Rest/bitstring>>) -> 
  ?debugFmt("drr_toname: ~p", [ToName]),
  {ok, #{ toname => zfs_lib:null_clean(ToName) }, Rest}.

drr_toname_0001_test() ->
  IN = <<>>,
  ?assertException(error, _, drr_toname(IN)).
drr_toname_0002_test() ->
  IN = <<"pool@test", 0:(256*8-72)>>,
  OUT = {ok, #{ toname => <<"pool@test">> }, <<>>},
  ?assertEqual(OUT, drr_toname(IN)).
drr_toname_0003_test() ->
  IN = <<"zpooltest", 0:(256*8-72)>>,
  OUT = {error, not_snapshot},
  ?assertEqual(OUT, drr_toname(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_type(bitstring()) 
        -> {ok, map(), bitstring()}.
drr_type(<<Type:32/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_type: ~p", [Type]),
  {ok, #{ type => type(Type) }, Rest}.

drr_type_0001_test() ->
  IN = <<0,0,0,0 >>,
  OUT = {ok, #{ type => drr_begin }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0002_test() ->
  IN = <<1,0,0,0>>,
  OUT = {ok, #{ type => drr_object }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0003_test() ->
  IN = <<2,0,0,0>>,
  OUT = {ok, #{ type => drr_freeobjects }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0004_test() ->
  IN = <<3,0,0,0>>,
  OUT = {ok, #{ type => drr_write}, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0005_test() ->
  IN = <<4,0,0,0>>,
  OUT = {ok, #{ type => drr_free }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0006_test() ->
  IN = <<5,0,0,0>>,
  OUT = {ok, #{ type => drr_end }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0007_test() ->
  IN = <<6,0,0,0>>,
  OUT = {ok, #{ type => drr_write_byref }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0008_test() ->
  IN = <<7,0,0,0>>,
  OUT = {ok, #{ type => drr_spill }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).
drr_type_0009_test() ->
  IN = <<8,0,0,0>>,
  OUT = {ok, #{ type => drr_umtypes }, <<>>},
  ?assertEqual(OUT, drr_type(IN)).

-define(TYPE(X,A), type(X) when is_integer(X) -> A; 
                   type(A) when is_atom(A) -> X ).
?TYPE(0, drr_begin);
?TYPE(1, drr_object);
?TYPE(2, drr_freeobjects);
?TYPE(3, drr_write);
?TYPE(4, drr_free);
?TYPE(5, drr_end);
?TYPE(6, drr_write_byref);
?TYPE(7, drr_spill);
?TYPE(8, drr_umtypes).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_versioninfo(bitstring()) -> {ok, map(), bitstring()}.
drr_versioninfo(<<VersionInfo:64/little, Rest/bitstring>>) -> 
  ?debugFmt("drr_versioninfo: ~p", [<<VersionInfo:64>>]),
  <<_:32, Features:30, HdrType:2>> = <<VersionInfo:64>>,
  {ok, #{ hdrtype => hdrtype(HdrType)
        , featuresflags => Features
        }, Rest}.

drr_versioninfo_0001_test() ->
  IN = <<17,0,0,0,0,0,0,0>>,
  OUT = {ok, #{ hdrtype => substream
              , featuresflags => 4 }, <<>>},
  ?assertEqual(OUT, drr_versioninfo(IN)).
drr_versioninfo_0002_test() ->
  IN = <<16,0,0,0,0,0,0,0>>,
  OUT = {ok, #{ hdrtype => compoundstream
              , featuresflags => 4 }, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-define(HDRTYPE(X,A), hdrtype(X) -> A; hdrtype(A) -> X).
-spec hdrtype(integer()) -> atom().
?HDRTYPE(1, substream);
?HDRTYPE(2, compoundstream).

hdrtype_test() ->
  [ hdrtype_test(X) || X <- [1,2] ].

hdrtype_test(1) ->
  IN = 1,
  OUT = substream,
  ?assertEqual(OUT, hdrtype(1));
hdrtype_test(2) ->
  IN = 2,
  OUT = compoundstream,
  ?assertEqual(OUT, hdrtype(2)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/sys/fs/zfs.h#58
%%--------------------------------------------------------------------
-define(OBJSET_TYPE(X,A), dmu_objset_type(X) -> A; 
                          dmu_objset_type(A) -> X).
?OBJSET_TYPE(0, none);
?OBJSET_TYPE(1, meta);
?OBJSET_TYPE(2, zfs);
?OBJSET_TYPE(3, zvol);
?OBJSET_TYPE(4, other);
?OBJSET_TYPE(5, any);
?OBJSET_TYPE(6, numtypes).

%%
%%
%%
-spec objset_type(bitstring()) -> {ok, atom(), bitstring()}.
objset_type(<<ObjsetType:32/little, Rest/bitstring>>) ->
  {ok, #{ type => dmu_objset_type(ObjsetType) }, Rest}.

objset_type_0001_test() ->
  IN = <<0,0,0,0>>,
  OUT = {ok, #{ type => none }, <<>>},
  ?assertEqual(OUT, objset_type(IN)).
objset_type_0002_test() ->
  IN = <<1,0,0,0>>,
  OUT = {ok, #{ type => meta }, <<>>},
  ?assertEqual(OUT, objset_type(IN)).
objset_type_0003_test() ->
  IN = <<2,0,0,0>>,
  OUT = {ok, #{ type => zfs}, <<>>},
 ?assertEqual(OUT, objset_type(IN)).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-define(DMU_BSWAP_UINT8,  0).
-define(DMU_BSWAP_UINT16, 1).
-define(DMU_BSWAP_UINT32, 2).
-define(DMU_BSWAP_UINT64, 3).
-define(DMU_BSWAP_ZAP,    4).
-define(DMU_BSWAP_DNODE,  5).
-define(DMU_BSWAP_OBJSET, 6).
-define(DMU_BSWAP_ZNODE,  7).
-define(DMU_BSWAP_OLDACL, 8).
-define(DMU_BSWAP_ACL,    9).
-define(DMU_BSWAP_NUMFUNCS, 10).
