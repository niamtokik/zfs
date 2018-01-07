%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_bonustype data structure 
%%% @doc 
%%% @end
%%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/dmu.h#126
%%% @todo ensure where this datastructure is defined and check if
%%%       its okay on our side.
%%%===================================================================
-module(drr_bonustype).
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
parse(<<BonusType:?DRR_BONUSTYPE_SIZE/little, Rest/bitstring>>
     , _Opts) ->
    ?debugFmt("drr_bonustype: ~p", [BonusType]),
    {ok, #{bonustype => bonustype(BonusType)}, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<0,0,0,0, 0,0,0,0>>,
    OUT = {ok, #{bonustype => none}, <<0,0,0,0>>},
    ?assertEqual(OUT, parse(IN)).
parse_0002_test() ->
    IN = <<1,0,0,0, 0,0,0,0>>,
    OUT = {ok, #{bonustype => object_directory}, <<0,0,0,0>>},
    ?assertEqual(OUT, parse(IN)).
parse_0003_test() ->
    IN = <<34,0,0,0, 0,0,0,0>>,
    OUT = {ok, #{bonustype => acl}, <<0,0,0,0>>},
    ?assertEqual(OUT, parse(IN)).
parse_0004_test() ->
    IN = <<255,3,4,1, 0,8,9,10>>,
    ?assertException(error, function_clause, parse(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/ddt.h#64
%%--------------------------------------------------------------------
-define(BONUSTYPE(X,A), bonustype(X) -> A;
                        bonustype(A) -> X).

-spec bonustype(integer()) -> atom();
	       (atom()) -> integer().
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
%%--------------------------------------------------------------------
bonustype_0001_test() ->
    IN = 50,
    OUT = dedup,
    ?assertEqual(OUT, bonustype(IN)),
    ?assertEqual(IN, bonustype(OUT)).
bonustype_0002_test() ->
    IN = 10,
    OUT = dnode,
    ?assertEqual(OUT, bonustype(IN)),
    ?assertEqual(IN, bonustype(OUT)).
bonustype_0003_test() ->
    IN = 255,
    OUT = nothing,
    ?assertException(error, function_clause, bonustype(IN)),
    ?assertException(error, function_clause, bonustype(OUT)).
