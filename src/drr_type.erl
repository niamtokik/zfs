%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018
%%% @version 0.1.0
%%% @title zfs_stream drr_type data structure 
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_type).
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
    ?DRR_TYPE_SIZE.

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
parse(<<Type:?DRR_TYPE_SIZE/little, Rest/bitstring>>, _Opts) -> 
    ?debugFmt("drr_type: ~p", [Type]),
    {ok, #{ type => dmu_objset_type(Type) }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<0,0,0,0>>,
    OUT = {ok, #{ type => none }, <<>>},
    ?assertEqual(OUT, parse(IN)).

parse_0002_test() ->
    IN = <<6,0,0,0>>,
    OUT = {ok, #{ type => numtypes }, <<>>},
    ?assertEqual(OUT, parse(IN)).

parse_0003_test() ->
    IN = <<255,1,2,3>>,
    ?assertException(error, function_clause, parse(IN)).

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

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
dmu_objset_type_0001_test() ->
    IN = 0,
    OUT = none,
    ?assertEqual(IN, dmu_objset_type(OUT)),
    ?assertEqual(OUT, dmu_objset_type(IN)).
dmu_objset_type_0002_test() ->
    IN = 2,
    OUT = zfs,
    ?assertEqual(IN, dmu_objset_type(OUT)),
    ?assertEqual(OUT, dmu_objset_type(IN)).
dmu_objset_type_0003_test() ->
    IN = 5,
    OUT = any,
    ?assertEqual(IN, dmu_objset_type(OUT)),
    ?assertEqual(OUT, dmu_objset_type(IN)).
dmu_objset_type_0004_test() ->
    IN = 255,
    ?assertException(error, function_clause, dmu_objset_type(IN)).

