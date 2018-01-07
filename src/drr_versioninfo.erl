%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_versioninfo data structure 
%%% @doc 
%%% @end
%%%===================================================================
-module(drr_versioninfo).
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
    ?DRR_VERSIONINFO_SIZE.

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
parse(<<VersionInfo:?DRR_VERSIONINFO_SIZE/little, Rest/bitstring>>
     , _Opts) -> 
    ?debugFmt("drr_versioninfo: ~p", [<<VersionInfo:64>>]),
    <<_:32, Features:30, HdrType:2>> = <<VersionInfo:64>>,
    {ok, #{ hdrtype => hdrtype(HdrType)
	  , featuresflags => Features
	  }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<17,0,0,0,0,0,0,0>>,
    OUT = {ok, #{ hdrtype => substream
		, featuresflags => 4 }, <<>>},
    ?assertEqual(OUT, parse(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0002_test() ->
    IN = <<18,0,0,0,0,0,0,0>>,
    OUT = {ok, #{ hdrtype => compoundstream
		, featuresflags => 4 }, <<>>},
    ?assertEqual(OUT, parse(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-define(HDRTYPE(X,A), hdrtype(X) -> A; hdrtype(A) -> X).
-spec hdrtype(integer()) -> atom().
?HDRTYPE(1, substream);
?HDRTYPE(2, compoundstream).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
hdrtype_0001_test() ->
  IN = 1,
  OUT = substream,
  ?assertEqual(OUT, hdrtype(IN)),
  ?assertEqual(IN, hdrtype(OUT)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
hdrtype_0002_test() ->
  IN = 2,
  OUT = compoundstream,
  ?assertEqual(OUT, hdrtype(IN)),
  ?assertEqual(IN, hdrtype(OUT)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/sys/fs/zfs.h#58
%%--------------------------------------------------------------------
-define(OBJSET_TYPE(X,A), dmu_objset_type(X) -> A; 
                          dmu_objset_type(A) -> X).
-spec dmu_objset_type(atom()) -> integer();
		      (integer()) -> atom().
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
    ?assertEqual(OUT, dmu_objset_type(IN)),
    ?assertEqual(IN, dmu_objset_type(OUT)).
dmu_objset_type_0002_test() ->
    IN = 6,
    OUT = numtypes,
    ?assertEqual(OUT, dmu_objset_type(IN)),
    ?assertEqual(IN, dmu_objset_type(OUT)).
dmu_objset_type_0003_test() ->
    IN = 29,
    ?assertException(error, function_clause, dmu_objset_type(IN)).

