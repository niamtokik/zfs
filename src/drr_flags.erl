%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_flags data structure 
%%% @doc 
%%% @end
%%% @todo what's represent this flags? 
%%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#114
%%%===================================================================
-module(drr_flags).
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
    ?DRR_FLAGS_SIZE.

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
	   -> {ok, bitstring(), bitstring()}.
parse(<<Flags:?DRR_FLAGS_SIZE/bitstring, Rest/bitstring>>, _Opts) -> 
    ?debugFmt("drr_flags: ~p", [Flags]),
    {ok, #{ flags => Flags }, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc not sure if its right currently...
%% @end
%%--------------------------------------------------------------------
-define(FLAGS(X,Y), flags(X) -> Y;
	            flags(Y) -> X).
-spec flags(atom()) -> integer();
	   (integer()) -> atom().
?FLAGS(1, clone);
?FLAGS(2, ci_data).
	        
