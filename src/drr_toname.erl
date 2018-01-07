%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_toname data structure
%%% @doc drr_toname is a fixed len data structure (256bytes).
%%% @end
%%%===================================================================
-module(drr_toname).
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
    ?DRR_TONAME_SIZE.

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @todo check snapshot name.
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
parse(<<ToName:?DRR_TONAME_SIZE/bitstring, Rest/bitstring>>, _Opts) -> 
    ?debugFmt("parse: ~p", [ToName]),
    {ok, #{ toname => zfs_lib:null_clean(ToName) }, Rest}.

%%--------------------------------------------------------------------
%% @doc check what's going on when we give an empty string
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<>>,
    ?assertException(error, function_clause, parse(IN)).

%%--------------------------------------------------------------------
%% @doc simple test on standard name followed by null char
%% @end
%%--------------------------------------------------------------------
parse_0002_test() ->
    IN = <<"pool@test", 0:(256*8-72)>>,
    OUT = {ok, #{ toname => <<"pool@test">> }, <<>>},

%%--------------------------------------------------------------------
%% @doc what's going on when we try to set snapshot name of 255bytes?
%% @end
%% @todo check if snapshot name should end with \0.
%%--------------------------------------------------------------------
  ?assertEqual(OUT, parse(IN)).
parse_0003_test() ->
    IN = <<"data/test@ddddddddddddddddddddddddddddddddddddddd",
	   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
	   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
	   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
	   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
	   "dddddddddd", 0>>,
    OUT = { ok
	  , #{ toname => <<"data/test@ddddddddddddddddddddddddddddddddddddddd",
			   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
			   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
			   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
			   "ddddddddddddddddddddddddddddddddddddddddddddddddd",
			   "dddddddddd">> }
	  ,<<>> },
    ?assertEqual(OUT, parse(IN)).
