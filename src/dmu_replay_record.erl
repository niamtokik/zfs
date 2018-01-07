%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream dmu_replay_record data structure
%%% @doc
%%% @end
%%% @todo redo all spec.
%%%===================================================================
-module(dmu_replay_record).
-export([struct/0, size/0]).
-export([parse/1, parse/2]).
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
    312.

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
-spec parse(bitstring(), list()) -> {ok, map(), bitstring()}.
parse(Bitstring, _Opts) ->
    parse(Bitstring, [], _Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse(<<>>, Buffer, _Opts) ->
    {ok, Buffer};
parse(<<DRR_TYPE:32/little, DRR_PAYLOAD:32, Bitstring/bitstring>>
     , Buffer, _Opts) ->
    Object = type(DRR_TYPE),
    ?debugFmt("object: ~p, payload: ~p", [Object, DRR_PAYLOAD]),
    {ok, Data, Rest} = Object:parse(Bitstring),
    parse(Rest, [{Object, DRR_PAYLOAD, Data}|Buffer], _Opts).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    _IN = <<>>,
    _OUT = {ok, [{drr_begin, #{}}
		,{drr_freeobjects, #{}}], <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-define(TYPE(X,Y), type(X) -> Y;
	           type(Y) -> X).
-spec type(atom()) -> integer();
	  (integer()) -> atom().
?TYPE(0, drr_begin);
?TYPE(1, drr_object);
?TYPE(2, drr_freeobjects);
?TYPE(3, drr_write);
?TYPE(4, drr_free);
?TYPE(5, drr_end);
?TYPE(6, drr_write_byref);
?TYPE(7, drr_spill);
?TYPE(8, drr_numtypes).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
type_0001_test() ->
    IN = 0,
    OUT = drr_begin,
    ?assertEqual(OUT, type(IN)),
    ?assertEqual(IN, type(OUT)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
type_0002_test() ->
    IN = 8,
    OUT = drr_numtypes,
    ?assertEqual(OUT, type(IN)),
    ?assertEqual(IN, type(OUT)).

