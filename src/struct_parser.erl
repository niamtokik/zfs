%%%===================================================================
%%%
%%%===================================================================
-module(struct_parser).
-compile(export_all).
-export([do/2, do/3]).
-include_lib("eunit/include/eunit.hrl").

-callback parse(Data :: bitstring()) -> ok.
-callback parse(Data :: bitstring(), Opts :: list()) -> ok.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
do(Bitstring, Actions) ->
    do(Bitstring, Actions, #{}).

do(Rest, [], State) ->
    {ok, State, Rest};
do(Bitstring, [Module|Tail], State) ->
    ?debugFmt("call ~p", [Module]),
    {ok, Value, Rest} = Module:parse(Bitstring),
    Return = maps:merge(Value, State),
    do(Rest, Tail, Return);
do(Bitstring, [{Module, Function}|Tail], State) ->
    {ok, Value, Rest} = Module:Function(Bitstring),
    Return = maps:merge({Module,Function}, Value, State),
    do(Rest, Tail, Return);
do(Bitstring, [{Module, Function, Args}|Tail], State) ->
    {ok, Value, Rest} = erlang:apply(Module,Function,[Bitstring|Args]),
    Return = maps:merge({Module,Function}, Value, State),
    do(Rest, Tail, Return).

do_0001_test() ->
    ok.
