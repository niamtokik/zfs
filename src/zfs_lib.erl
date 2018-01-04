%%%===================================================================
%%% @doc
%%% @end
%%% @see sys/cddl/contrib/opensolaris/common/zfs
%%%===================================================================
-module(zfs_lib).
-include("zfs.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec valid_char(integer()) -> integer().
valid_char(Char)
  when (Char >= $0 andalso Char =< $9) orelse
       (Char >= $a andalso Char =< $z) orelse
       (Char >= $A andalso Char =< $Z) orelse 
        Char =:= $_ orelse Char =:= $- orelse
        Char =:= $: orelse Char =:= $. orelse Char =:= $  ->
  Char.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec zfs_name(list() | bitstring()) 
      -> {ok, atom(), bitstring()} | 
         {error, term()}.
zfs_name(List) when is_list(List) ->
  zfs_name(erlang:list_to_bitstring(List));
zfs_name(Bitstring) ->
  zfs_name(Bitstring, <<>>).

-spec zfs_name(bitstring(), bitstring(), atom())
      -> {ok, atom(), bitstring()} |
         {error, term()}.
zfs_name(Bitstring, Buf) ->
  zfs_name(Bitstring, <<>>, pool).

% pool
zfs_name(<<>>, PoolName, pool) ->
  {ok, pool, PoolName};
zfs_name(<<"/", Rest/bitstring>>, <<>>, pool)  ->
  {error, badname};
zfs_name(<<"@", Rest/bitstring>>, <<>>, pool)  ->
  {error, badname};
zfs_name(<<"_", Rest/bitstring>>, <<>>, pool)  ->
  {error, badname};
zfs_name(<<"c", Char:8, Rest/bitstring>>, <<>>, pool) 
  when Char >= $0 andalso Char =< $9 ->
    {error, badname};
zfs_name(<<"log", Rest/bitstring>>, Buf, pool) ->
  {error, badname};
zfs_name(<<"mirror", Rest/bitstring>>, <<>>, pool) ->
  {error, badname};
zfs_name(<<"raidz", Rest/bitstring>>, <<>>, pool) ->
  {error, badname};
zfs_name(<<"raidz1", Rest/bitstring>>, <<>>, pool) ->
  {error, badname};
zfs_name(<<"raidz2", Rest/bitstring>>, <<>>, pool) ->
  {error, badname};
zfs_name(<<"raidz3", Rest/bitstring>>, <<>>, pool) ->
  {error, badname};
zfs_name(<<"spare", Rest/bitstring>>, <<>>, pool) ->
  {error, badname};
zfs_name(<<"@", Rest/bitstring>>, Buf, pool)  ->
  {error, badname};
zfs_name(<<"%", Rest/bitstring>>, Buf, pool) ->
  {error, badname};
zfs_name(<<"/", Rest/bitstring>>, Buf, pool) ->
  zfs_name(Rest, <<Buf/bitstring, "/">>, dataset);
zfs_name(<<Char:8, Rest/bitstring>>, Buf, pool) 
  when (Char >= $0 andalso Char =< $9) orelse
       (Char >= $a andalso Char =< $z) orelse
       (Char >= $A andalso Char =< $Z) orelse 
        Char =:= $_ orelse Char =:= $- orelse
        Char =:= $: orelse Char =:= $. ->
    zfs_name(Rest, <<Buf/bitstring, Char>>, pool);

% dataset
zfs_name(<<>>, DatasetName, dataset) ->
  {ok, dataset, DatasetName};
zfs_name(<<"%", Rest/bitstring>>, Buf, dataset) ->
  {error, badname};
zfs_name(<<"@", Rest/bitstring>>, Buf, dataset) ->
  zfs_name(Rest, <<Buf/bitstring, "@">>, snapshot);
zfs_name(<<Char:8, Rest/bitstring>>, Buf, dataset) 
  when (Char >= $0 andalso Char =< $9) orelse
       (Char >= $a andalso Char =< $z) orelse
       (Char >= $A andalso Char =< $Z) orelse 
        Char =:= $_ orelse Char =:= $- orelse
        Char =:= $: orelse Char =:= $. orelse 
        Char =:= $/ ->
    zfs_name(Rest, <<Buf/bitstring, Char>>, dataset);

% snapshot
zfs_name(<<>>, Buf, snapshot) ->
  {ok, snapshot, Buf};
zfs_name(<<Char:8, Rest/bitstring>>, Buf, snapshot)
  when (Char >= $0 andalso Char =< $9) orelse
       (Char >= $a andalso Char =< $z) orelse
       (Char >= $A andalso Char =< $Z) orelse 
        Char =:= $_ orelse Char =:= $- orelse
        Char =:= $: orelse Char =:= $. orelse 
        Char =:= $/ ->
  zfs_name(Rest, <<Buf/bitstring, Char>>, snapshot);

zfs_name(_, _, _) ->
  {error, badname}.

zfs_name_test() ->
  [ zfs_name_test(X) || X <- lists:seq(1,14) ].

zfs_name_test(1) ->
  IN = <<"pool">>,
  OUT = {ok, pool, IN},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(2) ->
  IN = <<"pool/test/toto">>,
  OUT = {ok, dataset, IN},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(3) ->
  IN = <<"pool/test/toto@snap1">>,
  OUT = {ok, snapshot, IN},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(4) ->
  IN = <<"/pool/test">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(5) ->
  IN = <<"@pool/test">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(6) ->
  IN = <<"pool@test@1">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(7) ->
  IN = <<"_pool/test">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(8) ->
  IN = <<"%OFDATA">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(9) ->
  IN = <<"ROOT">>,
  OUT = {ok, pool, IN},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(10) ->
  IN = <<"R_:0/level4-4">>,
  OUT = {ok, dataset, IN},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(11) ->
  IN = <<"  test">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN));
zfs_name_test(12) ->
  IN = [ <<"c012test">>, <<"c1">>, <<"c2">>
       , <<"c3">>, <<"c4">>, <<"c5">>, <<"c6">>
       , <<"c7">>, <<"c8">>, <<"c9">> 
       ],
  OUT = {error, badname},
  [ ?assertEqual(OUT, zfs_name(X)) || X <- IN ];
zfs_name_test(13) ->
  IN = [ <<"log">>, <<"mirror">>, <<"raidz">>
       , <<"raidz1">>, <<"raidz2">>, <<"raidz3">>
       , <<"spare">> 
       ],
  OUT = {error, badname},
  [ ?assertEqual(OUT, zfs_name(X)) || X <- IN ];
zfs_name_test(14) ->
  IN = <<"mypool/test%/1">>,
  OUT = {error, badname},
  ?assertEqual(OUT, zfs_name(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_pool(bitstring()) -> true | false.
is_pool(Bitstring) ->
  is_pool(Bitstring, <<>>).

-spec is_pool(bitstring(), bitstring()) -> true | false.
is_pool(<<>>, Buf) 
  when Buf =/= <<>> ->
    true;
is_pool(<<Char:8, Rest/bitstring>>, Buf) 
  when Char >= $0 andalso Char =< $9 orelse
       Char >= $A andalso Char =< $Z orelse 
       Char >= $a andalso Char =< $z ->
    is_pool(Rest, <<Buf/bitstring, Char>>);
is_pool(_, _) ->
  false.

is_pool_test() ->
  [ is_pool_test(X) || X <- [1] ].
is_pool_test(1) ->
  IN = <<"test">>,
  OUT = true,
  ?assertEqual(OUT, is_pool(IN));
is_pool_test(2) ->
  IN = <<"/test">>,
  OUT = false,
  ?assertEqual(OUT, is_pool(IN));
is_pool_test(3) ->
  IN = <<"@test">>,
  OUT = false,
  ?assertEqual(OUT, is_pool(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_dataset(bitstring()) -> true | false.
is_dataset(Bitstring) ->
  is_dataset(Bitstring, <<>>).

-spec is_dataset(bitstring(), bitstring()) -> true | false.
is_dataset(<<$/, _/bitstring>>, <<>>) ->
  false;
is_dataset(<<>>, Buf) 
  when Buf =/= <<>> ->
    true;
is_dataset(<<$/, $/, Rest/bitstring>>, _) ->
  false;
is_dataset(<<Char:8, Rest/bitstring>>, Buf) 
  when Char >= $0 andalso Char =< $9 orelse
       Char >= $A andalso Char =< $Z orelse 
       Char >= $a andalso Char =< $z orelse
       Char =:= $/ ->
    is_dataset(Rest, <<Buf/bitstring, Char>>);
is_dataset(_, _) ->
  false.

is_dataset_test() ->
  [ is_dataset_test(X) || X <- [1] ].

is_dataset_test(1) ->
  IN = <<"test/test">>,
  OUT = true,
  ?assertEqual(OUT, is_dataset(IN));
is_dataset_test(2) ->
  IN = <<"/test/test">>,
  OUT = false,
  ?assertEqual(OUT, is_dataset(IN));
is_dataset_test(3) ->
  IN = <<"test//test">>,
  OUT = false,
  ?assertEqual(OUT, is_dataset(IN)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_snapshot(bitstring()) -> true | false.
is_snapshot(Bitstring) -> 
  is_snapshot(Bitstring, <<>>).

-spec is_snapshot(bitstring(), bitstring()) -> true | false.
is_snapshot(<<>>, Buf) 
  when Buf =/= <<>> ->
    true;
is_snapshot(<<Char:8, Rest/bitstring>>, Buf) ->
  ok.

is_snapshot_test() ->
  [ is_snapshot_test(X) || X <- [1] ].

is_snapshot_test(1) ->
  ?assertEqual(ok, ok).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
null_clean(Bitstring) ->
  null_clean(Bitstring, <<>>).
null_clean(<<A, Rest/bitstring>>, Buf) 
  when A =/= 0 ->
    null_clean(Rest, <<Buf/bitstring, A>>);
null_clean(_, Buf) ->
  Buf.

null_clean_test() ->
  [ null_clean_test(X) || X <- lists:seq(1,2) ].

null_clean_test(1) ->
  IN = <<"thisisatest",0,0,0,0>>,
  OUT = <<"thisisatest">>,
  ?assertEqual(OUT, null_clean(IN));
null_clean_test(2) ->
  IN = <<"thisis",0,"atest">>,
  OUT = <<"thisis">>,
  ?assertEqual(OUT, null_clean(IN)).
