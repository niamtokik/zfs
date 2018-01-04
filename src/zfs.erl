%%%-------------------------------------------------------------------
%%% @title Erlang ZFS High Level Implementation
%%% @author Mathieu Kerjouan
%%% @copyright 2017
%%% @version 0.1.0
%%% @doc This module is the first step of Erlang ZFS Implementation
%%%      and contain all required functions for parsing ZFS streams.
%%% @end
%%% @see http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h
%%%-------------------------------------------------------------------
-module(zfs).
-compile(export_all).
-include("zfs.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc clean function remove null char from bitstring.
%% @end
%%--------------------------------------------------------------------
clean(Bitstring) ->
  clean(Bitstring, <<>>).
clean(<<A, Rest/bitstring>>, Buf) 
  when A =/= 0 ->
    clean(Rest, <<Buf/bitstring, A>>);
clean(_, Buf) ->
  Buf.

%%--------------------------------------------------------------------
%% @doc generic automaton handler function, take a list of action in 
%%      multiple form. Each action should take at least one argument
%%      wich is next action to do after executing the first one.
%% @end
%%--------------------------------------------------------------------
-spec next(list(), list()) -> {ok, list()} | term().
next([], Args) ->
  {ok, Args};
next([Function|T], Args) 
  when is_atom(Function) ->
    ?debugFmt("Function: ~p", [Function]),
    ?debugFmt("Rest: ~p", [T]),
    ?debugFmt("Args: ~p", [Args]),
    erlang:apply(?MODULE, Function, [T|Args]);
next([Function|T], Args) 
  when is_function(Function) ->
    ?debugFmt("Function: ~p", [Function]),
    ?debugFmt("Rest: ~p", [T]),
    ?debugFmt("Args: ~p", [Args]),
    erlang:apply(Function, [T|Args]);
next([{Module, Function}|T], Args) 
  when is_atom(Module), is_atom(Function) ->
    ?debugFmt("Module: ~p", [Module]),
    ?debugFmt("Function: ~p", [Function]),
    ?debugFmt("Rest: ~p", [T]),
    ?debugFmt("Args: ~p", [Args]),
    erlang:apply(Module, Function, [T|Args]).

%%--------------------------------------------------------------------
%% @doc simple function to put value in different structure like
%%      map and proplist.
%% @end
%%--------------------------------------------------------------------
put(Key, Value, Proplist) 
  when is_list(Proplist) ->
    [{Key, Value}|Proplist];
put(Key, Value, Map) 
  when is_map(Map) ->
    maps:put(Key, Value, Map).

%%--------------------------------------------------------------------
%% @doc main function to parse zfs stream. This function accept list
%%      or bitstring.
%% @end
%%--------------------------------------------------------------------
-spec dmu(bitstring() | list()) -> tuple().
dmu(List)
  when is_list(List) ->
    dmu(erlang:list_to_bitstring(List));
dmu(Bitstring) 
  when is_bitstring(Bitstring) ->
    drr_header(Bitstring, #{}).

%%--------------------------------------------------------------------
%% @doc drr_end/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_end() -> [atom(), ...].
drr_end() ->
  [ drr_checksum, drr_toguid, return ].

%%--------------------------------------------------------------------
%% @doc drr_object/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_object() -> [atom(), ...].
drr_object() ->
  [ drr_object, drr_type, drr_bonustype, drr_blksz
  , drr_bonuslen, drr_checksumtype, drr_compress
  , drr_pad, drr_toguid, return ].

%%--------------------------------------------------------------------
%% @doc drr_freeobjects/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_freeobjects() -> [atom(), ...].
drr_freeobjects() ->
  [ drr_firstobj, drr_numobjs, drr_toguid, return ].

%%--------------------------------------------------------------------
%% @doc drr_write/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_write() -> [atom(), ...].
drr_write() -> 
  [ drr_object, drr_type, drr_pad, drr_offset, drr_length
  , drr_toguid, drr_checksumtype, drr_checksumflags
  , drr_pad2, drr_key, return ].

%%--------------------------------------------------------------------
%% @doc drr_free/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_free() -> [atom(), ...].
drr_free() ->
  [ drr_object, drr_offset, drr_length, drr_toguid ].

%%--------------------------------------------------------------------
%% @doc drr_write_byref/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_write_byref() -> [atom(), ...].
drr_write_byref() ->
  [ drr_object, drr_offset, drr_length, drr_toguid
  , drr_refguid, drr_refobject, drr_refoffset
  , drr_checksumtype, drr_checksumflags, drr_pad2
  , drr_key, return ].

%%--------------------------------------------------------------------
%% @doc drr_spill/0.
%% @end
%%--------------------------------------------------------------------
-spec drr_spill() -> [atom(), ...].
drr_spill() ->
  [ drr_object, drr_length, drr_toguid, drr_pad, return ].

%%--------------------------------------------------------------------
%% @doc drr_header function will route type based on first 64bits of
%%      ZFS stream from send function. First 32bits define ZFS DRR,
%%      following 32bits define DRR payload length.
%%      Objects has defined size: 312bytes. This is the size of 
%%      the dmu_replay_record structure from zfs_ioctl header. So,
%%      we can retrieve all information with recursive call, but
%%      if our structure isn't well sized, we'll need to ensure
%%      to remove remaining data. Another method, more imperative,
%%      is to extract directly 312bytes from current stream and
%%      parse it after. Current implementation use pattern
%%      matching method.
%% @end
%%--------------------------------------------------------------------
drr_header(<<DrrType:32/little, DrrPayloadLen:32, Rest/bitstring>>, DmuHeader) ->
  ?debugFmt("type: ~p", [DrrType]),
  ?debugFmt("payload: ~p", [DrrPayloadLen]),
  DmuUpdate1 = maps:put(type, DrrType, DmuHeader),
  DmuUpdate2 = maps:put(payloadlen, DrrPayloadLen, DmuUpdate1),
  case DrrType of
    ?DRR_BEGIN -> 
      drr_begin(Rest, drr_begin(), #{}, DmuUpdate2);
    ?DRR_OBJECT -> 
      wip; % drr_object(Rest, drr_object(), #{}, DmuHeader);
    ?DRR_FREEOBJECTS -> 
      drr_freeobjects(Rest, drr_freeobjects(), #{});
    ?DRR_WRITE -> 
      wip; % drr_write(Rest, drr_write(), #{}, DmuHeader);
    ?DRR_FREE -> 
      wip; % drr_free(Rest, drr_free(), #{}, DmuHeader);
    ?DRR_END -> 
      wip; % drr_end(Rest, drr_end(), #{}, DmuHeader);
    ?DRR_WRITE_BYREF -> 
      wip; % drr_write_byref(Rest, drr_write_byref(), #{}, DmuHeader);
    ?DRR_SPILL -> 
      wip; % drr_spill(Rest, drr_spill(), #{}, DmuHeader);
    ?DRR_NUMTYPES -> 
      wip; % drr_numtypes(Rest, drr_numtypes(), #{}, DmuHeader)
    _ ->
      {error, "unsupported type"}
  end.

%%--------------------------------------------------------------------
%% @doc drr_begin/0 function return a list of all function to parse
%%      a begin ZFS stream.
%% @end
%%--------------------------------------------------------------------
-spec drr_begin() -> [atom(), ...].
drr_begin() ->
  [ drr_magic, drr_versioninfo, drr_creation_time, drr_type
  , drr_flags, drr_toguid, drr_fromguid, drr_toname, return ].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
drr_begin(Bitstring, Order, Buf, DmuHeader) ->
  ?debugMsg("drr_begin function"),
  next(Order, [Bitstring, Buf, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc drr_magic function extract magic ZFS value following and
%%      check if this value is well configured.
%% @end
%%--------------------------------------------------------------------
-spec drr_magic(bitstring()) -> {ok, list(), bitstring()}.
drr_magic(<<Magic:64/little, Rest/bitstring>>) ->
  ?debugFmt("magic: ~p", [Magic]),
  M = erlang:integer_to_binary(Magic, 16),
  case M =:= <<"2F5BACBAC">> of
      true ->
        ?debugFmt("magic: ~p", [M]),
        {ok, M, Rest}
  end.

-spec drr_magic(list(), bitstring(), map(), map()) -> tuple().
drr_magic(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
     {ok, Magic, Rest} = drr_magic(Bitstring),
     Return = maps:put(magic, Magic, Buf),
     next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc drr_versioninfo extract a 64bits from current bitstring and
%%      split it in 2 value: hdrtype and features flags.
%% @end
%%--------------------------------------------------------------------
-spec drr_versioninfo(bitstring) -> {ok, bitstring(), bitstring()}.
drr_versioninfo(<<VersionInfo:64/bitstring, Rest/bitstring>>) ->
  ?debugFmt("versioninfo: ~p", [VersionInfo]),
  {ok, VersionInfo, Rest}.

-spec drr_versioninfo(list(), bitstring(), map(), map()) -> tuple().
drr_versioninfo(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
  % we need to extract feature and hdrtype
  % but this thing currently doesn't work as expected...
  % <<Features:6/little, HdrType:2, _/bitstring>> = VersionInfo,
  % <<HdrType:16, Features/bitstring>> = VersionInfo,
  % ?debugFmt("feature: ~p:~p", [HdrType, Features]),
  % Return1 = maps:put(features, Features, Buf),
  % Return2 = maps:put(hdrtype, undef, Return1),
  {ok, VersionInfo, Rest} = drr_versioninfo(Bitstring),
  Return = maps:put(versioninfo, VersionInfo, Buf),
  next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc drr_creation_time extract a 64bits value from current
%%      bitstring.
%% @end
%%--------------------------------------------------------------------
-spec drr_creation_time(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_creation_time(<<CreationTime:64/bitstring, Rest/bitstring>>) ->
  ?debugFmt("creation_time: ~p", [CreationTime]),
  {ok, CreationTime, Rest}.

-spec drr_creation_time(list(), bitstring(), map(), map()) -> tuple().
drr_creation_time(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, CreationTime, Rest} = drr_creation_time(Bitstring),
    Return = maps:put(creation_time, CreationTime, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc drr_type extract 32bits value from current bitstring.
%% @end
%%--------------------------------------------------------------------
-spec drr_type(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_type(<<Type:32/bitstring, Rest/bitstring>>) ->
  ?debugFmt("type: ~p", [Type]),
  {ok, Type, Rest}.

-spec drr_type(list(), bitstring(), map(), map()) -> tuple().
drr_type(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, Type, Rest} = drr_type(Bitstring),
    Return = maps:put(type, Type, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc drr_flags function extract flags from current bitstring.
%% @end
%%--------------------------------------------------------------------
-spec drr_flags(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_flags(<<Flags:32/bitstring, Rest/bitstring>>) ->
  ?debugFmt("flags: ~p", [Flags]),
  {ok, Flags, Rest}.

-spec drr_flags(list(), bitstring(), map(), map()) -> tuple().
drr_flags(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, Flags, Rest} = drr_flags(Bitstring),
    Return = maps:put(flags, Flags, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc drr_toguid extract 64bits as bitstring from current 
%%      bitstring.
%% @end
%%--------------------------------------------------------------------
-spec drr_toguid(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_toguid(<<ToGuid:64/bitstring, Rest/bitstring>>) ->
  ?debugFmt("toguid: ~p", [ToGuid]),
  {ok, ToGuid, Rest}.

drr_toguid(Order, Bitstring, Buf) 
  when is_bitstring(Bitstring) ->
    {ok, ToGuid, Rest} = drr_toguid(Bitstring),
    Return = maps:put(toguid, ToGuid, Buf),
    next(Order, [Rest, Return]).

-spec drr_toguid(list(), bitstring(), map(), map()) ->  tuple().
drr_toguid(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, ToGuid, Rest} = drr_toguid(Bitstring),
    Return = maps:put(toguid, ToGuid, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_fromguid(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_fromguid(<<FromGuid:64/bitstring, Rest/bitstring>>) ->
  ?debugFmt("fromguid: ~p", [FromGuid]),
  {ok, FromGuid, Rest}.

drr_fromguid(Order,Bitstring, Buf) 
  when is_bitstring(Bitstring) ->
    {ok, FromGuid, Rest} = drr_fromguid(Bitstring),
    Return = maps:put(fromguid, FromGuid, Buf),
    next(Order, [Rest, Return]).

-spec drr_fromguid(list(), bitstring(), map(), map()) -> tuple().
drr_fromguid(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, FromGuid, Rest} = drr_fromguid(Bitstring),
    Return = maps:put(fromguid, FromGuid, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_toname(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_toname(<<ToName:(256*8)/bitstring, Rest/bitstring>>) ->
  ?debugFmt("toname: ~p", [clean(ToName)]),
  {ok, ToName, Rest}.

-spec drr_toname(list(), bitstring(), map(), map()) -> tuple().
drr_toname(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, ToName, Rest} = drr_toname(Bitstring),
    Return = maps:put(toname, clean(ToName), Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
return([], Rest, Buf ) ->
  {ok, Buf, Rest}.
return([], Rest, Buf, DmuHeader) ->
  {ok, maps:put(drr, Buf, DmuHeader), Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
drr_end(Bitstring, Order, DmuHeader) ->
  next(Order, [Bitstring, #{}, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_checksum(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_checksum(<<Checksum:256/bitstring, Rest/bitstring>>) ->
  ?debugFmt("Checksum: ~p", [Checksum]),
  {ok, Checksum, Rest}.

-spec drr_checksum(list(), bitstring(), map(), map()) -> tuple().
drr_checksum(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, Checksum, Rest} = drr_checksum(Bitstring),
    Return = maps:put(checksum, Checksum, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_object(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_object(<<Object:64, Rest/bitstring>>) ->
  ?debugFmt("Object: ~p", [Object]),
  {ok, Object, Rest}.

-spec drr_object(list(), bitstring(), map(), map()) -> tuple().
drr_object(Order, Bitstring, Buf, DmuHeader) 
  when is_bitstring(Bitstring) ->
    {ok, Object, Rest} = drr_object(Bitstring),
    Return = maps:put(object, Object, Buf),
    next(Order, [Rest, Return, DmuHeader]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_bonustype(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_bonustype(<<BonusType:64/bitstring, Rest/bitstring>>) -> 
  {ok, BonusType, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_blksz(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_blksz(<<BlkSz:32, Rest/bitstring>>) -> 
  {ok, BlkSz, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_bonuslen(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_bonuslen(<<BonusLen:32/bitstring, Rest/bitstring>>) ->
  {ok, BonusLen, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_checksumtype(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_checksumtype(<<ChecksumType:8, Rest/bitstring>>) -> 
  {ok, ChecksumType, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_compress(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_compress(<<Compress:8, Rest/bitstring>>) -> 
  {ok, Compress, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_pad(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_pad(<<Pad:8, Rest>>) -> 
  {ok, Pad, Rest}.

-spec drr_pad(bitstring(), non_neg_integer()) 
  -> {ok, bitstring(), bitstring()}.
drr_pad(Bitstring, Length) ->
  L = Length*8,
  <<Pad:L, Rest/bitstring>> = Bitstring,
  {ok, Pad, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
drr_freeobjects(Bitstring, Order, Buf) ->
  ?debugMsg("drr_freeobjects function"),
  next(Order, [Bitstring, Buf]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_firstobj(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_firstobj(<<FirstObj:64/little, Rest/bitstring>>) -> 
  {ok, FirstObj, Rest}.

drr_firstobj(Order, Bitstring, Buf) ->
  {ok, FirstObj, Rest} = drr_firstobj(Bitstring),
  Return = maps:put(firstobj, FirstObj, Buf),
  next(Order, [Rest, Return]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_numobjs(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_numobjs(<<NumObjs:64/little, Rest/bitstring>>) -> 
  {ok, NumObjs, Rest}.

drr_numobjs(Order, Bitstring, Buf) ->
  {ok, NumObjs, Rest} = drr_numobjs(Bitstring),
  Return = maps:put(numobjs, NumObjs, Buf),
  next(Order, [Rest, Return]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_offset(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_offset(<<Offset:64/bitstring, Rest/bitstring>>) -> 
  {ok, Offset, Rest}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec drr_length(bitstring()) -> {ok, bitstring(), bitstring()}.
drr_length(<<Length:64/bitstring, Rest/bitstring>>) -> 
  {ok, Length, Rest}.
