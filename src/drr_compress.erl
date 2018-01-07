%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @title zfs_stream drr_compress data structure 
%%% @doc this data structure is used in drr_object data structure. 
%%%      drr_compress is defined as unsigned 8bits integer (uint8_t),
%%%      and is not a composed data structure.
%%% @end
%%%===================================================================
-module(drr_compress).
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
-spec parse(bitstring()) -> {ok, map(), bitstring()}.
parse(Bitstring) ->
    parse(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec parse(bitstring(), list()) 
	   -> {ok, map(), bitstring()}.
parse(<<Compress:?DRR_COMPRESS_SIZE/little, Rest/bitstring>>
     ,_Opts) -> 
    ?debugFmt("drr_compress: ~p", [Compress]),
    {ok, #{compress => zio_compress(Compress)}, Rest}.

%%--------------------------------------------------------------------
%% @doc test inherit compression
%% @end
%%--------------------------------------------------------------------
parse_0001_test() ->
    IN = <<0>>,
    OUT = {ok, #{ compress => inherit }, <<>>},
    ?assertEqual(OUT, parse(IN)).

%%--------------------------------------------------------------------
%% @doc test activated compression
%% @end
%%--------------------------------------------------------------------
parse_0002_test() ->    
    IN = <<1>>,
    OUT = {ok, #{ compress => on }, <<>>},
    ?assertEqual(OUT, parse(IN)).

%%--------------------------------------------------------------------
%% @doc test lz4 compression
%% @end
%%--------------------------------------------------------------------
parse_0003_test() ->
    IN = <<15>>,
    OUT = {ok, #{ compress => lz4 }, <<>>},
    ?assertEqual(OUT, parse(IN)).

%%--------------------------------------------------------------------
%% @doc currently, if we don't support compression, we just crash
%%      and throw an exception
%% @end
%%--------------------------------------------------------------------
parse_0004_test() ->
    IN = <<255>>,
    ?assertException(error, function_clause, parse(IN)).
    
%%--------------------------------------------------------------------
%% @doc zio_compress/1 function enumerate all compatible compression
%%      algorithms, you can find this list in:
%%      http://bxr.su/FreeBSD/sys/cddl/boot/zfs/zfsimpl.h#429.
%% @end
%% @todo this function should be shared and not private to this
%%       module.
%%--------------------------------------------------------------------
-define(ZIO_COMPRESS(X,Y), zio_compress(X) -> Y;
	                   zio_compress(Y) -> X).
-spec zio_compress(integer()) -> atom();
		  (atom()) -> integer().
?ZIO_COMPRESS(0,  inherit);
?ZIO_COMPRESS(1,  on);
?ZIO_COMPRESS(2,  off);
?ZIO_COMPRESS(3,  lzjb);
?ZIO_COMPRESS(4,  empty);
?ZIO_COMPRESS(5,  gzip_1);
?ZIO_COMPRESS(6,  gzip_2);
?ZIO_COMPRESS(7,  gzip_3);
?ZIO_COMPRESS(8,  gzip_4);
?ZIO_COMPRESS(9,  gzip_5);
?ZIO_COMPRESS(10, gzip_6);
?ZIO_COMPRESS(11, gzip_7);
?ZIO_COMPRESS(12, gzip_8);
?ZIO_COMPRESS(13, gzip_9);
?ZIO_COMPRESS(14, zle);
?ZIO_COMPRESS(15, lz4);
?ZIO_COMPRESS(16, functions).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
zio_compress_0001_test() ->
    IN = 0,
    OUT = inherit,
    ?assertEqual(OUT, zio_compress(IN)),
    ?assertEqual(IN, zio_compress(OUT)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
zio_compress_0002_test() ->
    IN = 14,
    OUT = zle,
    ?assertEqual(OUT, zio_compress(IN)),
    ?assertEqual(IN, zio_compress(OUT)).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
zio_compress_0003_test() ->
    IN = 255,
    ?assertException(error, function_clause, zio_compress(IN)),
    ?assertException(error, function_clause, zio_compress(random_pattern)).
