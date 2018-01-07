%%%===================================================================
%%%
%%%===================================================================
-module(drr_end).
-export([struct/0]).
-export([parse/1, parse/2]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

struct() ->
    [drr_checksum, drr_toguid].

parse(Bitstring) ->
    parse(Bitstring, []).

parse(Bitstring, _Opts) ->
    struct_parser:do(Bitstring, struct()).

