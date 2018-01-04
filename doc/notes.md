# Notes

This is a kind of specification documentation
base on what I need and how to use it.

## ZFS Stream Parsing 

First thing first. ZFS library for Erlang was made
to easily parse ZFS send/receive stream and split
it in small piece. 

Parsing a zfs stream should be simple and return 
standard and comprehensible data-structure.

    zfs_stream:parse(Stream).

Sometime, our ZFS stream is a bit larger than expected, 
so, we can filter it.  This filtering will be done with 
Erlang Match syntax.

    zfs_stream:parse(Stream, [{'object', '_'}]).

### Specific Stream Parsing Feature

    zfs_stream:parse(Stream, []).

ZFS Stream is a complex data-structure containing
different type of objects. We can parse it independantly
of each others.

    % dmu replay record is a full
    % zfs stream sent from zfs send command
    dmu_replay_record:parse(Stream).
    dmu_replay_record:parse(Stream, _Opts).

    % drr_begin is the first data structure
    % used in ZFS Stream and contain common
    % information about this stream
    drr_begin:parse(Stream).
    drr_begin:parse(Stream; _Opts).

    % drr_end
    drr_end:parse(Stream).
    drr_end:parse(Stream, _Opts).

    % drr_object
    drr_object:parse(Stream).
    drr_object:parse(Stream, _Opts).

    % drr_freeobjects
    drr_freeobjects:parse(Stream).
    drr_freeobjects:parse(Stream, _Opts).

    % drr_write
    drr_write:parse(Stream).
    drr_write:parse(Stream, _Opts).

    % drr_write_byref
    drr_write_byref:parse(Stream).
    drr_write_byref:parse(Stream, _Opts).

    % drr_free
    drr_free:parse(Stream).
    drr_free:parse(Stream, _Opts).

    % drr_spill
    drr_spill:parse(Stream).
    drr_spill:parse(Stream, _Opts).

Those functions return erlang map data-structure only
if the stream or part of the stream is the right object.
dmu_replay_record:parse/1 parse an entire ZFS stream and is 
equivalent with zfs_stream:parse/1 function.

## ZFS Stream Splitting

A ZFS Stream is a range of ordered transactional ZFS objects.
This library is made to split stream in small pieces.

    zfs_stream:split(Stream).
    zfs_stream:split(Stream, _Opts).

You can split it in different way, just split all logical part
in bitstring or parse each part with erlang abstraction.

    zfs_stream:split(Stream, [bitstring]).
    zfs_stream:split(Stream, [map]).

## ZFS Stream Analysing

All ZFS data-structure are delivered with checksum, analyzing it
give us the change to validate every piece of a stream based
on checksum.

    zfs_stream:analyze(Stream).
    zfs_stream:analyze(Stream, _Opts).

## ZFS Stream Crafting

If we have the possibility to analyze or read a stream, we have
also the possibility to craft any kind of ZFS stream from scratch.

    zfs_stream:craft(Type, Content).

## ZFS Stream Fuzzing

ZFS is executed in Kernel Land, if something goes wrong, in the
best case, system crash, in the worst case, an attacker can
install a rootkit or gain privilege access to our server. Fuzzing
is a technique used widely on security to check program inputs.

This high level library give us the possibility to randomly craft 
ZFS stream and check them but also alterate an existing one.

    % create a totally random zfs stream from scratch
    zfs_stream:fuzz().

    % generete an object stream based on Type
    zfs_stream:fuzz(Type).

    % extra options 
    zfs_stream:fuzz(Type, _Opts).
    zfs_stream:fuzz(Type, [{destination, Destination}
                          ,{source, Source}
                          ,{alter, []}
                          ,{format, Format}]).
