# zfs

Erlang ZFS High and Low Level implementation.

## Usage

This is an Erlang Library, you can use it on any project 
based on Licensing.

## Documentation

This project is born from one idea, reverse engineering ZFS
as gray box, from the hard way. We want to document every
ZFS aspect and offer a high level implementation to check if
specification and current existing documentation fit well.

You can find more documentation in doc directory at the root
of this project.

## Build

    $ rebar3 compile

## Test

Erlang ZFS implementation is also a test battery for ZFS, you
can run different test to check if our implementation is well
supported on your hardware, but also directly on your OS.

For running unit test:

    $ rebar3 eunit

For running common_test:

    $ rebar3 ct

For running local low level test:

    $ cd c_src && make test
