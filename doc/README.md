# Documentation

## Why implement ZFS in Erlang?

Erlang is natively distributed, high level and stable language. 
ZFS is complexe file-system requiring lot of knowledge. This
file-system is also an old-one, developped in the 2000's, it
wasn't developped with security in mind. ZFS stream for example
should be trusted. 

Erlang is particularly efficient to parse binary string, ZFS
is a binary data-structure...

## What can we do with this library?

Currently, this library is only used to parse and check
ZFS stream send from `zfs send` command.

## Some idea of concrete work?

 * authenticated flow from external customers
 * authenticated flow from internal servers
 * zfs stream split
 * zfs stream fuzzing
 * zfs stream distributed check
 * and much more...
