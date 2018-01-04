%%%-------------------------------------------------------------------
%%%
%%%-------------------------------------------------------------------
-define(MAXNAMELEN, 256).

-define(DRR_BEGIN, 0).
-define(DRR_OBJECT, 1).
-define(DRR_FREEOBJECTS, 2).
-define(DRR_WRITE, 3).
-define(DRR_FREE, 4).
-define(DRR_END, 5).
-define(DRR_WRITE_BYREF, 6).
-define(DRR_SPILL, 7).
-define(DRR_NUMTYPES, 8).

% drr_begin
% http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#128
-define(DRR_MAGIC_SIZE, 64).
-define(DRR_MAGIC, ?DRR_MAGIC_SIZE/bitstring).

-define(DRR_VERSIONINFO_SIZE, 64).
-define(DRR_VERSIONINFO, ?DRR_VERSIONINFO_SIZE/bitstring).

-define(DRR_CREATIONTIME_SIZE, 64).
-define(DRR_CREATIONTIME, ?DRR_CREATIONTIME_SIZE/bitstring).

-define(DRR_TYPE_SIZE, 32).
-define(DRR_TYPE, ?DRR_TYPE_SIZE/bitstring).

-define(DRR_FLAGS_SIZE, 32).
-define(DRR_FLAGS, ?DRR_FLAGS_SIZE/bitstring).

-define(DRR_TOGUID_SIZE, 64).
-define(DRR_TOGUID, ?DRR_TOGUID_SIZE/bitstring).

-define(DRR_FROMGUID_SIZE, 64).
-define(DRR_FROMGUID, ?DRR_FROMGUID_SIZE/bitstring).

-define(DRR_TONAME_SIZE, ?MAXNAMELEN).
-define(DRR_TONAME, ?MAXNAMELEN/bitstring).


% drr_end
% http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#139
-define(DRR_CHECKSUM_SIZE, 64).

% drr_object
% http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#144
-define(DRR_OBJECT_SIZE, 64).
-define(DRR_BLKSZ_SIZE, 32).
-define(DRR_BONUSLEN_SIZE, 32).
-define(DRR_CHECKSUMTYPE_SIZE, 8).
-define(DRR_COMPRESS_SIZE, 8).
-define(DRR_PAD_SIZE, 8).

% drr_freeobjects
% http://bxr.su/FreeBSD/sys/cddl/contrib/opensolaris/uts/common/fs/zfs/sys/zfs_ioctl.h#157

% drr_write

% drr_free

% drr_write_byref
