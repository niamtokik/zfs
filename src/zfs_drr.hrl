%%%===================================================================
%%% @author Mathieu Kerjouan
%%% @copyright 2018 (c) Mathieu Kerjouan
%%% @version 0.1.0
%%% @doc
%%% @end
%%%===================================================================

-define(DRR_BLKSZ_SIZE, 32).
-define(DRR_BONUSLEN_SIZE, 32).
-define(DRR_BONUSTYPE_SIZE, 32).
-define(DRR_CHECKSUM_SIZE,256).
-define(DRR_CHECKSUMFLAGS_SIZE, 8).
-define(DRR_CHECKSUMTYPE_SIZE, 8).
-define(DRR_COMPRESS_SIZE, 8).
-define(DRR_CREATIONTIME_SIZE, 64).
-define(DRR_FIRSTOBJ_SIZE, 64).
-define(DRR_FLAGS_SIZE, 32).
-define(DRR_FROMGUID_SIZE, 64).
-define(DRR_LENGTH_SIZE, 64).
-define(DRR_MAGIC_SIZE, 64).
-define(DRR_NUMOBJS_SIZE, 64).
-define(DRR_OBJECT_SIZE, 64).
-define(DRR_OFFSET_SIZE, 64).
-define(DRR_REFGUID_SIZE, 64).
-define(DRR_REFOBJECT_SIZE, 64).
-define(DRR_REFOFFSET_SIZE, 64).
-define(DRR_TOGUID_SIZE, 64).
-define(DRR_TONAME_SIZE, (256*8)).
-define(DRR_TYPE_SIZE, 32).
-define(DRR_VERSIONINFO_SIZE, 64).
