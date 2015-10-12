#include "Storable_aux.h"

#define DEFINE(T) \
void write##T##OffPtr(Hs##T *arg1, HsInt arg2, Hs##T arg3) { arg1[arg2] = arg3; } \
Hs##T read##T##OffPtr(Hs##T *arg1, HsInt arg2) { return arg1[arg2]; }

DEFINE(Int       )
DEFINE(Char      )
DEFINE(Ptr       )
DEFINE(FunPtr    )
DEFINE(Float     )
DEFINE(Double    )
DEFINE(StablePtr )
DEFINE(Int8      )
DEFINE(Int16     )
DEFINE(Int32     )
DEFINE(Int64     )
DEFINE(Word8     )
DEFINE(Word16    )
DEFINE(Word32    )
DEFINE(Word64    )

#undef DEFINE
