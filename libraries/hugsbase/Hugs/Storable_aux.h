#ifndef STORABLE_AUX_H
#define STORABLE_AUX_H

#include "HsFFI.h"

#define DECLARE(T) \
void write##T##OffPtr(Hs##T *arg1, HsInt arg2, Hs##T arg3); \
Hs##T read##T##OffPtr(Hs##T *arg1, HsInt arg2);

DECLARE(Int       )
DECLARE(Char      )
DECLARE(Ptr       )
DECLARE(FunPtr    )
DECLARE(Float     )
DECLARE(Double    )
DECLARE(StablePtr )
DECLARE(Int8      )
DECLARE(Int16     )
DECLARE(Int32     )
DECLARE(Int64     )
DECLARE(Word8     )
DECLARE(Word16    )
DECLARE(Word32    )
DECLARE(Word64    )

#undef DECLARE

#endif /* STORABLE_AUX_H */
