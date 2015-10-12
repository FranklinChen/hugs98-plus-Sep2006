/* --------------------------------------------------------------------------
 * Machine dependent code
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * ------------------------------------------------------------------------*/
#ifndef __MACHDEP_H__
#define __MACHDEP_H__
#include "prelude.h"

#if HAVE_TIME_H
# include <time.h>
#endif

#if USE_REGISTRY
# if HAVE_WINDOWS_H
#include <windows.h>
# endif
#endif

/* --------------------------------------------------------------------------
 * Find information about a file:
 * ------------------------------------------------------------------------*/

#if RISCOS
typedef struct { unsigned hi, lo; } Time;
#define timeChanged(now,thn)    (now.hi!=thn.hi || now.lo!=thn.lo)
#define timeSet(var,tm)         var.hi = tm.hi; var.lo = tm.lo
#else
typedef time_t Time;
#define timeChanged(now,thn)    (now!=thn)
#define timeSet(var,tm)         var = tm
#endif

extern Void getFileInfo Args((String, Time *, Long *));

/* --------------------------------------------------------------------------
 * Prototypes for registry reading
 * ------------------------------------------------------------------------*/

#if USE_REGISTRY

#if HUGS_FOR_WINDOWS
extern Int    readRegInt     Args((String,Int));
extern Bool   writeRegInt    Args((String,Int));
#endif

extern String readRegString       Args((HKEY, String, String, String));
extern Bool   writeRegString      Args((String,String));
extern String readRegChildStrings Args((HKEY, String, String, String));
#endif /* USE_REGISTRY */

/* --------------------------------------------------------------------------
 * Search for script files on the HUGS path:
 * ------------------------------------------------------------------------*/

extern String RealPath  Args((String));
extern String substPath Args((String,String));
extern String uniqPath  Args((String));
#if !HUGS_SERVER
extern Bool startEdit   Args((Int,String));
#endif

extern  Int    shellEsc		Args((String,Bool,Bool));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal	Args((Void));
extern  Bool   getEchoTerminal	Args((Int));
extern  Void   setEchoTerminal	Args((Int,Bool));
extern  Bool   getBuffTerminal	Args((Int));
extern  Void   setBuffTerminal	Args((Int,Bool));
extern  Void   gcStarted	Args((Void));
extern  Void   gcScanning	Args((Void));
extern  Void   gcRecovered	Args((Int));
extern  Void   gcCStack		Args((Void));
extern  Void   needPrims        Args((Int,void*)); 

extern  String fromEnv		Args((String,String));
extern  Bool   initSystem       Args((Void));

extern	String dirname		Args((String));

#endif /* __MACHDEP_H__ */
