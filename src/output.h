/* --------------------------------------------------------------------------
 * Unparse expressions and types - for use in error messages, type checker
 * and for debugging.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * ------------------------------------------------------------------------*/
#ifndef __OUTPUT_H__
#define __OUTPUT_H__

extern Void printExp     Args((FILE *,Cell));
extern Void printType    Args((FILE *,Cell));
extern Void printContext Args((FILE *,List));
extern Void printPred    Args((FILE *,Cell));
extern Void printKind	 Args((FILE *,Kind));
extern Void printKinds	 Args((FILE *,Kinds));
extern Void printFD	 Args((FILE *,Pair));

#if OBSERVATIONS
#define ALLTAGS ""
extern Void printObserve Args((String));
#endif

#if DEBUG_SHOWSC
extern Void  printSc Args((FILE*, Text, Int, Cell));
#endif


#endif /* __OUTPUT_H__ */
