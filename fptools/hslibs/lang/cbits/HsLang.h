/* -----------------------------------------------------------------------------
 * $Id: HsLang.h,v 1.4 2001/10/19 05:55:02 sof Exp $
 *
 * Definitions for package `lang' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSLANG_H
#define HSLANG_H

/* PackedString.c */
extern StgInt byteArrayHasNUL__ (StgByteArray ba, StgInt len);

/* rawSystem.c */
extern HsInt rawSystemCmd(HsAddr cmd);

/* envHelper.c */
extern HsAddr getEnvBlock();

/* copyFile.c */
extern HsInt primCopyFile(char* from, char* to);

#ifdef _WIN32
extern HsAddr primGetLastErrorString();
extern void   primLocalFree(HsAddr ptr);
#endif

#endif
