/* --------------------------------------------------------------------------
 * Primitive functions, input output etc...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: builtin.c,v $
 * $Revision: 1.93 $
 * $Date: 2006/06/01 22:20:17 $
 * ------------------------------------------------------------------------*/

/* We include math.h before prelude.h because SunOS 4's cpp incorrectly
 * reports an error if you use "name(n)" as a macro with arguments and
 * "name" as a normal identifier (with no arguments).  ADR
 */
#include <math.h>
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "machdep.h"
#include "char.h"
#include <ctype.h>

#if HAVE_IO_H
# include <io.h>
#endif

/* Header files needed to compile the IO primitives */
#if IO_MONAD

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#elif HAVE_TYPES_H
# include <types.h>
#endif
#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#elif HAVE_STAT_H
# include <stat.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_TIMES_H && !mingw32_HOST_OS
# include <sys/times.h>
#endif

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_SYS_RESOURCE_H && !mingw32_HOST_OS
# include <sys/resource.h>
#endif

#if HAVE_ERRNO_H
# include <errno.h>
#endif

#if HAVE_SYS_TIMEB_H
# include <sys/timeb.h>
#endif

#if HAVE_WINDOWS_H
# include <windows.h>
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
#endif

#if HAVE_DIRECT_H
# include <direct.h>
#endif

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if defined(openbsd_HOST_OS) || defined(linux_HOST_OS)
/* Needed for mallocBytesRWX() */
#include <inttypes.h>
#include <sys/mman.h>
#endif

#endif /* IO_MONAD */

#include "builtin.h"

Name nameNegate,  nameFlip;             /* primitives reqd for parsing     */
Name nameFrom,    nameFromThen;
Name nameFromTo,  nameFromThenTo;
Name nameFatbar,  nameFail;             /* primitives reqd for translation */
Name nameIf,      nameSel;
Name nameId,      nameOtherwise;
Name nameConCmp,  nameEnRange;          /* primitives used for deriv inst  */
Name nameEnIndex, nameEnInRng;
Name nameEnToEn,  nameEnFrEn;
Name nameEnFrom,  nameEnFrTh;
Name nameEnFrTo;
Name nameBlackHole;                     /* for GC-detected black hole      */
Name nameInd;				/* for dict indirection		   */
Name namePrint,   nameNPrint;           /* primitives for printing         */

Name nameFst,     nameSnd;              /* 2-tuple selector functions      */
Name nameAnd,     nameOr;               /* built-in logical connectives    */
Name namePrimThrow;                     /* throw primitive function        */
Name nameComp;                          /* function composition            */
Name nameApp;                           /* list append                     */
Name nameShowField;                     /* display single field            */
Name nameShowParen;                     /* wrap with parens                */
Name nameReadField;                     /* read single field               */
Name nameReadParen;                     /* unwrap from parens              */
Name nameLex;                           /* lexer                           */
Name nameRangeSize;                     /* calculate size of index range   */
Name nameCompAux;                       /* auxiliary function for compares */
Name namePmInt,   namePmFlt;            /* primitives for pattern matching */
Name nameReturnIO;
Name namePmInteger;
#if NPLUSK
Name namePmNpk,   namePmSub;            /* primitives for (n+k) patterns   */
#endif
#if TREX
Name nameRecExt,  nameRecBrk;           /* Extend and break a record       */
Name nameRecSel,  nameRecShw;           /* Select and show a record        */
Name nameRecEq;				/* Compare records		   */
Name nameAddEv;				/* Add up evidence		   */
#endif

Name nameRationalToFloat;
Name nameRationalToDouble;
#if SHORT_CIRCUIT_COERCIONS
Name nameFloatToRational;
Name nameDoubleToRational;
Name nameDoubleToRatio;
Name nameIntToRatio;
Name nameIntToFloat;
Name nameIntToDouble;
Name nameDoubleToFloat;
Name nameFloatToDouble;
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

PROTO_PRIM(primFatbar);
PROTO_PRIM(primFail);
PROTO_PRIM(primCatchError);
PROTO_PRIM(primThrowException);
PROTO_PRIM(primCatchException);
PROTO_PRIM(primBlackHole);
PROTO_PRIM(primIndirect);
PROTO_PRIM(primSel);
PROTO_PRIM(primIf);
PROTO_PRIM(primStrict);
PROTO_PRIM(primSeq);
PROTO_PRIM(primConCmp);
PROTO_PRIM(primEnRange);
PROTO_PRIM(primEnIndex);
PROTO_PRIM(primEnInRng);
PROTO_PRIM(primEnFrEn);
PROTO_PRIM(primEnToEn);
PROTO_PRIM(primEnFrom);
PROTO_PRIM(primEnFrTh);
PROTO_PRIM(primEnFrTo);

PROTO_PRIM(primMinInt);
PROTO_PRIM(primMaxInt);
PROTO_PRIM(primPlusInt);
PROTO_PRIM(primMinusInt);
PROTO_PRIM(primMulInt);
PROTO_PRIM(primDivInt);
PROTO_PRIM(primQuotInt);
PROTO_PRIM(primModInt);
PROTO_PRIM(primRemInt);
PROTO_PRIM(primQrmInt);
PROTO_PRIM(primNegInt);

PROTO_PRIM(primAndInt);
PROTO_PRIM(primOrInt);
PROTO_PRIM(primXorInt);
PROTO_PRIM(primComplementInt);
PROTO_PRIM(primShiftInt);
PROTO_PRIM(primBitInt);
PROTO_PRIM(primTestInt);

PROTO_PRIM(primCharToInt);
PROTO_PRIM(primIntToChar);
PROTO_PRIM(primWordToInt);
PROTO_PRIM(primIntToWord);
PROTO_PRIM(primIntToFloat);
PROTO_PRIM(primIntToDouble);
PROTO_PRIM(primDummyCvt);

PROTO_PRIM(primRationalToFloat);
PROTO_PRIM(primRationalToDouble);

#if WORD_OPS
PROTO_PRIM(primMaxWord);
PROTO_PRIM(primPlusWord);
PROTO_PRIM(primMinusWord);
PROTO_PRIM(primNegateWord);
PROTO_PRIM(primMulWord);
PROTO_PRIM(primDivWord);
PROTO_PRIM(primQuotWord);
PROTO_PRIM(primModWord);
PROTO_PRIM(primRemWord);
PROTO_PRIM(primQrmWord);

PROTO_PRIM(primAndWord);
PROTO_PRIM(primOrWord);
PROTO_PRIM(primXorWord);
PROTO_PRIM(primComplementWord);
PROTO_PRIM(primShiftWord);
PROTO_PRIM(primRotateWord);
PROTO_PRIM(primBitWord);
PROTO_PRIM(primTestWord);

PROTO_PRIM(primItoI8);
PROTO_PRIM(primItoI16);
PROTO_PRIM(primItoI32);

PROTO_PRIM(primI8toI);
PROTO_PRIM(primI16toI);
PROTO_PRIM(primI32toI);

PROTO_PRIM(primI32toI64);
PROTO_PRIM(primI64toI32);

PROTO_PRIM(primWtoW8);
PROTO_PRIM(primWtoW16);
PROTO_PRIM(primWtoW32);

PROTO_PRIM(primW8toW);
PROTO_PRIM(primW16toW);
PROTO_PRIM(primW32toW);

PROTO_PRIM(primW32toW64);
PROTO_PRIM(primW64toW32);
#endif

PROTO_PRIM(primFreeHFunPtr);

PROTO_PRIM(primDoubleToFloat);
PROTO_PRIM(primFloatToDouble);

PROTO_PRIM(primPlusFloat);
PROTO_PRIM(primMinusFloat);
PROTO_PRIM(primMulFloat);
PROTO_PRIM(primDivFloat);
PROTO_PRIM(primNegFloat);

PROTO_PRIM(primPlusDouble);
PROTO_PRIM(primMinusDouble);
PROTO_PRIM(primMulDouble);
PROTO_PRIM(primDivDouble);
PROTO_PRIM(primNegDouble);

#if FLOATS_SUPPORTED
PROTO_PRIM(primSinFloat);
PROTO_PRIM(primCosFloat);
PROTO_PRIM(primTanFloat);
PROTO_PRIM(primAsinFloat);
PROTO_PRIM(primAcosFloat);
PROTO_PRIM(primAtanFloat);
#if 0 /* Not used in current Prelude */
PROTO_PRIM(primAtan2Float);
#endif
PROTO_PRIM(primExpFloat);
PROTO_PRIM(primLogFloat);
PROTO_PRIM(primSqrtFloat);
PROTO_PRIM(primFloatToInt);
PROTO_PRIM(primFloatRadix);
PROTO_PRIM(primFloatDigits);
PROTO_PRIM(primFloatMinExp);
PROTO_PRIM(primFloatMaxExp);
PROTO_PRIM(primFloatDecode);
PROTO_PRIM(primFloatEncode);

PROTO_PRIM(primSinDouble);
PROTO_PRIM(primCosDouble);
PROTO_PRIM(primTanDouble);
PROTO_PRIM(primAsinDouble);
PROTO_PRIM(primAcosDouble);
PROTO_PRIM(primAtanDouble);
#if 0 /* Not used in current Prelude */
PROTO_PRIM(primAtan2Double);
#endif
PROTO_PRIM(primExpDouble);
PROTO_PRIM(primLogDouble);
PROTO_PRIM(primSqrtDouble);
PROTO_PRIM(primDoubleToInt);
PROTO_PRIM(primDoubleDigits);
PROTO_PRIM(primDoubleMinExp);
PROTO_PRIM(primDoubleMaxExp);
PROTO_PRIM(primDoubleDecode);
PROTO_PRIM(primDoubleEncode);
#endif /* FLOATS_SUPPORTED */

PROTO_PRIM(primNullPtr);
PROTO_PRIM(primPlusPtr);
PROTO_PRIM(primAlignPtr);
PROTO_PRIM(primMinusPtr);
PROTO_PRIM(primEqPtr);
PROTO_PRIM(primCmpPtr);

PROTO_PRIM(primEqInt);
PROTO_PRIM(primCmpInt);

PROTO_PRIM(primEqWord);
PROTO_PRIM(primCmpWord);

PROTO_PRIM(primEqChar);
PROTO_PRIM(primCmpChar);

PROTO_PRIM(primEqFloat);
PROTO_PRIM(primCmpFloat);

PROTO_PRIM(primEqDouble);
PROTO_PRIM(primCmpDouble);

PROTO_PRIM(primMaxChar);
PROTO_PRIM(primIsUpper);
PROTO_PRIM(primIsLower);
PROTO_PRIM(primIsAlpha);
PROTO_PRIM(primIsAlphaNum);
PROTO_PRIM(primIsPrint);
PROTO_PRIM(primToUpper);
PROTO_PRIM(primToLower);
PROTO_PRIM(primToTitle);
PROTO_PRIM(primUniGenCat);

#if TREX
PROTO_PRIM(primRecExt);
PROTO_PRIM(primRecBrk);
PROTO_PRIM(primRecSel);
PROTO_PRIM(primRecShw);
PROTO_PRIM(primRecEq);
#endif

#if OBSERVATIONS
EXT_PROTO_PRIM(primObserve);
EXT_PROTO_PRIM(primBkpt);
EXT_PROTO_PRIM(primSetBkpt);
#endif

PROTO_PRIM(primUnsafePtrEq);
PROTO_PRIM(primUnsafePtrToInt);

static Cell local followInd Args(( Cell ));

/* --------------------------------------------------------------------------
 * Table of primitive/built-in values:
 * ------------------------------------------------------------------------*/

static struct primitive builtinPrimTable[] = {
  {"fatbar",            2, primFatbar},
  {"fail",              0, primFail},
  {"catchError",        1, primCatchError},
  {"primThrowException",1, primThrowException},
  {"primCatchException",1, primCatchException},
  {"gcBhole",           0, primBlackHole},
  {"dictIndirect",      1, primIndirect},
  {"sel",               3, primSel},
  {"if",                3, primIf},
  {"conCmp",            2, primConCmp},
  {"enRange",           1, primEnRange},
  {"enIndex",           2, primEnIndex},
  {"enInRng",           2, primEnInRng},
  {"enToEn",            2, primEnToEn},
  {"enFrEn",            1, primEnFrEn},
  {"enFrom",            1, primEnFrom},
  {"enFrTh",            2, primEnFrTh},
  {"enFrTo",            2, primEnFrTo},

  {"primMinInt",        0, primMinInt},
  {"primMaxInt",        0, primMaxInt},
  {"primPlusInt",       2, primPlusInt},
  {"primMinusInt",      2, primMinusInt},
  {"primMulInt",        2, primMulInt},
  {"primDivInt",        2, primDivInt},
  {"primQuotInt",       2, primQuotInt},
  {"primModInt",        2, primModInt},
  {"primRemInt",        2, primRemInt},
  {"primNegInt",        1, primNegInt},
  {"primQrmInt",        2, primQrmInt},

  {"primAndInt",        2, primAndInt},
  {"primOrInt",         2, primOrInt},
  {"primXorInt",        2, primXorInt},
  {"primComplementInt", 1, primComplementInt},
  {"primShiftInt",      2, primShiftInt},
  {"primBitInt",        1, primBitInt},
  {"primTestInt",       2, primTestInt},

#if WORD_OPS
  {"primMaxWord",       0, primMaxWord},
  {"primPlusWord",      2, primPlusWord},
  {"primMinusWord",     2, primMinusWord},
  {"primNegateWord",    1, primNegateWord},
  {"primMulWord",       2, primMulWord},
  {"primDivWord",       2, primDivWord},
  {"primQuotWord",      2, primQuotWord},
  {"primModWord",       2, primModWord},
  {"primRemWord",       2, primRemWord},
  {"primQrmWord",       2, primQrmWord},

  {"primAndWord",       2, primAndWord},
  {"primOrWord",        2, primOrWord},
  {"primXorWord",       2, primXorWord},
  {"primComplementWord",1, primComplementWord},
  {"primShiftWord",     2, primShiftWord},
  {"primRotateWord",    3, primRotateWord},
  {"primBitWord",       1, primBitWord},
  {"primTestWord",      2, primTestWord},

  {"primIntToInt8",     1, primItoI8},
  {"primIntToInt16",    1, primItoI16},
  {"primIntToInt32",    1, primItoI32},

  {"primInt8ToInt",     1, primI8toI},
  {"primInt16ToInt",    1, primI16toI},
  {"primInt32ToInt",    1, primI32toI},

  {"primInt32ToInt64",  2, primI32toI64},
  {"primInt64ToInt32",  1, primI64toI32},

  {"primWordToWord8",   1, primWtoW8},
  {"primWordToWord16",  1, primWtoW16},
  {"primWordToWord32",  1, primWtoW32},

  {"primWord8ToWord",   1, primW8toW},
  {"primWord16ToWord",  1, primW16toW},
  {"primWord32ToWord",  1, primW32toW},

  {"primWord32ToWord64",2, primW32toW64},
  {"primWord64ToWord32",1, primW64toW32},
#endif

  {"freeHaskellFunPtr", 1+IOArity, primFreeHFunPtr},

#if !BIGNUMS                            /* Implement Integer as Int        */
  {"primPlusInteger",   2, primPlusInt},
  {"primMinusInteger",  2, primMinusInt},
  {"primMulInteger",    2, primMulInt},
  {"primQrmInteger",    2, primQrmInt},
  {"primNegInteger",    1, primNegInt},
  {"primIntToInteger",  1, primDummyCvt},
  {"primIntegerToInt",  1, primDummyCvt},
  {"primIntegerToFloat",1, primIntToFloat},
  {"primIntegerToDouble",1,primIntToDouble},
  {"primEqInteger",     2, primEqInt},
  {"primCmpInteger",    2, primCmpInt},
#endif

  {"primPlusFloat",     2, primPlusFloat},
  {"primMinusFloat",    2, primMinusFloat},
  {"primMulFloat",      2, primMulFloat},
  {"primDivFloat",      2, primDivFloat},
  {"primNegFloat",      1, primNegFloat},

  {"primPlusDouble",    2, primPlusDouble},
  {"primMinusDouble",   2, primMinusDouble},
  {"primMulDouble",     2, primMulDouble},
  {"primDivDouble",     2, primDivDouble},
  {"primNegDouble",     1, primNegDouble},

#if FLOATS_SUPPORTED
  {"primSinFloat",      1, primSinFloat},
  {"primCosFloat",      1, primCosFloat},
  {"primTanFloat",      1, primTanFloat},
  {"primAsinFloat",     1, primAsinFloat},
  {"primAcosFloat",     1, primAcosFloat},
  {"primAtanFloat",     1, primAtanFloat},
  {"primExpFloat",      1, primExpFloat},
  {"primLogFloat",      1, primLogFloat},
  {"primSqrtFloat",     1, primSqrtFloat},
  {"primFloatToInt",    1, primFloatToInt},
  {"primFloatRadix",    0, primFloatRadix},
  {"primFloatDigits",   0, primFloatDigits},
  {"primFloatMinExp",   0, primFloatMinExp},
  {"primFloatMaxExp",   0, primFloatMaxExp},
  {"primFloatDecode",   1, primFloatDecode},
  {"primFloatEncode",   2, primFloatEncode},

  {"primSinDouble",     1, primSinDouble},
  {"primCosDouble",     1, primCosDouble},
  {"primTanDouble",     1, primTanDouble},
  {"primAsinDouble",    1, primAsinDouble},
  {"primAcosDouble",    1, primAcosDouble},
  {"primAtanDouble",    1, primAtanDouble},
  {"primExpDouble",     1, primExpDouble},
  {"primLogDouble",     1, primLogDouble},
  {"primSqrtDouble",    1, primSqrtDouble},
  {"primDoubleToInt",   1, primDoubleToInt},
  {"primDoubleRadix",   0, primFloatRadix},
  {"primDoubleDigits",  0, primDoubleDigits},
  {"primDoubleMinExp",  0, primDoubleMinExp},
  {"primDoubleMaxExp",  0, primDoubleMaxExp},
  {"primDoubleDecode",  1, primDoubleDecode},
  {"primDoubleEncode",  2, primDoubleEncode},
#endif

  {"primIntToChar",     1, primIntToChar},
  {"primCharToInt",     1, primCharToInt},
  {"primIntToWord",     1, primIntToWord},
  {"primWordToInt",     1, primWordToInt},
  {"primIntToFloat",    1, primIntToFloat},
  {"primIntToDouble",   1, primIntToDouble},
  {"primDoubleToFloat", 1, primDoubleToFloat},
  {"primFloatToDouble", 1, primFloatToDouble},

  {"primRationalToFloat",  1, primRationalToFloat},
  {"primRationalToDouble", 1, primRationalToDouble},

  {"nullPtr",           0, primNullPtr},
  {"plusPtr",           2, primPlusPtr},
  {"alignPtr",          2, primAlignPtr},
  {"minusPtr",          2, primMinusPtr},
  {"primEqPtr",         2, primEqPtr},
  {"primCmpPtr",        2, primCmpPtr},

  {"primEqInt",         2, primEqInt},
  {"primCmpInt",        2, primCmpInt},
  {"primEqWord",        2, primEqWord},
  {"primCmpWord",       2, primCmpWord},
  {"primEqChar",        2, primEqChar},
  {"primCmpChar",       2, primCmpChar},
  {"primEqFloat",       2, primEqFloat},
  {"primCmpFloat",      2, primCmpFloat},
  {"primEqDouble",      2, primEqDouble},
  {"primCmpDouble",     2, primCmpDouble},

  {"primUnsafeCoerce",  1, primDummyCvt},       /* breaks the type system */

  {"strict",            2, primStrict},
  {"seq",               2, primSeq},

  {"primMaxChar",	0, primMaxChar},
  {"isUpper",		1, primIsUpper},
  {"isLower",		1, primIsLower},
  {"isAlpha",		1, primIsAlpha},
  {"isAlphaNum",	1, primIsAlphaNum},
  {"isPrint",		1, primIsPrint},
  {"toUpper",		1, primToUpper},
  {"toLower",		1, primToLower},
  {"toTitle",		1, primToTitle},
  {"primUniGenCat",	1, primUniGenCat},

#if TREX
  {"recExt",            3, primRecExt},
  {"recBrk",            2, primRecBrk},
  {"recSel",            2, primRecSel},
  {"recShw",            5, primRecShw},
  {"recEq",		6, primRecEq},
#endif		        

#if OBSERVATIONS
  {"observe",           2, primObserve},
  {"bkpt",              2, primBkpt},
  {"setBkpt",           2+IOArity, primSetBkpt},
#endif

  {"unsafePtrEq",       2, primUnsafePtrEq},    /* breaks the semantics  */
  {"unsafePtrToInt",    1, primUnsafePtrToInt}, /* breaks the semantics  */

  {0,                   0, 0}
};

/* --------------------------------------------------------------------------
 * Primitive functions:
 *
 * IMPORTANT NOTICE: the primitive function definitions in this file
 * should be written in a style that permits correct execution *without*
 * conservative garbage collection (i.e., without marking from the C stack).
 * Adding primitive definitions that do not meet this requirement may
 * corrupt the heap and lead to failed execution; do not modify this code
 * unless you are really confident about what you are doing.
 *
 * Some general guidelines follow, using c, e to denote expressions that
 * involve either at most 1 allocation, or the possibility/certainty of
 * multiple allocations, resp.
 *
 * push(c);             Ok.
 * push(e);             Bad -- intermediate result may be lost if GC occurs
 *                      in the middle of building e; break e into steps, and
 *                      use toparg(), topfun(), etc.
 *
 * Cell x = ...;        Safe if value assigned to x will never be an
 * <any code with a     indirection.  (Otherwise, cell assigned to x may
 * possible alloc>      be returned to freeList *before* the value is used.)
 * ... x ...            Probably best avoided in other circumstances.
 *
 * updateRoot(e);       All ok.
 * updapRoot(e,e);
 * updateRoot(mkInt(n));
 * eval(pop());
 *
 * eval(ap(c,pop()));   Bad -- a GC call may corrupt value pop'd off stack.
 *
 * It is also worth a reminder that the fst and snd values passed in any call
 * to the allocator are automatically marked and preserved if a GC is needed.
 * As a result, code like the following is guaranteed to be safe:
 *  return ap(ap(mkTuple(2),ZERONUM),ZERONUM);    (ZERONUM is a constant)
 *  for ( ... )                                   (PROVIDED that ds is the
 *     ds = cons(consChar(c),ds);                  only var that needs GC).
 *
 * If these restrictions are judged to be too onerous in particular cases,
 * temporarily enable conservative GC (and reset it to the original state,
 * either on or off at the beginning of the operation).  See bignums.c
 * for an example.
 *
 * There are also certain conventions that must always be obeyed, regardless
 * of whether conservative GC is in use.  For example:
 *
 * lhs = expr;          If lhs involves an address calculation that may be
 *                      invalidated by a gc, and expr could trigger an alloc,
 *                      then this expression is bad, or at least not portable:
 *                      it will only do the right thing under some evaluation
 *                      orders.  For example:  hd(top()) = ap(..,..) is bad,
 *                      unless you know that top() will never be modified
 *                      during a GC.
 *
 *                      This is no different from the problems that occur
 *                      with non-portable combinations of stack operators
 *                      like push(top());  The solution is also the same:
 *                      use an intermediate variable to make the order
 *                      of evaluation explicit.
 *
 * If this version of Hugs has been modified to allow different or
 * additional run-time representations for certain values, then the
 * examples and principles illustrated here may need to be reconsidered,
 * and possibly reclassified.  The same will also be true if the execution
 * mechanisms etc., are changed in any way.  (And all this is assuming
 * that the original implementations are correct...)
 * ------------------------------------------------------------------------*/

primFun(primFatbar) {                   /* Fatbar primitive                */
    Cell temp = evalWithNoError(primArg(2));
    if (nonNull(temp))
	if (temp==nameFail)             /* _FAIL [] r = r                  */
	    updateRoot(primArg(1));
	else
	    throwException(temp);
    else
	updateRoot(primArg(2));         /* l     [] r = l  -- otherwise    */
}

primFun(primFail) {                     /* Failure primitive               */
    throwException(nameFail);
}

primFun(primBlackHole) {
    throwException(nameNonTermination);
}

primFun(primIndirect) {
    throwException(nameNonTermination);
}

primFun(primCatchError) {               /* Error catching  primitive       */
    Cell err = NIL;
    err = evalWithNoError(primArg(1));  /*  :: a -> Maybe a                */
    if (isNull(err)) {
	updapRoot(nameJust, primArg(1));
    } else {
	updateRoot(nameNothing);
    }
}

primFun(primThrowException) {           /* Failure primitive               */
    throwException(primArg(1));         /*  :: Exception -> a              */
}

/* This function ought to be in the IO monad to preserve referential       */
/* transparency but it has tricky interactions with the concurrency parts  */
/* of the IO monad so we provide it in unsafe form here and make it safe   */
/* in the Prelude.                                                         */

primFun(primCatchException) {	       /* Error catching primitive         */
    Cell err = NIL;                    /*  :: a -> Either Exception a      */
    err = evalWithNoError(primArg(1)); 
    if (isNull(err)) {
	updapRoot(nameRight, primArg(1));
    } else {
	updapRoot(nameLeft, err);
    }
}

primFun(primSel) {                      /* Component selection             */
    eval(primArg(2));                   /* _sel c e n  return nth component*/
    if (whnfHead==primArg(3))           /* in expr e, built with cfun c    */
	updateRoot(pushed(intOf(primArg(1))-1));
    else
	internal("primSel");
}

primFun(primIf) {                       /* Conditional primitive           */
    eval(primArg(3));
    checkBool();
    if (whnfHead==nameTrue)
	updateRoot(primArg(2));
    else
	updateRoot(primArg(1));
}

primFun(primStrict) {                   /* Strict application primitive    */
    eval(primArg(1));                   /* evaluate 2nd argument           */
    updapRoot(primArg(2),primArg(1));   /* and apply 1st argument to result*/
}

primFun(primSeq) {                      /* Strict sequencing primitive     */
    eval(primArg(2));                   /* evaluate 1st argument           */
    updateRoot(primArg(1));             /* and return the first            */
}

primFun(primConCmp) {                   /* compare constructors            */
    Int l,r;                            /*  :: a -> a -> Ordering          */
    ConArg(l,2);
    ConArg(r,1);
    updateRoot(l<r ? nameLT : (l>r ? nameGT : nameEQ));
}

primFun(primEnRange) {                  /* derived range for enum type     */
    eval(primArg(1));                   /* :: (a,a) -> [a]                 */
    updapRoot(ap(nameEnFrTo,primArg(3)),primArg(2));
}

primFun(primEnIndex) {                  /* derived index for enum type     */
    Int l,h,ix;                         /*  :: (a,a) -> a -> Int           */
    eval(primArg(2));
    ConArg(l,4);                        /* evaluate lower bound            */
    ConArg(h,3);                        /* evaluate upper bound            */
    ConArg(ix,1);                       /* evaluate index                  */
    if (l<=ix && ix<=h) {
	IntResult(ix-l);
    } else {
	throwException(ap(nameErrorCall,
		 mkStr(findText("Ix.index: Index out of range."))));
    }
}

primFun(primEnInRng) {                  /* derived inRange for enum type   */
    Int l,h,ix;                         /*  :: (a,a) -> a -> Bool          */
    eval(primArg(2));
    ConArg(l,4);                        /* evaluate lower bound            */
    ConArg(h,3);                        /* evaluate upper bound            */
    ConArg(ix,1);                       /* evaluate index                  */
    BoolResult(l<=ix && ix<=h);
}

primFun(primEnToEn) {                   /* derived toEnum for enum type    */
    Name n;                             /* :: a -> Int -> a                */
    Int  i;
    eval(primArg(2));
    checkCon();
    n = whnfHead;
    IntArg(i,1);
    if (nonNull(n = cfunByNum(n,i)))
	updateRoot(n);
    else
	throwException(ap(nameErrorCall,
		mkStr(findText("toEnum: out of range"))));
}

primFun(primEnFrEn) {                   /* derived fromEnum for enum type  */
    Int i;                              /* :: a -> Int                     */
    ConArg(i,1);
    IntResult(i==0 ? 0 : (i-1));
}

primFun(primEnFrom) {                   /* derived enumFrom for enum type  */
    Name cfs;                           /* :: a -> [a]                     */
    eval(primArg(1));
    checkCon();
    cfs = succCfun(whnfHead);
    push(isNull(cfs) ? nameNil : ap(nameEnFrom,cfs));
    updapRoot(ap(nameCons,whnfHead),top());
}

primFun(primEnFrTo) {                   /* derived enumFromTo for enum type*/
    Name l,r;                           /* :: a -> a -> [a]                */
    eval(primArg(2));
    checkCon();
    l = whnfHead;
    eval(primArg(1));
    checkCon();
    r = whnfHead;
    if (cfunOf(l) < cfunOf(r)) {
	push(ap(nameEnFrTo,succCfun(l)));
	updapRoot(ap(nameCons,l),ap(top(),whnfHead));
    }
    else if (l==r) {
	updapRoot(ap(nameCons,l),nameNil);
    } else {
	updateRoot(nameNil);
    }
}

primFun(primEnFrTh) {                   /* derived enumFromThen for enum ty*/
    Name f,n;                           /* :: a -> a -> [a]                */
    eval(primArg(2));   
    checkCon();
    f = whnfHead;
    eval(primArg(1));
    checkCon();
    n = nextCfun(f,whnfHead);
    if (isNull(n)) {
	push(ap(nameCons,whnfHead));
	toparg(nameNil);
    }
    else {
	push(ap(nameEnFrTh,whnfHead));
	toparg(n);
    }
    updapRoot(ap(nameCons,f),top());
}



/* --------------------------------------------------------------------------
 * Integer arithmetic primitives:
 * ------------------------------------------------------------------------*/

CAFInt(primMinInt,MINNEGINT)           /* minimum integer CAF              */
CAFInt(primMaxInt,MAXPOSINT)           /* maximum integer CAF              */
IntInt2Int(primPlusInt,x+y)            /* Integer addition primitive       */
IntInt2Int(primMinusInt,x-y)           /* Integer subtraction primitive    */
IntInt2Int(primMulInt,x*y)             /* Integer multiplication primitive */
Int2Int(primNegInt,-x)                 /* Integer negation primitive       */
IntInt2IntNonZero(primQuotInt,x/y)     /* Integer division primitive       */
				       /* truncated towards zero           */
IntInt2IntNonZero(primRemInt,x%y)      /* Integer remainder primitive      */

/* quot and rem satisfy:                                                   */
/*     (x `quot` y)*y + (x `rem` y) == x                                   */
/* which is exactly the property described in K&R 2:                       */
/*     (a/b)*b + a%b == a                                                  */

primFun(primQrmInt) {                  /* Integer quotient and remainder   */
    Int x, y;                          /* truncated towards zero           */
    IntArg(x,2);
    IntArg(y,1);
    if (y==0)
	throwException(ap(nameArithException, nameDivideByZero));
    IntIntResult(x/y,x%y);
}

primFun(primDivInt) {                  /* Integer division primitive       */
    Int x,y,r;                         /* truncated towards -ve infinity   */
    IntArg(x,2);
    IntArg(y,1);
    if (y==0)
	throwException(ap(nameArithException, nameDivideByZero));
    r = x%y;
    x = x/y;
    if ((y<0 && r>0) || (y>0 && r<0))
	x--;
    IntResult(x);
}

primFun(primModInt) {                  /* Integer modulo primitive         */
    Int x,y,r;
    IntArg(x,2);
    IntArg(y,1);
    if (y==0)
	throwException(ap(nameArithException, nameDivideByZero));
    r = x%y;                           /* "... the modulo having the sign  */
    if ((r<0 && y>0) ||                /*              of the divisor ..." */
	(r>0 && y<0)) {                /* See definition on p.91 of Haskell*/
	IntResult(r+y);                /* report... (Haskell 1.1?)         */
    } else {
	IntResult(r);
    }
}

IntInt2Int(primAndInt,x&y)  
IntInt2Int(primOrInt, x|y)  
IntInt2Int(primXorInt,(x&~y) | (~x&y))   
Int2Int(primComplementInt,~x)
Int2Int(primBitInt, 1<<x)
IntInt2Bool(primTestInt,(x >> y) & 1)

primFun(primShiftInt) {
    Int x,y;
    IntArg(x,2);
    IntArg(y,1);
    if (y >= 0) {
	IntResult(x << y);
    } else {
	IntResult(x >> (-y));
    }
}

/* --------------------------------------------------------------------------
 * Unsigned arithmetic primitives:
 * ------------------------------------------------------------------------*/

#if WORD_OPS
CAFWord(primMaxWord,MAXHUGSWORD)       /* maximum integer CAF              */
WordWord2Word(primPlusWord,x+y)        /* Word addition primitive          */
WordWord2Word(primMinusWord,x-y)       /* Word subtraction primitive       */
Word2Word(primNegateWord,-(Int)x)      /* Word negation (modulo MAXWORD)   */
WordWord2Word(primMulWord,x*y)         /* Word multiplication primitive    */
WordWord2WordNonZero(primQuotWord,x/y) /* Word division primitive          */
				       /* truncated towards zero           */
WordWord2WordNonZero(primDivWord,x/y)  /* Word division primitive          */
				       /* truncated towards zero           */
WordWord2WordNonZero(primRemWord,x%y)  /* Word remainder primitive         */
WordWord2WordNonZero(primModWord,x%y)  /* Word modulo primitive            */

/* quot and rem satisfy:                                                   */
/*     (x `quot` y)*y + (x `rem` y) == x                                   */
/* which is exactly the property described in K&R 2:                       */
/*     (a/b)*b + a%b == a                                                  */

primFun(primQrmWord) {                 /* Integer quotient and remainder   */
    Unsigned x, y;                     /* truncated towards zero           */
    WordArg(x,2);
    WordArg(y,1);
    if (y==0)
	throwException(ap(nameArithException, nameDivideByZero));
    WordWordResult(x/y,x%y);
}

WordWord2Word(primAndWord,x&y)  
WordWord2Word(primOrWord, x|y)  
WordWord2Word(primXorWord,(x&~y) | (~x&y))   
Word2Word(primComplementWord,~x)
Int2Word(primBitWord, 1<<x)
WordInt2Bool(primTestWord,(x >> y) & 1)

primFun(primShiftWord) {
    Unsigned x;         
    Int      y;
    WordArg(x,2);
    IntArg(y,1);
    if (y >= 0) {
	/* << isn't defined for y larger than word size */
	WordResult(y >= (Int)(sizeof(x) * 8)  ? 0 : x << y);
    } else {
        y = -y;
	WordResult(y >= (Int)(sizeof(x) * 8) ? 0 : x >> y);
    }
}

primFun(primRotateWord) {
    Unsigned x;         
    Int      y, z;
    WordArg(x,2);
    IntArg(y,1);
    IntArg(z,3);
    y = y % z;
    if (y >= 0) {
	WordResult((x << y) | (x >> (z - y)));
    } else {
	WordResult((x >> (-y)) | (x << (z + y)));
    }
}

Int2Int(primItoI8,  x&0xff)
Int2Int(primItoI16, x&0xffff)
Int2Int(primItoI32, x&0xffffffff)

Int2Int(primI8toI,  (Int8)x)        /* casts used to cause sign extension */
Int2Int(primI16toI, (Int16)x)       /* casts used to cause sign extension */
Int2Int(primI32toI, x)

Word2Word(primWtoW8,  x&0xff)
Word2Word(primWtoW16, x&0xffff)
Word2Word(primWtoW32, x&0xffffffff)

Word2Word(primW8toW,  x)
Word2Word(primW16toW, x)
Word2Word(primW32toW, x)

primFun(primI64toI32) {
    Cell x, y;
    eval(primArg(1)); 
    x = fst(snd(whnfHead));
    y = snd(snd(whnfHead));
    updapRoot(ap(mkTuple(2),x),y);
}

primFun(primI32toI64) {
    Int x, y;
    IntArg(x,2);
    IntArg(y,1);
    updateRoot(pair(I64CELL,pair(mkInt(x),mkInt(y))));
}

primFun(primW64toW32) {
    Cell x, y;
    eval(primArg(1)); 
    x = fst(snd(whnfHead));
    y = snd(snd(whnfHead));
    updapRoot(ap(mkTuple(2),x),y);
}

primFun(primW32toW64) {
    Unsigned x, y;
    WordArg(x,2);
    WordArg(y,1);
    updateRoot(pair(I64CELL,pair(mkInt(x),mkInt(y))));
}

#endif /* WORD_OPS */

/* --------------------------------------------------------------------------
 * Haskell Integer (bignum) primitives:
 * ------------------------------------------------------------------------*/

#if BIGNUMS
#include "bignums.c"
#endif

/* --------------------------------------------------------------------------
 * Coercion primitives:
 * ------------------------------------------------------------------------*/

Char2Int(primCharToInt,x)              /* Character to integer primitive   */

primFun(primIntToChar) {               /* Integer to character primitive   */
    Int i;
    IntArg(i,1);
    if (i<0  || i>MAXCHARVAL)
	throwException(ap(nameErrorCall, mkStr(findText("chr: out of range"))));
    CharResult(i);
}

primFun(primWordToInt) {               /* Word to integer primitive        */
    Unsigned x;
    WordArg(x,1);
    IntResult(x);
}

primFun(primIntToWord) {               /* Integer to word primitive        */
    Int i;
    IntArg(i,1);
    WordResult(i);
}

primFun(primIntToFloat) {              /* Integer to Float primitive       */
    Int i;
    IntArg(i,1);
    FloatResult((Float)i);
}

primFun(primIntToDouble) {             /* Integer to Double primitive      */
    Int i;
    IntArg(i,1);
    DoubleResult((Double)i);
}

primFun(primDummyCvt) {                /* dummy (identity) conversion      */
    updateRoot(primArg(1));
}

primFun(primRationalToFloat) {
#if SHORT_CIRCUIT_COERCIONS
    /* Optimisation: we try to short-circuit trivial conversions */
    Cell x = followInd(primArg(1));
    if (isAp(x)) {
	Cell f = followInd(fun(x));
	Cell a = arg(x);
	if (f == nameFloatToRational) {
	    updateRoot(a);
	    return;
	} else if (f == nameDoubleToRational) {
	    updapRoot(nameDoubleToFloat,a);
	    return;
	} else if (isAp(f)) {
	    Cell g = followInd(fun(f));
	    if (g == nameDoubleToRatio) {
		/* ignore the dict - it must be right */
		updapRoot(nameDoubleToFloat,a);
		return;
	    } else if (g == nameIntToRatio) {
		updapRoot(nameIntToFloat,a);
		return;
	    }
	}
    }
#endif
    updapRoot(nameRationalToFloat,primArg(1));
}

primFun(primRationalToDouble) {
#if SHORT_CIRCUIT_COERCIONS
    /* Optimisation: we try to short-circuit trivial conversions */
    Cell x = followInd(primArg(1));
    if (isAp(x)) {
	Cell f = followInd(fun(x));
	Cell a = arg(x);
	if (f == nameFloatToRational) {
	    updapRoot(nameFloatToDouble,a);
	    return;
	} else if (f == nameDoubleToRational) {
	    updateRoot(a);
	    return;
	} else if (isAp(f)) {
	    Cell g = followInd(fun(f));
	    if (g == nameDoubleToRatio) {
		updateRoot(a); /* ignore the dict - it must be right */
		return;
	    } else if (g == nameIntToRatio) {
		updapRoot(nameIntToDouble,a);
		return;
	    }
	}
    }
#endif
    updapRoot(nameRationalToDouble,primArg(1));
}

/* --------------------------------------------------------------------------
 * Float arithmetic primitives:
 * ------------------------------------------------------------------------*/

primFun(primFloatToDouble) {
    Float f;
    FloatArg(f,1);
    DoubleResult((Double)f);
} 

primFun(primDoubleToFloat) {
    Double f;
    DoubleArg(f,1);
    FloatResult((Float)f);
} 

FloatFloat2Float(primPlusFloat,x+y)    /* Float addition primitive         */
FloatFloat2Float(primMinusFloat,x-y)   /* Float subtraction primitive      */
FloatFloat2Float(primMulFloat,x*y)     /* Float multiplication primitive   */
Float2Float(primNegFloat,-x)           /* Float negation primitive         */
FloatFloat2Float(primDivFloat,x/y)     /* Float division primitive       */

DoubleDouble2Double(primPlusDouble,x+y) /* Double addition primitive        */
DoubleDouble2Double(primMinusDouble,x-y)/* Double subtraction primitive    */
DoubleDouble2Double(primMulDouble,x*y)  /* Double multiplication primitive  */
Double2Double(primNegDouble,-x)         /* Double negation primitive        */
DoubleDouble2Double(primDivDouble,x/y)  /* Double division primitive  */

#if FLOATS_SUPPORTED
Float2Float(primSinFloat,sin(x))       /* Float sin (trig) primitive       */
Float2Float(primCosFloat,cos(x))       /* Float cos (trig) primitive       */
Float2Float(primTanFloat,tan(x))       /* Float tan (trig) primitive       */
Float2Float(primAsinFloat,asin(x))     /* Float arc sin (trig) primitive   */
Float2Float(primAcosFloat,acos(x))     /* Float arc cos (trig) primitive   */
Float2Float(primAtanFloat,atan(x))     /* Float arc tan (trig) primitive   */
#if 0 /* not used in current version of Prelude */
FloatFloat2Float(primAtan2Float,atan2(x,y)) /* Float arc tan with quadrant info*/
#endif
				       /*               (trig) primitive   */
Float2Float(primExpFloat,exp(x))       /* Float exponential primitive      */
Float2FloatPre(primLogFloat,log(x),x>0)/* Float logarithm primitive        */
Float2FloatPre(primSqrtFloat,sqrt(x),x>=0) /* Float square root primitive  */

Double2Double(primSinDouble,sin(x))    /* Double sin (trig) primitive      */
Double2Double(primCosDouble,cos(x))    /* Double cos (trig) primitive      */
Double2Double(primTanDouble,tan(x))    /* Double tan (trig) primitive      */
Double2Double(primAsinDouble,asin(x))  /* Double arc sin (trig) primitive  */
Double2Double(primAcosDouble,acos(x))  /* Double arc cos (trig) primitive  */
Double2Double(primAtanDouble,atan(x))  /* Double arc tan (trig) primitive  */
Double2Double(primExpDouble,exp(x))    /* Double exponential primitive     */
Double2DoublePre(primLogDouble,log(x),x>0)/* Double logarithm primitive    */
Double2DoublePre(primSqrtDouble,sqrt(x),x>=0) /* Double square root primitive */

#if 0 /* This was in Hugs 1.01 - not needed by prelude */
Float2FloatPre(primLog10Float,log10(x),x>0) /* Float logarithm (base 10) prim*/
#endif
/* Not used in Hugs prelude, rounds towards zero */
Float2Int(primFloatToInt,(Int) x)      /* Adhoc Float --> Int conversion   */
Double2Int(primDoubleToInt,(Int) x)    /* Adhoc Double --> Int conversion  */

#if BIGNUMS
CAFBignum(primFloatRadix,bigInt(HUGS_FLT_RADIX)) /* Float radix primitive  */
#else                                    
CAFInt(primFloatRadix,HUGS_FLT_RADIX)  /* from K&R2, I hope it's portable  */
#endif

CAFInt(primFloatDigits,HUGS_FLT_MANT_DIG)/* Float sig. digits primitive    */
				       /* again, courtesy K&R2             */

CAFInt(primFloatMinExp,HUGS_FLT_MIN_EXP)/* Float min exponent primitive    */
CAFInt(primFloatMaxExp,HUGS_FLT_MAX_EXP)/* Float max exponent primitive    */

CAFInt(primDoubleDigits,HUGS_DBL_MANT_DIG)/* Double sig. digits primitive  */
				       /* again, courtesy K&R2             */

CAFInt(primDoubleMinExp,HUGS_DBL_MIN_EXP)/* Double min exponent primitive  */
CAFInt(primDoubleMaxExp,HUGS_DBL_MAX_EXP)/* Double max exponent primitive  */

/* ToDo: GHC stole its decode code from Lennart - maybe we should too?     */
primFun(primFloatDecode) {             /* Float decode primitive           */
    double f;                          /*  :: Float -> (Integer,Int)       */
    Int    n;                          /* another gruesome hack            */
    FloatArg(f,1);
    f  = frexp((double)(f),&n);        /* 0.5   <= f < 1                   */
    f  = ldexp(f,HUGS_FLT_MANT_DIG);   /* 2^m-1 <= f < 2^m, m=HUGS_FLT_MANT_DIG*/
    n -= HUGS_FLT_MANT_DIG;
#if BIGNUMS
    push(bigDouble(f));
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
#else
    push(mkInt((Int)f));
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
#endif
}

primFun(primFloatEncode) {             /* Float encode primitive           */
    Int n;                             /*  :: Integer -> Int -> Float      */
    Float f;                           /* Ugly hack, don't use Hugs for    */
    IntArg(n,1);                       /* numerical work                   */
    eval(primArg(2));                  /* get integer                      */
#if DJGPP2                                     
    _fpreset();                        /* Get round a possible DJGPP bug?  */
#endif                                         
#if BIGNUMS                                    
    f = (Float)bigToDouble(whnfHead);  /* and turn it into a float         */
#else                                          
    f = (Float) whnfInt;               /* and turn it into a float         */
#endif
    updateRoot(mkFloat(ldexp(f,n)));
}

/* ToDo: GHC stole its decode code from Lennart - maybe we should too?     */
primFun(primDoubleDecode) {            /* Double decode primitive           */
    double f;                          /*  :: Double -> (Integer,Int)       */
    Int    n;                          /* another gruesome hack            */
    DoubleArg(f,1);
    f  = frexp((double)(f),&n);        /* 0.5   <= f < 1                   */
    f  = ldexp(f,HUGS_DBL_MANT_DIG);   /* 2^m-1 <= f < 2^m, m=HUGS_DBL_MANT_DIG*/
    n -= HUGS_DBL_MANT_DIG;
#if BIGNUMS
    push(bigDouble(f));
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
#else
    push(mkInt((Int)f));
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
#endif
}

primFun(primDoubleEncode) {            /* Double encode primitive          */
    Int n;                             /*  :: Integer -> Int -> Double     */
    Double f;                          /* Ugly hack, don't use Hugs for    */
    IntArg(n,1);                       /* numerical work                   */
    eval(primArg(2));                  /* get integer                      */
#if DJGPP2                                     
    _fpreset();                        /* Get round a possible DJGPP bug?  */
#endif                                         
#if BIGNUMS                                    
    f = (Double)bigToDouble(whnfHead); /* and turn it into a double        */
#else                                          
    f = (Double)whnfInt;               /* and turn it into a double        */
#endif
    DoubleResult(ldexp(f,n));
}

#endif /* FLOATS_SUPPORTED */

/* --------------------------------------------------------------------------
 * Ptr primitives:
 * ------------------------------------------------------------------------*/

CAFPtr(primNullPtr,0)                  /* Null pointer                     */
PtrInt2Ptr(primPlusPtr,(char*)x+y)     /* Pointer arithmetic               */
PtrInt2Ptr(primAlignPtr,(char*)x+(int)((y - (long)x%y)%y))
				       /* Aligning the pointer             */
PtrPtr2Int(primMinusPtr,(char*)x-(char*)y) /* Pointer arithmetic           */
PtrPtr2Bool(primEqPtr,x==y)            /* Pointer equality primitive       */

primFun(primCmpPtr) {                  /* Pointer compare primitive        */
    Pointer x, y;
    PtrArg(x,2);
    PtrArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

/* --------------------------------------------------------------------------
 * Comparison primitives:
 * ------------------------------------------------------------------------*/

IntInt2Bool(primEqInt,x==y)            /* Integer equality primitive       */
WordWord2Bool(primEqWord,x==y)         /* Natural equality primitive       */
CharChar2Bool(primEqChar,x==y)         /* Character equality primitive     */
FloatFloat2Bool(primEqFloat, x==y)     /* Float equality primitive         */
DoubleDouble2Bool(primEqDouble, x==y)  /* Double equality primitive        */

primFun(primCmpInt) {                  /* Integer compare primitive        */
    Int x, y;
    IntArg(x,2);
    IntArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpWord) {                 /* Natural compare primitive        */
    Unsigned x, y;
    WordArg(x,2);
    WordArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpChar) {                 /* Character compare primitive      */
    Char x, y;
    CharArg(x,2);
    CharArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpFloat) {                /* Float compare primitive          */
    Float x, y;
    FloatArg(x,2);
    FloatArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpDouble) {               /* Double compare primitive         */
    Double x, y;
    DoubleArg(x,2);
    DoubleArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

/* --------------------------------------------------------------------------
 * Print primitives:
 * ------------------------------------------------------------------------*/

#include "printer.c"

/* --------------------------------------------------------------------------
 * Evaluate name, obtaining a C string from a Hugs string:
 * ------------------------------------------------------------------------*/

#if FILENAME_MAX < 1024
# define MAX_STRING 1024
#else
# define MAX_STRING FILENAME_MAX
#endif

String evalName(es)                     /* evaluate es :: [Char] and save  */
Cell es; {                              /* in char array... return ptr to  */
    static char buffer[MAX_STRING+1];   /* string or 0, if error occurs    */
    char	*bp = buffer;
    StackPtr    saveSp = sp;

    eval(es);
    while (whnfHead==nameCons && bp<=buffer+MAX_STRING-MAX_CHAR_ENCODING) {
	eval(pop());
	AddChar(charOf(whnfHead), bp);
	eval(pop());
    }
    if (whnfHead==nameNil) {
	*bp = '\0';
	return buffer;
    }
    sp = saveSp;                        /* stack pointer must be the same  */
    return 0;                           /* as it was on entry              */
}

/* --------------------------------------------------------------------------
 * Top-level printing mechanism:
 * ------------------------------------------------------------------------*/

Void outputString(fp)                   /* Evaluate string on top of stack */
FILE *fp; {                             /* and print it on fp              */
    StackPtr origSp = sp;
    for (;;) {
	Cell temp = evalWithNoError(pop());
	if (nonNull(temp)) {
	    sp = origSp;
	    top()  = printException((top()=temp),nameNil);
	}
	else if (whnfHead==nameCons) {
	    if (nonNull(temp=evalWithNoError(pop()))) {
		sp = origSp;
		onto(temp);
		pushed(1) = printException(pushed(0),pushed(1));
		drop();
	    }
	    else {
		FPutChar(charOf(whnfHead),fp);
		fflush(fp);
	    }
	}
	else
	    return;
    }
}

/* --------------------------------------------------------------------------
 * IO monad implementation
 * ------------------------------------------------------------------------*/

#if IO_MONAD
#include "iomonad.c"
#endif

/* --------------------------------------------------------------------------
 * Time and CPUTime module implementations
 * ------------------------------------------------------------------------*/

#if TIME_MODULE
#include "timeprim.c"
#endif

/* --------------------------------------------------------------------------
 * Directory module implementation
 * ------------------------------------------------------------------------*/

#if DIRECTORY_MODULE
#include "dirprim.c"
#endif

/* --------------------------------------------------------------------------
 * Error catching primitives
 * (not standard Haskell but jolly useful)
 * ------------------------------------------------------------------------*/

#if INTERNAL_PRIMS
#include "interns.c"
#endif

/* --------------------------------------------------------------------------
 * Array primitives:
 * ------------------------------------------------------------------------*/

#if HASKELL_ARRAYS
#include "array.c"
#endif

/* --------------------------------------------------------------------------
 * Char primitives:
 * ------------------------------------------------------------------------*/

CAFChar(primMaxChar,MAXCHARVAL)

Char2Bool(primIsUpper,isUpper(x))
Char2Bool(primIsLower,isLower(x))
Char2Bool(primIsAlpha,isAlpha(x))
Char2Bool(primIsAlphaNum,isAlphaNum(x))
Char2Bool(primIsPrint,isPrint(x))

Char2Char(primToLower,toLower(x))
Char2Char(primToUpper,toUpper(x))
Char2Char(primToTitle,toTitle(x))

Char2Int(primUniGenCat,uni_gencat(x))

/* --------------------------------------------------------------------------
 * Extensible records: (Gaster and Jones, 1996)
 * ------------------------------------------------------------------------*/

#if TREX
primFun(primRecExt) {                   /* :: Int -> a -> Rec ? -> Rec ?   */
    Int  n;
    Cell b = NIL;
    Cell r;
    eval(primArg(3));
    n = whnfInt;
    eval(primArg(1));
    for (r=arg(whnfHead); n>0; n--) {
	b = cons(fun(r),b);
	r = arg(r);
    }
    b = cons(primArg(2),b);
    updapRoot(RECORD,revOnto(b,r));
}

primFun(primRecBrk) {                   /* :: Int -> Rec ? -> (?, Rec ?)   */
    Int  n;
    Cell b;
    Cell r;
    eval(primArg(2));
    n = whnfInt;
    eval(primArg(1));
    b = cons(RECORD,NIL);
    for (r=arg(whnfHead); n>0; n--) {
	b = cons(fun(r),b);
	r = arg(r);
    }
    pushed(1) = revOnto(b,arg(r));
    pushed(0) = ap(mkTuple(2),fun(r));
    updapRoot(pushed(0),pushed(1));
}

primFun(primRecSel) {                   /* :: Int -> Rec ? -> ?            */
    Int  n;
    Cell r;
    eval(primArg(2));
    n = whnfInt;
    eval(primArg(1));
    for (r=arg(whnfHead); n>0; n--)
	r = arg(r);
    updateRoot(fun(r));
}

/* recShw :: primArg(5) Label l       ->
 *           primArg(4) ShowD a       ->
 *           primArg(3) Lacks_l r     ->
 *           primArg(2) ShowRecRowD r ->
 *           primArg(1) Rec (l::a|r)  -> [(String,ShowS)]
 * recShw l d e f r
 *    = case recBrk e r of
 *        (v,s) -> insertField l (showsPrec d 0 v) (showRecRow f s)
 */

primFun(primRecShw) {
    push(nameRecBrk);
    toparg(primArg(3));
    toparg(primArg(1));
    eval(pop());
    primArg(2) = ap(nameShowRecRow,primArg(2));
    primArg(4) = ap(nameShowsPrec,primArg(4));
    primArg(4) = ap(primArg(4),mkInt(0));
    primArg(5) = ap(nameInsFld,primArg(5));
    pushed(1)  = ap(primArg(2),pushed(1));
    pushed(0)  = ap(primArg(4),pushed(0));
    pushed(0)  = ap(primArg(5),pushed(0));
    updapRoot(pushed(0),pushed(1));
}

/* recEq :: primArg(6) Label l       ->
 *	    primArg(5) EqD a         ->
 *	    primArg(4) Lacks_x r     ->
 *	    primArg(3) EqRecRowD r   ->
 *	    primArg(2) Rec (l::a|r)  ->
 *	    primArg(1) Rec (l::a|r)  -> [(String,Bool)]
 * reqEq l eqa e eqr r1 r2
 *    = case recBrk e r1 of
 *	  (v,s1) -> case recBrk e r2 of
 *		      (w,s2) -> insertField l ((==) eqa v w)
 *						      (eqRecRow eqr s1 s2)
 */

primFun(primRecEq) {
    push(nameRecBrk);
    toparg(primArg(4));
    toparg(primArg(2));
    eval(pop());
    push(nameRecBrk);
    toparg(primArg(4));
    toparg(primArg(1));
    eval(pop());
    primArg(3) = ap(nameEqRecRow,primArg(3));
    primArg(3) = ap(primArg(3),pushed(3));
    primArg(3) = ap(primArg(3),pushed(1));
    primArg(5) = ap(nameEq,primArg(5));
    primArg(5) = ap(primArg(5),pushed(2));
    primArg(5) = ap(primArg(5),pushed(0));
    primArg(6) = ap(nameInsFld,primArg(6));
    primArg(6) = ap(primArg(6),primArg(5));
    updapRoot(primArg(6),primArg(3));
}
#endif

/* --------------------------------------------------------------------------
 * Auxilliary functions
 * ------------------------------------------------------------------------*/

static Cell local followInd(c)    /* follow chain of indirections and CAFs */
Cell c; {
    do {
	switch (whatIs(c)) {
	case INDIRECT : c = snd(c);
		break;
#if OBSERVATIONS
        case OBSERVE  : c = markedExpr(c);
                break;
#endif
	case NAME     : if (isCfun(c)
		|| name(c).arity != 0 
		|| isNull(name(c).defn)) {
		return c;
		}
		c = name(c).defn;
		break;
	default       : return c;
    }
    allowBreak();
    } while (1);
}
		   
/* --------------------------------------------------------------------------
 * Pointer equality
 * ------------------------------------------------------------------------*/

/* Pointer equality tests break referential transparency.
 * However, they can be useful in implementing referentially transparent
 * functions such as lazy memo-tables.
 *
 *   foo = cache sin
 *
 *   cache :: (a -> b) -> (a -> b)
 *   cache f = \x -> unsafePerformIO (check x)
 *    where
 *     ref = unsafePerformIO (newRef (error "cache", error "cache"))
 *     check x = derefRef ref >>= \ (x',a) ->
 *               if x `unsafePtrEq` x' then
 *                 return a
 *               else
 *                 let a = f x in
 *                 assignRef ref (x, a) >>
 *                 return a
 */

primFun(primUnsafePtrEq) {		 /* Unsafe pointer equality test     */
    Cell x = followInd(primArg(2));
    Cell y = followInd(primArg(1));
    updateRoot( (x==y) ? nameTrue : nameFalse );
}

/* Companion function for use when debugging uses of unsafePtrEq.
 * Converts a heap pointer to an Int so you can look at it.
 * I don't think there's any way of using this function that 
 * doesn't break the semantics - debugging use only.
 */
primFun(primUnsafePtrToInt) {
    updateRoot(mkInt(followInd(primArg(1))));
}

/*---------------------------------------------------------------------------
 * GreenCard entry points
 *
 * GreenCard generated code accesses Hugs data structures and functions 
 * (only) via these functions (which are stored in the virtual function
 * table hugsAPI4).
 *-------------------------------------------------------------------------*/

static void           getUnit        Args((void));
static HsInt          getInt         Args((void));
static HsWord         getWord        Args((void));
static HsAddr         getAddr        Args((void));
static char           getChar4       Args((void));
static HsChar         getChar        Args((void));
static HugsForeign    getForeign     Args((void));
static HsBool         getBool        Args((void));
static HsInt8         getInt8        Args((void));
static HsInt16        getInt16       Args((void));
static HsInt32        getInt32       Args((void));
static HsInt64        getInt64       Args((void));
static HsWord8        getWord8       Args((void));
static HsWord16       getWord16      Args((void));
static HsWord32       getWord32      Args((void));
static HsWord64       getWord64      Args((void));
static HsPtr          getPtr         Args((void));
static HsFunPtr       getFunPtr      Args((void));
static HsForeignPtr   getForeignPtr  Args((void));	      
static HsStablePtr    getStablePtr4  Args((void));
static HsFloat        getFloat       Args((void));
static HsDouble       getDouble      Args((void));

static HugsStablePtr  getStablePtr   Args((void));

static void           putInt         Args((HsInt));
static void           putWord        Args((HsWord));
static void           putAddr        Args((HsAddr));
static void           putChar4       Args((char));
static void           putChar        Args((HsChar));
static void           putForeign     Args((HugsForeign, void (*)(void *)));
static void           putStablePtr4  Args((HsStablePtr));
static void           putBool        Args((HsBool));
static void           putInt8        Args((HsInt8));
static void           putInt16       Args((HsInt16));
static void           putInt32       Args((HsInt32));
static void           putInt64       Args((HsInt64));
static void           putWord8       Args((HsWord8));
static void           putWord16      Args((HsWord16));
static void           putWord32      Args((HsWord32));
static void           putWord64      Args((HsWord64));
static void           putPtr         Args((HsPtr));
static void           putFunPtr      Args((HsFunPtr));
static void           putForeignPtr  Args((HsForeignPtr));
static void           putFloat       Args((HsFloat));
static void           putDouble      Args((HsDouble));

static void           freeStablePtr4 Args((HsStablePtr));

	      
static void           returnIO       Args((HugsStackPtr, int));
static void           returnId       Args((HugsStackPtr, int));
static int            runIO          Args((int));
static void           apMany         Args((int));

static void           getUnit()      { eval(pop()); }
static HsInt          getInt()       { eval(pop()); checkInt();   return whnfInt; }
static HsWord         getWord()      { eval(pop()); checkWord();  return (unsigned int) whnfInt; }
static HsAddr         getAddr()      { eval(pop()); checkPtr();   return ptrOf(whnfHead); }
static char           getChar4()     { eval(pop()); checkChar();  return charOf(whnfHead); }
static HsChar         getChar()      { eval(pop()); checkChar();  return charOf(whnfHead); }
static HugsForeign    getForeign()   { eval(pop()); return derefMP(whnfHead); }
static HsBool         getBool()      { eval(pop()); checkBool();  return (whnfHead == nameTrue); }
static HsInt8         getInt8()      { eval(pop()); checkInt();   return whnfInt; } 
static HsInt16        getInt16()     { eval(pop()); checkInt();   return whnfInt; }
static HsInt32        getInt32()     { eval(pop()); checkInt();   return whnfInt; }
static HsInt64        getInt64()     { eval(pop()); return int64FromParts(intOf(fst(snd(whnfHead))), intOf(snd(snd(whnfHead)))); }
static HsWord8        getWord8()     { eval(pop()); checkWord();  return (unsigned int) whnfInt; } 
static HsWord16       getWord16()    { eval(pop()); checkWord();  return (unsigned int) whnfInt; } 
static HsWord32       getWord32()    { eval(pop()); checkWord();  return (unsigned int) whnfInt; } 
static HsWord64       getWord64()    { eval(pop()); return int64FromParts(intOf(fst(snd(whnfHead))), intOf(snd(snd(whnfHead)))); }
static HsPtr          getPtr()       { eval(pop()); checkPtr();   return ptrOf(whnfHead); }
static HsFunPtr       getFunPtr()    { eval(pop()); checkPtr();   return (HsFunPtr)ptrOf(whnfHead); }

static HsForeignPtr   getForeignPtr() {
    ERRMSG(0) "getForeignPtr: not implemented in Hugs"
    EEND;
    return 0;
}

static HugsStablePtr  getStablePtr() { 
    Cell c = mkStablePtr(pop());
    if (isNull(c)) {
	ERRMSG(0) "Stable pointer table full"
	EEND;
    }
    return c;
}

static HugsStablePtr  lookupName Args((String, String));
static HugsStablePtr  lookupName(q,n)
String q;
String n; { 
    Name nm = findQualFun(findText(q), findText(n));
    Cell c;
    
    if (isNull(nm)) {
	ERRMSG(0) "Can't find qualified name '%s.%s'", q, n
	EEND;
    }
    c = mkStablePtr(nm);
    if (isNull(c)) {
	ERRMSG(0) "Stable pointer table full"
	EEND;
    }
    return c;
}

static void putInt (HsInt  x) { push(mkInt(x)); }
static void putWord(HsWord x) { push(mkInt((int)x)); }
static void putAddr(HsAddr x) { push(mkPtr(x)); }
static void putChar4(char x) { push(mkChar(x)); }
static void putChar(HsChar x) { push(mkChar(x)); }
static void putForeign(HugsForeign x, void (*f)(HugsForeign)) { push(mkMallocPtr(x,f)); }
static void putStablePtr   (HugsStablePtr x) { push(derefStablePtr(x)); }
static void putBool        (HsBool x)        { push(x?nameTrue:nameFalse); }
                                            
static void putInt8 (HsInt8  x) { push(mkInt(x)); }
static void putInt16(HsInt16 x) { push(mkInt(x)); }
static void putInt32(HsInt32 x) { push(mkInt(x)); }
static void putInt64(HsInt64 x) { push(pair(I64CELL,pair(mkInt(part1Int64(x)),mkInt(part2Int64(x))))); }
static void putWord8 (HsWord8  x) { push(mkInt((int)x)); }
static void putWord16(HsWord16 x) { push(mkInt((int)x)); }
static void putWord32(HsWord32 x) { push(mkInt((int)x)); }
static void putWord64(HsWord64 x) { push(pair(I64CELL,pair(mkInt(part1Int64(x)),mkInt(part2Int64(x))))); }
static void putPtr   (HsPtr    x) { push(mkPtr(x)); }
static void putFunPtr(HsFunPtr x) { push(mkPtr((Pointer)x)); }

static void putStablePtr4(HsStablePtr   x) {
    push((HugsStablePtr)x);
}

static HsStablePtr getStablePtr4(void) { 
    HugsStablePtr x = pop();
    return (HsStablePtr)x;
}

static Void freeStablePtr4(HsStablePtr x) {
    if (x) freeStablePtr((HugsStablePtr)x);
}

static HsFloat        getFloat()     { eval(pop()); checkFloat(); return whnfFloat; }
static HsDouble       getDouble()    { eval(pop()); checkDouble(); return whnfDouble; }

static void putFloat(HsFloat x) {
  push(mkFloat(x));
}

static void putDouble(HsDouble x) {
  push(mkDouble(x));
}

static void putForeignPtr(HsForeignPtr x) {
    ERRMSG(0) "putForeignPtr: not implemented in Hugs"
    EEND;
}

static void returnIO(root,n) /* return in IO monad */
HugsStackPtr root;
int          n; {
    /* There should be n return values on the top of the stack */
    if (n == 0) {
	push(nameUnit);
    } else if (n == 1) {
       /* do nothing */
    } else {
    int i;
    push(mkTuple(n));
    for(i=0; i<n; ++i) {
	pushed(1) = ap(pushed(0),pushed(1));
	drop();
    }
    }
    IOReturn(top());
}

static void returnId(root,n) /* return in identity monad */
HugsStackPtr root;
int          n; {
    /* There should be n return values on the top of the stack */
    if (n == 0) {
	push(nameUnit);
    } else if (n == 1) {
        /* do nothing, already there. */
    } else {
	int i;
	push(mkTuple(n));
	for(i=0; i<n; ++i) {
	    pushed(1) = ap(pushed(0),pushed(1));
	    drop();
	}
    }
    /*
     * Note: have to be a bit careful when returning, since we could
     * be returning from evaluating a CAF. In the non-CAF case,
     * 'root' points to the object we've entered, so we can just
     * go ahead and update it. No problem there.
     *
     * For CAFs, the evaluator leaves 'root' pointing at the Name
     * of the CAF, so updating it directly is not going to work.
     * Instead, we just leave the result at the top of the stack & 
     * let the evaluator do what it has always done; clean up & 
     * update the 'defn' field inside the CAF Name.
     * 
     *  [6/01 --sof]
     */
    if ( isPair(stack(root))
#if 0 || DEBUG   
	 || (whatIs(stack(root)) == AP)
#endif
	 ) {
      updateRoot(top());
    }
}

static int runIO(n)
int n; {
    /* stack = argn : ... : arg1 : fun : rest */
    StackPtr old_sp   = sp - n - 1;
    Cell     temp     = NIL;
    Int i;
    /* build application node */
    for(i=n-1; i >= 0; --i) {
	pushed(n) = ap(pushed(n), pushed(i));
    }
    sp -= n;

    /* evaluate it - should have type IO a */
    temp = evalWithNoError(ap(nameIORun,pop()));
    if (nonNull(temp)) {
	ERRMSG(0) "runIO: uncaught error"
	EEND;
    }
    if (sp != old_sp+1) {
	ERRMSG(0) "runIO: unbalanced stack (%d)", sp-old_sp
	EEND;
    }
    if (whnfHead == nameRight) {
	return 0;
    } else if (whnfHead != nameLeft) { /* Called "exit" */
	ERRMSG(0) "runIO: bad return value"
	EEND;
    }
    return 1;
}    

static void apMany(n)
int n; {
    /* stack = argn : ... : arg1 : fun : rest */
    Int i;
    /* build application node */
    for(i=n-1; i >= 0; --i) {
	pushed(n) = ap(pushed(n), pushed(i));
    }
    sp -= n;
    /* stack = ap(...(ap(fun,arg1),...),argn) : rest */
}

static int runId Args((int));

static int runId(n)
int n; {
    apMany(n);
    top() = ap(nameReturnIO,top());
    return runIO(0);
}

/* This allocates a small object and writes some machine code into it.  
 *
 * The code generated by the generated code is equivalent to:
 *  
 *   rty f(ty1 a1, ... tym am) {
 *     return (*app)(s,a1, ... am);
 *   }
 *
 * Where s is a stable pointer (an int).
 *
 * But, because this is machine code, we can do it without knowing
 * anything about the argument types.  This works because the C
 * calling convention on most machines has the stack looking something
 * like this (where leftmost = top of stack)
 *
 *   ret_addr : a1 : ... am : rest of stack
 *
 * If this is not the case for some architecture/calling convention,
 * the easiest thing to do might be to make the stable pointer be
 * the last argument (if that is easily found) or to put it in a callee
 * saves register - and then adapt the apply function generated by 
 * implementForeignExport.  
 *
 * thunk->{next,prev}: a doubly linked list (makes deletion easy)
 * thunk->stable:      the stable pointer (makes deletion easy)
 * 
 * At the end of execution, we run down the list freeing every thunk
 * that the user did not explicitly deallocate.
 */

struct thunk_data {
    struct thunk_data* next;
    struct thunk_data* prev;
    HugsStablePtr      stable;
#if i386_HOST_ARCH
    char               code[17];
#elif powerpc_HOST_ARCH && defined(__GNUC__)
    char               code[13*sizeof(unsigned long)];
#elif sparc_HOST_ARCH && defined(__GNUC__)
    char               code[11*sizeof(unsigned long)];
#else
    /* This is a placeholder intended to avoid compile-time warnings.
     * A runtime warning will be generated by mkThunk if an attempt is
     * make to use foreign wrappers
     */
    char               code[1];
#endif
};

static void* mkThunk      Args((void (*)(void), HugsStablePtr));
static void freeThunkAux  Args((struct thunk_data*));
static void freeAllThunks Args((void));
static void initAdjustor  Args((void));

static struct thunk_data* foreignThunks = 0;

#if i386_HOST_ARCH
/* Comment from GHC's Adjustor.c:

   Now here's something obscure for you:

   When generating an adjustor thunk that uses the C calling
   convention, we have to make sure that the thunk kicks off
   the process of jumping into Haskell with a tail jump. Why?
   Because as a result of jumping in into Haskell we may end
   up freeing the very adjustor thunk we came from using
   freeHaskellFunctionPtr(). Hence, we better not return to
   the adjustor code on our way  out, since it could by then
   point to junk.

   The fix is readily at hand, just include the opcodes
   for the C stack fixup code that we need to perform when
   returning in some static piece of memory and arrange
   to return to it before tail jumping from the adjustor thunk.
*/
static unsigned char *obscure_ccall_ret_code;	/* set by initAdjustor() */
#endif /* i386_HOST_ARCH */

/* Heavily arch-specific, I'm afraid.. */

/*
 * Allocate len bytes which are readable, writable, and executable.
 *
 * ToDo: If this turns out to be a performance bottleneck, one could
 * e.g. cache the last VirtualProtect/mprotect-ed region and do
 * nothing in case of a cache hit.
 */
static void* local mallocBytesRWX(int len) {
    void *addr = (void *)malloc(len);
#if defined(i386_HOST_ARCH) && defined(_WIN32)
    /* This could be necessary for processors which distinguish between
       READ and EXECUTE memory accesses, e.g. Itaniums. */
    DWORD dwOldProtect = 0;
    if (VirtualProtect(addr, len, PAGE_EXECUTE_READWRITE, &dwOldProtect) == 0) {
        ERRMSG(0) "mallocBytesRWX: failed to protect 0x%p\n", addr
	EEND;
    }
#elif defined(openbsd_HOST_OS) || defined(linux_HOST_OS)
    /* malloced memory isn't executable by default on OpenBSD */
    uintptr_t pageSize         = sysconf(_SC_PAGESIZE);
    uintptr_t mask             = ~(pageSize - 1);
    uintptr_t startOfFirstPage = ((uintptr_t)addr          ) & mask;
    uintptr_t startOfLastPage  = ((uintptr_t)addr + len - 1) & mask;
    uintptr_t size             = startOfLastPage - startOfFirstPage + pageSize;
    if (mprotect((void*)startOfFirstPage, 
                        (size_t)size, PROT_EXEC | PROT_READ | PROT_WRITE) != 0) {
        ERRMSG(0) "mallocBytesRWX: failed to protect 0x%p\n", addr
	EEND;
    }
#endif
    return addr;
}

/* Perform initialisation of adjustor thunk layer (if needed). */
static void local initAdjustor() {
#if i386_HOST_ARCH
    obscure_ccall_ret_code = (unsigned char *)mallocBytesRWX(4);
    obscure_ccall_ret_code[0x00] = (unsigned char)0x83;  /* addl $0x4, %esp */
    obscure_ccall_ret_code[0x01] = (unsigned char)0xc4;
    obscure_ccall_ret_code[0x02] = (unsigned char)0x04;
    obscure_ccall_ret_code[0x03] = (unsigned char)0xc3;  /* ret */
#endif
}

static void* mkThunk(void (*app)(void), HugsStablePtr s) {
    /* The code part of the thunk_data needs to be marked as executable,
       but it's simple to do the whole struct. */
    struct thunk_data* thunk
        = (struct thunk_data*)mallocBytesRWX(sizeof(struct thunk_data));
    if (!thunk) {
        /* ToDo: better cleanup */
        printf("Can't allocate thunk for foreign import wrapper\n");
        exit(1);
    }
    if (foreignThunks) { /* non-empty list */
        foreignThunks->prev = thunk;
    }
    thunk->next = foreignThunks;
    thunk->prev = 0;
    foreignThunks = thunk;
    thunk->stable = s;
#if i386_HOST_ARCH
    /* Mostly cut-n-pasted from GHC's Adjustor.c. */
    {
	unsigned char *adj_code = (unsigned char*)thunk->code;

	adj_code[0] = (char)0x68;	/* pushl s */
	*((HugsStablePtr*)(adj_code + 1)) = s;

	adj_code[5] = (char)0xb8;	/* movl app, %eax */
	*((HsFunPtr*)(adj_code + 6)) = app;

	adj_code[10] = (char)0x68;	/* pushl obscure_ccall_ret_code */
	*((unsigned char**)(adj_code + 11)) = obscure_ccall_ret_code;

	adj_code[15] = (char)0xff;	/* jmp *%eax */
	adj_code[16] = (char)0xe0;
    }
#elif powerpc_HOST_ARCH && defined(__GNUC__)
     /* This is only for MacOS X.
      * It does not work on MacOS 9 because of the very strange
      * handling of function pointers in OS 9.
      * I don't know about LinuxPPC calling conventions.
      * Please note that it only works for up to 7 arguments.
      */
    {
	unsigned long *adj_code = (unsigned long*)thunk->code;

	/* make room for extra arguments */
	adj_code[0] = 0x7d2a4b78;	/* mr r10,r9 */
	adj_code[1] = 0x7d094378;	/* mr r9,r8  */
	adj_code[2] = 0x7ce83b78;	/* mr r8,r7  */
	adj_code[3] = 0x7cc73378;	/* mr r7,r6  */
	adj_code[4] = 0x7ca62b78;	/* mr r6,r5  */
	adj_code[5] = 0x7c852378;	/* mr r5,r4  */
	adj_code[6] = 0x7c641b78;	/* mr r4,r3  */

	adj_code[7] = 0x3c000000;	/* lis r0,hi(app) */
	adj_code[7] |= ((unsigned long)app) >> 16;

	adj_code[8] = 0x3c600000;	/* lis r3,hi(s) */
	adj_code[8] |= ((unsigned long)s) >> 16;

	adj_code[9] = 0x60000000;	/* ori r0,r0,lo(app) */
	adj_code[9] |= ((unsigned long)app) & 0xFFFF;

	adj_code[10] = 0x60630000;	/* ori r3,r3,lo(s) */
	adj_code[10] |= ((unsigned long)s) & 0xFFFF;

	adj_code[11] = 0x7c0903a6;	/* mtctr r0 */
	adj_code[12] = 0x4e800420;	/* bctr */

	/* Flush the Instruction cache: */
	/* MakeDataExecutable(adjustor,4*13); */
	/* This would require us to link with CoreServices.framework */
	{ /* this should do the same: */
	    int n = 13;
	    unsigned long *p = adj_code;
	    while(n--)
	    {
		__asm__ volatile ("dcbf 0,%0\n\tsync\n\ticbi 0,%0"
				: : "g" (p));
		p++;
	    }
	    __asm__ volatile ("sync\n\tisync");
	}
    }
#elif sparc_HOST_ARCH && defined(__GNUC__)
    /* Mostly cut-n-pasted from GHC's Adjustor.c:

	<00>: 9C23A008   sub   %sp, 8, %sp     ! make room for %o4/%o5 in caller's frame
	<04>: DA23A060   st    %o5, [%sp + 96] ! shift registers by 2 positions
	<08>: D823A05C   st    %o4, [%sp + 92]
	<0C>: 9A10000B   mov   %o3, %o5
	<10>: 9810000A   mov   %o2, %o4
	<14>: 96100009   mov   %o1, %o3
	<18>: 94100008   mov   %o0, %o2
	<1C>: 13000000   sethi %hi(app), %o1   ! load up app (1 of 2)
	<20>: 11000000   sethi %hi(s), %o0     ! load up s (1 of 2)
	<24>: 81C26000   jmp   %o1 + %lo(app)  ! jump to app (load 2 of 2)
	<28>: 90122000   or    %o0, %lo(), %o0 ! load up s (2 of 2, delay slot)

	ccall'ing on SPARC is easy, because we are quite lucky to push a
	multiple of 8 bytes (1 word stable pointer + 1 word dummy arg) in front
	of the existing arguments (note that %sp must stay double-word aligned at
	all times, see ABI spec at http://www.sparc.org/standards/psABI3rd.pdf).
	To do this, we extend the *caller's* stack frame by 2 words and shift
	the output registers used for argument passing (%o0 - %o5, we are a
	*leaf* procedure because of the tail-jump) by 2 positions. This makes
	room in %o0 and %o1 for the additinal arguments, namely  the stable
	pointer and a dummy (used for destination addr of jump on SPARC). This
	shouldn't cause any problems for a C-like caller: alloca is implemented
	similarly, and local variables should be accessed via %fp, not %sp. In
	a nutshell: This should work! (Famous last words! :-)
     */
    {
	unsigned long *adj_code = (unsigned long *)thunk->code;
	adj_code[ 0]  = 0x9C23A008UL;	/* sub   %sp, 8, %sp         */
	adj_code[ 1]  = 0xDA23A060UL;	/* st    %o5, [%sp + 96]     */
	adj_code[ 2]  = 0xD823A05CUL;	/* st    %o4, [%sp + 92]     */
	adj_code[ 3]  = 0x9A10000BUL;	/* mov   %o3, %o5            */
	adj_code[ 4]  = 0x9810000AUL;	/* mov   %o2, %o4            */
	adj_code[ 5]  = 0x96100009UL;	/* mov   %o1, %o3            */
	adj_code[ 6]  = 0x94100008UL;	/* mov   %o0, %o2            */
	adj_code[ 7]  = 0x13000000UL;	/* sethi %hi(app), %o1       */
	adj_code[ 7] |= ((unsigned long)app) >> 10;
	adj_code[ 8]  = 0x11000000UL;	/* sethi %hi(s), %o0         */
	adj_code[ 8] |= ((unsigned long)s) >> 10;
	adj_code[ 9]  = 0x81C26000UL;	/* jmp   %o1 + %lo(app)      */
	adj_code[ 9] |= ((unsigned long)app) & 0x000003FFUL;
	adj_code[10]  = 0x90122000UL;	/* or    %o0, %lo(s), %o0    */
	adj_code[10] |= ((unsigned long)s) & 0x000003FFUL;

	/* flush cache */
	asm("flush %0" : : "r" (adj_code     ));
	asm("flush %0" : : "r" (adj_code +  2));
	asm("flush %0" : : "r" (adj_code +  4));
	asm("flush %0" : : "r" (adj_code +  6));
	asm("flush %0" : : "r" (adj_code + 10));

	/* max. 5 instructions latency, and we need at >= 1 for returning */
	asm("nop");
	asm("nop");
	asm("nop");
	asm("nop");
    }
#else
    ERRMSG(0) "Foreign import wrapper is not supported on this architecture"
    EEND;
#endif
    return &thunk->code; /* a pointer into the middle of the thunk */
}

static void freeThunkAux(struct thunk_data* thunk) {
    freeStablePtr(thunk->stable);
    if (thunk->prev) {
        assert(foreignThunks != thunk);
        thunk->prev->next = thunk->next;
    } else {        
        assert(foreignThunks == thunk);
        foreignThunks = thunk->next;
    }        
    if (thunk->next) {
        thunk->next->prev = thunk->prev;
    }
    free(thunk);
}

static void freeAllThunks(void) {
    while (foreignThunks) {
        freeThunkAux(foreignThunks);
    }
}

/* This frees the object allocated by mkThunk.
 * [A useful debugging mode would not deallocate the thunk but would
 *  overwrite the thunk with code which prints an error message.]
 */
static void freeHaskellFunctionPtr(void* t) {
    struct thunk_data* thunk = (struct thunk_data*)((char*)t - (char*)&((struct thunk_data*)0)->code);
    freeThunkAux(thunk);
}

primFun(primFreeHFunPtr) {		/*  :: FunPtr a -> IO ()           */
    Pointer x;
    PtrArg(x,1+IOArity);
    freeHaskellFunctionPtr(x);
    IOReturn(nameUnit);
}

HugsAPI4* hugsAPI4() { /* build virtual function table */
    static HugsAPI4 api;
    static Bool initialised = FALSE;

    if (!initialised) {

        /* evaluate next argument */
      	api.getInt        = getInt;
	api.getWord       = getWord;
	api.getAddr       = getAddr;
	api.getFloat      = getFloat;
	api.getDouble     = getDouble;
	api.getChar       = getChar4;
	api.getForeign    = getForeign;
	api.getStablePtr  = getStablePtr;

	/* push part of result   */
	api.putInt        = putInt;
	api.putWord       = putWord;
	api.putAddr       = putAddr;
	api.putFloat      = putFloat;
	api.putDouble     = putDouble;
	api.putChar       = putChar4;
	api.putForeign    = putForeign;
	api.putStablePtr  = putStablePtr;

	/* return n values in IO monad or Id monad */
	api.returnIO      = returnIO;
        api.returnId      = returnId;
	api.runIO         = runIO;

	/* free a stable pointer */	    			 
	api.freeStablePtr = freeStablePtr;

	/* register the prim table */	    			 
	api.registerPrims = registerPrims;

	/* garbage collect */
	api.garbageCollect = garbageCollect;

	/* API3 additions follow */
        api.lookupName    = lookupName;
        api.ap            = apMany;
        api.getUnit       = getUnit;
        api.mkThunk       = mkThunk;
        api.freeThunk     = freeHaskellFunctionPtr;
	api.getBool       = getBool;
	api.putBool       = putBool;

	/* API4 additions follow */
        api.getInt8       = getInt8;
        api.getInt16      = getInt16;
        api.getInt32      = getInt32;
        api.getInt64      = getInt64;
        api.getWord8      = getWord8;
        api.getWord16     = getWord16;
        api.getWord32     = getWord32;
        api.getWord64     = getWord64;
        api.getPtr        = getPtr;
        api.getFunPtr     = getFunPtr;
        api.getForeignPtr = getForeignPtr;

        api.putInt8       = putInt8;
        api.putInt16      = putInt16;
        api.putInt32      = putInt32;
        api.putInt64      = putInt64;
        api.putWord8      = putWord8;
        api.putWord16     = putWord16;
        api.putWord32     = putWord32;
        api.putWord64     = putWord64;
        api.putPtr        = putPtr;
        api.putFunPtr     = putFunPtr;
        api.putForeignPtr = putForeignPtr;

	api.makeStablePtr4 = getStablePtr;
	api.derefStablePtr4= putStablePtr;

	api.putStablePtr4 = putStablePtr4;
	api.getStablePtr4 = getStablePtr4;
	api.freeStablePtr4 = freeStablePtr4;
			  
        api.runId         = runId;
    }
    return &api;
}

HugsAPI5* hugsAPI5() { /* build virtual function table */
    static HugsAPI5 api;
    static Bool initialised = FALSE;
    if (!initialised) {
	api.getBool       = getBool;
      	api.getInt        = getInt;
	api.getWord       = getWord;
	api.getAddr       = getAddr;
	api.getFloat      = getFloat;
	api.getDouble     = getDouble;
	api.getChar       = getChar;
	api.getForeign    = getForeign;
        api.getInt8       = getInt8;
        api.getInt16      = getInt16;
        api.getInt32      = getInt32;
        api.getInt64      = getInt64;
        api.getWord8      = getWord8;
        api.getWord16     = getWord16;
        api.getWord32     = getWord32;
        api.getWord64     = getWord64;
        api.getPtr        = getPtr;
        api.getFunPtr     = getFunPtr;
        api.getForeignPtr = getForeignPtr;
	api.getStablePtr4 = getStablePtr4;

	api.putBool       = putBool;
	api.putInt        = putInt;
	api.putWord       = putWord;
	api.putAddr       = putAddr;
	api.putFloat      = putFloat;
	api.putDouble     = putDouble;
	api.putChar       = putChar;
	api.putForeign    = putForeign;
        api.putInt8       = putInt8;
        api.putInt16      = putInt16;
        api.putInt32      = putInt32;
        api.putInt64      = putInt64;
        api.putWord8      = putWord8;
        api.putWord16     = putWord16;
        api.putWord32     = putWord32;
        api.putWord64     = putWord64;
        api.putPtr        = putPtr;
        api.putFunPtr     = putFunPtr;
        api.putForeignPtr = putForeignPtr;
	api.putStablePtr4 = putStablePtr4;
			  
	api.returnIO      = returnIO;
	api.runIO         = runIO;
        api.returnId      = returnId;
        api.runId         = runId;

	api.registerPrims = registerPrims;

        api.lookupName    = lookupName;
        api.ap            = apMany;
        api.getUnit       = getUnit;
        api.mkThunk       = mkThunk;
        api.freeThunk     = freeHaskellFunctionPtr;

	api.makeStablePtr4 = getStablePtr;
	api.derefStablePtr4= putStablePtr;
	api.freeStablePtr4 = freeStablePtr4;
    }
    return &api;
}

void hs_perform_gc(void)
{
    garbageCollect();
}

void hs_free_stable_ptr(HsStablePtr sp)
{
    freeStablePtr4(sp);
}

void hs_free_fun_ptr(HsFunPtr fp)
{
    freeHaskellFunctionPtr((void *)fp);
}

/* --------------------------------------------------------------------------
 * Built-in control:
 * ------------------------------------------------------------------------*/

/* Dummy entry */
static Void builtinControl Args((Int));
static Void builtinControl(what)
Int what; {
}
static struct primInfo builtinPrims = { builtinControl, builtinPrimTable, 0 };

Void builtIn(what)
Int what; {
    switch (what) {
	case INSTALL : 
		       initAdjustor();

		       registerPrims(&builtinPrims);
		       registerPrims(&printerPrims);
#if HASKELL_ARRAYS
		       registerPrims(&arrayPrims);
#endif
#if BIGNUMS
		       registerPrims(&bignumPrims);
#endif
#if IO_MONAD
		       registerPrims(&iomonadPrims);
#endif
#if TIME_MODULE
		       registerPrims(&timePrims);
#endif
#if DIRECTORY_MODULE
		       registerPrims(&dirPrims);
#endif
#if INTERNAL_PRIMS
		       registerPrims(&internalPrims);
#endif
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(nameFatbar,    "_FATBAR",  "fatbar");
		       pFun(nameFail,      "_FAIL",    "fail");
		       pFun(nameIf,        "_IF",      "if");
		       pFun(nameSel,       "_SEL",     "sel");

		       pFun(nameConCmp,    "_concmp",  "conCmp");
		       pFun(nameEnRange,   "_range",   "enRange");
		       pFun(nameEnIndex,   "_index",   "enIndex");
		       pFun(nameEnInRng,   "_inRange", "enInRng");
		       pFun(nameEnToEn,    "_toEnum",  "enToEn");
		       pFun(nameEnFrEn,    "_frEnum",  "enFrEn");
		       pFun(nameEnFrom,    "_from",    "enFrom");
		       pFun(nameEnFrTo,    "_fromTo",  "enFrTo");
		       pFun(nameEnFrTh,    "_fromThen","enFrTh");

		       pFun(namePrimThrow, "_throw",    "primThrowException");
		       pFun(nameBlackHole, "_Gc Black Hole",    "gcBhole");
		       pFun(nameInd,	   "_indirect", "dictIndirect");
		       name(nameInd).number = DFUNNAME;
#if    TREX
		       pFun(nameRecExt,	   "_recExt",	"recExt");
		       pFun(nameRecBrk,	   "_recBrk",	"recBrk");
		       pFun(nameRecSel,	   "_recSel",	"recSel");
		       pFun(nameRecShw,	   "_recShw",	"recShw");
		       pFun(nameRecEq,	   "_recEq",	"recEq");
		       pFun(nameAddEv,	   "_addEv",	"primPlusInt");
		       name(nameAddEv).number = DFUNNAME;
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       predef(nameNegate,       "negate");
		       predef(nameFlip,         "flip");
		       predef(nameFrom,         "enumFrom");
		       predef(nameFromThen,     "enumFromThen");
		       predef(nameFromTo,       "enumFromTo");
		       predef(nameFromThenTo,   "enumFromThenTo");
		       predef(nameFst,		"fst");
		       predef(nameSnd,		"snd");
		       predef(nameAnd,          "&&");
		       predef(nameOr,           "||");
		       predef(nameId,           "id");
		       predef(nameOtherwise,    "otherwise");
		       predef(nameComp,         ".");
		       predef(nameApp,          "++");
		       predef(nameShowField,    "showField");
		       predef(nameShowParen,    "showParen");
		       predef(nameReadField,    "readField");
		       predef(nameReadParen,    "readParen");
		       predef(nameLex,          "lex");
		       predef(nameRangeSize,    "rangeSize");
		       predef(nameCompAux,      "primCompAux");
		       predef(namePmInt,        "primPmInt");
		       predef(namePmInteger,    "primPmInteger");
		       predef(namePmFlt,        "primPmFlt");
		       predef(nameReturnIO,     "primretIO");
#if NPLUSK
		       predef(namePmNpk,        "primPmNpk");
		       predef(namePmSub,        "primPmSub");
#endif
		       predef(nameRationalToDouble, "rationalToDouble");
		       predef(nameRationalToFloat,  "rationalToFloat");
#if SHORT_CIRCUIT_COERCIONS
		       predef(nameFloatToRational,  "floatToRational");
		       predef(nameDoubleToRational, "doubleToRational");
		       predef(nameDoubleToRatio,    "doubleToRatio");
		       predef(nameIntToRatio,       "intToRatio");
		       predef(nameIntToFloat,       "primIntToFloat");
		       predef(nameIntToDouble,      "primIntToDouble");
		       predef(nameDoubleToFloat,    "primDoubleToFloat");
		       predef(nameFloatToDouble,    "primFloatToDouble");
#endif
#undef  predef
		       break;

        case RESET   : freeAllThunks();
                       break;
    }
}

/*-------------------------------------------------------------------------*/

