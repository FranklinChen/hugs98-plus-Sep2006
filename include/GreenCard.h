/* --------------------------------------------------------------------------
 * GreenCard include file.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: GreenCard.h,v $
 * $Revision: 1.5 $
 * $Date: 2003/10/14 13:56:19 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 *
 *                                  WARNING
 *
 * Most of the code in this file must exactly match corresponding definitions
 * in the Hugs source code.
 *
 * We have chosen to copy this code over to avoid the need to #include huge
 * chunks of the Hugs internal definitions (which sometimes conflict with
 * Xlib, Win32 or other libraries which we might also have to #include).
 *
 * ------------------------------------------------------------------------*/
#ifndef __GREENCARD_H__
#define __GREENCARD_H__

/* Configuration details -- set to 0 if your C compiler doesn't support function protos */
#define HAVE_PROTOTYPES 1

#if HAVE_PROTOTYPES       /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

/* What version of the API this file defines (and corresponding Hugs
 * sources prefer).
 *
 * Introduced in 'version 3', so the absence of this #define indicates
 * 'version 2' (version 1 is, by now, just a hazy memory to one Hugs
 * developer :-) ).
 */
#define HUGSAPI_VERSION 3
#define HUGSAPI_NAME    HugsAPI3

/* based on code in builtin.c */

typedef int   HugsStackPtr;
typedef int   HugsStablePtr;
typedef void* HugsForeign;

#define PROTO_PRIM(name) static void name Args((HugsStackPtr))
#define primFun(name)	 static void name(HugsStackPtr hugs_root)
#define hugs_returnIO(n) hugs->returnIO(hugs_root,n)
#define hugs_returnId(n) hugs->returnId(hugs_root,n)

/* These declarations must exactly match those in storage.h */

typedef void (*Prim) Args((HugsStackPtr)); /* primitive function	   */

extern struct primitive {		/* table of primitives		   */
    char*  ref;				/* primitive reference string	   */
    int	   arity;			/* primitive function arity	   */
    Prim   imp;				/* primitive implementation	   */
} primitives[];

struct primInfo {
    void              (*controlFun) Args((int));
    struct primitive  *primFuns;
    struct primInfo   *nextPrimInfo;
};

/* This is an exact copy of the declaration found in storage.h */

typedef struct {

  /* evaluate next argument */
  int            (*getInt   )     Args(());  
  unsigned int   (*getWord)       Args(());
  void*     	 (*getAddr  )     Args(());
  float     	 (*getFloat )     Args(());
  double    	 (*getDouble)     Args(());
  char      	 (*getChar  )     Args(());
  HugsForeign    (*getForeign)    Args(());
  HugsStablePtr  (*getStablePtr)  Args(());

  /* push part of result   */
  void      	 (*putInt   )     Args((int));           
  void      	 (*putWord  )     Args((unsigned int));
  void      	 (*putAddr  )     Args((void *));
  void      	 (*putFloat )     Args((double));
  void      	 (*putDouble)     Args((double));
  void      	 (*putChar  )     Args((char));
  void      	 (*putForeign)    Args((HugsForeign, void (*)(HugsForeign)));
  void      	 (*putStablePtr)  Args((HugsStablePtr));

  /* return n values in IO monad or Id monad */
  void      	 (*returnIO)      Args((HugsStackPtr, int));
  void      	 (*returnId)      Args((HugsStackPtr, int));
  int      	 (*runIO)         Args((int));

  /* free a stable pointer */	    			 
  void      	 (*freeStablePtr) Args((HugsStablePtr));

  /* register the prim table */	    			 
  void      	 (*registerPrims) Args((struct primInfo*));
			   
  /* garbage collect */
  void		 (*garbageCollect) Args(());

  /* API3 additions follow */
  HugsStablePtr  (*lookupName)     Args((char*, char*));
  void           (*ap)             Args((int));
  void           (*getUnit)        Args(());
  void*          (*mkThunk)        Args((void*, HugsStablePtr));
  void           (*freeThunk)      Args((void*));
  int     	 (*getBool)        Args(());
  void      	 (*putBool)        Args((int));
} HugsAPI3;

static HugsAPI3 *hugs = 0; /* pointer to virtual function table */

/* Copied verbatim from prelude.h */

#ifdef _MSC_VER /* Microsoft Visual C++ */
#define DLLIMPORT(rty) __declspec(dllimport) rty
#define DLLEXPORT(rty) __declspec(dllexport) rty
#elif defined __BORLANDC__ 
#define DLLIMPORT(rty) rty far _import
#define DLLEXPORT(rty) rty far _export
#else 
#define DLLIMPORT(rty) rty
#define DLLEXPORT(rty) rty
#endif /* Don't need to declare DLL exports */

#endif /* __GREENCARD_H__ */
