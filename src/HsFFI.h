#ifndef __HSFFI_H__
#define __HSFFI_H__

typedef unsigned char      hugs_uint8_t;
typedef unsigned short     hugs_uint16_t;
typedef unsigned int       hugs_uint32_t;
typedef signed   char      hugs_int8_t;
typedef signed   short     hugs_int16_t;
typedef signed   int       hugs_int32_t;
# ifdef _MSC_VER
typedef unsigned __int64   hugs_uint64_t;
typedef          __int64   hugs_int64_t;
# else
typedef unsigned long long hugs_uint64_t;
typedef signed   long long hugs_int64_t;
# endif

/* 
 * The ifdef Args is a crude way of testing whether this file is
 * #included into Hugs.  Use it to eliminate non-portable stuff.
 */

#ifdef Args /* #included into Hugs */

typedef Int            HsInt;        
typedef Int8           HsInt8;         
typedef Int16          HsInt16;        
typedef Int            HsInt32;        
typedef unsigned int   HsWord;       
typedef unsigned char  HsWord8;        
typedef unsigned short HsWord16;       
typedef unsigned int   HsWord32;       

#else /* #included into user-provided C code */

typedef int            HsInt;        
typedef hugs_int8_t    HsInt8;         
typedef hugs_int16_t   HsInt16;        
typedef hugs_int32_t   HsInt32;        
typedef unsigned int   HsWord;       
typedef hugs_uint8_t   HsWord8;        
typedef hugs_uint16_t  HsWord16;       
typedef hugs_uint32_t  HsWord32;       

#endif   

/* 
 * Here we deviate from the FFI specification:
 * If we make them both float, then there's no way to pass a double
 * to C which means we can't call common C functions like sin.
 */           
typedef float          HsFloat;      
typedef double         HsDouble;     

typedef hugs_int64_t   HsInt64;        
typedef hugs_uint64_t  HsWord64;       
typedef int            HsChar;
typedef int            HsBool;         
typedef void*          HsAddr;       
typedef void*          HsPtr;          
typedef void           (*HsFunPtr)(void);
typedef void*          HsForeignPtr;   
typedef void*          HsStablePtr;  
                       
#define HS_CHAR_MIN             0
#define HS_CHAR_MAX             0x10FFFF
#define HS_BOOL_FALSE           0
#define HS_BOOL_TRUE            1
#define HS_BOOL_MIN             HS_BOOL_FALSE
#define HS_BOOL_MAX             HS_BOOL_TRUE
#define HS_INT_MIN              __INT32_MIN
#define HS_INT_MAX              __INT32_MAX
#define HS_INT8_MIN             __INT8_MIN
#define HS_INT8_MAX             __INT8_MAX
#define HS_INT16_MIN            __INT16_MIN
#define HS_INT16_MAX            __INT16_MAX
#define HS_INT32_MIN            __INT32_MIN
#define HS_INT32_MAX            __INT32_MAX
#define HS_INT64_MIN            __INT64_MIN
#define HS_INT64_MAX            __INT64_MAX
#define HS_WORD8_MAX            __UINT8_MAX
#define HS_WORD16_MAX           __UINT16_MAX
#define HS_WORD32_MAX           __UINT32_MAX
#define HS_WORD64_MAX           __UINT64_MAX

#ifndef Args

#include <float.h>

#define HS_FLOAT_RADIX         FLT_RADIX
#define HS_FLOAT_ROUNDS        FLT_ROUNDS
#define HS_FLOAT_EPSILON       FLT_EPSILON
#define HS_FLOAT_DIG           FLT_DIG
#define HS_FLOAT_MANT_DIG      FLT_MANT_DIG
#define HS_FLOAT_MIN           FLT_MIN
#define HS_FLOAT_MIN_EXP       FLT_MIN_EXP
#define HS_FLOAT_MIN_10_EXP    FLT_MIN_10_EXP
#define HS_FLOAT_MAX           FLT_MAX
#define HS_FLOAT_MAX_EXP       FLT_MAX_EXP
#define HS_FLOAT_MAX_10_EXP    FLT_MAX_10_EXP

#define HS_DOUBLE_RADIX         DBL_RADIX
#define HS_DOUBLE_ROUNDS        DBL_ROUNDS
#define HS_DOUBLE_EPSILON       DBL_EPSILON
#define HS_DOUBLE_DIG           DBL_DIG
#define HS_DOUBLE_MANT_DIG      DBL_MANT_DIG
#define HS_DOUBLE_MIN           DBL_MIN
#define HS_DOUBLE_MIN_EXP       DBL_MIN_EXP
#define HS_DOUBLE_MIN_10_EXP    DBL_MIN_10_EXP
#define HS_DOUBLE_MAX           DBL_MAX
#define HS_DOUBLE_MAX_EXP       DBL_MAX_EXP
#define HS_DOUBLE_MAX_10_EXP    DBL_MAX_10_EXP

#endif /* included into user code */

typedef int            HugsStackPtr;
typedef void*          HugsForeign;   
typedef int            HugsStablePtr;  

typedef void (*HugsPrim) (HugsStackPtr); /* primitive function	   */

#ifndef Args  
struct hugs_primitive {		         /* table of primitives		   */
    char*  ref;				 /* primitive reference string	   */
    int	   arity;			 /* primitive function arity	   */
    HugsPrim imp;		         /* primitive implementation	   */
};

struct hugs_primInfo {
    void                  (*controlFun)(int);
    struct hugs_primitive *primFuns;
    struct hugs_primInfo  *nextPrimInfo;
};
#else
#define hugs_primInfo primInfo
#endif

/* API Version number */
#define HUGS_API_VERSION 5

typedef struct {

  /* evaluate next argument */
  HsBool         (*getBool)        (void);
  HsInt          (*getInt)         (void);
  HsWord         (*getWord)        (void);
  HsAddr    	 (*getAddr)        (void);
  HsFloat        (*getFloat)       (void);
  HsDouble       (*getDouble)      (void);
  HsChar         (*getChar)        (void);
  HugsForeign    (*getForeign)     (void);
  HsStablePtr    (*getStablePtr4)  (void);
  HsInt8         (*getInt8)        (void);
  HsInt16        (*getInt16)       (void);
  HsInt32        (*getInt32)       (void);
  HsInt64        (*getInt64)       (void);
  HsWord8        (*getWord8)       (void);
  HsWord16       (*getWord16)      (void);
  HsWord32       (*getWord32)      (void);
  HsWord64       (*getWord64)      (void);
  HsPtr          (*getPtr)         (void);
  HsFunPtr       (*getFunPtr)      (void);
  HsForeignPtr   (*getForeignPtr)  (void);

  /* push result   */
  void           (*putBool)        (HsBool);
  void           (*putInt)         (HsInt);
  void      	 (*putWord)        (HsWord);
  void      	 (*putAddr)        (HsAddr);
  void           (*putFloat)       (HsFloat);
  void           (*putDouble)      (HsDouble);
  void           (*putChar)        (HsChar);
  void      	 (*putForeign)     (HugsForeign, void (*)(HugsForeign));
  void           (*putInt8)        (HsInt8);
  void           (*putInt16)       (HsInt16);
  void           (*putInt32)       (HsInt32);
  void           (*putInt64)       (HsInt64);
  void           (*putWord8)       (HsWord8);
  void           (*putWord16)      (HsWord16);
  void           (*putWord32)      (HsWord32);
  void           (*putWord64)      (HsWord64);
  void           (*putPtr)         (HsPtr);
  void           (*putFunPtr)      (HsFunPtr);
  void           (*putForeignPtr)  (HsForeignPtr);
  void           (*putStablePtr4)  (HsStablePtr);

  /* return in IO monad or Id monad */
  void      	 (*returnIO)       (HugsStackPtr, int);
  int      	 (*runIO)          (int);
  void           (*returnId)       (HugsStackPtr, int);
  int            (*runId)          (int);

  /* register the prim table */	    			 
  void      	 (*registerPrims)  (struct hugs_primInfo*);
			   
  HugsStablePtr  (*lookupName)     (char*, char*);
  void           (*ap)             (int);
  void           (*getUnit)        (void);
  void*          (*mkThunk)        (HsFunPtr, HugsStablePtr);
  void           (*freeThunk)      (void*);

  HugsStablePtr  (*makeStablePtr4) (void);
  void           (*derefStablePtr4)(HugsStablePtr);
  void      	 (*freeStablePtr4) (HsStablePtr);
} HugsAPI5;

/* Note: the change in going from version 4 to 5 is that
   with 5 (and later), the DLLs specify the HugsAPI version
   assumed by the DLL primitives. The HugsAPI method table
   is _not_ identical to 4's; it has been re-orged and
   tidied up.
*/

extern  HugsAPI5* hugsAPI5     (void);
typedef void (*InitModuleFun5) (HugsAPI5*);
typedef int  (*APIVersionFun)  (void);

/* To ensure backward compatibility, HugsAPI4 is also supported:
 * (due to the HugsAPI4 vtbl being an extension of HugsAPI3's and
 *  HugsAPI2's, support for these two comes for free.)
 */
typedef struct {

  /* evaluate next argument */
  HsInt          (*getInt)         (void);
  HsWord         (*getWord)        (void);
  HsAddr    	 (*getAddr)        (void);
  HsFloat        (*getFloat)       (void);
  HsDouble       (*getDouble)      (void);
  char           (*getChar)        (void);
  HugsForeign    (*getForeign)     (void);
  HugsStablePtr  (*getStablePtr)   (void); /* deprecated */

  /* push part of result   */
  void           (*putInt)         (HsInt);
  void      	 (*putWord)        (HsWord);
  void      	 (*putAddr)        (HsAddr);
  void           (*putFloat)       (HsFloat);
  void           (*putDouble)      (HsDouble);
  void           (*putChar)        (char);
  void      	 (*putForeign)     (HugsForeign, void (*)(HugsForeign));
  void           (*putStablePtr)   (HugsStablePtr); /* deprecated */

  /* return n values in IO monad or Id monad */
  void      	 (*returnIO)       (HugsStackPtr, int);
  void      	 (*returnId)       (HugsStackPtr, int);
  int      	 (*runIO)          (int);

  /* free a stable pointer */	    			 
  void      	 (*freeStablePtr)  (HugsStablePtr); /* deprecated */

  /* register the prim table */	    			 
  void      	 (*registerPrims)  (struct hugs_primInfo*);
			   
  /* garbage collect */
  void		 (*garbageCollect) (void);

  /* API3 additions follow */
  HugsStablePtr  (*lookupName)     (char*, char*);
  void           (*ap)             (int);
  void           (*getUnit)        (void);
  void*          (*mkThunk)        (HsFunPtr, HugsStablePtr);
  void           (*freeThunk)      (void*);
  HsBool         (*getBool)        (void);
  void           (*putBool)        (HsBool);

  /* API4 additions follow */
  HsInt8         (*getInt8)        (void);
  HsInt16        (*getInt16)       (void);
  HsInt32        (*getInt32)       (void);
  HsInt64        (*getInt64)       (void);
  HsWord8        (*getWord8)       (void);
  HsWord16       (*getWord16)      (void);
  HsWord32       (*getWord32)      (void);
  HsWord64       (*getWord64)      (void);
  HsPtr          (*getPtr)         (void);
  HsFunPtr       (*getFunPtr)      (void);
  HsForeignPtr   (*getForeignPtr)  (void);

  void           (*putInt8)        (HsInt8);
  void           (*putInt16)       (HsInt16);
  void           (*putInt32)       (HsInt32);
  void           (*putInt64)       (HsInt64);
  void           (*putWord8)       (HsWord8);
  void           (*putWord16)      (HsWord16);
  void           (*putWord32)      (HsWord32);
  void           (*putWord64)      (HsWord64);
  void           (*putPtr)         (HsPtr);
  void           (*putFunPtr)      (HsFunPtr);
  void           (*putForeignPtr)  (HsForeignPtr);

  HugsStablePtr  (*makeStablePtr4) (void);
  void           (*derefStablePtr4)(HugsStablePtr);

  void           (*putStablePtr4)  (HsStablePtr);
  HsStablePtr    (*getStablePtr4)  (void);
  void      	 (*freeStablePtr4) (HsStablePtr);

  int      	 (*runId)          (int);
} HugsAPI4;

extern  HugsAPI4* hugsAPI4     (void);
typedef void (*InitModuleFun4) (HugsAPI4*);

extern void hs_perform_gc(void);
extern void hs_free_stable_ptr(HsStablePtr sp);
extern void hs_free_fun_ptr(HsFunPtr fp);

/* Copied verbatim from prelude.h */

#if defined(__BORLANDC__)
# define DLLIMPORT(rty) rty far _import
# define DLLEXPORT(rty) rty far _export
#elif defined(_WIN32) /* Microsoft Windows */
# define DLLIMPORT(rty) __declspec(dllimport) rty
# define DLLEXPORT(rty) __declspec(dllexport) rty
#else 
# define DLLIMPORT(rty) rty
# define DLLEXPORT(rty) rty
#endif /* Don't need to declare DLL exports */

#endif /* __HSFFI_H__ */
