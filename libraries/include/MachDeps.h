/* This file is included into various Haskell files in the hierarchical 
 * libraries.
 *
 * It provides a variety of constants and symbols required by the
 * foreign function interface libraries.
 */

#include "ghcconfig.h"

#define SIZEOF_HSCHAR      SIZEOF_INT
#define SIZEOF_HSINT	   SIZEOF_INT
#define SIZEOF_HSWORD	   SIZEOF_INT
#define SIZEOF_HSPTR	   SIZEOF_VOID_P
#define SIZEOF_HSFUNPTR    SIZEOF_VOID_P
#define SIZEOF_HSSTABLEPTR SIZEOF_VOID_P
#define SIZEOF_HSFLOAT	   SIZEOF_FLOAT
#define SIZEOF_HSDOUBLE    SIZEOF_DOUBLE

#define SIZEOF_WORD8       1
#define SIZEOF_WORD16      2	    
#define SIZEOF_WORD32      4    
#define SIZEOF_WORD64      8

#define SIZEOF_INT8        1    
#define SIZEOF_INT16       2    
#define SIZEOF_INT32       4    
#define SIZEOF_INT64       8    


#define ALIGNMENT_HSCHAR      ALIGNMENT_INT
#define ALIGNMENT_HSINT	      ALIGNMENT_INT
#define ALIGNMENT_HSWORD      ALIGNMENT_INT
#define ALIGNMENT_HSPTR	      ALIGNMENT_VOID_P
#define ALIGNMENT_HSFUNPTR    ALIGNMENT_VOID_P
#define ALIGNMENT_HSSTABLEPTR ALIGNMENT_VOID_P
#define ALIGNMENT_HSFLOAT     ALIGNMENT_FLOAT
#define ALIGNMENT_HSDOUBLE    ALIGNMENT_DOUBLE

#define ALIGNMENT_WORD8       ALIGNMENT_UNSIGNED_CHAR     
#define ALIGNMENT_WORD16      ALIGNMENT_UNSIGNED_SHORT    
#define ALIGNMENT_WORD32      ALIGNMENT_UNSIGNED_INT      
#define ALIGNMENT_WORD64      ALIGNMENT_UNSIGNED_LONG_LONG

#define ALIGNMENT_INT8        ALIGNMENT_CHAR         
#define ALIGNMENT_INT16       ALIGNMENT_SHORT        
#define ALIGNMENT_INT32       ALIGNMENT_INT          
#define ALIGNMENT_INT64       ALIGNMENT_LONG_LONG    

