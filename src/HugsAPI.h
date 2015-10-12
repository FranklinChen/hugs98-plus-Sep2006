/* --------------------------------------------------------------------------
 * Definition of the external Hugs server API
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: HugsAPI.h,v $
 * $Revision: 1.3 $
 * $Date: 2004/02/04 17:53:38 $
 * ------------------------------------------------------------------------*/
#ifndef __HUGSAPI_H__
#define __HUGSAPI_H__


#ifndef Args
# if PROTOTYPES
#  define Args(x) x
# else
#  define Args(x) ()
# endif
#endif /* !defined Args */

#ifdef __cplusplus 
extern "C" {
#endif

typedef int HVal;     /* Haskell values are represented by stable pointers */

typedef struct _HugsServerAPI {
    char* (*clearError     ) Args((void));
    void  (*setHugsArgs    ) Args((int, char**));
    int   (*getNumScripts  ) Args((void));
    void  (*reset          ) Args((int));
    void  (*setOutputEnable) Args((unsigned));
    void  (*changeDir      ) Args((char*));
    void  (*loadProject    ) Args((char*));
    void  (*loadFile       ) Args((char*));
    void  (*loadFromBuffer ) Args((char*));
    void  (*setOptions     ) Args((char*));
    char* (*getOptions     ) Args((void));
    HVal  (*compileExpr    ) Args((char*,char*));
    void  (*garbageCollect ) Args((void));

    void  (*lookupName     ) Args((char*,char*)); /* push values onto stack*/

    void  (*mkInt          ) Args((int));
    void  (*mkAddr         ) Args((void*));
    void  (*mkString       ) Args((char*));

    void  (*apply          ) Args((void));      /* manipulate top of stack */

    int   (*evalInt        ) Args((void));      /* evaluate top of stack   */
    void* (*evalAddr       ) Args((void));
    char* (*evalString     ) Args((void));

    int   (*doIO           ) Args((void));
    int   (*doIO_Int       ) Args((int*));
    int   (*doIO_Addr      ) Args((void**));

    HVal  (*popHVal        ) Args((void));      /* pop stack               */
    void  (*pushHVal       ) Args((HVal));      /* push back onto stack    */
    void  (*freeHVal       ) Args((HVal)); 
} HugsServerAPI;

DLLEXPORT(HugsServerAPI*) initHugsServer Args((Int, String[]));
DLLEXPORT(Void) shutdownHugsServer Args((HugsServerAPI*));

#ifdef __cplusplus 
};
#endif

#endif /* __HUGSAPI_H__ */
