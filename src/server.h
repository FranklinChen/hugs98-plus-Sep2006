/* --------------------------------------------------------------------------
 * Definition of the Hugs server API
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: server.h,v $
 * $Revision: 1.12 $
 * $Date: 2003/10/14 13:56:25 $
 * ------------------------------------------------------------------------*/
#include "HugsAPI.h"

/* These have non-local scope, as they're used when creating 
 * extended/delegated versions of the server API (cf. the server
 * interface provided by the .NET extensions.)
 */
extern Void   setError        Args((String));
extern Void   startEval       Args((Void));
extern Bool   safeEval        Args((Cell));
extern String ClearError      Args((Void));
extern Cell   getTypeableDict Args((Type));
extern char*  lastError;

/* Get the API method table from the currently running interpreter.
 * => the interpreter / server is assumed to have already been initialized.
 */
extern HugsServerAPI* getHugsAPI Args((Void));
/* ------------------------------------------------------------------------*/
