/*
 * Maintaining a stack of files / scripts.
 * 
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 */
#ifndef __SCRIPT_H__
#define __SCRIPT_H__

/*
 * The Script module handles the management of the stack of scripts
 * known to the interpreter. 
 *
 * When the interpreter processes a module, it adds the module's
 * imports to the script stack which are then subsequently loaded in.
 * Assuming the imported modules were all found and loaded without error,
 * the original module and its imports is then processed again, 
 * but this time all the imports are found on the stack of scripts
 * already loaded in.
 *
 */

extern Void initScripts         Args((Void));
extern Void stopScripts         Args((Void));
extern Void setScriptStableMark Args((Void));

extern String getScriptName     Args((Script));
extern String getScriptRealName Args((Script));
extern Int    getScriptHwMark   Args((Void));
extern Int    numLoadedScripts  Args((Void));

extern Void addScriptName       Args((String,Bool));
extern Bool chase               Args((List));
extern Void addScriptsFromArgs  Args((Int,String []));

extern Void forgetScriptsFrom Args((Script));
extern Void forgetAllScripts  Args((Void));
extern Void forgetAScript     Args((Script));

extern String scriptFile;

extern Void whatScripts       Args((Void));
extern Void readScripts       Args((Int));

extern Void script     Args((int));

#if HUGS_FOR_WINDOWS
extern Void setNumLoadedScripts Args((Script));
extern Void setScriptHwMark     Args((Script));
extern Void setScriptName       Args((Script,String));
extern Void setScriptRealName   Args((Script,String));
#endif


#endif /* __SCRIPT_H__ */
