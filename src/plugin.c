/* --------------------------------------------------------------------------
 * Statically linked plugins
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: plugin.c,v $
 * $Revision: 1.10 $
 * $Date: 2003/10/28 11:47:22 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"

/* This file is often compiled with a command-line argument such as
 *   '-DPLUGINS={"Xlib",initXlib},'
 * default to empty if not present.
 */
#ifndef PLUGINS
# define PLUGINS
#endif

struct pluginInfo {
    String         nm;            /* Name of plugin module                 */
    InitModuleFun5 initModule;    /* Initialisation code for the plugin    */
};

static struct pluginInfo pluginList[] = { /* hardwired list of all plugins */
  /* {"Test",  initTest},  */
  /* {"Xlib",  initXlib},  */
  PLUGINS
  {0,0}
};

Bool havePlugin(mod)                /* can we statically link this plugin? */
String mod; {                       /* (called when each module is loaded) */
    Int i;
    for(i=0; pluginList[i].nm; ++i) {
	if (0 == strcmp(mod, pluginList[i].nm)) {
	    (*pluginList[i].initModule)(hugsAPI5());
	    return TRUE;
	}
    }
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Plugins control:
 * ------------------------------------------------------------------------*/

Void plugins(what)
Int what; {
}

/*-------------------------------------------------------------------------*/
