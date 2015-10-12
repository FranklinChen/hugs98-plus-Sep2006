/* --------------------------------------------------------------------------
 * Interpreter command structure
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: command.h,v $
 * $Revision: 1.15 $
 * $Date: 2006/01/10 23:31:44 $
 * ------------------------------------------------------------------------*/
#ifndef __COMMAND_H__
#define __COMMAND_H__

typedef Int Command;

struct cmd {
    String cmdString;
    Command cmdCode;
};

extern Command readCommand Args((struct cmd *, Char, Char));

#define EDIT    0
#define FIND    1
#define LOAD    2
#define ALSO    3
#define RELOAD  4
#define EVAL    5
#define TYPEOF  6
#define HELP    7
#define NAMES   8
#define BADCMD  9
#define SET     10
#define QUIT    11
#define SYSTEM  12
#define CHGDIR  13
#define INFO    14
#define COLLECT 15
#define SETMODULE 16
#define BROWSE  17
#define XPLAIN  18
#define PNTVER  19
#define NOCMD   20
#ifdef __SYMBIAN32__
#define PRNDIR  21
#endif
#define MAIN    22

#if OBSERVATIONS
/*-------------------------------------------------------------------------*
 * Commands available after breakpoint                                     *
 *-------------------------------------------------------------------------*/
#define BRK_DISPLAY     0
#define BRK_CONTINUE    1
#define BRK_SET         2
#define BRK_RESET       3
#endif

/*-------------------------------------------------------------------------*/

#endif /* __COMMAND_H__ */
