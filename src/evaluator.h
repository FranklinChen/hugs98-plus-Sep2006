/*
 * The Hugs evaluator / command interpreter + support functions.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 */
#ifndef __EVALUATOR_H__
#define __EVALUATOR_H__

extern Void startEvaluator    Args((Void));
extern Void stopEvaluator     Args((Void));
extern Void evaluator         Args((Module));
extern Void everybody         Args((Int));
extern Void loadPrelude       Args((Void));

#endif /* __EVALUATOR_H__ */
