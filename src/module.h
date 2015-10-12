/* --------------------------------------------------------------------------
 * Haskell 98 module system implementation for Hugs.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * -sof 2002.
 * 
 * ------------------------------------------------------------------------*/
#ifndef __MODULE_H__
#define __MODULE_H__

extern Void addQualImport       Args((Text,Text,List));
extern Void addUnqualImport     Args((Text,Text,List));
extern Void importName          Args((Module,Name));
extern Void checkImportList     Args((Bool,Pair));
extern Void checkQualImportList Args((Pair));
extern Void fixupImportExports  Args((List));
extern List checkExports        Args((List));
extern Void browseModule	Args((Module,Bool));

#endif /* __MODULE_H__ */
