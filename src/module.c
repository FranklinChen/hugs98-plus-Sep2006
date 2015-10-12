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
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "module.h"
#include "output.h"

/* --------------------------------------------------------------------------
 * Static analysis of modules:
 *
 * The static checks of the import and export lists are invoked
 * via the entry points in module.h  (cf. static.c:checkDefns()).
 *
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * local function prototypes:
 * ------------------------------------------------------------------------*/
static Name   local lookupName		Args((Text,List));
static Module local modOfEntity         Args((Cell));
static Void   local reportAmbigEntity   Args((Text,Text,List));
static List   local checkSubentities	Args((List,List,List,String,Text));
static Void   local checkExportDistinct Args((List,Bool,Cell));
static List   local checkExportTycon	Args((List,Text,Bool,Cell,Tycon));
static List   local checkExportClass	Args((List,Text,Bool,Cell,Class));
static List   local checkExportModule	Args((List,Text,Cell));
static List   local checkExport		Args((List,Text,Cell));
static List   local checkImportEntity	Args((List,Module,Bool,Cell));
static List   local resolveImportList	Args((Module,Cell,Bool));
static Cell   local entityIsMember      Args((Cell,List));
static List   local augmentEntity       Args((Bool,Cell,Cell,List));
static List   local addEntity           Args((Cell,Cell,List));
static List   local addEntityPair       Args((Cell,List));
static List   local mergeImportLists    Args((List,List));
static List   local getIEOrphans        Args((List));
static List   local fixupIEList         Args((List));
static List   local allMethodsOrDCons   Args((List,Cell,Module,Bool));

static Cell   local importEntity	Args((Module,Cell));
static Void   local importTycon		Args((Module,Tycon));
static Void   local importClass		Args((Module,Class));

static Void   local browseName		Args((Name));
static Void   local browseEntity	Args((Cell));

Void addQualImport(orig,new,entities) /* Add to qualified import list       */
Cell orig;       /* Original name of module                                */
Cell new;        /* Name module is called within this module (or NIL)      */
List entities; { /* List of entity names */
    /* Record the entities imported */
    module(currentModule).qualImports = 
      addEntity(orig,entities,module(currentModule).qualImports);
    /* Record the module --> alias mapping */
    module(currentModule).modAliases =  
      cons(pair(isNull(new)?orig:new,orig), module(currentModule).modAliases);
}

Void addUnqualImport(mod,new,entities) /* An unqualified import            */
Cell mod;         /* Name of module                                        */
Cell new;         /* Local alias                                           */
List entities;  { /* List of entity names                                  */
    /* Add to unqualified import list */
    unqualImports = addEntity(mod,entities,unqualImports);
    /* Record the module --> alias mapping */
    module(currentModule).modAliases =  
      cons(pair(isNull(new)?mod:new,mod), module(currentModule).modAliases);
}


static Name local lookupName(t,nms)    /* find text t in list of Names     */
Text t;
List nms; { /* :: [Name] */
    for(; nonNull(nms); nms=tl(nms)) {
      if ( t == name(hd(nms)).text ) {
	    return hd(nms);
      }
    }
    return NIL;
}

static List local checkSubentities(imports,named,wanted,description,textParent)
List   imports;
List   named;       /* :: [ Q?(Var|Con)(Id|Op) ]                  */
List   wanted;      /* :: [Name]                                  */
String description; /* "<constructor>|<member> of <type>|<class>" */
Text   textParent; {
    for(; nonNull(named); named=tl(named)) {
	Pair x = hd(named);
	/* ToDo: ignores qualifier; doesn't check that entity is in scope */
	Text t = isPair(snd(x)) ? qtextOf(x) : textOf(x);
	Name n = lookupName(t,wanted);
	if (isNull(n)) {
	    ERRMSG(0) "Entity \"%s\" is not a %s \"%s\"",
		      textToStr(t),
		      description,
		      textToStr(textParent)
	    EEND;
	}
	imports = cons(n,imports);
    }
    return imports;
}

#if 0
/* debugging code - dumping IE lists */
static Void showIEEntity Args((Cell));
static Void showIEList   Args((List,char));

static Void
showIEEntity(e)
Cell e; {
  if (isName(e)) {
    fprintf(stderr, "%s", textToStr(name(e).text));
  } else if (isClass(e)) {
    fprintf(stderr, "%s", textToStr(cclass(e).text));
  } else if (isTycon(e)) {
    fprintf(stderr, "%s", textToStr(tycon(e).text));
  } else if (isPair(e)) {
    showIEEntity(fst(e));
    fprintf(stderr, "(");
    showIEList(snd(e),' ');
    fprintf(stderr, ")");
  } else {
    fprintf(stderr, "showIEEntity: unknown entity kind %d\n", whatIs(e));
    fflush(stderr);
  }
}

static Void
showIEList(ieList,sep)
List ieList;
char sep; {
  List xs = ieList;
  for(;nonNull(xs);xs=tl(xs)) {
    showIEEntity(hd(xs));
    if (tl(xs) != NIL) {
      fputc(sep, stderr);
    }
  }
}
#endif

static List getIEOrphans(ieList) /* locate methods/dcons appearing on their */
List ieList; {                   /* own in an import/export list.           */
  List orphans = NIL;
  List xs;
  
  for(xs=ieList;nonNull(xs);xs=tl(xs)) {
    Cell e = hd(xs);
    if ( isName(e) ) {
      if (isClass(name(e).parent)) {
	/* a lone member */
	orphans = cons(pair(name(e).parent, singleton(e)), orphans);
      } else if (isCfun(e)) {
	/* a lone data constructor (can only appear in a hiding list.) */
	orphans = cons(pair(name(e).parent, singleton(e)), orphans);
      } else if (isSfun(e)) {
	/* a field name */
	Cell p = name(e).parent;      /* the data constructor */
	Cell t = name(p).parent;      /* the type constructor */
	orphans = cons(pair(t,singleton(e)), orphans);
      }
    }
  }
  return orphans;
}

/*
 * fixupIEList() traverses an import/export list, adjusting
 * the list in the following ways:
 *
 *  1. 'orphan'/subordinate names are joined up with their
 *     parents. An orphan E is either a class member, field
 *     name or a data constructor (in 'hiding' lists *only*)
 *     that's imported/exported without referring to its
 *     parent P -- E appears in an import/export list rather
 *     than P(E). If P is appearing elsewhere in the
 *     import/export list, float E inside of P.
 *
 * ToDo: remove duplicates from the resulting list also; it's
 *       harmless for there to be any, but may lead to confusion
 *       later on.
 * 
 */
static List fixupIEList(ieList)
List ieList; {
  List orphans = NIL;
  List xs;

  orphans = getIEOrphans(ieList);
  
  if (nonNull(orphans)) {
#if 0
    /* Debugging - show the orphan list */
    fprintf(stderr, "Orphan list{%s}:", textToStr(module(currentModule).text)); fflush(stderr);
    showIEList(orphans,'\n'); fflush(stderr);
#endif
    /* Transformation 1 (we're actually being a bit sloppy here
       and not removing the orphan from the IE list if it
       can be floated inside its parent.) */
    for(xs=orphans; nonNull(xs);xs=tl(xs)) {
      ieList = augmentEntity(FALSE,fst(hd(xs)),snd(hd(xs)),ieList);
    }
  }
  return ieList;
}          

Void fixupImportExports (ls)
List ls; {
    List xs;
    for(xs=ls;nonNull(xs);xs=tl(xs)) {
      if ( isPair(hd(xs)) && !fst(snd(hd(xs))) ) {
        snd(snd(hd(xs))) = fixupIEList(snd(snd(hd(xs))));
      }
    }
}

static List local checkImportEntity(imports,exporter,isHidden,entity)
List   imports; /* Accumulated list of things to import */
Module exporter;
Bool   isHidden;
Cell   entity; { /* Entry from import/hiding list */
    Bool impFound    = FALSE;
    Bool isId        = isIdent(entity);
    Cell subEntities = !isId ? snd(entity) : NIL;
    Text t           = isId ? textOf(entity) : textOf(fst(entity));
    List es          = module(exporter).exports;
    Bool lookForVar  = isVar(entity);

    /* In H98, a data con may be named in a 'hiding' list, so we
     * have to grovel around inside each tycon looking for it.
     */
    Bool lookForDataCon = 
      isHidden &&
      subEntities == NONE && 
      isCon(isId ? entity : fst(entity));
      
    /* The use of NONE heralds a dcon */
    subEntities = ( (subEntities == NONE) ? NIL : subEntities);
    
    for(; nonNull(es); es=tl(es)) {
	Cell e = hd(es); /* :: Entity | (Entity, NIL|DOTDOT|[Entity]) */
	if (isPair(e)) {
	    Cell f = fst(e);
	    if (isTycon(f)) {
		if (tycon(f).text == t) {
		    impFound = TRUE;
		    if (!isId) {
			switch (tycon(f).what) {
			case NEWTYPE:
			case DATATYPE:
			    if (DOTDOT == subEntities) {
				/* Want all dcons that are _exported_ by
				   the importing module.
				*/
				Cell dcons;
				if (snd(e) == DOTDOT) {
				    dcons = tycon(f).defn;
				} else {
				    dcons = snd(e);
				}
				imports=addEntity(f,dcons,imports);
			    } else if ( NIL == subEntities) {
				imports=addEntity(f,NIL,imports);
			    } else {
				List xs = NIL;
				xs = checkSubentities(xs, subEntities, tycon(f).defn,"constructor of type",t);
				imports=addEntity(f,xs,imports);
			    }
			    break;
			case SYNONYM:
			case RESTRICTSYN:
			    imports=addEntity(f,DOTDOT,imports);
			    break;
			default:;
			  /* deliberate fall thru */
			    }
		    } else {
			imports = addEntity(f,NIL,imports);
		    }
		    if (!lookForDataCon) break;
		}
		/* check the data constructors or field labels for match */
		if (tycon(f).what != SYNONYM     && 
		    tycon(f).what != RESTRICTSYN &&
		    (lookForVar || lookForDataCon)) {
		    /* The type's exported dcons/fields */
		    Cell dcons;
		    if (snd(e) == DOTDOT) {
			dcons = tycon(f).defn;
		    } else {
			dcons = snd(e);
		    }
		    while(nonNull(dcons)) {
			if (isName(hd(dcons)) && name(hd(dcons)).text == t) {
			    impFound = TRUE;
			    imports=addEntity(hd(dcons),NIL,imports);
			    break;
			}
			dcons=tl(dcons);
		    }
		}
	    } else if (isClass(f)) {
		List sigs = NIL;
		/* Want all members that are _exported_ by the importing module. */
		if (isPair(e)) {
		    if (snd(e) == DOTDOT) {
			sigs = cclass(f).members;
		    } else {
			sigs = snd(e);
		    }
		}
		if (cclass(f).text == t) {
		    impFound = TRUE;
		    if (!isId) {
			if (DOTDOT == subEntities) {
			    imports=addEntity(f,sigs,imports);
			    return imports;
			} else if ( NIL == subEntities) {
			    imports=addEntity(f,NIL,imports);
			} else {
			    List xs = NIL;
			    xs = checkSubentities(xs, subEntities, cclass(f).members,"member of class",t);
			    imports=addEntity(f,xs,imports);
			}
		    }
		    break;
		}
		if (!impFound && isId) {
		    List xs = sigs;
		    while(nonNull(xs)) {
			if (isName(hd(xs)) && name(hd(xs)).text == t) {
			    impFound = TRUE;
			    imports=cons(hd(xs),imports);
			    break;
			}
			xs=tl(xs);
		    }
		}
	    } else {
		internal("checkImportEntity2");
	    }
	} else if (isName(e)) {
	    if (isId && name(e).text == t) {
		impFound = TRUE;
		imports=cons(e,imports);
		if (!lookForDataCon) break;
	    }
	} else if (isTycon(e)) {
	    if (isId && tycon(e).text == t) {
		impFound = TRUE;
		imports = addEntity(e,NIL,imports);
		if (!lookForDataCon) break;
	    }
	} else {
	    internal("checkImportEntity3");
	}
    }
    if (!impFound) {
	ERRMSG(0) "Unknown entity \"%s\" %s from module \"%s\"",
                  textToStr(t), ((!isHidden) ? "imported" : "hidden"),
		  textToStr(module(exporter ).text)
	EEND;
    }
    return imports;
}

static List local resolveImportList(m,impList,isHidden)
Module m;  /* exporting module */
Cell   impList;
Bool   isHidden; {
    List imports = NIL;
    if (DOTDOT == impList) {
	List es = module(m).exports;
	for(; nonNull(es); es=tl(es)) {
	    Cell e = hd(es);
	    if (isName(e)) {
		imports = cons(e,imports);
            } else {
                Cell c = fst(e);
                List subentities = NIL;
                if ( isClass(c) || 
                     (isTycon(c)
                       && (tycon(c).what == DATATYPE ||
                           tycon(c).what == NEWTYPE)) ) {
                    if (snd(e) != DOTDOT) {
                        List ys = snd(e);
                        Name sub;
                        while (nonNull(ys)) {
                            if (isPair(hd(ys))) {
                                if (nonNull(sub = findQualName(hd(ys)))) {
                                  subentities = cons (sub,subentities);
                                }
                            } else {
                                subentities = cons(hd(ys),subentities);
                            }
                            ys=tl(ys);
                        }
			subentities = rev(subentities);
                    } else {
                        if (isClass(c)) {
                            subentities = cclass(c).members;
                        } else {
                            subentities = tycon(c).defn;
                        }
                    }
                }
                imports = addEntity(c,subentities,imports);
            }
        }
	imports = rev(imports);
    } else {
        map2Accum(checkImportEntity,imports,m,isHidden,impList);
    }
    return imports;
}

Void checkQualImportList(importSpec)
Pair importSpec; {
  /* checkQualImport() has verified that the module has already been loaded;
   * just locate the Module for it here & update the qualImports.
   */
  Module m = findModid(fst(importSpec));
  fst(importSpec) = m;

  checkImportList(TRUE,importSpec);
}

static List local addEntityPair(e,is) /* For pair (e,ls) add ls to the 'is' */
                                      /* import/export list.                   */
Cell e;
List is; {
  if (isPair(e)) {
    return addEntity(fst(e),snd(e), is);
  } else if (isName(e)) {
      if (isClass(name(e).parent)) {
        return addEntity(name(e).parent, singleton(e),is);
      } else if (isTycon(name(e).parent) && isCfun(e)) {
        return addEntity(name(e).parent, singleton(e),is);
      } else if (isSfun(e)) {
        /* a field name */
        Cell p = name(e).parent;      /* the data constructor */
        Cell t = name(p).parent;      /* the type constructor */
        return addEntity(t, singleton(e), is);
      }
  }
  return addEntity(e,NIL,is);
}

static List local augmentEntity(addNew,e,ls,is) 
Bool addNew;                      /* For entity e, add 'ls' to the 'is'   */
Cell e;                           /* import/export list,combining it with */
Cell ls;                          /* previous entries (if any.)           */
List is; {

    Cell ms = entityIsMember(e,is);
    
    if (!ms) {
      if (!addNew) {
        return is;
      } else {
        if (isName(e) && (ls == NIL || ls == NONE)) {
           return cons(e,is);
        } else {
            return cons(pair(e,ls),is);
        }
      }
    } else {
        /* concat the two lists; remove duplicates. */
        if (!isPair(hd(ms)) && ls != NIL) {
            hd(ms) = pair(e,ls);
        } else if (ls == NIL || ls == NONE || snd(hd(ms)) == DOTDOT) {
            ;
        } else if (ls == DOTDOT || snd(hd(ms)) == NIL || snd(hd(ms)) == NONE) {
            snd(hd(ms)) = ls;
        } else {
            snd(hd(ms)) = nubList(dupOnto(ls,snd(hd(ms))));
        }
        return is;
    }
}

static List local addEntity(e,ls,is)
Cell e;
Cell ls;
List is; {
  return augmentEntity(TRUE,e,ls,is);
}

static Cell local entityIsMember(x,xs) /* Test for membership of specific  */
Cell x;                                /* entity x in import/export list xs  */
List xs; {
    for (; nonNull(xs); xs=tl(xs)) {
        if (x == hd(xs))
            return xs;
        if (isPair (hd(xs)) && x==fst(hd(xs)))
            return xs;
    }
    return NIL;
}

static List local mergeImportLists(ls1,ls2)
List ls1;
List ls2; {
  List xs;

  for (xs=ls1;nonNull(xs);xs=tl(xs)) {
      ls2 = addEntityPair(hd(xs),ls2);
  }
  return ls2;
}

Void checkImportList(isQual,importSpec) /*Import a module (un)qualified*/
Bool isQual;
Pair importSpec; {
    Module m       = fst(importSpec);
    Cell   impList = snd(importSpec);

    List   imports = NIL; /* entities we want to import */
    List   hidden  = NIL; /* entities we want to hide   */

    List   modImps = NIL; /* The effective import list */
    List   es = NIL;
    Bool   isHidden = (isPair(impList) && HIDDEN == fst(impList));

    if (!isQual && moduleThisScript(m)) { 
        ERRMSG(0) "Module \"%s\" recursively imports itself",
                  textToStr(module(m).text)
        EEND;
    }
    if ( isHidden ) {
        List orphans;
        /* Somewhat inefficient - but obviously correct:
         * imports = importsOf("module Foo") `setDifference` hidden;
	 */
        hidden  = fixupIEList(resolveImportList(m, snd(impList),TRUE));
	imports = resolveImportList(m, DOTDOT,FALSE);
	
	/* Get the lone method/field/dcons that appear in the hiding list. */
	orphans = getIEOrphans(hidden);
	
	/* remove them from their parents in the import list. */
	for (;nonNull(orphans);orphans=tl(orphans)) {
	  /* the 'hd.snd' is the orphan entity, 'fst' is its parent. */
	  
	  /* Locate and remove the sub-entity. */
	  Cell ls = entityIsMember(fst(hd(orphans)), imports);
	  if (ls && isPair(hd(ls))) {
	    snd(hd(ls)) = removeCell(hd(snd(hd(orphans))), dupList(snd(hd(ls))));
	  }
	}

	/* With the orphans in the 'hiding' list accounted for,
	 * compute the effective import list by traversing over the
	 * entire import list, checking whether any of the entities
	 * do appear in the hiding list.
	 */
	for(; nonNull(imports); imports=tl(imports)) {
	  Cell e = hd(imports);

	  if (isPair(e)) {
	    /* A tycon/class */
	    Cell tc   = fst(e);
	    Cell subs = snd(e);
	    List ms = entityIsMember(tc,hidden);
	    
	    if (!ms) {
	      /* not in the hiding list, add it to effective import list. */
	      if (isQual) {
		modImps = cons(pair(tc,subs),modImps);
	      } else {
		modImps = cons(importEntity(m,e), modImps);
	      }
	    } else if isPair(hd(ms)) {
		/* The parent tycon/class is hidden, but perhaps
		   not all of its subentities. */
	        Cell ent = fst(hd(ms));
		List hiddenSubs = snd(hd(ms));
		Module impMod;
		
		/* Figure out what module the entity was imported from */
		if ( isClass(ent) ) {
		  impMod = cclass(ent).mod;
		} else if ( isTycon(ent) ) {
		  impMod = tycon(ent).mod;
		} else {
		  internal("checkImportList");
		}
		
		for(;nonNull(subs);subs=tl(subs)) {
		  if (!entityIsMember(hd(subs),hiddenSubs)) {
		    /* Register the sub-entity as imported */
		    if (!isQual)
		      importName(impMod, hd(subs));
		    modImps = cons(hd(subs), modImps);
		  }
		}
	    }
	  } else {
	    if (!entityIsMember(e,hidden)) {
	      if (isQual) {
		modImps = cons(pair(e,NIL),modImps);
	      } else {
		modImps = cons(importEntity(m,e), modImps);
	      }
	    }
	  }
	}
	
    } else {  /* the more common case, no hidings. */
	imports = resolveImportList(m, impList,FALSE);
	if (isQual) {
	  modImps = imports;
	} else {
	  for(; nonNull(imports); imports=tl(imports)) {
	      modImps = addEntityPair(importEntity(m,hd(imports)), modImps);
	  }
	}
    }
    /* To be able to handle re-exportation of modules, each module
     * keeps track of the effective import list of all its imports,
     * so that we later on can constrain re-exportation to only
     * contain what was imported.
     */

    /* If there's more than one import decl for the same module,
     * combine the import lists.
     */
    for(es=module(currentModule).modImports;nonNull(es);es=tl(es)) {
      if (isPair(hd(es)) && fst(hd(es)) == m) {
	fst(snd(hd(es))) = FALSE; /* => perform fixup at the end. */
	snd(snd(hd(es))) = mergeImportLists(modImps, snd(snd(hd(es))));
	break;
      }
    }
    if (isNull(es)) {
      /* Module not already present, add it. */
      module(currentModule).modImports = 
	 cons(pair(m,pair(isHidden,modImps)),module(currentModule).modImports);
    }
}

static Cell local importEntity(source,e)
Module source;
Cell e; {
    Cell ent = e;
    Cell cs  = NIL;

    /* If a pair, then the snd component gives the
     * constructors/methods that are specifically imported
     * with the tycon/class.
     */
    if ( isPair(e) ) {
	ent = fst(e);
	cs  = snd(e);
    }
    if (cs != NIL && cs != DOTDOT) {
      List xs;
      for (xs=cs;nonNull(xs);xs=tl(xs)) {
	importEntity(source,hd(xs));
      }
    }
    switch (whatIs(ent)) {
      case VARIDCELL  : 
      case VAROPCELL  : 
      case CONIDCELL  : 
      case CONOPCELL  : 
	           importName(source,snd(ent));
	           return e;
      case NAME  : 
	           importName(source,ent); 
	           return e;
      case TYCON : 
	           importTycon(source,ent); 
	           return pair(ent,cs);
      case CLASS : importClass(source,ent);
	           return pair(ent,cs);
		   
    default:   
               internal("importEntity");
	       return NIL;
    }
}

Void importName(source,n)
Module source;
Name n; {
    Name clash = addName(n);
    if (nonNull(clash) && clash!=n
	/* 'n' contains a name imported from another module's
	 * export list. Due to module re-exportation, its 'home
	 * module' (i.e., the module where 'n' was actually declared)
	 * may not be equal to that of the module we're now importing
	 * from here ('source'.) So, we've only got a name clash if
	 * the home module of 'n' is different from that of 'clash'.
	 */
     && name(n).mod != name(clash).mod ) {
      name(clash).clashes = cons(n,name(clash).clashes);
    }
}

static Void local importTycon(source,tc)
Module source;
Tycon tc; {
    Tycon clash=addTycon(tc);
    Class cc;
    if (nonNull(clash) && clash!=tc
      /* See importName() comment. */
     && tycon(tc).mod != tycon(clash).mod ) {
      tycon(clash).clashes = cons(tc,tycon(clash).clashes);
    }
    if ( nonNull(cc = findClass(tycon(tc).text)) ) {
      cclass(cc).clashes = cons(tc,cclass(cc).clashes);
    }
}

static Void local importClass(source,c)
Module source;
Class c; {
    Class clash=addClass(c);
    if (nonNull(clash) && clash!=c
      /* See importName() comment. */
     && cclass(c).mod != cclass(clash).mod ) {
	/* Hmm..don't quite understand why we need to record the clash
	   on both the class values here..*/
        cclass(c).clashes = cons(clash,cclass(c).clashes);
        cclass(clash).clashes = cons(c,cclass(clash).clashes);
    }
    if (nonNull(findTycon(cclass(c).text))) {
        cclass(clash).clashes = cons(c,cclass(clash).clashes);
    }
}

static Module local modOfEntity(ent) /* get at the module of name/tycon/class */
Cell ent; {
  if (isName(ent)) {
      return name(ent).mod;
  } else if (isTycon(ent)) {
      return tycon(ent).mod;
  } else if (isClass(ent)) {
      return cclass(ent).mod;
  }
  return NIL;
}

static Void local reportAmbigEntity(mt,t,clashes) 
Text mt;
Text t;
List clashes; {
    if (nonNull(clashes)) {
	/* Unqualified name is ambiguous, report this. */
	Module m1;
	ERRMSG(0) "Ambiguous export of entity \"%s\"",
	    textToStr(t) ETHEN
	    ERRTEXT "\n*** Could refer to: %s.%s ",
	    textToStr(mt),
	    textToStr(t) ETHEN
	    for(;nonNull(clashes);clashes=tl(clashes)) {
		m1 = modOfEntity(hd(clashes));
		
		if (m1) {
		    ERRTEXT "%s.%s ", 
			textToStr(module(m1).text),
			textToStr(t)
			ETHEN
		}
	    }
	ERRTEXT "\n" EEND;
    }
}

/* verify that the entity is unique in unqualified form */
static Void local checkExportDistinct(exports,ambigCheck,ent) 
List exports;  
Bool ambigCheck;
Cell ent; {
  Name  clashNm;
  Tycon clashTc;
  Class clashCc;
  Module mod1,mod2;
  Text txt;
  Bool inConflict = FALSE;
  List clashes = NIL;

  if ( isName(ent) ) {
      clashes = name(ent).clashes;
      txt     = name(ent).text;
      mod1    = name(ent).mod;

      if ( (clashNm = nameInIEList(ent,exports)) &&
	   (name(clashNm).mod != name(ent).mod) ) {
	  mod2 = name(clashNm).mod;
	  inConflict = TRUE;
      }
  } else if ( isTycon(ent) ) {
      clashes = tycon(ent).clashes;
      txt     = tycon(ent).text;
      mod1    = tycon(ent).mod;
      
      if ( (clashTc = tyconInIEList(tycon(ent).text,exports)) &&
	   (tycon(clashTc).mod != tycon(ent).mod) ) {
	  mod2 = tycon(clashTc).mod;
	  inConflict = TRUE;
      }
  } else if ( isClass(ent) ) {
      clashes = cclass(ent).clashes;
      txt     = cclass(ent).text;
      mod1    = cclass(ent).mod;
      
      if ( (clashCc = classInIEList(cclass(ent).text,exports)) &&
	   (cclass(clashCc).mod != cclass(ent).mod) ) {
	  mod2 = cclass(clashCc).mod;
	  inConflict = TRUE;
      }
  } else if (isPair(ent)) {
      List subs = NIL;
      checkExportDistinct(exports, ambigCheck, fst(ent));
      if (snd(ent) == DOTDOT) {
	if (isTycon(fst(ent))) {
	  if (tycon(fst(ent)).what == SYNONYM ||
	      tycon(fst(ent)).what == RESTRICTSYN) {
	    subs = NIL;
	  } else {
	    subs = tycon(fst(ent)).defn;
	  }
	} else if (isClass(fst(ent))) {
	  subs = cclass(fst(ent)).members;
	} 
      } else {
	subs = snd(ent);
      }
      map2Proc(checkExportDistinct,exports,ambigCheck,subs);
      return;
  } else {
    return;
  }
      
  if (inConflict) {
      ERRMSG(0) "Conflicting exports of entity \"%s\"",
                textToStr(txt) ETHEN
      ERRTEXT "\n*** Could refer to %s.%s or %s.%s",
	      textToStr(module(mod1).text),
    	      textToStr(txt),
	      textToStr(module(mod2).text),
	      textToStr(txt)
      EEND;
  }
  
  if (ambigCheck && nonNull(clashes)) {
      reportAmbigEntity(module(mod1).text,txt,clashes);
  }
}

static List local checkExportTycon(exports,mt,viaModExport,spec,tc)
List  exports;
Text  mt;
Bool  viaModExport;
Cell  spec;
Tycon tc; {
    checkExportDistinct(exports,!viaModExport,pair(tc,spec));
    if (DOTDOT == spec || SYNONYM == tycon(tc).what) {
	return addEntity(tc,DOTDOT,exports);
    } else {
	return addEntity(tc,NIL,exports);
    }
}

static List local checkExportClass(exports,mt,viaModExport,spec,cl)
List  exports;
Text  mt;
Bool  viaModExport;
Class cl;
Cell  spec; {
    checkExportDistinct(exports,!viaModExport,pair(cl,spec));
    if (DOTDOT == spec) {
	return addEntity(cl,DOTDOT,exports);
    } else {
	return addEntity(cl,NIL,exports);
    }
}

static List local checkExportModule(exports,mt,e) 
List exports;
Text mt;
Cell e; {
  /* The name refers to the module alias; get at the modules it refers to */
    Text alias = textOf(snd(e));
    List mods = findQualifiers(alias);
    Module m;

    /* Re-exporting a module we didn't import isn't allowed. */
    if (isNull(mods)) {
        ERRMSG(0) "Unknown module \"%s\" exported from module \"%s\"",
	          textToStr(alias),
	          textToStr(mt)
	EEND;
    }
    for (;nonNull(mods);mods=tl(mods)) {
         m = hd(mods);
	 if (m == currentModule) {
	     /* Exporting the current module exports all local definitions */
	     List xs;

	     for(xs=module(m).classes; nonNull(xs); xs=tl(xs)) {
	       if (cclass(hd(xs)).mod==m) 
	           exports = checkExportClass(exports,mt,TRUE,DOTDOT,hd(xs));
	     }
	     for(xs=module(m).tycons; nonNull(xs); xs=tl(xs)) {
	       if (tycon(hd(xs)).mod==m) 
		   exports = checkExportTycon(exports,mt,TRUE,DOTDOT,hd(xs));
	     }
	     for(xs=module(m).names; nonNull(xs); xs=tl(xs)) {
	         if (name(hd(xs)).mod==m && 
		     /* don't add dcons or class members */
		     (!isCfun(hd(xs)) &&
		      !isClass(name(hd(xs)).parent))) {
		     checkExportDistinct(exports,FALSE,hd(xs));
		     exports = cons(hd(xs),exports);
		 }
	     }
	 } else {
	     /* Re-exporting a module alias M exports all unqualified
	      * entities that have been imported into scope by modules
	      * having that alias _and_ for which the qualified (by
	      * the _alias_ M) entities are also visible.
	      */
	     List xs = module(currentModule).modImports;
	     List ents = NIL;
	
	     for(;nonNull(xs);xs=tl(xs)) {
	       if (isPair(hd(xs)) && fst(hd(xs)) == m) {
		   ents = snd(snd(hd(xs)));
		   break;
	       }
	     }
	     if (isNull(xs)) {
	         ents = module(m).exports;
	     }
	     for(;nonNull(ents);ents=tl(ents)) {
	       Cell  qid;
	       Text  txtNm;
	       Cell ent = NIL;
	       Name  nm;
	       Tycon tc;
	       Class cc;
	       
	       /* Build the (alias) qualified entity and test whether it's
		  in scope -- ugly. */
	       if (isName(hd(ents))) {
		   txtNm = name(hd(ents)).text;
		   qid = mkQId(alias,mkVar(txtNm));
	       } else if (isTycon(hd(ents))) {
		   txtNm = tycon(hd(ents)).text;
		   qid = mkQId(alias,mkCon(txtNm));
	       } else if (isClass(hd(ents))) {
		   txtNm = cclass(hd(ents)).text;
		   qid = mkQId(alias,mkCon(txtNm));
	       } else if (isQualIdent(hd(ents))) {
		   txtNm = qtextOf(hd(ents));
		   qid = mkQId(alias,snd(snd(hd(ents))));
	       } else {
		   /* ({tycon|class}, [entity]) */
		   if (isTycon(fst(hd(ents)))) {
		       txtNm = tycon(fst(hd(ents))).text;
		       qid = mkQId(alias,mkCon(txtNm));
		   } else if (isClass(fst(hd(ents)))) {
		       txtNm = cclass(fst(hd(ents))).text;
		       qid = mkQId(alias,mkCon(txtNm));
		   } else {
		       internal("checkExportModule");
		   }
	       }
	       /* Decide whether an entity E is to be exported;
	        * it needs to satisfy the following conditions:
		*
		*  - it needs to be visible in unqualified form,
		*    _unambiguously_.
		*  - it is also available as A.E (where A is the
		*    alias used in the module re-exportation element
		*    in the export list.)
		*  - the two names refer to the same (declared) name.
		*/
	       if ( ( (ent = findName(txtNm))            && 
		      !isNull((nm = findQualName(qid)))  &&
		      (nonNull((name(ent).clashes)) ||
		       name(ent).mod == name(nm).mod))        ||
		    ( (ent = findTycon(txtNm))           &&
		      !isNull((tc = findQualTycon(qid))) &&
		      (nonNull((tycon(ent).clashes)) ||
		       tycon(ent).mod == tycon(tc).mod))       ||
		    ( (ent = findClass(txtNm))           && 
		      !isNull((cc = findQualClass(qid))) &&
		      (nonNull((cclass(ent).clashes)) ||
		       cclass(ent).mod == cclass(cc).mod))   ) {

		   checkExportDistinct(exports,FALSE,hd(ents));
		   exports=cons(hd(ents),exports);
	       }
	     }
	 }
    }
    return exports;
}

static List local allMethodsOrDCons(imps,nm,mod,wantMethods)
List   imps;
Cell   nm;
Module mod;
Bool   wantMethods; {
  /* For a non-local tycon / class exported using (..), locate
   * the list of dcons/methods that are in scope.
   *
   * This requires going through all the import lists, locating
   * the tycon/class and take the union of all the dcons/methods
   * found.
   */
  List xs;
  List resList = NIL;

  for (xs = imps; nonNull(xs); xs=tl(xs)) {
    if ( isPair(hd(xs)) && isPair(snd(hd(xs))) ) {
      List ns;
      /* Find the entry for 'nm' tycon.. */
      for (ns = snd(snd(hd(xs))); nonNull(ns); ns=tl(ns)) {
	if ( isPair(hd(ns)) && 
	     ((!wantMethods         &&
	       isTycon(fst(hd(ns))) &&
	       fst(hd(ns)) == nm    &&
	       tycon(fst(hd(ns))).mod == mod) ||
	      (wantMethods          &&
	       isClass(fst(hd(ns))) &&
	       fst(hd(ns)) == nm    &&
	       cclass(fst(hd(ns))).mod == mod)) ) {
	  resList=dupOnto(snd(hd(ns)),resList);
	  /* Assumption: tycon/class may appear more than
	     once in an import list; */
	}
      }
    }
  }
  if (nonNull(resList)) { resList = nubList(resList); }
  return resList;
}

static List local checkExport(exports,mt,e) /* Process entry in export list */
List exports;
Text mt; 
Cell e; {
    if (isIdent(e)) {
	Name export;
	Bool expFound = FALSE;

	if (isQCon(e) && nonNull(export=findQualTycon(e))) {
	    expFound = TRUE;
	    exports = checkExportTycon(exports,mt,FALSE,NIL,export);
	} else if (isQCon(e) && nonNull(export=findQualClass(e))) {
	    /* opaque class export */
	    expFound = TRUE;
	    exports = checkExportClass(exports,mt,FALSE,NIL,export);
	} else if (nonNull(export=findQualName(e))) {
	    /* Data constructors cannot appear in export lists,
	     * so flag an error if they do.
	     *
	     * Notice that we have to be a bit careful when testing
	     * for this, as both data constructors and type synonyms
	     * have a tycon as parent. (In the case of type synonyms,
	     * the parent is the type on the RHS.)
	     *
	     */
	  if (  isCfun(export)    &&
	       !isPreludeScript() && 
	        currentModule != moduleUserPrelude) {
	    /* Special case reqd for Prelude(s) to handle (:) */
		ERRMSG(0) "Illegal export of a lone data constructor \"%s\"",
		          textToStr(name(export).text)
	        EEND;
	  }
	  expFound = TRUE;
	  /* Re-use static analysis code to verify that 
	   * a qualified export isn't ambiguous. Unqualified
	   * ones are better handled by checkExportDistinct().
	   */
	  if (isQualIdent(e)) {
	    depExpr(1,e);
	  }
	  checkExportDistinct(exports,!isQualIdent(e),export);
	  exports=cons(export,exports);
	}
	if (!expFound) {
	    ERRMSG(0) "Unknown entity \"%s\" exported from module \"%s\"",
		      identToStr(e),
		      textToStr(mt)
	    EEND;
	}
	return exports;
    } else if (MODULEENT == fst(e)) {
	return checkExportModule(exports,mt,e);
    } else {
	Cell ident = fst(e); /* class name or type name */
	Cell parts = snd(e); /* members or constructors */
	Cell nm;
	if (isQCon(ident) && nonNull(nm=findQualTycon(ident))) {
	    switch (tycon(nm).what) {
	    case SYNONYM:
		if (DOTDOT!=parts) {
		    ERRMSG(0) "Explicit constructor list given for type synonym \"%s\" in export list of module \"%s\"",
			      identToStr(ident),
			      textToStr(mt)
		    EEND;
		}
		exports = addEntity(nm,DOTDOT,exports);
		return exports;
	    case RESTRICTSYN:	
		ERRMSG(0) "Transparent export of restricted type synonym \"%s\" in export list of module \"%s\"",
			  identToStr(ident),
			  textToStr(mt)
		EEND;
		return exports; /* Not reached */
	    case NEWTYPE:
	    case DATATYPE:
		if (DOTDOT==parts) {
		    Module thisModule = lastModule();
		    if ( tycon(nm).mod == thisModule ) {
		      exports = addEntity(nm,DOTDOT,exports);
		    } else {
		      exports = addEntity(nm,
					  allMethodsOrDCons(module(thisModule).modImports,
							    nm,
							    tycon(nm).mod,
							    FALSE),
					  exports);
		    }
		} else {
		  List ps = NIL;
		  ps = checkSubentities(ps,parts,tycon(nm).defn,
					"constructor of type",
					tycon(nm).text);
		  exports = addEntity(nm,ps,exports);
		}
		return exports;
	    default:
		internal("checkExport1");
	    }
	} else if (isQCon(ident) && nonNull(nm=findQualClass(ident))) {
	    if (DOTDOT == parts) {
		Module thisModule = lastModule();
		if ( cclass(nm).mod == thisModule ) {
		  exports = addEntity(nm,DOTDOT,exports);
		} else {
		  exports = addEntity(nm,
				      allMethodsOrDCons(module(thisModule).modImports,
							nm,
							cclass(nm).mod,
							TRUE),
				      exports);
		}
	    } else {
	      List ps = NIL;
	      ps = checkSubentities(ps,parts,cclass(nm).members,
				    "member of class",cclass(nm).text);
	      exports=addEntity(nm,ps,exports);
	    }
	    return exports;
	} else {
	    ERRMSG(0) "Explicit export list given for non-class/datatype \"%s\" in export list of module \"%s\"",
		      identToStr(ident),
		      textToStr(mt)
	    EEND;
	}
    }
    return exports; /*NOTUSED*/
}

List checkExports(exports)
List exports; {
    Module m  = lastModule();
    Text   mt = module(m).text;
    List   es = NIL;             /* [Entity | (Entity,DOTDOT|NIL|[Entity])] */

    /* To properly handle methods and field names that are exported 
     * separately from their class/type ('orphans'), we construct the effective
     * export list in two passes. First, we resolve and collect up
     * all the entities, be they orphans or not. Secondly, we fix up
     * this list, attempting to join up the each 'orphan' with its
     * parent, but only if that parent (class/tycon) is also exported.
     * i.e., just exporting a method _does not_ cause its class to
     * implicitly be added to the export list.
     *
     * This only applies to methods and field names; data constructors
     * cannot be exported on their own.
     */
    map1Accum(checkExport,es,mt,exports);
    es = fixupIEList(es);

#if DEBUG_MODULES
    for(xs=es; nonNull(xs); xs=tl(xs)) {
	Printf(" %s", textToStr(textOfEntity(hd(xs))));
    }
#endif
    return es;
}

/* --------------------------------------------------------------------------
 * Browsing module exports
 * ------------------------------------------------------------------------*/

Void browseModule(mod,all)
Module mod;
Bool all; {			/* include all names in scope in the module? */
    List exports = resolveImportList(mod, DOTDOT, FALSE);
    Printf("module %s where\n",textToStr(module(mod).text));
    if (all) {
	List all_names = dupList(module(mod).names);
	mapProc(browseName,rev(all_names));
    } else {
	mapProc(browseEntity,exports);
    }
}

static Void local browseEntity(entity)
Cell entity; {			 /* Entity | (Entity,[Entity]) */
    if (isName(entity)) {
	browseName(entity);
    } else {			 /* (Entity,[Entity]) */
	mapProc(browseName,snd(entity));
    }
}

static Void local browseName(nm)
Name nm; {
    /* unwanted artifacts, like lambda lifted values,
       are in the list of names, but have no types */
    if (nonNull(name(nm).type)) {
	printExp(stdout,nm);
	Printf(" :: ");
	printType(stdout,name(nm).type);
	if (isCfun(nm)) {
	    Printf("  -- data constructor");
	} else if (isMfun(nm)) {
	    Printf("  -- class member");
	} else if (isSfun(nm)) {
	    Printf("  -- selector function");
	}
	if (name(nm).primDef) {
	    Printf("  -- primitive");
	}
	Printf("\n");
    }
}
