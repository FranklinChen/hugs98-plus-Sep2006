/* --------------------------------------------------------------------------
 * This is the Hugs type checker
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: type.c,v $
 * $Revision: 1.81 $
 * $Date: 2005/12/10 11:25:13 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "output.h"
#include "subst.h"
#include "goal.h"
#include "opts.h"	/* needed for EXPLAIN_INSTANCE_RESOLUTION|MULTI_INST */

Bool catchAmbigs       = FALSE;		/* TRUE => functions with ambig.   */
					/* 	   types produce error	   */

Type typeArrow,   typeList;		/* Important primitive types	   */
Type typeUnit;

Module modulePrelude;
Module moduleUserPrelude;

Type typeInt;                          
Type typeInt8;                          
Type typeInt16;                          
Type typeInt32;                          
Type typeInt64;                          
Type typeWord;
Type typeWord8;
Type typeWord16;
Type typeWord32;
Type typeWord64;
Type typeFunPtr;
Type typePtr;
Type typeAddr;
Type typeFloat;
Type typeDouble;
Type typeChar;
Type typeForeignP;
Type typeForeign;
Type typeStable;
Type typeBool;
Type typeString;
#ifdef DOTNET
Type typeObject;
#endif

static Type typeInteger; 
static Type typeMaybe;
static Type typeOrdering;

Class classEq,    classOrd;		/* `standard' classes		   */
Class classIx,    classEnum;
Class classShow,  classRead;
Class classBounded;

Class classReal,       classIntegral;	/* `numeric' classes		   */
Class classRealFrac,   classRealFloat;
Class classFractional, classFloating;
Class classNum;

List stdDefaults;			/* standard default values	   */

Name nameFromInt, nameFromDouble;	/* coercion of numerics		   */
Name nameFromInteger;
Name nameEq,      nameCompare;		/* derivable names		   */
Name nameLe;
Name nameShowsPrec;
Name nameReadsPrec;
Name nameMinBnd,  nameMaxBnd;
Name nameIndex,	  nameInRange;
Name nameRange;
Name nameMult,	  namePlus;
Name nameTrue,	  nameFalse;		/* primitive boolean constructors  */
Name nameNil,     nameCons;		/* primitive list constructors	   */
Name nameJust,	  nameNothing;		/* primitive Maybe constructors	   */
Name nameLeft,    nameRight;		/* primitive Either constructors   */
Name nameUnit;				/* primitive Unit type constructor */
Name nameLT,	  nameEQ;		/* Ordering constructors	   */
Name nameGT;
Class classMonad;			/* Monads			   */
Name nameReturn,  nameBind, nameThen;   /* for translating do/monad comps  */
Name nameMFail;
Name nameGt;				/* for readsPrec		   */

#if MUDO
Class classMonadRec;			/* Recursive monads		   */
Name nameMFix;
#endif

#if    IO_MONAD
Type   typeIO;			        /* For the IO monad, IO 	   */
Type   typeProgIO;			/* For the IO monad, IO a	   */

Name   nameIOError, nameUserErr;	/* loosely coupled IOError cfuns   */
Name   namePermDenied;
Name   nameAlreadyExists, nameAlreadyInUse, nameDoesNotExist, nameIsFull;
Name   nameIllegal;
#endif
#if    IO_HANDLES
Name   nameEOFErr;
Name   nameProtocolError;
#endif
Name   nameArithException;
Name   nameArrayException;
Name   nameErrorCall;
Name   nameIOException;
Name   nameNoMethodError;
Name   nameNonTermination;
Name   namePatternMatchFail;
Name   nameRecConError;
Name   nameRecSelError;
Name   nameRecUpdError;

Name   nameOverflow;
Name   nameDivideByZero;
Name   nameIndexOutOfBounds;
Name   nameUndefinedElement;

#if TREX
Type  typeNoRow;			/* Empty row			   */
Type  typeRec;				/* Record formation		   */
Name  nameNoRec;			/* Empty record			   */
#endif

#if DOTNET
Name   nameNetException;
#endif


/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local emptyAssumption   Args((Void));
static Void   local enterBindings     Args((Void));
static Void   local leaveBindings     Args((Void));
static Int    local defType	      Args((Cell));
static Type   local useType	      Args((Cell));
static Void   local markAssumList     Args((List));
static Cell   local findAssum         Args((Text));
static Pair   local findInAssumList   Args((Text,List));
static List   local intsIntersect     Args((List,List));
static List   local genvarAllAss      Args((List));
static List   local genvarAnyAss      Args((List));
static Int    local newVarsBind       Args((Cell));
static Void   local newDefnBind       Args((Cell,Type));

static Void   local enterPendingBtyvs Args((Void));
static Void   local leavePendingBtyvs Args((Void));
static Cell   local patBtyvs	      Args((Cell));
static Void   local doneBtyvs	      Args((Int));
static Void   local enterSkolVars     Args((Void));
static Void   local leaveSkolVars     Args((Int,Type,Int,Int));

static Void   local typeError         Args((Int,Cell,Cell,String,Type,Int));
static Void   local reportTypeError   Args((Int,Cell,Cell,String,Type,Type));
static Void   local cantEstablish     Args((Int,String,Cell,Type,List));
static Void   local tooGeneral        Args((Int,Cell,Type,Type));

static Cell   local typeExpr          Args((Int,Cell));

static Cell   local typeAp	      Args((Int,Cell));
static Type   local typeExpected      Args((Int,String,Cell,Type,Int,Int,Bool));
static Void   local typeAlt	      Args((String,Cell,Cell,Type,Int,Int));
static Int    local funcType          Args((Int));
static Void   local typeCase          Args((Int,Int,Cell));
static Void   local typeComp	      Args((Int,Type,Cell,List));
static Cell   local typeMonadComp     Args((Int,Cell));
#if ZIP_COMP
static Cell   local typeZComp	      Args((Int,Type,Cell,List));
static Void   local typeCompy	      Args((Int,Type,List));
static Cell   local typeMonadZComp    Args((Int,Cell));
static List   local getPats           Args((List));
static Cell   local tupleUp           Args((List));
#endif
static Void   local typeDo	      Args((Int,Cell));
#if MUDO
static Void   local typeRecComp	      Args((Int,Type,Cell,List));
static Void   local typeMDo	      Args((Int,Cell));
static Void   local typeRecursiveDo   Args((Int,Cell));
#endif
static Void   local typeConFlds	      Args((Int,Cell));
static Void   local typeUpdFlds	      Args((Int,Cell));
#if IPARAM
static Cell   local typeWith	      Args((Int,Cell));
#endif
static Cell   local typeFreshPat      Args((Int,Cell));

static Void   local typeBindings      Args((List));
static Void   local removeTypeSigs    Args((Cell));

static Void   local monorestrict      Args((List));
static Void   local restrictedBindAss Args((Cell));
static Void   local restrictedAss     Args((Int,Cell,Type));

static Void   local unrestricted      Args((List));
static List   local itbscc	      Args((List));
static Void   local addEvidParams     Args((List,Cell));

static Void   local typeClassDefn     Args((Class));
static Void   local typeInstDefn      Args((Inst));
static Void   local typeMember	      Args((String,Name,Cell,List,Cell,Int));

static Void   local typeBind          Args((Cell));
static Void   local typeDefAlt        Args((Int,Cell,Pair));
static Cell   local typeRhs           Args((Cell));
static Void   local guardedType       Args((Int,Cell));

static Void   local genBind	      Args((List,Cell));
static Void   local genAss	      Args((Int,List,Cell,Type));
static Type   local genTest	      Args((Int,Cell,List,Type,Type,Int));
static Type   local generalize	      Args((List,Type));
static Bool   local equalTypes        Args((Type,Type));

static Void   local typeDefnGroup     Args((List));
static Void   local typeForeignExport Args((Name));
static Pair   local typeSel	      Args((Name));

static Name   local linkName          Args((String));
static Tycon  local linkTycon         Args((String));
static Class  local linkClass         Args((String));

/* --------------------------------------------------------------------------
 * Frequently used type skeletons:
 * ------------------------------------------------------------------------*/

static Type  arrow;			/* mkOffset(0) -> mkOffset(1)      */
static Type  boundPair;			/* (mkOffset(0),mkOffset(0))	   */
static Type  listof;			/* [ mkOffset(0) ] 	    	   */
static Type  typeVarToVar;		/* mkOffset(0) -> mkOffset(0)  	   */

static Cell  predNum;			/* Num (mkOffset(0))		   */
static Cell  predFractional;		/* Fractional (mkOffset(0))	   */
static Cell  predIntegral;		/* Integral (mkOffset(0))	   */
static Kind  starToStar;		/* Type -> Type			   */
static Cell  predMonad;			/* Monad (mkOffset(0))		   */
#if MUDO
static Cell  predMonadRec;		/* MonadRec/MonadFix (mkOffset(0)) */
#endif

/* --------------------------------------------------------------------------
 * Assumptions:
 *
 * A basic typing statement is a pair (Var,Type) and an assumption contains
 * an ordered list of basic typing statements in which the type for a given
 * variable is given by the most recently added assumption about that var.
 *
 * In practice, the assumption set is split between a pair of lists, one
 * holding assumptions for vars defined in bindings, the other for vars
 * defined in patterns/binding parameters etc.	The reason for this
 * separation is that vars defined in bindings may be overloaded (with the
 * overloading being unknown until the whole binding is typed), whereas the
 * vars defined in patterns have no overloading.  A form of dependency
 * analysis (at least as far as calculating dependents within the same group
 * of value bindings) is required to implement this.  Where it is known that
 * no overloaded values are defined in a binding (i.e., when the `dreaded
 * monomorphism restriction' strikes), the list used to record dependents
 * is flagged with a NODEPENDS tag to avoid gathering dependents at that
 * level.
 *
 * To interleave between vars for bindings and vars for patterns, we use
 * a list of lists of typing statements for each.  These lists are always
 * the same length.  The implementation here is very similar to that of the
 * dependency analysis used in the static analysis component of this system.
 *
 * To deal with polymorphic recursion, variables defined in bindings can be
 * assigned types of the form (POLYREC,(def,use)), where def is a type
 * variable for the type of the defining occurence, and use is a type
 * scheme for (recursive) calls/uses of the variable.
 * ------------------------------------------------------------------------*/

static List defnBounds;			/*::[[(Var,Type)]] possibly ovrlded*/
static List varsBounds;			/*::[[(Var,Type)]] not overloaded  */
static List depends;			/*::[?[Var]] dependents/NODEPENDS  */
static List skolVars;			/*::[[Var]] skolem vars		   */
static List localEvs;			/*::[[(Pred,offset,ev)]]	   */
static List savedPs;			/*::[[(Pred,offset,ev)]]	   */
static Cell dummyVar;			/* Used to put extra tvars into	ass*/

#define saveVarsAss()     List saveAssump = hd(varsBounds)
#define restoreVarsAss()  hd(varsBounds)  = saveAssump
#define addVarAssump(v,t) hd(varsBounds)  = cons(pair(v,t),hd(varsBounds))
#define findTopBinding(v) findInAssumList(textOf(v),hd(defnBounds))

static Void local emptyAssumption() {  	/* set empty type assumption	   */
    defnBounds = NIL;
    varsBounds = NIL;
    depends    = NIL;
    skolVars   = NIL;
    localEvs   = NIL;
    savedPs    = NIL;
}

static Void local enterBindings() {    /* Add new level to assumption sets */
    defnBounds = cons(NIL,defnBounds);
    varsBounds = cons(NIL,varsBounds);
    depends    = cons(NIL,depends);
}

static Void local leaveBindings() {    /* Drop one level of assumptions    */
    defnBounds = tl(defnBounds);
    varsBounds = tl(varsBounds);
    depends    = tl(depends);
}

static Int local defType(a)		/* Return type for defining occ.   */
Cell a; {				/* of a var from assumption pair  */
    return (isPair(a) && fst(a)==POLYREC) ? fst(snd(a)) : a;
}

static Type local useType(a)		/* Return type for use of a var	   */
Cell a; {				/* defined in an assumption	   */
    return (isPair(a) && fst(a)==POLYREC) ? snd(snd(a)) : a;
}

static Void local markAssumList(as)	/* Mark all types in assumption set*/
List as; {				/* :: [(Var, Type)]		   */
    for (; nonNull(as); as=tl(as)) {	/* No need to mark generic types;  */
	Type t = defType(snd(hd(as)));	/* the only free variables in those*/
	if (!isPolyType(t))		/* must have been free earlier too */
	    markType(t,0);
    }
}

static Cell local findAssum(t)	       /* Find most recent assumption about*/
Text t; {			       /* variable named t, if any	   */
    List defnBounds1 = defnBounds;     /* return translated variable, with */
    List varsBounds1 = varsBounds;     /* type in typeIs		   */
    List depends1    = depends;

    while (nonNull(defnBounds1)) {
	Pair ass = findInAssumList(t,hd(varsBounds1));/* search varsBounds */
	if (nonNull(ass)) {
	    typeIs = snd(ass);
	    return fst(ass);
	}

	ass = findInAssumList(t,hd(defnBounds1));     /* search defnBounds */
	if (nonNull(ass)) {
	    Cell v = fst(ass);
	    typeIs = snd(ass);

	    if (hd(depends1)!=NODEPENDS &&	      /* save dependent?   */
		  isNull(v=varIsMember(t,hd(depends1))))
		/* N.B. make new copy of variable and store this on list of*/
		/* dependents, and in the assumption so that all uses of   */
		/* the variable will be at the same node, if we need to    */
		/* overwrite the call of a function with a translation...  */
		hd(depends1) = cons(v=mkVar(t),hd(depends1));

	    return v;
	}

	defnBounds1 = tl(defnBounds1);		      /* look in next level*/
	varsBounds1 = tl(varsBounds1);		      /* of assumption set */
	depends1    = tl(depends1);
    }
    return NIL;
}

static Pair local findInAssumList(t,as)/* Search for assumption for var    */
Text t;				       /* named t in list of assumptions as*/
List as; {
    for (; nonNull(as); as=tl(as))
	if (textOf(fst(hd(as)))==t)
	    return hd(as);
    return NIL;
}

static List local intsIntersect(as,bs)	/* calculate intersection of lists */
List as, bs; {				/* of integers (as sets)	   */
    List ts = NIL;			/* destructively modifies as	   */
    while (nonNull(as))
	if (intIsMember(intOf(hd(as)),bs)) {
	    List temp = tl(as);
	    tl(as)    = ts;
	    ts	      = as;
	    as	      = temp;
	}
	else
	    as = tl(as);
    return ts;
}

static List local genvarAllAss(as)	/* calculate generic vars that are */
List as; {				/* in every type in assumptions as */
    List vs = genvarTyvar(intOf(defType(snd(hd(as)))),NIL);
    for (as=tl(as); nonNull(as) && nonNull(vs); as=tl(as))
	vs = intsIntersect(vs,genvarTyvar(intOf(defType(snd(hd(as)))),NIL));
    return vs;
}

static List local genvarAnyAss(as)	/* calculate generic vars that are */
List as; {				/* in any type in assumptions as   */
    List vs = genvarTyvar(intOf(defType(snd(hd(as)))),NIL);
    for (as=tl(as); nonNull(as); as=tl(as))
	vs = genvarTyvar(intOf(defType(snd(hd(as)))),vs);
    return vs;
}

static Int local newVarsBind(v)        /* make new assump for pattern var  */
Cell v; {
    Int beta = newTyvars(1);
    addVarAssump(v,mkInt(beta));
#if DEBUG_TYPES
    Printf("variable, assume ");
    printExp(stdout,v);
    Printf(" :: _%d\n",beta);
#endif
    return beta;
}

static Void local newDefnBind(v,type)  /* make new assump for defn var	   */
Cell v; 			       /* and set type if given (nonNull)  */
Type type; {
    Int  beta	   = newTyvars(1);
    Cell ta        = mkInt(beta);
    instantiate(type);
    if (nonNull(type) && isPolyType(type))
	ta = pair(POLYREC,pair(ta,type));
    hd(defnBounds) = cons(pair(v,ta), hd(defnBounds));
#if DEBUG_TYPES
    Printf("definition, assume ");
    printExp(stdout,v);
    Printf(" :: _%d\n",beta);
#endif
    bindTv(beta,typeIs,typeOff);       /* Bind beta to new type skeleton   */
}

/* --------------------------------------------------------------------------
 * Predicates:
 * ------------------------------------------------------------------------*/

#include "preds.c"

/* --------------------------------------------------------------------------
 * Bound and skolemized type variables:
 * ------------------------------------------------------------------------*/

static List pendingBtyvs = NIL;

static Void local enterPendingBtyvs() {
    enterBtyvs();
    pendingBtyvs = cons(NIL,pendingBtyvs);
}

static Void local leavePendingBtyvsQuietly() {
    pendingBtyvs = tl(pendingBtyvs);
    leaveBtyvs();
}

static Void local leavePendingBtyvs() {
    List pts     = hd(pendingBtyvs);
    pendingBtyvs = tl(pendingBtyvs);
    for (; nonNull(pts); pts=tl(pts)) {
	Int  line = intOf(fst(hd(pts)));
	List vs   = snd(hd(pts));
	Int  i    = 0;
	clearMarks();
	for (; nonNull(vs); vs=tl(vs)) {
	    Cell v = fst(hd(vs));
	    Cell t = copyTyvar(intOf(snd(hd(vs))));
	    if (!isOffset(t)) {
		ERRMSG(line) "Type annotation uses variable " ETHEN ERREXPR(v);
		ERRTEXT      " where a more specific type "   ETHEN ERRTYPE(t);
		ERRTEXT      " was inferred"
		EEND;
	    }
	    else if (offsetOf(t)!=i) {
		List us = snd(hd(pts));
		Int  j  = offsetOf(t);
		if (j>=i)
		    internal("leavePendingBtyvs");
		for (; j>0; j--)
		    us = tl(us);
		ERRMSG(line) "Type annotation uses distinct variables " ETHEN
		ERREXPR(v);  ERRTEXT " and " ETHEN ERREXPR(fst(hd(us)));
		ERRTEXT      " where a single variable was inferred"
		EEND;
	    }
	    else
		i++;
	}
    }
    leaveBtyvs();
}

static Cell local patBtyvs(p)		/* Strip bound type vars from pat  */
Cell p; {
    if (whatIs(p)==BIGLAM) {
	List bts = hd(btyvars) = fst(snd(p));
	for (p=snd(snd(p)); nonNull(bts); bts=tl(bts)) {
	    Int beta          = newTyvars(1);
	    tyvar(beta)->kind = snd(hd(bts));
	    snd(hd(bts))      = mkInt(beta);
	}
    }
    return p;
}

static Void local doneBtyvs(l)
Int l; {
    if (nonNull(hd(btyvars))) {		/* Save bound tyvars		   */
	hd(pendingBtyvs) = cons(pair(mkInt(l),hd(btyvars)),hd(pendingBtyvs));
	hd(btyvars)	 = NIL;
    }
}

static Void local enterSkolVars() {
    skolVars = cons(NIL,skolVars);
    localEvs = cons(NIL,localEvs);
    savedPs  = cons(preds,savedPs);
    preds    = NIL;
}

static Void local leaveSkolVars(l,t,o,m)
Int  l;
Type t;
Int  o;
Int  m; {
    if (nonNull(hd(localEvs))) {	/* Check for local predicates	   */
	List sks = hd(skolVars);
	List sps = NIL;
	if (isNull(sks)) {
	    internal("leaveSkolVars");
	}
	markAllVars();			/* Mark all variables in current   */
	do {				/* substitution, then unmark sks.  */
	    tyvar(intOf(fst(hd(sks))))->offs = UNUSED_GENERIC;
	    sks = tl(sks);
	} while (nonNull(sks));
	normPreds(l);
	sps   = elimPredsUsing(hd(localEvs),sps);
	preds = revOnto(preds,sps);
    }

    if (nonNull(hd(skolVars))) {	/* Check that Skolem vars do not   */
	List vs;			/* escape their scope		   */
	Int  i = 0;

	clearMarks();			/* Look for occurences in the	   */
	for (; i<m; i++)		/* inferred type		   */
	    markTyvar(o+i);
	markType(t,o);

	for (vs=hd(skolVars); nonNull(vs); vs=tl(vs)) {
	    Int vn = intOf(fst(hd(vs)));
	    if (tyvar(vn)->offs == FIXED_TYVAR) {
		Cell tv = copyTyvar(vn);
		Type ty = liftRank2(t,o,m);
		ERRMSG(l) "Existentially quantified variable in inferred type"
		ETHEN
		ERRTEXT   "\n*** Variable     : " ETHEN ERRTYPE(tv);
		ERRTEXT   "\n*** From pattern : " ETHEN ERREXPR(snd(hd(vs)));
		ERRTEXT   "\n*** Result type  : " ETHEN ERRTYPE(ty);
		ERRTEXT   "\n"
		EEND;
	    }
	}

	markBtyvs();			/* Now check assumptions	   */
	mapProc(markAssumList,defnBounds);
	mapProc(markAssumList,varsBounds);
	mapProc(markPred,preds);

	for (vs=hd(skolVars); nonNull(vs); vs=tl(vs)) {
	    Int vn = intOf(fst(hd(vs)));
	    if (tyvar(vn)->offs == FIXED_TYVAR) {
		ERRMSG(l)
		  "Existentially quantified variable escapes from pattern "
		ETHEN ERREXPR(snd(hd(vs)));
		ERRTEXT "\n"
		EEND;
	    }
	}
    }
    localEvs = tl(localEvs);
    skolVars = tl(skolVars);
    preds    = revOnto(preds,hd(savedPs));
    savedPs  = tl(savedPs);
}

/* --------------------------------------------------------------------------
 * Type errors:
 * ------------------------------------------------------------------------*/

static Void local typeError(l,e,in,wh,t,o)
Int    l;			      /* line number near type error	   */
Cell   e;			      /* source of error		   */
Cell   in;			      /* context if any (NIL if not)	   */
String wh;			      /* place in which error occurs	   */
Type   t;			      /* should be of type (t,o)	   */
Int    o; {			      /* type inferred is (typeIs,typeOff) */

    clearMarks();		      /* types printed here are monotypes  */
				      /* use marking to give sensible names*/
#if DEBUG_KINDS
{ List vs = genericVars;
  for (; nonNull(vs); vs=tl(vs)) {
     Int v = intOf(hd(vs));
     Printf("%c :: ", ('a'+tyvar(v)->offs));
     printKind(stdout,tyvar(v)->kind);
     Putchar('\n');
  }
}
#endif

    reportTypeError(l,e,in,wh,copyType(typeIs,typeOff),copyType(t,o));
}

static Void local reportTypeError(l,e,in,wh,inft,expt)
Int    l;				/* Error printing part of typeError*/
Cell   e, in;
String wh;
Type   inft, expt; {
    ERRMSG(l)   "Type error in %s", wh    ETHEN
    if (nonNull(in)) {
	ERRTEXT "\n*** Expression     : " ETHEN ERREXPR(in);
    }
    ERRTEXT     "\n*** Term           : " ETHEN ERREXPR(e);
    ERRTEXT     "\n*** Type           : " ETHEN ERRTYPE(inft);
    ERRTEXT     "\n*** Does not match : " ETHEN ERRTYPE(expt);
    if (unifyFails) {
	ERRTEXT "\n*** Because        : %s", unifyFails ETHEN
    }
    ERRTEXT "\n"
    EEND;
}

#define shouldBe(l,e,in,where,t,o) if (!unify(typeIs,typeOff,t,o)) \
				       typeError(l,e,in,where,t,o);
#define check(l,e,in,where,t,o)    e=typeExpr(l,e); shouldBe(l,e,in,where,t,o)
#define inferType(t,o)		   typeIs=t; typeOff=o
#if IPARAM
#define spTypeExpr(l,e)			svPreds = preds; preds = NIL; e = typeExpr(l,e); preds = revOnto(preds,svPreds);
#define spCheck(l,e,in,where,t,o)	svPreds = preds; preds = NIL; check(l,e,in,where,t,o); preds = revOnto(preds,svPreds);
#else
#define spTypeExpr(l,e)			e = typeExpr(l,e);
#define spCheck(l,e,in,where,t,o)	check(l,e,in,where,t,o);
#endif

static Void local cantEstablish(line,wh,e,t,ps)
Int    line;				/* Complain when declared preds	   */
String wh;				/* are not sufficient to discharge */
Cell   e;				/* or defer the inferred context.  */
Type   t;
List   ps; {
    ERRMSG(line) "Cannot justify constraints in %s", wh ETHEN
    ERRTEXT	 "\n*** Expression    : " ETHEN ERREXPR(e);
    ERRTEXT	 "\n*** Type          : " ETHEN ERRTYPE(t);
    ERRTEXT	 "\n*** Given context : " ETHEN ERRCONTEXT(ps);
    ERRTEXT	 "\n*** Constraints   : " ETHEN ERRCONTEXT(copyPreds(preds));
    ERRTEXT "\n"
    EEND;
}

static Void local tooGeneral(l,e,dt,it)	/* explicit type sig. too general  */
Int  l;
Cell e;
Type dt, it; {
    ERRMSG(l) "Inferred type is not general enough" ETHEN
    ERRTEXT   "\n*** Expression    : " ETHEN ERREXPR(e);
    ERRTEXT   "\n*** Expected type : " ETHEN ERRTYPE(dt);
    ERRTEXT   "\n*** Inferred type : " ETHEN ERRTYPE(it);
    ERRTEXT   "\n"
    EEND;
}

/* --------------------------------------------------------------------------
 * Typing of expressions:
 * ------------------------------------------------------------------------*/

#define EXPRESSION  0			/* type checking expression	   */
#define NEW_PATTERN 1			/* pattern, introducing new vars   */
#define OLD_PATTERN 2			/* pattern, involving bound vars   */
static int tcMode = EXPRESSION;

#if DEBUG_TYPES
static Cell local mytypeExpr	Args((Int,Cell));
static Cell local typeExpr(l,e)
Int l;
Cell e; {
    static int number = 0;
    Cell retv;
    int  mynumber = number++;
    STACK_CHECK
    Printf("%d) to check: ",mynumber);
    printExp(stdout,e);
    Putchar('\n');
    retv = mytypeExpr(l,e);
    Printf("%d) result: ",mynumber);
    printType(stdout,debugType(typeIs,typeOff));
    Printf("\n%d) preds: ",mynumber);
    printContext(stdout,debugContext(preds));
    Putchar('\n');
    return retv;
}
static Cell local mytypeExpr(l,e)	/* Determine type of expr/pattern  */
#else
static Cell local typeExpr(l,e)		/* Determine type of expr/pattern  */
#endif
Int  l;
Cell e; {
    static String cond	  = "conditional";
    static String list	  = "list";
    static String discr   = "case discriminant";
    static String aspat   = "as (@) pattern";
    static String typeSig = "type annotation";
    static String lambda  = "lambda expression";
#if IPARAM
    List svPreds;
#endif

    switch (whatIs(e)) {

	/* The following cases can occur in either pattern or expr. mode   */

	case AP		:
	case NAME	:
	case VAROPCELL	:
	case VARIDCELL	:
#if IPARAM
	case IPVAR	:
#endif
	  		  return typeAp(l,e);

	case TUPLE	: typeTuple(e);
			  break;

#if BIGNUMS
	case POSNUM	:
	case ZERONUM	:
	case NEGNUM	: {   Int alpha = newTyvars(1);
			      inferType(aVar,alpha);
			      return ap(ap(nameFromInteger,
					   assumeEvid(predNum,alpha)),
					   e);
			  }
#endif
	case INTCELL	: {   Int alpha = newTyvars(1);
			      inferType(aVar,alpha);
			      return ap(ap(nameFromInt,
					   assumeEvid(predNum,alpha)),
					   e);
			  }

	case DOUBLECELL	: {   Int alpha = newTyvars(1);
			      inferType(aVar,alpha);
			      return ap(ap(nameFromDouble,
					   assumeEvid(predFractional,alpha)),
					   e);
			  }

	case STRCELL	: inferType(typeString,0);
			  break;

	case CHARCELL	: inferType(typeChar,0);
			  break;

	case CONFLDS	: typeConFlds(l,e);
			  break;

	case ESIGN	: snd(snd(e)) = localizeBtyvs(snd(snd(e)));
			  return typeExpected(l,typeSig,
					      fst(snd(e)),snd(snd(e)),
					      0,0,FALSE);

#if TREX
	case EXT	: {   Int beta = newTyvars(2);
			      Cell pi  = ap(e,aVar);
			      Type t   = fn(aVar,
					 fn(ap(typeRec,bVar),
					    ap(typeRec,ap(ap(e,aVar),bVar))));
			      tyvar(beta+1)->kind = ROW;
			      inferType(t,beta);
			      return ap(e,assumeEvid(pi,beta+1));
			  }
#endif

	/* The following cases can only occur in expr mode		   */

	case UPDFLDS	: typeUpdFlds(l,e);
			  break;

#if IPARAM
	case WITHEXP	: return typeWith(l,e);
#endif

	case COND	: {   Int beta = newTyvars(1);
			      check(l,fst3(snd(e)),e,cond,typeBool,0);
			      spCheck(l,snd3(snd(e)),e,cond,aVar,beta);
			      spCheck(l,thd3(snd(e)),e,cond,aVar,beta);
			      tyvarType(beta);
			  }
			  break;

	case LETREC	: enterBindings();
			  enterSkolVars();
			  mapProc(typeBindings,fst(snd(e)));
			  spTypeExpr(l,snd(snd(e)));
			  leaveBindings();
			  leaveSkolVars(l,typeIs,typeOff,0);
			  break;

	case FINLIST	: {   Int  beta = newTyvars(1);
			      List xs;
			      for (xs=snd(e); nonNull(xs); xs=tl(xs)) {
				 spCheck(l,hd(xs),e,list,aVar,beta);
			      }
			      inferType(listof,beta);
			  }
			  break;

	case DOCOMP	: typeDo(l,e);
			  break;

#if MUDO
	case MDOCOMP	: typeMDo(l,e);
			  break;
#endif  
	case COMP	: return typeMonadComp(l,e);

#if ZIP_COMP
	case ZCOMP	: return typeMonadZComp(l,e);
#endif

	case CASE	: {    Int beta = newTyvars(2);    /* discr result */
			       check(l,fst(snd(e)),NIL,discr,aVar,beta);
			       map2Proc(typeCase,l,beta,snd(snd(e)));
			       tyvarType(beta+1);
			  }
			  break;

	case LAMBDA	: {   Int beta = newTyvars(1);
			      enterPendingBtyvs();
			      typeAlt(lambda,e,snd(e),aVar,beta,1);
			      leavePendingBtyvs();
			      tyvarType(beta);
			  }
			  break;

#if TREX
	case RECSEL	: {   Int beta = newTyvars(2);
			      Cell pi  = ap(snd(e),aVar);
			      Type t   = fn(ap(typeRec,
					       ap(ap(snd(e),aVar),
							    bVar)),aVar);
			      tyvar(beta+1)->kind = ROW;
			      inferType(t,beta);
			      return ap(e,assumeEvid(pi,beta+1));
			  }
#endif

	/* The remaining cases can only occur in pattern mode: */

	case WILDCARD	: inferType(aVar,newTyvars(1));
			  break;

	case ASPAT	: {   Int beta = newTyvars(1);
			      snd(snd(e)) = typeExpr(l,snd(snd(e)));
			      bindTv(beta,typeIs,typeOff);
			      check(l,fst(snd(e)),e,aspat,aVar,beta);
			      tyvarType(beta);
			  }
			  break;

	case LAZYPAT	: snd(e) = typeExpr(l,snd(e));
			  break;

#if NPLUSK
	case ADDPAT	: {   Int alpha = newTyvars(1);
			      inferType(typeVarToVar,alpha);
			      return ap(e,assumeEvid(predIntegral,alpha));
			  }
#endif

	default 	: internal("typeExpr");
   }

   return e;
}

/* --------------------------------------------------------------------------
 * Typing rules for particular special forms:
 * ------------------------------------------------------------------------*/

static Cell local typeAp(l,e)		/* Type check application, which   */
Int  l;					/* may be headed with a variable   */
Cell e; {				/* requires polymorphism, qualified*/
    static String app = "application";	/* types, and possible rank2 args. */
    Cell h = getHead(e);
    Int  n = argCount;
    Cell p = NIL;
    Cell a = e;
    Int  i;
#if IPARAM
    List svPreds;
#endif

    switch (whatIs(h)) {
	case NAME      : typeIs = name(h).type;
			 break;

	case VAROPCELL :
	case VARIDCELL : if (tcMode==NEW_PATTERN) {
			     inferType(aVar,newVarsBind(e));
			 }
			 else {
			     Cell v = findAssum(textOf(h));
			     if (nonNull(v)) {
				 h      = v;
				 typeIs = (tcMode==OLD_PATTERN)
						? defType(typeIs)
						: useType(typeIs);
			     }
			     else {
				 h = findName(textOf(h));
				 if (isNull(h))
				     internal("typeAp0");
				 typeIs = name(h).type;
			     }
			 }
			 break;
#if IPARAM
	case IPVAR    :	{   Text t    = textOf(h);
			    Int alpha = newTyvars(1);
			    Cell ip   = pair(ap(IPCELL,t),aVar);
			    Cell ev   = assumeEvid(ip,alpha);
			    typeIs    = mkInt(alpha);
			    h	      = ap(h,ev);
			}
			break;
#endif

	default        : h = typeExpr(l,h);
			 break;
    }

    if (isNull(typeIs))
       internal("typeAp1");

    instantiate(typeIs);		/* Deal with polymorphism ...	   */
    if (nonNull(predsAre)) {		/* ... and with qualified types.   */
	List evs = NIL;
	for (; nonNull(predsAre); predsAre=tl(predsAre)) {
	    evs = cons(assumeEvid(hd(predsAre),typeOff),evs);
	}
	if (!isName(h) || !isCfun(h)) {
	    h = applyToArgs(h,rev(evs));
	}
    }

    if (whatIs(typeIs)==CDICTS) {	/* Deal with local dictionaries	   */
	List evs = makePredAss(fst(snd(typeIs)),typeOff);
	List ps  = evs;
	typeIs   = snd(snd(typeIs));
	for (; nonNull(ps); ps=tl(ps)) {
	    h = ap(h,thd3(hd(ps)));
	}
	if (tcMode==EXPRESSION) {
	    preds = revOnto(evs,preds);
	} else {
	    hd(localEvs) = revOnto(evs,hd(localEvs));
	}
    }

    if (whatIs(typeIs)==EXIST) {	/* Deal with existential arguments */
	Int n  = intOf(fst(snd(typeIs)));
	typeIs = snd(snd(typeIs));
	if (!isCfun(getHead(h)) || n>typeFree) {
	    internal("typeAp2");
	} else if (tcMode!=EXPRESSION) {
	    Int alpha = typeOff + typeFree;
	    for (; n>0; n--) {
		bindTv(alpha-n,SKOLEM,0);
		hd(skolVars) = cons(pair(mkInt(alpha-n),e),hd(skolVars));
	    }
	}
    }

    if (whatIs(typeIs)==RANK2) {	/* Deal with rank 2 arguments	   */
	Int  alpha = typeOff;
	Int  m     = typeFree;
	Int  nr2   = intOf(fst(snd(typeIs)));
	Type body  = snd(snd(typeIs));
	List as    = e;
	Bool added = FALSE;

	if (n<nr2) {			/* Must have enough arguments	   */
	    ERRMSG(l)   "Use of " ETHEN ERREXPR(h);
	    if (n>1) {
		ERRTEXT " in "    ETHEN ERREXPR(e);
	    }
	    ERRTEXT     " requires at least %d argument%s\n",
			nr2, (nr2==1 ? "" : "s")
	    EEND;
	}

	for (i=nr2; i<n; ++i)		/* Find rank two arguments	   */
	    as = fun(as);

	for (as=getArgs(as); nonNull(as); as=tl(as), body=arg(body)) {
	    Type expect = dropRank1(arg(fun(body)),alpha,m);
	    if (isPolyOrQualType(expect)) {
		if (tcMode==EXPRESSION)		/* poly/qual type in expr  */
		    hd(as) = typeExpected(l,app,hd(as),expect,alpha,m,TRUE);
		else if (hd(as)!=WILDCARD) {	/* Pattern binding/match   */
		    if (!isVar(hd(as))) {
			ERRMSG(l) "Argument "    ETHEN ERREXPR(arg(as));
			ERRTEXT   " in pattern " ETHEN ERREXPR(e);
			ERRTEXT   " where a variable is required\n"
			EEND;
		    }
		    if (tcMode==NEW_PATTERN) {	/* Pattern match	   */
			if (m>0 && !added) {
			    for (i=0; i<m; i++)
				addVarAssump(dummyVar,mkInt(alpha+i));
			    added = TRUE;
			}
			addVarAssump(hd(as),expect);
		    }
		    else {			/* Pattern binding	   */
			Text t = textOf(hd(as));
			Cell a = findInAssumList(t,hd(defnBounds));
			if (isNull(a))
			    internal("typeAp3");
			instantiate(expect);
			if (nonNull(predsAre)) {
			    ERRMSG(l) "Cannot use pattern binding for " ETHEN
			    ERREXPR(hd(as));
			    ERRTEXT   " as a component with a qualified type\n"
			    EEND;
			}
			shouldBe(l,hd(as),e,app,aVar,intOf(defType(snd(a))));
		    }
		}
	    }
	    else {				/* Not a poly/qual type	   */
		spCheck(l,hd(as),e,app,expect,alpha);
	    }
	    h = ap(h,hd(as));			/* Save checked argument   */
	}
	inferType(body,alpha);
	n -= nr2;
    }

    if (n>0) {				/* Deal with remaining args	   */
	Int beta = funcType(n);		/* check h::t1->t2->...->tn->rn+1  */
	shouldBe(l,h,e,app,aVar,beta);
	for (i=n; i>0; --i) {		/* check e_i::t_i for each i	   */
	    spCheck(l,arg(a),e,app,aVar,beta+2*i-1);
	    p = a;
	    a = fun(a);
	}
	tyvarType(beta+2*n);		/* Inferred type is r_n+1          */
    }

    if (isNull(p))			/* Replace head with translation   */
	e = h;
    else
	fun(p) = h;

    return e;
}

static Cell local typeExpected(l,wh,e,reqd,alpha,n,addEvid)
Int    l;				/* Type check expression e in wh   */
String wh;				/* at line l, expecting type reqd, */
Cell   e;				/* and treating vars alpha through */
Type   reqd;				/* (alpha+n-1) as fixed.	   */
Int    alpha;
Int    n;
Bool   addEvid; {			/* TRUE => add \ev -> ...	   */
    List savePreds = preds;
    Type t;
    Int  o;
    Int  m;
    List ps;
    Int  i;

    instantiate(reqd);
    t     = typeIs;
    o     = typeOff;
    m     = typeFree;
    ps    = makePredAss(predsAre,o);

    preds = NIL;
    check(l,e,NIL,wh,t,o);
    improve(l,ps,preds);

    clearMarks();
    mapProc(markAssumList,defnBounds);
    mapProc(markAssumList,varsBounds);
    mapProc(markPred,savePreds);
    markBtyvs();

    if (n > 0) {		  /* mark alpha thru alpha+n-1, plus any   */
				  /* type vars that are functionally	   */
	List us = NIL, vs = NIL;  /* dependent on them			   */
	List fds = calcFunDepsPreds(preds);
	for (i=0; i<n; i++) {
	    Type t1 = zonkTyvar(alpha+i);
	    us = zonkTyvarsIn(t1,us);
	}
	vs = oclose(fds,us);
	for (; nonNull(vs); vs=tl(vs))
	    markTyvar(intOf(hd(vs)));
    }

    normPreds(l);
    savePreds = elimPredsUsing(ps,savePreds);
    if (nonNull(preds) && resolveDefs(genvarType(t,o,NIL),FALSE))
	savePreds = elimPredsUsing(ps,savePreds);
    elimTauts();
    if (nonNull(preds)) {
	Type ty = copyType(t,o);
	List qs = copyPreds(ps);
	cantEstablish(l,wh,e,ty,qs);
    }

    resetGenerics();
    for (i=0; i<m; i++)
	if (copyTyvar(o+i)!=mkOffset(i)) {
	    List qs = copyPreds(ps);
	    Type it = copyType(t,o);
	    tooGeneral(l,e,reqd,generalize(qs,it));
	}

    if (addEvid) {
	e     = qualifyExpr(l,ps,e);
	preds = savePreds;
    }
    else
	preds = revOnto(ps,savePreds);

    inferType(t,o);
    return e;
}

static Void local typeAlt(wh,e,a,t,o,m)	/* Type check abstraction (Alt)	   */
String wh;				/* a = ( [p1, ..., pn], rhs )	   */
Cell   e;
Cell   a;
Type   t;
Int    o;
Int    m; {
    Type origt = t;
    List ps    = fst(a) = patBtyvs(fst(a));
    Int  n     = length(ps);
    Int  l     = rhsLine(snd(a));
    Int  nr2   = 0;
    List as    = NIL;
    Bool added = FALSE;

    saveVarsAss();
    enterSkolVars();
    if (whatIs(t)==RANK2) {
	if (n<(nr2=intOf(fst(snd(t))))) {
	    ERRMSG(l) "Definition requires at least %d parameters on lhs",
		      intOf(fst(snd(t)))
	    EEND;
	}
	t = snd(snd(t));
    }

    while (getHead(t)==typeArrow && argCount==2 && nonNull(ps)) {
	Type ta = arg(fun(t));
	if (isPolyOrQualType(ta)) {
	    if (hd(ps)!=WILDCARD) {
		if (!isVar(hd(ps))) {
		   ERRMSG(l) "Argument " ETHEN ERREXPR(hd(ps));
		   ERRTEXT   " used where a variable or wildcard is required\n"
		   EEND;
		}
		if (m>0 && !added) {
		    Int i = 0;
		    for (; i<m; i++)
			addVarAssump(dummyVar,mkInt(o+i));
		    added = TRUE;
		}
		addVarAssump(hd(ps),ta);
	    }
	}
	else {
	    hd(ps) = typeFreshPat(l,hd(ps));
	    shouldBe(l,hd(ps),NIL,wh,ta,o);
	}
	t  = arg(t);
	ps = tl(ps);
	as = fn(ta,as);
	n--;
    }

    if (n==0)
	snd(a) = typeRhs(snd(a));
    else {
	Int beta = funcType(n);
	Int i    = 0;
	for (; i<n; ++i) {
	    hd(ps) = typeFreshPat(l,hd(ps));
	    bindTv(beta+2*i+1,typeIs,typeOff);
	    ps = tl(ps);
	}
	snd(a) = typeRhs(snd(a));
	bindTv(beta+2*n,typeIs,typeOff);
	tyvarType(beta);
    }

    if (!unify(typeIs,typeOff,t,o)) {
	Type req, got;
	clearMarks();
	req = liftRank2(origt,o,m);
	liftRank2Args(as,o,m);
	got = ap(RANK2,pair(mkInt(nr2),revOnto(as,copyType(typeIs,typeOff))));
	reportTypeError(l,e,NIL,wh,got,req);
    }

    restoreVarsAss();
    doneBtyvs(l);
    leaveSkolVars(l,origt,o,m);
}

static Int local funcType(n)		/*return skeleton for function type*/
Int n; {				/*with n arguments, taking the form*/
    Int beta = newTyvars(2*n+1);	/*    r1 t1 r2 t2 ... rn tn rn+1   */
    Int i;				/* with r_i := t_i -> r_i+1	   */
    for (i=0; i<n; ++i)
	bindTv(beta+2*i,arrow,beta+2*i+1);
    return beta;
}

static Void local typeCase(l,beta,c)   /* type check case: pat -> rhs	   */
Int  l; 			       /* (case given by c == (pat,rhs))   */
Int  beta;			       /* need:  pat :: (var,beta)	   */
Cell c; {			       /*	 rhs :: (var,beta+1)	   */
    static String casePat  = "case pattern";
    static String caseExpr = "case expression";

    saveVarsAss();
    enterSkolVars();
    fst(c) = typeFreshPat(l,patBtyvs(fst(c)));
    shouldBe(l,fst(c),NIL,casePat,aVar,beta);
    snd(c) = typeRhs(snd(c));
    shouldBe(l,rhsExpr(snd(c)),NIL,caseExpr,aVar,beta+1);

    restoreVarsAss();
    doneBtyvs(l);
    leaveSkolVars(l,typeIs,typeOff,0);
}

static Void local typeComp(l,m,e,qs)	/* type check comprehension	   */
Int  l;
Type m;					/* monad (mkOffset(0))		   */
Cell e;
List qs; {
    static String boolQual = "boolean qualifier";
    static String genQual  = "generator";
#if IPARAM
    List svPreds;
#endif

    STACK_CHECK
    if (isNull(qs)) {			/* no qualifiers left		   */
	spTypeExpr(l,fst(e));
    } else {
	Cell q   = hd(qs);
	List qs1 = tl(qs);
	switch (whatIs(q)) {
	    case BOOLQUAL : spCheck(l,snd(q),NIL,boolQual,typeBool,0);
			    typeComp(l,m,e,qs1);
			    break;

	    case QWHERE   : enterBindings();
			    enterSkolVars();
			    mapProc(typeBindings,snd(q));
			    typeComp(l,m,e,qs1);
			    leaveBindings();
			    leaveSkolVars(l,typeIs,typeOff,0);
			    break;

	    case FROMQUAL : {   Int beta = newTyvars(1);
				saveVarsAss();
				enterPendingBtyvs();
				spCheck(l,snd(snd(q)),NIL,genQual,m,beta);
				enterSkolVars();
				fst(snd(q))
				    = typeFreshPat(l,patBtyvs(fst(snd(q))));
				shouldBe(l,fst(snd(q)),NIL,genQual,aVar,beta);
				typeComp(l,m,e,qs1);
				restoreVarsAss();
			        leavePendingBtyvs();
				leaveSkolVars(l,typeIs,typeOff,0);
			    }
			    break;

	    case DOQUAL   : spCheck(l,snd(q),NIL,genQual,m,newTyvars(1));
			    typeComp(l,m,e,qs1);
			    break;
	}
    }
}

#if ZIP_COMP

/* --------------------------------------------------------------------------
 * Parallel comprehensions
 *
 * This is an extension to the standard list comprehension notation,
 * allowing parallel lists of qualifiers that generate independently
 * of each other.  Parallel qualifier lists are separated with additional
 * `|' symbols:
 *     [ e | p1 <- e11, p2 <- e12, ...
 *         | q1 <- e21, q2 <- e22, ...
 *         ... ]
 *
 * The meaning of a parallel comprehension can be defined in terms of zip
 * and a regular comprehension:
 *     [ e | ((p1,p2), (q1,q2)) <- zip [(p1,p2) | p1 <- e11, p2 <- e12]
 *                                     [(q1,q2) | q1 <- e21, q2 <- e22]]
 * The use of let-bindings in the qualifier lists complicates this slightly.
 * Let-bound variables will scope over the rest of their qualifier list
 * as well as `e', but not over any part of a parallel qualifier list.
 * We can express this in the translation by including in the derived
 * patterns all the let-bound variables:
 *     [ e | p1 <- e11, let v1 = e12, p2 <- e13
 *         | q1 <- e21, let v2 = e22, q2 <- e23]
 *     =>
 *     [ e | ((p1,v1,p2), (q1,v2,q2)) <-
 *               zip [(p1,v1,p2) | p1 <- e11, let v1 = e12, p2 <- e13]
 *                   [(q1,v2,q2) | q1 <- e21, let v2 = e22, q2 <- e23]]
 * Where it is understood that the `v' patterns in the tuples are given
 * rank-2 types, so we don't lose the polymorphism.  We also have to be
 * careful to preserve any shadowing.
 *
 * ZZ We aren't dealing with WHEREs correctly...
 * ZZ We aren't dealing with skolem vars correctly...
 * ------------------------------------------------------------------------*/

static List gatheredAss;
static List gatheredDefns;
static List gatheredTyvars;
static List gatheredPTyvars;

#define enterGathering() List svGA = gatheredAss, svGD = gatheredDefns, svGT = gatheredTyvars, svGP = gatheredPTyvars; gatheredAss = gatheredDefns = gatheredTyvars = gatheredPTyvars = NIL
#define leaveGathering() gatheredAss = svGA; gatheredDefns = svGD; gatheredTyvars = svGT; gatheredPTyvars = svGP

static List local getPats(bs)
List bs; {
    List ps = NIL;
    for (; nonNull(bs); bs=tl(bs)) {
	ps = cons(fst(hd(bs)), ps);
    }
    return ps;
}

static Cell local tupleUp(xs)
List xs; {
    Int n = length(xs);
    if (n == 0)
	return nameUnit;
    else if (n == 1)
	return hd(xs);
    else {
	Cell x = mkTuple(n);
	for (; nonNull(xs); xs=tl(xs))
	    x = ap(x, hd(xs));
	return x;
    }
}

static Cell local typeZComp(l,m,e,qss)	/* type check comprehension	   */
Int l;
Type m;					/* monad (mkOffset(0))		   */
Cell e;
List qss; {
    List pss, ass;
    List zpat, zexp;
    Int  len;
    Text zName;
#if IPARAM
    List svPreds;
#endif
    enterGathering();
    enterBindings();
    for (pss = qss;nonNull(pss);pss=tl(pss)) {
	gatheredAss = cons(NIL,gatheredAss);
	gatheredDefns = cons(NIL,gatheredDefns);
	typeCompy(l,m,hd(pss));
	/* reset for next list of qualifiers */
	hd(varsBounds) = NIL;
    }
    /* add gathered vars */
    hd(varsBounds) = revOnto(concat(gatheredAss),hd(varsBounds));
    enterBindings();
    hd(defnBounds) = revOnto(concat(gatheredDefns),hd(defnBounds));
    enterPendingBtyvs();
    hd(btyvars) = gatheredTyvars;
    hd(pendingBtyvs) = gatheredPTyvars;
    spTypeExpr(l,fst(e));
    leavePendingBtyvs();
    leaveBindings();
    leaveBindings();

    /* now, we construct a regular comprehension out of the parallel one */
    len   = length(qss);
    zName = zipName(len);
    zpat  = mkTuple(len);
    zexp  = findQualFun(findText("Data.List"),zName);
    if (isNull(zexp))
	zexp = findQualFun(findText("List"),zName);
    if (isNull(zexp)) {
	/* if they don't have List loaded, we can still handle the most
	   common cases, because `zip' and `zip3' are defined in the Prelude */
	zexp = findQualFun(textPrelude,zName);
    }
    if (isNull(zexp)) {
	ERRMSG(l) "\"%s\" not in scope (introduced by parallel comprehension)", textToStr(zName) ETHEN
	ERRTEXT   "\n*** Possible cause: \"List\" module not imported"
	EEND;
    
    }
    for (pss=qss, ass=rev(gatheredAss);nonNull(pss);pss=tl(pss), ass=tl(ass)) {
	List ps = tupleUp(getPats(hd(ass)));
	zpat = ap(zpat, ps);
	zexp = ap(zexp, ap(MONADCOMP,pair(nameListMonad,pair(ps, hd(pss)))));
    }
    leaveGathering();
    return pair(fst(e),singleton(ap(FROMQUAL,pair(zpat,zexp))));
}

static Void local typeCompy(l,m,qs)	/* type check comprehension	   */
Int  l;
Type m;					/* monad (mkOffset(0))		   */
List qs; {
    static String boolQual = "boolean qualifier";
    static String genQual  = "generator";
#if IPARAM
    List svPreds;
#endif

    STACK_CHECK
    if (!isNull(qs)) {			/* no qualifiers left		   */
	Cell q   = hd(qs);
	List qs1 = tl(qs);
	switch (whatIs(q)) {
	    case BOOLQUAL : spCheck(l,snd(q),NIL,boolQual,typeBool,0);
			    typeCompy(l,m,qs1);
			    break;

	    case QWHERE   : enterBindings();
			    enterSkolVars();
			    mapProc(typeBindings,snd(q));
			    hd(gatheredAss) = dupOnto(hd(varsBounds),hd(gatheredAss));
			    /* ZZ what is gatheredDefns used for ??? */
			    hd(gatheredDefns) = dupOnto(hd(defnBounds),hd(gatheredDefns));
			    typeCompy(l,m,qs1);
			    leaveBindings();
			    leaveSkolVars(l,typeIs,typeOff,0);
			    break;

	    case FROMQUAL : {   Int beta = newTyvars(1);
				enterPendingBtyvs();
				spCheck(l,snd(snd(q)),NIL,genQual,m,beta);
				enterSkolVars();
				fst(snd(q))
				    = typeFreshPat(l,patBtyvs(fst(snd(q))));
				shouldBe(l,fst(snd(q)),NIL,genQual,aVar,beta);
				hd(gatheredAss) = dupOnto(hd(varsBounds),hd(gatheredAss));
				gatheredTyvars = dupOnto(hd(btyvars),gatheredTyvars);
				gatheredPTyvars = dupOnto(hd(pendingBtyvs),gatheredPTyvars);
				typeCompy(l,m,qs1);
			        leavePendingBtyvsQuietly();
				leaveSkolVars(l,typeIs,typeOff,0);
			    }
			    break;

	    case DOQUAL   : spCheck(l,snd(q),NIL,genQual,m,newTyvars(1));
			    typeCompy(l,m,qs1);
			    break;

	    default:	    internal("typeComp");
	}
    }
}
#endif

static Cell local typeMonadComp(l,e)	/* type check monad comprehension  */
Int  l;
Cell e; {
    Int  alpha	      = newTyvars(1);
    Int  beta	      = newTyvars(1);
    Cell mon	      = ap(mkInt(beta),aVar);
    Cell m	      = assumeEvid(predMonad,beta);
    tyvar(beta)->kind = starToStar;
#if !MONAD_COMPS
    bindTv(beta,typeList,0);
    m = nameListMonad;
#endif

    typeComp(l,mon,snd(e),snd(snd(e)));
    bindTv(alpha,typeIs,typeOff);
    inferType(mon,alpha);
    return ap(MONADCOMP,pair(m,snd(e)));
}

#if ZIP_COMP
static Cell local typeMonadZComp(l,e)	/* type check monad comprehension  */
Int  l;
Cell e; {
    Int  alpha	      = newTyvars(1);
    Int  beta	      = newTyvars(1);
    Cell mon	      = ap(mkInt(beta),aVar);
    Cell m	      = assumeEvid(predMonad,beta);
    Cell new;
    tyvar(beta)->kind = starToStar;
#if !MONAD_COMPS
    bindTv(beta,typeList,0);
    m = nameListMonad;
#endif

    new = typeZComp(l,mon,snd(e),snd(snd(e)));
    bindTv(alpha,typeIs,typeOff);
    inferType(mon,alpha);
    return ap(MONADCOMP,pair(m,new));
}
#endif

static Void local typeDo(l,e)		/* type check do-notation	   */
Int  l;
Cell e; {
    static String finGen = "final generator";
    Int  alpha		 = newTyvars(1);
    Int  beta		 = newTyvars(1);
    Cell mon		 = ap(mkInt(beta),aVar);
    Cell m		 = assumeEvid(predMonad,beta);
    tyvar(beta)->kind    = starToStar;

    typeComp(l,mon,snd(e),snd(snd(e)));
    shouldBe(l,fst(snd(e)),NIL,finGen,mon,alpha);
    snd(e) = pair(m,snd(e));
}

#if MUDO

#define segRecs(seg)	fst3(fst(seg))
#define segExps(seg)	snd3(fst(seg))
#define segDefs(seg)	thd3(fst(seg))
#define segQuals(seg)	snd(seg)

static Void local typeRecComp(l,m,e,qs)		/* type check rec-comp	     */
Int  l;
Type m;						/* monad (mkOffset(0))	     */
Cell e;
List qs; {
    static String boolQual = "boolean qualifier";
    static String genQual  = "generator";
    static String letQual  = "mdo-transformed let generator";
    String mesg		   = genQual;

#if IPARAM
    List svPreds;
#endif


    STACK_CHECK
    if (isNull(qs)) {			/* no qualifiers left		   */
	spTypeExpr(l,fst(e));
    } else {
	Cell q   = hd(qs);
	List qs1 = tl(qs);
	switch (whatIs(q)) {
	    case BOOLQUAL : spCheck(l,snd(q),NIL,boolQual,typeBool,0);
			    typeRecComp(l,m,e,qs1);
			    break;

	    case QWHERE	  : mesg = letQual;
			    fst(q) = FROMQUAL;
			    
			    /* intentional fall-thru */
	    case FROMQUAL : {   Int beta = newTyvars(1);
				saveVarsAss();
				enterPendingBtyvs();
				spCheck(l,snd(snd(q)),NIL,mesg,m,beta);
				enterSkolVars();

				tcMode = OLD_PATTERN;
				fst(snd(q))
				    = typeExpr(l,patBtyvs(fst(snd(q))));
				tcMode = EXPRESSION;

				shouldBe(l,fst(snd(q)),NIL,mesg,aVar,beta);
				typeRecComp(l,m,e,qs1);
				restoreVarsAss();
				leavePendingBtyvs();
				leaveSkolVars(l,typeIs,typeOff,0);
			    }
			    break;

	    case DOQUAL   : spCheck(l,snd(q),NIL,genQual,m,newTyvars(1));
			    typeRecComp(l,m,e,qs1);
			    break;
	}
    }
}

static Void local typeMDo(l,e)		/* type check recursive-do	   */
Int l;
Cell e; {
    String fixLib     = "Control.Monad.Fix";
    String fixClass   = "MonadFix";

    if( !classMonadRec ) {
	ERRMSG(0) "%s class not defined", fixClass ETHEN
        ERRTEXT   "\n*** Possible cause: \"%s\" module not imported", fixLib
	EEND;
    }

    predMonadRec = ap(classMonadRec,aVar);

    /* Now we're safe: do the actual type-checking now: */
    typeRecursiveDo(l,e);
}

static Void local typeRecursiveDo(l,e)	/* type check recursive-do exp.   */
Int l;
Cell e; {
    /* The structure at this point:
	e = (TAG, (1, [((2, 3, 4), 5)]))
	where
	    1 = expression
	    2 = rec vars of the segment
	    3 = exported vars of the segment
	    4 = defined vars of the segment
	    5 = qualifiers
    */
    static String finGen = "final generator";
    Int alpha		 = newTyvars(1);
    Int beta		 = newTyvars(1);
    Cell mon		 = ap(mkInt(beta),aVar);
    Cell monDict         = assumeEvid(predMonad,beta); 
    Cell m		 = assumeEvid(predMonadRec,beta);
    List tmp;
    List whole		 = NIL;

    tyvar(beta)->kind	 = starToStar;
    
    enterBindings();

    /* introduce defined variables into the typing environment: */
    for(tmp = snd(snd(e)); nonNull(tmp); tmp = tl(tmp)) {
	List rtmp = segDefs(hd(tmp));
	for(; nonNull(rtmp); rtmp = tl(rtmp)) {
	    newVarsBind(hd(rtmp)); 
	}
    }

    /* collect all qualifiers from all segments: */
    for(tmp = snd(snd(e)); nonNull(tmp); tmp = tl(tmp)) {
	List tmp2;
	for(tmp2 = segQuals(hd(tmp)); nonNull(tmp2); tmp2 = tl(tmp2)) {
	    whole=cons(hd(tmp2),whole);
	}
    }
	
    typeRecComp(l,mon,snd(e),rev(whole));
    shouldBe(l,fst(snd(e)),NIL,finGen,mon,alpha);
    leaveBindings();

    snd(e) = pair(pair(m,monDict),snd(e));
}

#undef segRecs
#undef segExps
#undef segDefs
#undef segQuals
#endif

static Void local typeConFlds(l,e)	/* Type check a construction	   */
Int  l;
Cell e; {
    static String conExpr = "value construction";
    Name c  = fst(snd(e));
    List fs = snd(snd(e));
    Type tc;
    Int  to;
    Int  tf;
    Int  i;
#if IPARAM
    List svPreds;
#endif

    instantiate(name(c).type);
    for (; nonNull(predsAre); predsAre=tl(predsAre))
	assumeEvid(hd(predsAre),typeOff);
    if (whatIs(typeIs)==RANK2)
	typeIs = snd(snd(typeIs));
    tc = typeIs;
    to = typeOff;
    tf = typeFree;

    for (; nonNull(fs); fs=tl(fs)) {
	Type t = tc;
	for (i=sfunPos(fst(hd(fs)),c); --i>0; t=arg(t))
	    ;
	t = dropRank1(arg(fun(t)),to,tf);
	if (isPolyOrQualType(t))
	    snd(hd(fs)) = typeExpected(l,conExpr,snd(hd(fs)),t,to,tf,TRUE);
	else {
	    spCheck(l,snd(hd(fs)),e,conExpr,t,to);
	}
    }
    for (i=name(c).arity; i>0; i--)
	tc = arg(tc);
    inferType(tc,to);
}

static Void local typeUpdFlds(line,e)	/* Type check an update		   */
Int  line;				/* (Written in what might seem a   */
Cell e; {				/* bizarre manner for the benefit  */
    static String update = "update";	/* of as yet unreleased extensions)*/
    List cs    = snd3(snd(e));		/* List of constructors		   */
    List fs    = thd3(snd(e));		/* List of field specifications	   */
    List ts    = NIL;			/* List of types for fields	   */
    Int  n     = length(fs);
    Int  alpha = newTyvars(2+n);
    Int  i;
    List fs1;
#if IPARAM
    List svPreds;
#endif

    /* Calculate type and translation for each expr in the field list	   */
    for (fs1=fs, i=alpha+2; nonNull(fs1); fs1=tl(fs1), i++) {
	spTypeExpr(line,snd(hd(fs1)));
	bindTv(i,typeIs,typeOff);
    }

    clearMarks();
    mapProc(markAssumList,defnBounds);
    mapProc(markAssumList,varsBounds);
    mapProc(markPred,preds);
    markBtyvs();

    for (fs1=fs, i=alpha+2; nonNull(fs1); fs1=tl(fs1), i++) {
	resetGenerics();
	ts = cons(generalize(NIL,copyTyvar(i)),ts);
    }
    ts = rev(ts);

    /* Type check expression to be updated				   */
    spTypeExpr(line,fst3(snd(e)));
    bindTv(alpha,typeIs,typeOff);

    for (; nonNull(cs); cs=tl(cs)) {	/* Loop through constrs		   */
	Name c  = hd(cs);
	List ta = replicate(name(c).arity,NIL);
	Type td, tr;
	Int  od, or;

	tcMode = NEW_PATTERN;		/* Domain type			   */
	instantiate(name(c).type);
	tcMode = EXPRESSION;
	td     = typeIs;
	od     = typeOff;
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),typeOff);

	if (whatIs(td)==RANK2)	/* Skip rank2 annotation, if any   */
	    td = snd(snd(td));

	instantiate(name(c).type);	/* Range type			   */
	tr = typeIs;
	or = typeOff;
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),typeOff);

	if (whatIs(tr)==RANK2)	/* Skip rank2 annotation, if any   */
	    tr = snd(snd(tr));

	for (fs1=fs, i=1; nonNull(fs1); fs1=tl(fs1), i++) {
	    Int n    = sfunPos(fst(hd(fs1)),c);
	    Cell ta1 = ta;
	    for (; n>1; n--)
		ta1 = tl(ta1);
	    hd(ta1) = mkInt(i);
	}

	for (; nonNull(ta); ta=tl(ta)) {	/* For each cfun arg	   */
	    if (nonNull(hd(ta))) {		/* Field to updated?	   */
		Int  n = intOf(hd(ta));
		Cell f = fs;
		Cell t = ts;
		for (; n-- > 1; f=tl(f), t=tl(t))
		    ;

		if (isPolyOrQualType(arg(fun(td)))) {
		    ERRMSG(line) "Sorry, record update syntax cannot currently be used for polymorphic components"
		    EEND;
		}

		f = hd(f);
		t = hd(t);
		instantiate(t);
		shouldBe(line,snd(f),e,update,arg(fun(tr)),or);
	    }					/* Unmentioned component   */
	    else if (!unify(arg(fun(td)),od,arg(fun(tr)),or))
		internal("typeUpdFlds");

	    tr = arg(tr);
	    td = arg(td);
	}

	inferType(td,od);			/* Check domain type	   */
	shouldBe(line,fst3(snd(e)),e,update,aVar,alpha);
	inferType(tr,or);			/* Check range type	   */
	shouldBe(line,e,NIL,update,aVar,alpha+1);
    }
    /* (typeIs,typeOff) still carry the result type when we exit the loop  */
}

#if IPARAM
static Cell local typeWith(line,e)	/* Type check a with		   */
Int  line;
Cell e; {
    List fs    = snd(snd(e));		/* List of field specifications	   */
    Int  n     = length(fs);
    Int  alpha = newTyvars(2+n);
    Int  i;
    List fs1;
    Cell tIs;
    Cell tOff;
    List dpreds = NIL, dp;
    Cell bs = NIL;

    /* Type check expression to be updated				   */
    fst(snd(e)) = typeExpr(line,fst(snd(e)));
    bindTv(alpha,typeIs,typeOff);
    tIs = typeIs;
    tOff = typeOff;
    /* elim duplicates */
    improve(line,NIL,preds);
    preds = scSimplify(preds);
    /* extract preds that we're going to bind */
    for (fs1=fs; nonNull(fs1); fs1=tl(fs1)) {
        Text t = textOf(fst(hd(fs1)));
	Cell p = findIPEvid(t);
	dpreds = cons(p, dpreds);
	if (nonNull(p)) {
	    removeIPEvid(t);
	} else {
	    /* maybe give a warning message here... */
	}
    }
    dpreds = rev(dpreds);

    /* Calculate type and translation for each expr in the field list	   */
    for (fs1=fs, dp=dpreds, i=alpha+2; nonNull(fs1); fs1=tl(fs1), dp=tl(dp), i++) {
	static String with = "with";
        Cell ev = hd(dp);
	snd(hd(fs1)) = typeExpr(line,snd(hd(fs1)));
	bindTv(i,typeIs,typeOff);
	if (nonNull(ev)) {
	    shouldBe(line,fst(hd(fs1)),e,with,snd(fst3(ev)),intOf(snd3(ev)));
	    bs = cons(cons(pair(thd3(ev), cons(triple(NIL, mkInt(line), snd(hd(fs1))), NIL)), NIL), bs);
	}
    }
    typeIs = tIs;
    typeOff = tOff;
    return (ap(LETREC,pair(bs,fst(snd(e)))));
}
#endif

static Cell local typeFreshPat(l,p)    /* find type of pattern, assigning  */
Int  l; 			       /* fresh type variables to each var */
Cell p; {			       /* bound in the pattern		   */
    tcMode = NEW_PATTERN;
    p	   = typeExpr(l,p);
    tcMode = EXPRESSION;
    return p;
}

/* --------------------------------------------------------------------------
 * Type check group of bindings:
 * ------------------------------------------------------------------------*/

static Void local typeBindings(bs)	/* type check a binding group	   */
List bs; {
    Bool usesPatBindings = FALSE;	/* TRUE => pattern binding in bs   */
    Bool usesUntypedVar  = FALSE;	/* TRUE => var bind w/o type decl  */
    List bs1;

    /* The following loop is used to determine whether the monomorphism	   */
    /* restriction should be applied.  It could be written marginally more */
    /* efficiently by using breaks, but clarity is more important here ... */

    for (bs1=bs; nonNull(bs1); bs1=tl(bs1)) {  /* Analyse binding group    */
	Cell b = hd(bs1);
	if (!isVar(fst(b)))
	    usesPatBindings = TRUE;
	else if (isNull(fst(hd(snd(snd(b)))))		/* no arguments	   */
		 && whatIs(fst(snd(b)))==IMPDEPS)	/* implicitly typed*/
	    usesUntypedVar  = TRUE;
    }

    if (usesPatBindings || usesUntypedVar)
	monorestrict(bs);
    else
	unrestricted(bs);

    elimTauts();			       /* clean up any additional */
    					       /* tauts that arose due to */
					       /* late-stage `improvement' */

    mapProc(removeTypeSigs,bs);		       /* Remove binding type info */
    hd(varsBounds) = revOnto(hd(defnBounds),   /* transfer completed assmps*/
			     hd(varsBounds));  /* out of defnBounds        */
    hd(defnBounds) = NIL;
    hd(depends)    = NIL;
}

static Void local removeTypeSigs(b)    /* Remove type info from a binding  */
Cell b; {
    snd(b) = snd(snd(b));
}

/* --------------------------------------------------------------------------
 * Type check a restricted binding group:
 * ------------------------------------------------------------------------*/

static Void local monorestrict(bs)	/* Type restricted binding group   */
List bs; {
    List savePreds = preds;
    Int  line 	   = isVar(fst(hd(bs))) ? rhsLine(snd(hd(snd(snd(hd(bs))))))
					: rhsLine(snd(snd(snd(hd(bs)))));
    hd(defnBounds) = NIL;
    hd(depends)    = NODEPENDS;		/* No need for dependents here	   */

    preds = NIL;			/* Type check the bindings	   */
    mapProc(restrictedBindAss,bs);
    mapProc(typeBind,bs);
    improve(line,NIL,preds);
    normPreds(line);
    elimTauts();
    preds = revOnto(preds,savePreds);


    clearMarks();			/* Mark fixed variables		   */
    mapProc(markAssumList,tl(defnBounds));
    mapProc(markAssumList,tl(varsBounds));
    mapProc(markPred,preds);
    markBtyvs();

    if (isNull(tl(defnBounds))) {	/* Top-level may need defaulting   */
	normPreds(line);
	if (nonNull(preds) && resolveDefs(genvarAnyAss(hd(defnBounds)),FALSE))
	    elimTauts();

	clearMarks();
	reducePreds();
	improve(line,NIL,preds);
	if (nonNull(preds))
	    resolveDefs(NIL,FALSE);		/* Nearly Haskell 1.4?		   */
	elimTauts();

	if (nonNull(preds)) {		/* Look for unresolved overloading */
	    Cell v   = isVar(fst(hd(bs))) ? fst(hd(bs)) : hd(fst(hd(bs)));
	    Cell ass = findInAssumList(textOf(v),hd(varsBounds));
	    preds    = scSimplify(preds);

	    ERRMSG(line) "Unresolved top-level overloading" ETHEN
	    ERRTEXT     "\n*** Binding             : %s", textToStr(textOf(v))
	    ETHEN
	    if (nonNull(ass)) {
		ERRTEXT "\n*** Inferred type       : " ETHEN ERRTYPE(snd(ass));
	    }
	    ERRTEXT     "\n*** Outstanding context : " ETHEN
						ERRCONTEXT(copyPreds(preds));
	    ERRTEXT     "\n"
	    EEND;
	}
    }

    map1Proc(genBind,NIL,bs);		/* Generalize types of def'd vars  */
}

static Void local restrictedBindAss(b)	/* Make assums for vars in binding */
Cell b; {				/* gp with restricted overloading  */

    if (isVar(fst(b))) {		/* function-binding?		   */
	Cell t = fst(snd(b));
	if (whatIs(t)==IMPDEPS)	 {	/* Discard implicitly typed deps   */
	    fst(snd(b)) = t = NIL;	/* in a restricted binding group.  */
	}
	fst(snd(b)) = localizeBtyvs(t);
	restrictedAss(rhsLine(snd(hd(snd(snd(b))))), fst(b), t);
    } else {				/* pattern-binding?		   */
	List vs   = fst(b);
	List ts   = fst(snd(b));
	Int  line = rhsLine(snd(snd(snd(b))));

	for (; nonNull(vs); vs=tl(vs)) {
	    if (nonNull(ts)) {
		restrictedAss(line,hd(vs),hd(ts)=localizeBtyvs(hd(ts)));
		ts = tl(ts);
	    } else {
		restrictedAss(line,hd(vs),NIL);
	    }
	}
    }
}

static Void local restrictedAss(l,v,t) /* Assume that type of binding var v*/
Int  l; 			       /* is t (if nonNull) in restricted  */
Cell v; 			       /* binding group 		   */
Type t; {
    newDefnBind(v,t);
    if (nonNull(predsAre)) {
	ERRMSG(l) "Explicit overloaded type for \"%s\"",textToStr(textOf(v))
	ETHEN
	ERRTEXT   " not permitted in restricted binding"
	EEND;
    }
}

/* --------------------------------------------------------------------------
 * Unrestricted binding group:
 * ------------------------------------------------------------------------*/

static Void local unrestricted(bs)	/* Type unrestricted binding group */
List bs; {
    List savePreds = preds;
    List imps      = NIL;		/* Implicitly typed bindings	   */
    List exps      = NIL;		/* Explicitly typed bindings	   */
    List bs1;

    /* ----------------------------------------------------------------------
     * STEP 1: Separate implicitly typed bindings from explicitly typed	
     * bindings and do a dependency analyis, where f depends on g iff f
     * is implicitly typed and involves a call to g.
     * --------------------------------------------------------------------*/

    for (; nonNull(bs); bs=tl(bs)) {
	Cell b = hd(bs);
	if (whatIs(fst(snd(b)))==IMPDEPS)
	    imps = cons(b,imps);	/* N.B. New lists are built to	   */
	else				/* avoid breaking the original	   */
	    exps = cons(b,exps);	/* list structure for bs.	   */
    }

    for (bs=imps; nonNull(bs); bs=tl(bs)) {
	Cell b  = hd(bs);		/* Restrict implicitly typed dep   */
	List ds = snd(fst(snd(b)));	/* lists to bindings in imps	   */
	List cs = NIL;
	while (nonNull(ds)) {
	    bs1 = tl(ds);
	    if (cellIsMember(hd(ds),imps)) {
		tl(ds) = cs;
		cs     = ds;
	    }
	    ds = bs1;
	}
	fst(snd(b)) = cs;
    }
    imps = itbscc(imps);		/* Dependency analysis on imps	   */
    for (bs=imps; nonNull(bs); bs=tl(bs))
	for (bs1=hd(bs); nonNull(bs1); bs1=tl(bs1))
	    fst(snd(hd(bs1))) = NIL; 	/* reset imps type fields	   */

#if DEBUG_DEPENDS
    Printf("Binding group:");
    for (bs1=imps; nonNull(bs1); bs1=tl(bs1)) {
	Printf(" [imp:");
	for (bs=hd(bs1); nonNull(bs); bs=tl(bs))
	    Printf(" %s",textToStr(textOf(fst(hd(bs)))));
	Printf("]");
    }
    if (nonNull(exps)) {
	Printf(" [exp:");
	for (bs=exps; nonNull(bs); bs=tl(bs))
	    Printf(" %s",textToStr(textOf(fst(hd(bs)))));
	Printf("]");
    }
    Printf("\n");
#endif

    /* ----------------------------------------------------------------------
     * STEP 2: Add type assumptions about any explicitly typed variable.
     * --------------------------------------------------------------------*/

    for (bs=exps; nonNull(bs); bs=tl(bs)) {
	fst(snd(hd(bs))) = localizeBtyvs(fst(snd(hd(bs))));
	hd(varsBounds)   = cons(pair(fst(hd(bs)),fst(snd(hd(bs)))),
				hd(varsBounds));
    }

    /* ----------------------------------------------------------------------
     * STEP 3: Calculate types for each group of implicitly typed bindings.
     * --------------------------------------------------------------------*/

    for (; nonNull(imps); imps=tl(imps)) {
	Cell b   = hd(hd(imps));
	Int line = isVar(fst(b)) ? rhsLine(snd(hd(snd(snd(b)))))
				 : rhsLine(snd(snd(snd(b))));
	hd(defnBounds) = NIL;
	hd(depends)    = NIL;
	for (bs1=hd(imps); nonNull(bs1); bs1=tl(bs1))
	    newDefnBind(fst(hd(bs1)),NIL);

	preds = NIL;
	mapProc(typeBind,hd(imps));
	improve(line,NIL,preds);

	clearMarks();
	mapProc(markAssumList,tl(defnBounds));
	mapProc(markAssumList,tl(varsBounds));
	mapProc(markPred,savePreds);
	markBtyvs();

	normPreds(line);
	savePreds = elimOuterPreds(savePreds);
	if (nonNull(preds) && resolveDefs(genvarAllAss(hd(defnBounds)),FALSE)) {
	    savePreds = elimOuterPreds(savePreds);
	}

	map1Proc(genBind,preds,hd(imps));
	if (nonNull(preds)) {
	    map1Proc(addEvidParams,preds,hd(depends));
	    map1Proc(qualifyBinding,preds,hd(imps));
	}

	h98CheckInferredType(line,
			fst(hd(hd(defnBounds))),snd(hd(hd(defnBounds))));
	hd(varsBounds) = revOnto(hd(defnBounds),hd(varsBounds));
    }

    /* ----------------------------------------------------------------------
     * STEP 4: Now infer a type for each explicitly typed variable and
     * check for compatibility with the declared type.
     * --------------------------------------------------------------------*/

    for (; nonNull(exps); exps=tl(exps)) {
	static String extbind = "explicitly typed binding";
	Cell b    = hd(exps);
	List alts = snd(snd(b));
	Int  line = rhsLine(snd(hd(alts)));
	Type t;
	Int  o;
	Int  m;
	List ps;

	hd(defnBounds) = NIL;
	hd(depends)    = NODEPENDS;
	preds	       = NIL;

	instantiate(fst(snd(b)));
	o	       = typeOff;
	m	       = typeFree;
	t              = dropRank2(typeIs,o,m);
	ps	       = makePredAss(predsAre,o);

	enterPendingBtyvs();
	for (; nonNull(alts); alts=tl(alts))
	    typeAlt(extbind,fst(b),hd(alts),t,o,m);
	improve(line,ps,preds);
	leavePendingBtyvs();

	if (nonNull(ps))		/* Add dict params, if necessary   */
	    qualifyBinding(ps,b);

	clearMarks();
	mapProc(markAssumList,tl(defnBounds));
	mapProc(markAssumList,tl(varsBounds));
	mapProc(markPred,savePreds);
	markBtyvs();

	normPreds(line);
	savePreds = elimPredsUsing(ps,savePreds);
	if (nonNull(preds)) {
	    List vs = NIL;
	    Int  i  = 0;
	    for (; i<m; ++i)
		vs = cons(mkInt(o+i),vs);
	    if (resolveDefs(vs,FALSE)) {
		savePreds = elimPredsUsing(ps,savePreds);
	    }
	    if (nonNull(preds)) {
		clearMarks();
		reducePreds();
		if (nonNull(preds) && resolveDefs(vs,FALSE))
		    savePreds = elimPredsUsing(ps,savePreds);
	    }
	}

	resetGenerics();		/* Make sure we're general enough  */
	ps = copyPreds(ps);
	t  = generalize(ps,liftRank2(t,o,m));

	if (!sameSchemes(t,fst(snd(b))))
	    tooGeneral(line,fst(b),fst(snd(b)),t);
	h98CheckInferredType(line,fst(b),t);

	if (nonNull(preds))		/* Check context was strong enough */
	    cantEstablish(line,extbind,fst(b),t,ps);
    }

    preds	   = savePreds;			/* Restore predicates	   */
    hd(defnBounds) = NIL;
}

#define  SCC		 itbscc		/* scc for implicitly typed binds  */
#define  LOWLINK	 itblowlink
#define  DEPENDS(t)	 fst(snd(t))
#define  SETDEPENDS(c,v) fst(snd(c))=v
#include "scc.c"
#undef   SETDEPENDS
#undef	 DEPENDS
#undef 	 LOWLINK
#undef	 SCC

static Void local addEvidParams(qs,v)  /* overwrite VARID/OPCELL v with	   */
List qs;			       /* application of variable to evid. */
Cell v; {			       /* parameters given by qs	   */
    if (nonNull(qs)) {
	Cell nv;

	if (!isVar(v))
	    internal("addEvidParams");

	for (nv=mkVar(textOf(v)); nonNull(tl(qs)); qs=tl(qs))
	    nv = ap(nv,thd3(hd(qs)));
	fst(v) = nv;
	snd(v) = thd3(hd(qs));
    }
}

/* --------------------------------------------------------------------------
 * Type check bodies of class and instance declarations:
 * ------------------------------------------------------------------------*/

static Void local typeClassDefn(c)	/* Type check implementations of   */
Class c; {				/* defaults for class c		   */

    /* ----------------------------------------------------------------------
     * Generate code for default dictionary builder functions:
     * --------------------------------------------------------------------*/

    Int  beta   = newKindedVars(cclass(c).kinds);
    Cell d      = inventDictVar();
    List dparam = singleton(triple(cclass(c).head,mkInt(beta),d));
    List mems   = cclass(c).members;
    List defs   = cclass(c).defaults;
    List dsels  = cclass(c).dsels;
    Cell pat    = cclass(c).dcon;
    Int  width  = cclass(c).numSupers + cclass(c).numMembers;
    Int  i      = 0;

    if (isNull(defs) && nonNull(mems)) {
        defs = cclass(c).defaults = cons(NIL,NIL);
    }

    for (; nonNull(mems); mems=tl(mems)) {
	Name   n;
	Text member           = name(hd(mems)).text;
#if 0
	char buf[FILENAME_MAX+1];
	Int  j      = 0;
	/* This is unsafe, creating a new name that's in the
	   same namespace as the user's -- indeed, a bug was
	   reported as a result of this.
	   
	   Instead, invent a new name. Simpler and safer.
	*/
	static String deftext = "default_";
	String s	      = textToStr(member);
	for (i = 0; i<FILENAME_MAX && deftext[i]!='\0'; i++) {
	    buf[i] = deftext[i];
	}
	for(j = 0; (i+j)<FILENAME_MAX && s[j]!='\0'; j++) {
	    buf[i+j] = s[j];
	}
	buf[i+j] = '\0';
	n = newName(findText(buf),c);
#endif
	n = newName(inventText(),c);

	if (isNull(hd(defs))) {		/* No default definition	   */
	    name(n).line  = cclass(c).line;
	    name(n).arity = 1;
	    name(n).defn  = singleton(pair(singleton(d),
					   ap(mkInt(cclass(c).line),
					      ap(namePrimThrow,
					         ap(nameNoMethodError,
						    mkStr(member))))));
	} else {			/* User supplied default defn	   */
	    List alts = snd(hd(defs));
	    Int  line = rhsLine(snd(hd(alts)));

	    typeMember("default member binding",
		       hd(mems),
		       alts,
		       dparam,
		       cclass(c).head,
		       beta);

	    name(n).line  = line;
	    name(n).arity = 1+length(fst(hd(alts)));
	    name(n).defn  = alts;

	    for (; nonNull(alts); alts=tl(alts)) {
		fst(hd(alts)) = cons(d,fst(hd(alts)));
	    }
	}

        hd(defs) = n;
	genDefns = cons(n,genDefns);
	if (isNull(tl(defs)) && nonNull(tl(mems))) {
	    tl(defs) = cons(NIL,NIL);
	}
	defs     = tl(defs);
    }

    /* ----------------------------------------------------------------------
     * Generate code for superclass and member function selectors:
     * --------------------------------------------------------------------*/

    for (i=0; i<width; i++) {
	pat = ap(pat,inventVar());
    }
    pat = singleton(pat);
    for (i=0; nonNull(dsels); dsels=tl(dsels)) {
	name(hd(dsels)).defn = singleton(pair(pat,
					      ap(mkInt(cclass(c).line),
						 nthArg(i++,hd(pat)))));
	genDefns	     = cons(hd(dsels),genDefns);
    }
    for (mems=cclass(c).members; nonNull(mems); mems=tl(mems)) {
	name(hd(mems)).defn  = singleton(pair(pat,
					      ap(mkInt(name(hd(mems)).line),
						 nthArg(i++,hd(pat)))));
	genDefns	     = cons(hd(mems),genDefns);
    }
}

static Void local typeInstDefn(in)	/* Type check implementations of   */
Inst in; {				/* member functions for instance in*/

    /* ----------------------------------------------------------------------
     * Generate code for instance specific dictionary builder function:
     *
     *   inst.maker d1 ... dn = let sc1    = ...
     *					   .
     *					   .
     *					   .
     *				    scm    = ...
     *				    vj ... = ...
     *				    d      = Make.C sc1 ... scm v1 ... vk
     *				in d
     *
     * where sci are superclass dictionaries, d is a new name, vj
     * is a newly generated name corresponding to the implementation of a
     * member function.  (Additional line number values must be added at
     * appropriate places but, for clarity, these are not shown above.)
     * If no implementation of a particular vj is available, then we use
     * the default implementation, partially applied to d.
     * --------------------------------------------------------------------*/

    Int  alpha   = newKindedVars(cclass(inst(in).c).kinds);
    List supers  = makePredAss(cclass(inst(in).c).supers,alpha);
    Int  beta    = newKindedVars(inst(in).kinds);
    List params  = makePredAss(inst(in).specifics,beta);
    Cell d       = inventDictVar();
    /* A previous comment indicated that evids may be wrong
       in some cases when you have overlapping instances.
       (the fix was to add self to the list of evidence)
       I can't reconstruct what I meant by that comment,
       and adding self can cause definitions that are unintentionally
       self-recursive. */
    List evids   = dupList(params);

    List imps    = inst(in).implements;
    Cell l	 = mkInt(inst(in).line);
    Cell dictDef = cclass(inst(in).c).dcon;
    List mems    = cclass(inst(in).c).members;
    List defs    = cclass(inst(in).c).defaults;
    List args	 = NIL;
    List locs	 = NIL;
    List ps;

    if (!unifyPred(cclass(inst(in).c).head,alpha,inst(in).head,beta))
	internal("typeInstDefn");

    for (ps=params; nonNull(ps); ps=tl(ps))	/* Build arglist	   */
	args = cons(thd3(hd(ps)),args);
    args = rev(args);

    for (ps=supers; nonNull(ps); ps=tl(ps)) {	/* Superclass dictionaries */
	Cell pi = hd(ps);
	Cell ev = NIL;
#if EXPLAIN_INSTANCE_RESOLUTION
	if (showInstRes) {
	    fputs("scEntail: ", stdout);
	    printContext(stdout,copyPreds(params));
	    fputs(" ||- ", stdout);
	    printPred(stdout, copyPred(fst3(pi),intOf(snd3(pi))));
	    fputc('\n', stdout);
	}
#endif
	ev = scEntail(params,fst3(pi),intOf(snd3(pi)),0);
	if (isNull(ev)) {
#if EXPLAIN_INSTANCE_RESOLUTION
	    if (showInstRes) {
		fputs("inEntail: ", stdout);
		printContext(stdout,copyPreds(evids));
		fputs(" ||- ", stdout);
		printPred(stdout, copyPred(fst3(pi),intOf(snd3(pi))));
		fputc('\n', stdout);
	    }
#endif
	    ev = inEntail(evids,fst3(pi),intOf(snd3(pi)),0);
	}
	if (isNull(ev)) {
	    clearMarks();
	    ERRMSG(inst(in).line) "Cannot build superclass instance" ETHEN
	    ERRTEXT "\n*** Instance            : " ETHEN
		ERRPRED(copyPred(inst(in).head,beta));
	    ERRTEXT "\n*** Context supplied    : " ETHEN
		ERRCONTEXT(copyPreds(params));
	    ERRTEXT "\n*** Required superclass : " ETHEN
		ERRPRED(copyPred(fst3(pi),intOf(snd3(pi))));
	    ERRTEXT "\n"
	    EEND;
	}
	locs	= cons(pair(thd3(pi),singleton(pair(NIL,ap(l,ev)))),locs);
	dictDef = ap(dictDef,thd3(pi));
    }

    for (; nonNull(defs); defs=tl(defs)) {
	Cell imp = NIL;
	if (nonNull(imps)) {
	    imp  = hd(imps);
	    imps = tl(imps);
	}
	if (isNull(imp)) {
	    dictDef = ap(dictDef,ap(hd(defs),d));
	} else {
	    Cell v  = inventVar();
	    dictDef = ap(dictDef,v);
	    typeMember("instance member binding",
		       hd(mems),
		       snd(imp),
		       evids,
		       inst(in).head,
		       beta);
	    locs     = cons(pair(v,snd(imp)),locs);
	}
	mems = tl(mems);
    }
    locs = cons(pair(d,singleton(pair(NIL,ap(l,dictDef)))),locs);

    name(inst(in).builder).defn			/* Register builder imp	   */
	     = singleton(pair(args,ap(LETREC,pair(singleton(locs),
						  ap(l,d)))));
    genDefns = cons(inst(in).builder,genDefns);
}

static Void local typeMember(wh,mem,alts,evids,head,beta)
String wh;				/* Type check alternatives alts of */
Name   mem;				/* member mem for inst type head   */
Cell   alts;				/* at offset beta using predicate  */
List   evids;				/* assignment evids		   */
Cell   head;
Int    beta; {
    Int  line = rhsLine(snd(hd(alts)));
    Type t;
    Int  o;
    Int  m;
    List ps;
    List qs;
    Type rt;

#if DEBUG_TYPES
    Printf("Type check member: ");
    printExp(stdout,mem);
    Printf(" :: ");
    printType(stdout,name(mem).type);
    Printf("\nfor the instance: ");
    printPred(stdout,head);
    Printf("\n");
#endif

    instantiate(name(mem).type);	/* Find required type		   */
    o  = typeOff;
    m  = typeFree;
    t  = dropRank2(typeIs,o,m);
    ps = makePredAss(predsAre,o);
    if (!unifyPred(hd(predsAre),typeOff,head,beta))
	internal("typeMember1");
    clearMarks();
    qs = copyPreds(ps);
    rt = generalize(qs,liftRank2(t,o,m));

#if DEBUG_TYPES
    Printf("Required type is: ");
    printType(stdout,rt);
    Printf("\n");
#endif

    hd(defnBounds) = NIL;		/* Type check each alternative	   */
    hd(depends)    = NODEPENDS;
    enterPendingBtyvs();
    for (preds=NIL; nonNull(alts); alts=tl(alts)) {
	typeAlt(wh,mem,hd(alts),t,o,m);
	qualify(tl(ps),hd(alts));	/* Add any extra dict params	   */
    }
    improve(line,evids,preds);
    leavePendingBtyvs();

    evids = appendOnto(dupList(tl(ps)),	/* Build full complement of dicts  */
		       evids);
    clearMarks();
    normPreds(line);
    qs = elimPredsUsing(evids,NIL);
    if (nonNull(preds) && resolveDefs(genvarType(t,o,NIL),FALSE))
	qs = elimPredsUsing(evids,qs);
    if (nonNull(qs)) {
	ERRMSG(line)
		"Implementation of %s requires extra context",
		 textToStr(name(mem).text) ETHEN
	ERRTEXT "\n*** Expected type   : " ETHEN ERRTYPE(rt);
	ERRTEXT "\n*** Missing context : " ETHEN ERRCONTEXT(copyPreds(qs));
	ERRTEXT "\n"
	EEND;
    }

    resetGenerics();			/* Make sure we're general enough  */
    ps = copyPreds(ps);
    t  = generalize(ps,liftRank2(t,o,m));
#if DEBUG_TYPES
    Printf("Inferred type is: ");
    printType(stdout,t);
    Printf("\n");
#endif
    if (!sameSchemes(t,rt))
	tooGeneral(line,mem,rt,t);
    if (nonNull(preds)) {
	preds = scSimplify(preds);
	cantEstablish(line,wh,mem,t,ps);
    }
}

/* --------------------------------------------------------------------------
 * Type check bodies of bindings:
 * ------------------------------------------------------------------------*/

static Void local typeBind(b)	       /* Type check binding		   */
Cell b; {
    if (isVar(fst(b))) {			       /* function binding */
	Cell ass = findTopBinding(fst(b));
	Int  beta;

	if (isNull(ass))
	    internal("typeBind");

	beta = intOf(defType(snd(ass)));
	enterPendingBtyvs();
	map2Proc(typeDefAlt,beta,fst(b),snd(snd(b)));
	leavePendingBtyvs();
    }
    else {					       /* pattern binding  */
	static String lhsPat = "lhs pattern";
	static String rhs    = "right hand side";
	Int  beta	     = newTyvars(1);
	Pair pb		     = snd(snd(b));
	Int  l		     = rhsLine(snd(pb));

	tcMode  = OLD_PATTERN;
	enterPendingBtyvs();
	fst(pb) = patBtyvs(fst(pb));
	check(l,fst(pb),NIL,lhsPat,aVar,beta);
	tcMode  = EXPRESSION;
	snd(pb) = typeRhs(snd(pb));
	shouldBe(l,rhsExpr(snd(pb)),NIL,rhs,aVar,beta);
	doneBtyvs(l);
	leavePendingBtyvs();
    }
}

static Void local typeDefAlt(beta,v,a) /* type check alt in func. binding  */
Int  beta;
Cell v;
Pair a; {
    static String valDef = "function binding";
    typeAlt(valDef,v,a,aVar,beta,0);
}

static Cell local typeRhs(e)	       /* check type of rhs of definition  */
Cell e; {
    switch (whatIs(e)) {
	case GUARDED : {   Int beta = newTyvars(1);
			   map1Proc(guardedType,beta,snd(e));
			   tyvarType(beta);
		       }
		       break;

	case LETREC  : enterBindings();
		       enterSkolVars();
		       mapProc(typeBindings,fst(snd(e)));
		       snd(snd(e)) = typeRhs(snd(snd(e)));
		       leaveBindings();
		       leaveSkolVars(rhsLine(snd(snd(e))),typeIs,typeOff,0);
		       break;

	case RSIGN   : fst(snd(e)) = typeRhs(fst(snd(e)));
		       shouldBe(rhsLine(fst(snd(e))),
				rhsExpr(fst(snd(e))),NIL,
				"result type",
				snd(snd(e)),0);
		       return fst(snd(e));

	default      : snd(e) = typeExpr(intOf(fst(e)),snd(e));
		       break;
    }
    return e;
}

static Void local guardedType(beta,gded)/* check type of guard (li,(gd,ex))*/
Int  beta;			       /* should have gd :: Bool,	   */
Cell gded; {			       /*	      ex :: (var,beta)	   */
    static String guarded = "guarded expression";
    static String guard   = "guard";
    Int line = intOf(fst(gded));
#if IPARAM
    List svPreds;
#endif

    gded     = snd(gded);
    spCheck(line,fst(gded),NIL,guard,typeBool,0);
    spCheck(line,snd(gded),NIL,guarded,aVar,beta);
}

Cell rhsExpr(rhs)		       /* find first expression on a rhs   */
Cell rhs; {
    STACK_CHECK
    switch (whatIs(rhs)) {
	case GUARDED : return snd(snd(hd(snd(rhs))));
	case LETREC  : return rhsExpr(snd(snd(rhs)));
	case RSIGN   : return rhsExpr(fst(snd(rhs)));
	default      : return snd(rhs);
    }
}

Int rhsLine(rhs)		       /* find line number associated with */
Cell rhs; {			       /* a right hand side		   */
    STACK_CHECK
    switch (whatIs(rhs)) {
	case GUARDED : return intOf(fst(hd(snd(rhs))));
	case LETREC  : return rhsLine(snd(snd(rhs)));
	case RSIGN   : return rhsLine(fst(snd(rhs)));
	default      : return intOf(fst(rhs));
    }
}

/* --------------------------------------------------------------------------
 * Calculate generalization of types and compare with declared type schemes:
 * ------------------------------------------------------------------------*/

static Void local genBind(ps,b)		/* Generalize the type of each var */
List ps;				/* defined in binding b, qualifying*/
Cell b; {				/* each with the predicates in ps. */
    Cell v = fst(b);
    Cell t = fst(snd(b));

    if (isVar(fst(b)))
	genAss(rhsLine(snd(hd(snd(snd(b))))),ps,v,t);
    else {
	Int line = rhsLine(snd(snd(snd(b))));
	for (; nonNull(v); v=tl(v)) {
	    Type ty = NIL;
	    if (nonNull(t)) {
		ty = hd(t);
		t  = tl(t);
	    }
	    genAss(line,ps,hd(v),ty);
	}
    }
}

static Void local genAss(l,ps,v,dt)	/* Calculate inferred type of v and*/
Int  l;					/* compare with declared type, dt, */
List ps;				/* if given & check for ambiguity. */
Cell v;
Type dt; {
    Cell ass = findTopBinding(v);

    if (isNull(ass))
	internal("genAss");

    snd(ass) = genTest(l,v,ps,dt,aVar,intOf(defType(snd(ass))));

#if DEBUG_TYPES
    printExp(stdout,v);
    Printf(" :: ");
    printType(stdout,snd(ass));
    Printf("\n");
#endif
}

static Type local genTest(l,v,ps,dt,t,o)/* Generalize and test inferred	   */
Int  l;					/* type (t,o) with context ps	   */
Cell v;					/* against declared type dt for v. */
List ps;
Type dt;
Type t;
Int  o; {
    Type bt = NIL;			/* Body of inferred type	   */
    Type it = NIL;			/* Full inferred type		   */

    resetGenerics();			/* Calculate Haskell typing	   */
    ps = copyPreds(ps);
    bt = copyType(t,o);
    it = generalize(ps,bt);

    if (nonNull(dt)) {			/* If a declared type was given,   */
	instantiate(dt);		/* check body for match.	   */
	if (!equalTypes(typeIs,bt))
	    tooGeneral(l,v,dt,it);
    }
    else if (nonNull(ps))		/* Otherwise test for ambiguity in */
	if (isAmbiguous(it))		/* inferred type.		   */
	    ambigError(l,"inferred type",v,it);

    return it;
}

static Type local generalize(qs,t)	/* calculate generalization of t   */
List qs;				/* having already marked fixed vars*/
Type t; {				/* with qualifying preds qs	   */
    if (nonNull(qs))
	t = ap(QUAL,pair(qs,t));
    if (nonNull(genericVars)) {
	Kind k  = STAR;
	List vs = genericVars;
	for (; nonNull(vs); vs=tl(vs)) {
	    Tyvar *tyv = tyvar(intOf(hd(vs)));
	    Kind   ka  = tyv->kind;
	    k = ap(ka,k);
	}
	t = mkPolyType(k,t);
#if DEBUG_KINDS
	Printf("Generalized type: ");
	printType(stdout,t);
	Printf(" ::: ");
	printKind(stdout,k);
	Printf("\n");
#endif
    }
    return t;
}

static Bool local equalTypes(t1,t2)    /* Compare simple types for equality*/
Type t1, t2; {
    STACK_CHECK
et: if (whatIs(t1)!=whatIs(t2))
	return FALSE;

    switch (whatIs(t1)) {
#if TREX
	case EXT     :
#endif
	case TYCON   :
	case OFFSET  :
	case TUPLE   : return t1==t2;

	case INTCELL : return intOf(t1)!=intOf(t2);

	case AP      : if (equalTypes(fun(t1),fun(t2))) {
			   t1 = arg(t1);
			   t2 = arg(t2);
			   goto et;
		       }
		       return FALSE;

	default      : internal("equalTypes");
    }

    return TRUE;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Entry points to type checker:
 * ------------------------------------------------------------------------*/

Type typeCheckExp(useDefs)		/* Type check top level expression */
Bool useDefs; {				/* using defaults if reqd	   */
    Type type;
    List ctxt;
    Int  beta;

    typeChecker(RESET);
    emptySubstitution();
    enterBindings();
    inputExpr = typeExpr(0,inputExpr);
    type      = typeIs;
    beta      = typeOff;
    clearMarks();
    improve(0,NIL,preds);
    normPreds(0);
    elimTauts();
    preds     = scSimplify(preds);
    if (useDefs && nonNull(preds)) {
	clearMarks();
	reducePreds();
	if (nonNull(preds) && resolveDefs(NIL,TRUE)) /* Nearly Haskell 1.4? */
	    elimTauts();
    }
    resetGenerics();
    ctxt      = copyPreds(preds);
    type      = generalize(ctxt,copyType(type,beta));
    inputExpr = qualifyExpr(0,preds,inputExpr);
    h98CheckInferredType(0,inputExpr,type);
    typeChecker(RESET);
    emptySubstitution();
    return type;
}

Void typeCheckDefns() { 	       /* Type check top level bindings    */
    Target t  = length(selDefns)  + length(valDefns) +
		length(instDefns) + length(classDefns);
    Target i  = 0;
    List   gs;

    typeChecker(RESET);
    emptySubstitution();
    enterSkolVars();
    enterBindings();
    setGoal("Type checking",t);

    for (gs=selDefns; nonNull(gs); gs=tl(gs)) {
	mapOver(typeSel,hd(gs));
	soFar(i++);
    }
    for (gs=valDefns; nonNull(gs); gs=tl(gs)) {
	typeDefnGroup(hd(gs));
	soFar(i++);
    }
    mapProc(typeForeignExport,foreignExports); /* ToDo: soFar magic */
    clearTypeIns();
    for (gs=classDefns; nonNull(gs); gs=tl(gs)) {
	emptySubstitution();
	typeClassDefn(hd(gs));
	soFar(i++);
    }
    for (gs=instDefns; nonNull(gs); gs=tl(gs)) {
	emptySubstitution();
	typeInstDefn(hd(gs));
	soFar(i++);
    }

    typeChecker(RESET);
    emptySubstitution();
    done();
}

static Void local typeDefnGroup(bs)	/* type check group of value defns */
List bs; {				/* (one top level scc)		   */
    List as;

    emptySubstitution();
    hd(defnBounds) = NIL;
    preds	   = NIL;
    setTypeIns(bs);
    typeBindings(bs);			/* find types for vars in bindings */

    if (nonNull(preds)) {
	Cell v = fst(hd(hd(varsBounds)));
	Name n = findName(textOf(v));
	Int  l = nonNull(n) ? name(n).line : 0;
	preds  = scSimplify(preds);
	ERRMSG(l) "Instance%s of ", (length(preds)==1 ? "" : "s") ETHEN
	ERRCONTEXT(copyPreds(preds));
	ERRTEXT	  " required for definition of " ETHEN
	ERREXPR(nonNull(n)?n:v);
	ERRTEXT   "\n"
	EEND;
    }

    if (nonNull(hd(skolVars))) {
	Cell b = hd(bs);
	Name n = findName(isVar(fst(b)) ? textOf(fst(b)) : textOf(hd(fst(b))));
	Int  l = nonNull(n) ? name(n).line : 0;
	leaveSkolVars(l,typeUnit,0,0);
	enterSkolVars();
    }

    for (as=hd(varsBounds); nonNull(as); as=tl(as)) {
	Cell a = hd(as);		/* add infered types to environment*/
	Name n = findName(textOf(fst(a)));
	if (isNull(n))
	    internal("typeDefnGroup");
	name(n).type = snd(a);
    }
    hd(varsBounds) = NIL;
}

static Void local typeForeignExport(n)	/* Typecheck a foreign export decl */
Name n; {				
    Int line = name(n).line;
    /* todo */
#if 0
    /* Old comment from checkForeignExport: */

    /* The following doesn't work because the type written into the
     * dummy binding has been through the typechecker once already
     * so it has the wrong type.
     * What's needed here is something like what we do for bindings
     * in instance decls: insert enough dictionaries to make the export
     * have the stated type (or report why this can't be done).
     */
    /* We have to generate a dummy definition to
     * pass to the typechecker.  This is done here rather than in
     * foreign export because valDefns gets set at the end of parsing
     * which would overwrite the result of the following assignment.
     */
    Cell v   = mkVar(name(p).text);
    Cell rhs = pair(mkInt(line),name(p).defn);
    Cell alt = pair(NIL,rhs);
    valDefns = cons(pair(v,pair(name(p).type,singleton(alt))),valDefns);
#else
    ERRMSG(line) "Foreign export not implemented yet."
    EEND;
#endif
}

static Pair local typeSel(s)		/* Calculate a suitable type for a */
Name s; {				/* particular selector, s.	   */
    List cns  = name(s).defn;
    Int  line = name(s).line;
    Type dom  = NIL;			/* Inferred domain		   */
    Type rng  = NIL;			/* Inferred range		   */
    Cell nv   = inventVar();
    List alts = NIL;
    Int  o;
    Int  m;

#if DEBUG_SELS
    Printf("Selector %s, cns=",textToStr(name(s).text));
    printExp(stdout,cns);
    Putchar('\n');
#endif

    emptySubstitution();
    preds = NIL;

    for (; nonNull(cns); cns=tl(cns)) {
	Name c   = fst(hd(cns));
	Int  n   = intOf(snd(hd(cns)));
	Int  a   = name(c).arity;
	Cell pat = c;
	Type dom1;
	Type rng1;
	Int  o1;
	Int  m1;

	instantiate(name(c).type);	/* Instantiate constructor type	   */
	o1 = typeOff;
	m1 = typeFree;
	for (; nonNull(predsAre); predsAre=tl(predsAre))
	    assumeEvid(hd(predsAre),o1);

	if (whatIs(typeIs)==RANK2)	/* Skip rank2 annotation, if any   */
	    typeIs = snd(snd(typeIs));
	for (; --n>0; a--) {		/* Get range			   */
	    pat    = ap(pat,WILDCARD);
	    typeIs = arg(typeIs);
	}
	rng1   = dropRank1(arg(fun(typeIs)),o1,m1);
	pat    = ap(pat,nv);
	typeIs = arg(typeIs);
	while (--a>0) {			/* And then look for domain	   */
	    pat    = ap(pat,WILDCARD);
	    typeIs = arg(typeIs);
	}
	dom1   = typeIs;

	if (isNull(dom)) {		/* Save first domain type and then */
	    dom = dom1;			/* unify with subsequent domains to*/
	    o   = o1;			/* match up preds and range types  */
	    m   = m1;
	}
	else if (!unify(dom1,o1,dom,o))
	    internal("typeSel1");

	if (isNull(rng))		/* Compare component types	   */
	    rng = rng1;
	else if (!sameSchemes(rng1,rng)) {
	    clearMarks();
	    rng  = liftRank1(rng,o,m);
	    rng1 = liftRank1(rng1,o1,m1);
	    ERRMSG(name(s).line) "Mismatch in field types for selector \"%s\"",
				 textToStr(name(s).text) ETHEN
	    ERRTEXT "\n*** Field type     : " 		 ETHEN ERRTYPE(rng1);
	    ERRTEXT "\n*** Does not match : " 		 ETHEN ERRTYPE(rng);
	    ERRTEXT "\n"
	    EEND;
	}
	alts = cons(pair(singleton(pat),pair(mkInt(line),nv)),alts);
    }
    alts = rev(alts);

    if (isNull(dom) || isNull(rng))	/* Should have been initialized by */
	internal("typeSel2");		/* now, assuming length cns >= 1.  */

    clearMarks();			/* No fixed variables here	   */
    preds = scSimplify(preds);		/* Simplify context		   */
    dom   = copyType(dom,o);		/* Calculate domain type	   */
    instantiate(rng);
    rng   = copyType(typeIs,typeOff);
    if (nonNull(predsAre)) {
	List ps    = makePredAss(predsAre,typeOff);
	List alts1 = alts;
	for (; nonNull(alts1); alts1=tl(alts1)) {
	    Cell body = nv;
	    List qs   = ps;
	    for (; nonNull(qs); qs=tl(qs))
		body = ap(body,thd3(hd(qs)));
	    snd(snd(hd(alts1))) = body;
	}
	preds = appendOnto(preds,ps);
    }
    name(s).type  = generalize(copyPreds(preds),fn(dom,rng));
    name(s).arity = 1 + length(preds);
    map1Proc(qualify,preds,alts);

#if DEBUG_SELS
    Printf("Inferred arity = %d, type = ",name(s).arity);
    printType(stdout,name(s).type);
    Putchar('\n');
#endif

    return pair(s,alts);
}

/* --------------------------------------------------------------------------
 * Type checker control:
 * ------------------------------------------------------------------------*/

Void typeChecker(what)
Int what; {
    switch (what) {
	case RESET   : tcMode	    = EXPRESSION;
		       daSccs	    = NIL;
		       preds	    = NIL;
		       pendingBtyvs = NIL;
		       emptyAssumption();
		       break;

	case MARK    : mark(defnBounds);
		       mark(varsBounds);
		       mark(depends);
		       mark(pendingBtyvs);
		       mark(skolVars);
		       mark(localEvs);
		       mark(savedPs);
		       mark(dummyVar);
		       mark(daSccs);
		       mark(preds);
		       mark(stdDefaults);
		       mark(arrow);
		       mark(boundPair);
		       mark(listof);
		       mark(typeVarToVar);
		       mark(predNum);
		       mark(predFractional);
		       mark(predIntegral);
		       mark(starToStar);
		       mark(predMonad);
#if MUDO
		       mark(predMonadRec);
#endif

#if IO_MONAD
		       mark(typeProgIO);
#endif
		       break;

	case INSTALL : typeChecker(RESET);

		       dummyVar     = inventVar();

		       modulePrelude = newModule(textPrelude);
		       moduleUserPrelude = 0;
		       setCurrModule(modulePrelude);

		       starToStar   = simpleKind(1);

		       typeUnit     = addPrimTycon(findText("()"),
						   STAR,0,DATATYPE,NIL);
		       typeArrow    = addPrimTycon(findText("(->)"),
						   simpleKind(2),2,
						   DATATYPE,NIL);
		       typeList	    = addPrimTycon(findText("[]"),
						   starToStar,1,
						   DATATYPE,NIL);

		       arrow	    = fn(aVar,bVar);
		       listof	    = ap(typeList,aVar);
		       boundPair    = ap(ap(mkTuple(2),aVar),aVar);

		       nameUnit	    = addPrimCfun(findText("()"),0,0,typeUnit);
		       tycon(typeUnit).defn
				    = singleton(nameUnit);

		       nameNil	    = addPrimCfun(findText("[]"),0,1,
						   mkPolyType(starToStar,
							      listof));
		       nameCons     = addPrimCfun(findText(":"),2,2,
						   mkPolyType(starToStar,
							      fn(aVar,
							      fn(listof,
								 listof))));
		       name(nameCons).syntax
				    = mkSyntax(RIGHT_ASS,5);

		       tycon(typeList).defn
				    = cons(nameNil,cons(nameCons,NIL));

		       typeVarToVar = fn(aVar,aVar);
#if TREX
		       typeNoRow    = addPrimTycon(findText("EmptyRow"),
						   ROW,0,DATATYPE,NIL);
		       typeRec	    = addPrimTycon(findText("Rec"),
						   pair(ROW,STAR),1,
						   DATATYPE,NIL);
		       nameNoRec    = addPrimCfun(findText("EmptyRec"),0,0,
							ap(typeRec,typeNoRow));
#else
		       /* bogus definitions to avoid changing the prelude */
		       addPrimCfun(findText("Rec"),      0,0,typeUnit);
		       addPrimCfun(findText("EmptyRow"), 0,0,typeUnit);
		       addPrimCfun(findText("EmptyRec"), 0,0,typeUnit);
#endif

		       break;
    }
}

static Name local linkName(s)
String s; {
    Name n = findName(findText(s));
    if (isNull(n)) {
        ERRMSG(0) "Prelude does not define standard name \"%s\"", s
        EEND;
    }
    return n;
}

static Tycon local linkTycon(s)
String s; {
    Tycon tc = findTycon(findText(s));
    if (isNull(tc)) {
        ERRMSG(0) "Prelude does not define standard type \"%s\"", s
        EEND;
    }
    return tc;
}

static Class local linkClass(s)
String s; {
    Class cc = findClass(findText(s));
    if (isNull(cc)) {
        ERRMSG(0) "Prelude does not define standard class \"%s\"", s
        EEND;
    }
    return cc;
}

Void linkPreludeTC() {			/* Hook to tycons and classes in   */
    if (isNull(typeBool)) {		/* prelude when first loaded	   */
	Int i;

	typeInt      = linkTycon("Int");
	typeInt8     = linkTycon("Int8");
	typeInt16    = linkTycon("Int16");
	typeInt32    = linkTycon("Int32");
	typeInt64    = linkTycon("Int64");
	typeWord     = linkTycon("Word");       /* deprecated */
	typeWord8    = linkTycon("Word8");
	typeWord16   = linkTycon("Word16");
	typeWord32   = linkTycon("Word32");
	typeWord64   = linkTycon("Word64");
	typeFunPtr   = linkTycon("FunPtr");
	typePtr      = linkTycon("Ptr");
	typeAddr     = linkTycon("Addr");       /* deprecated */
	typeFloat    = linkTycon("Float");
	typeDouble   = linkTycon("Double");
	typeChar     = linkTycon("Char");
	typeForeignP = linkTycon("ForeignPtr");
	typeForeign  = linkTycon("ForeignObj"); /* deprecated */
	typeStable   = linkTycon("StablePtr");
#ifdef DOTNET
	typeObject   = linkTycon("Object");
#endif
	typeBool     = linkTycon("Bool");
	typeString   = linkTycon("String");
	typeInteger  = linkTycon("Integer");
	typeMaybe    = linkTycon("Maybe");
	typeOrdering = linkTycon("Ordering");

	stdDefaults  = cons(typeInteger,cons(typeDouble,NIL));

	classEq      = linkClass("Eq");
	classOrd     = linkClass("Ord");
	classIx      = linkClass("Ix");
	classEnum    = linkClass("Enum");
	classShow    = linkClass("Show");
	classRead    = linkClass("Read");
	classBounded = linkClass("Bounded");

	classReal       = linkClass("Real");
	classIntegral   = linkClass("Integral");
	classRealFrac   = linkClass("RealFrac");
	classRealFloat  = linkClass("RealFloat");
	classFractional = linkClass("Fractional");
	classFloating   = linkClass("Floating");
	classNum        = linkClass("Num");
	predNum	        = ap(classNum,aVar);
	predFractional  = ap(classFractional,aVar);
	predIntegral    = ap(classIntegral,aVar);

	classMonad      = linkClass("Monad");
	predMonad       = ap(classMonad,aVar);

#if IO_MONAD
	typeIO          = linkTycon("IO");
	typeProgIO      = ap(typeIO,aVar);
#endif

	/* The following primitives are referred to in derived instances and
	 * hence require types; the following types are a little more general
	 * than we might like, but they are the closest we can get without a
	 * special datatype class.
	 */
	name(nameConCmp).type
	    = mkPolyType(starToStar,fn(aVar,fn(aVar,typeOrdering)));
	name(nameEnRange).type
	    = mkPolyType(starToStar,fn(boundPair,listof));
	name(nameEnIndex).type
	    = mkPolyType(starToStar,fn(boundPair,fn(aVar,typeInt)));
	name(nameEnInRng).type
	    = mkPolyType(starToStar,fn(boundPair,fn(aVar,typeBool)));
	name(nameEnToEn).type
	    = mkPolyType(starToStar,fn(aVar,fn(typeInt,aVar)));
	name(nameEnFrEn).type
	    = mkPolyType(starToStar,fn(aVar,typeInt));
	name(nameEnFrom).type
	    = mkPolyType(starToStar,fn(aVar,listof));
	name(nameEnFrTo).type
	    = name(nameEnFrTh).type
	    = mkPolyType(starToStar,fn(aVar,fn(aVar,listof)));

	for (i=2; i<=NUM_DTUPLES; i++) {/* Add derived instances of tuples */
	    addTupInst(classEq,i);
	    addTupInst(classOrd,i);
	    addTupInst(classShow,i);
	    addTupInst(classRead,i);
	    addTupInst(classIx,i);
	}
    }
}

Void linkPreludeCM() {			/* Hook to cfuns and mfuns in	   */
    if (isNull(nameFalse)) {		/* prelude when first loaded	   */
	nameFalse       = linkName("False");
	nameTrue        = linkName("True");
	nameJust        = linkName("Just");
	nameNothing     = linkName("Nothing");
	nameLeft        = linkName("Left");
	nameRight       = linkName("Right");
	nameLT	        = linkName("LT");
	nameEQ	        = linkName("EQ");
	nameGT	        = linkName("GT");

	nameFromInt     = linkName("fromInt");
	nameFromInteger = linkName("fromInteger");
	nameFromDouble  = linkName("fromDouble");
	nameEq	        = linkName("==");
	nameCompare     = linkName("compare");
	nameLe	        = linkName("<=");
	nameGt	        = linkName(">");
	nameShowsPrec   = linkName("showsPrec");
	nameReadsPrec   = linkName("readsPrec");
	nameIndex       = linkName("index");
	nameInRange     = linkName("inRange");
	nameRange       = linkName("range");
	nameMult        = linkName("*");
	namePlus        = linkName("+");
	nameMinBnd	= linkName("minBound");
	nameMaxBnd	= linkName("maxBound");
	nameReturn      = linkName("return");
	nameBind        = linkName(">>=");
	nameThen        = linkName(">>");
	nameMFail       = linkName("fail");

#if IO_MONAD
        /* The constructor names better match up with the defn
	   of IOErrorType in Prelude. 
	*/
	nameIOError        = linkName("IOError");
	nameAlreadyExists  = linkName("AlreadyExists");
	nameDoesNotExist   = linkName("NoSuchThing");
	nameAlreadyInUse   = linkName("ResourceBusy");
	nameIsFull         = linkName("ResourceExhausted");
	nameEOFErr         = linkName("EOF");
	nameProtocolError  = linkName("ProtocolError");
	nameIllegal        = linkName("IllegalOperation");
	namePermDenied     = linkName("PermissionDenied");
	nameUserErr        = linkName("UserError");
#ifdef DOTNET
	nameNetException   = linkName("DotNetException");
#endif
#endif

	nameArithException = linkName("ArithException");
	nameArrayException = linkName("ArrayException");
	nameErrorCall      = linkName("ErrorCall");
	nameIOException    = linkName("IOException");
	nameNoMethodError  = linkName("NoMethodError");
	nameNonTermination = linkName("NonTermination");
	namePatternMatchFail = linkName("PatternMatchFail");
	nameRecConError    = linkName("RecConError");
	nameRecSelError    = linkName("RecSelError");
	nameRecUpdError    = linkName("RecUpdError");

	nameOverflow       = linkName("Overflow");
	nameDivideByZero   = linkName("DivideByZero");

	nameIndexOutOfBounds = linkName("IndexOutOfBounds");
	nameUndefinedElement = linkName("UndefinedElement");
    }
}

Void linkPreludeFuns() {		/* Hook to cfuns and mfuns in	   */
					/* prelude when first loaded	   */
}
/*-------------------------------------------------------------------------*/
