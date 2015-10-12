/* --------------------------------------------------------------------------
 * Unparse expressions and types - for use in error messages, type checker
 * and for debugging.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: output.c,v $
 * $Revision: 1.40 $
 * $Date: 2005/11/02 15:57:56 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "output.h"
#include "char.h"
#include <ctype.h>

#if OBSERVATIONS
#define DEPTH_LIMIT     150
#else
#define DEPTH_LIMIT     15
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local putChr         Args((Int));
static Void local putStr         Args((String));
static Void local putInt         Args((Int));

static Void local put            Args((Int,Cell));
static Void local putFlds        Args((Cell,List));
static Void local putComp        Args((Cell,List));
static Void local putQual        Args((Cell));
static Bool local isDictVal      Args((Cell));
static Cell local maySkipDict    Args((Cell));
static Void local putAp          Args((Int,Cell));
static Void local putOverInfix   Args((Int,Text,Syntax,Cell));
static Void local putInfix       Args((Int,Text,Syntax,Cell,Cell));
static Void local putSimpleAp    Args((Cell,Int));
static Void local putTuple       Args((Int,Cell));
static Int  local unusedTups     Args((Int,Cell));
static Void local unlexVar       Args((Text));
static Void local unlexFullVar   Args((Name));
static Void local unlexOp        Args((Text));
static Void local unlexCharConst Args((Char));
static Void local unlexStrConst  Args((Text));

static Void local putSigType     Args((Cell));
static Void local putContext     Args((List,List,Int));
static Void local putPred        Args((Cell,Int));
static Void local putType        Args((Cell,Int,Int));
static Void local putModule      Args((Module));
static Void local putTyVar       Args((Int));
static Bool local putTupleType   Args((Cell,Int));
static Void local putApType      Args((Type,Int,Int));

static Void local putKind        Args((Kind));
static Void local putKinds	 Args((Kinds));

#if OBSERVATIONS
static Bool local printObsList   Args((Cell,Int,Bool));
static Void local printArg       Args((FILE *,Cell));
#endif

static Bool local isStrConst	 Args((Cell));
static Void local putStrConst	 Args((Cell));
static Bool local isCharCell	 Args((Cell));
static Char local getCellChar    Args((Cell));

/* --------------------------------------------------------------------------
 * Basic output routines:
 * ------------------------------------------------------------------------*/

static FILE *outputStream;             /* current output stream        	   */
#if DEBUG_SHOWSC || OBSERVATIONS
static Int  outColumn = 0;             /* current output column number 	   */
#endif				       				       
								       
#define OPEN(b)    if (b) putChr('('); 				       
#define CLOSE(b)   if (b) putChr(')'); 				       
								       
static Void local putChr(c)            /* print single character       	   */
Int c; {			       				       
    Putc(c,outputStream);	       				       
#if DEBUG_SHOWSC || OBSERVATIONS		       				       
    outColumn++;		       				       
#endif				       				       
}				       				       
								       
static Void local putStr(s)            /* print string                 	   */
String s; {			       				       
    for (; *s; s++) {		       				       
	Putc(*s,outputStream);	       				       
#if DEBUG_SHOWSC || OBSERVATIONS
	outColumn++;		       				       
#endif				       				       
    }				       				       
}				       				       
								       
static Void local putInt(n)            /* print integer                	   */
Int n; {
    static char intBuf[16];
    sprintf(intBuf,"%d",n);
    putStr(intBuf);
}

/* --------------------------------------------------------------------------
 * Precedence values (See Haskell 1.3 report, p.12):
 * ------------------------------------------------------------------------*/

#define ALWAYS      FUN_PREC           /* Always use parens (unless atomic)*/
				       /* User defined operators have prec */
				       /* in the range MIN_PREC..MAX_PREC  */
#define ARROW_PREC  MAX_PREC           /* for printing -> in type exprs    */
#define COCO_PREC   (MIN_PREC-1)       /* :: is left assoc, low precedence */
#define COND_PREC   (MIN_PREC-2)       /* conditional expressions          */
#define WHERE_PREC  (MIN_PREC-3)       /* where expressions                */
#define LAM_PREC    (MIN_PREC-4)       /* lambda abstraction               */
#define NEVER       LAM_PREC           /* Never use parentheses            */


/* --------------------------------------------------------------------------
 * Print an expression (used to display context of type errors):
 * ------------------------------------------------------------------------*/

static Int putDepth = 0;               /* limits depth of printing DBG     */
static Bool inString= 0;	       /* is char string being put?        */

static Void local put(d,e)             /* print expression e in context of */
Int  d;                                /* operator of precedence d         */
Cell e; {
    List xs;

    if (putDepth>DEPTH_LIMIT) {
	putStr("...");
	return;
    }
    else
	putDepth++;

#if OBSERVATIONS
    if (printingObservations) {
        Cell caf;

        if (!isWhnf(e)) {
            if (inString) putStr("..."); else putStr("_");
            putDepth--;
            return;
        }
        if ((caf = getCaf(e)) && !isNull(name(caf).defn)) {
            put(d, name(caf).defn);
            putDepth--;
            return;
        }
    }
#endif

    switch (whatIs(e)) {
	case FINLIST    : putChr('[');
			  xs = snd(e);
			  if (nonNull(xs)) {
			      put(NEVER,hd(xs));
			      while (nonNull(xs=tl(xs))) {
				  putChr(',');
				  put(NEVER,hd(xs));
			      }
			  }
			  putChr(']');
			  break;

        case AP         : putAp(d,e);
			  break;

	case NAME       : 
#if OBSERVATIONS
			  if (inString) break;
#endif
	                  putModule(name(e).mod);
	                  unlexVar(name(e).text);
			  break;

	case VARIDCELL  :
	case VAROPCELL  :
	case DICTVAR    :
	case CONIDCELL  :
	case CONOPCELL  : 
	                  unlexVar(textOf(e));
			  break;
#if IPARAM
	case IPVAR	: putChr('?');
			  unlexVar(textOf(e));
			  break;

	case WITHEXP	: OPEN(d>WHERE_PREC);
			  putStr("dlet {...} in ");
			  put(WHERE_PREC+1,fst(snd(e)));
			  CLOSE(d>WHERE_PREC);
			  break;
#endif

#if TREX
	case RECSEL	: putChr('#');
			  unlexVar(extText(snd(e)));
			  break;
#endif

	case FREECELL   : putStr("{free!}");
			  break;

	case TUPLE      : putTuple(tupleOf(e),e);
			  break;

	case WILDCARD   : putChr('_');
			  break;

	case ASPAT      : put(NEVER,fst(snd(e)));
			  putChr('@');
			  put(ALWAYS,snd(snd(e)));
			  break;

	case LAZYPAT    : putChr('~');
			  put(ALWAYS,snd(e));
			  break;

#if MUDO
	case MDOCOMP    : putStr("mdo {...}");
			  break;
#endif

	case DOCOMP     : putStr("do {...}");
			  break;

	case COMP       : putComp(fst(snd(e)),snd(snd(e)));
			  break;

	case MONADCOMP  : putComp(fst(snd(snd(e))),snd(snd(snd(e))));
			  break;

	case CHARCELL   : unlexCharConst(charOf(e));
			  break;

	case INTCELL    : {   Int i = intOf(e);
			      if (i<0 && d>=UMINUS_PREC) putChr('(');
			      putInt(i);
			      if (i<0 && d>=UMINUS_PREC) putChr(')');
			  }
			  break;

#if BIGNUMS
	case NEGNUM     :
	case ZERONUM    :
	case POSNUM     : xs = bigOut(e,NIL,d>=UMINUS_PREC);
			  for (; nonNull(xs); xs=tl(xs))
			      putChr(charOf(arg(hd(xs))));
			  break;
#endif

	case DOUBLECELL : {   Double f = (Double)doubleOf(e);
			      if (f<0 && d>=UMINUS_PREC) putChr('(');
			      putStr(doubleToString(f));
			      if (f<0 && d>=UMINUS_PREC) putChr(')');
			  }
			  break;

	case STRCELL    : unlexStrConst(textOf(e));
			  break;

	case LETREC     : OPEN(d>WHERE_PREC);
#if DEBUG_CODE
			  putStr("let {");
			  put(NEVER,fst(snd(e)));
			  putStr("} in ");
#else
			  putStr("let {...} in ");
#endif
			  put(WHERE_PREC+1,snd(snd(e)));
			  CLOSE(d>WHERE_PREC);
			  break;

	case COND       : OPEN(d>COND_PREC);
			  putStr("if ");
			  put(COND_PREC+1,fst3(snd(e)));
			  putStr(" then ");
			  put(COND_PREC+1,snd3(snd(e)));
			  putStr(" else ");
			  put(COND_PREC+1,thd3(snd(e)));
			  CLOSE(d>COND_PREC);
			  break;

	case LAMBDA     : xs = fst(snd(e));
			  if (whatIs(xs)==BIGLAM)
			      xs = snd(snd(xs));
			  while (nonNull(xs) && isDictVal(hd(xs)))
			      xs = tl(xs);
			  if (isNull(xs)) {
			      put(d,snd(snd(snd(e))));
			      break;
			  }
			  OPEN(d>LAM_PREC);
			  putChr('\\');
			  if (nonNull(xs)) {
			      put(ALWAYS,hd(xs));
			      while (nonNull(xs=tl(xs))) {
				  putChr(' ');
				  put(ALWAYS,hd(xs));
			      }
			  }
			  putStr(" -> ");
			  put(LAM_PREC,snd(snd(snd(e))));
			  CLOSE(d>LAM_PREC);
			  break;

	case ESIGN      : OPEN(d>COCO_PREC);
			  put(COCO_PREC,fst(snd(e)));
			  putStr(" :: ");
			  putSigType(snd(snd(e)));
			  CLOSE(d>COCO_PREC);
			  break;

	case BIGLAM	: put(d,snd(snd(e)));
			  break;

	case CASE       : putStr("case ");
			  put(NEVER,fst(snd(e)));
#if DEBUG_CODE
			  putStr(" of {");
			  put(NEVER,snd(snd(e)));
			  putChr('}');
#else
			  putStr(" of {...}");
#endif
			  break;

	case CONFLDS    : putFlds(fst(snd(e)),snd(snd(e)));
			  break;

	case UPDFLDS    : putFlds(fst3(snd(e)),thd3(snd(e)));
			  break;

#if OBSERVATIONS
        case INDIRECT   : if(printingObservations)
                              put(d, snd(e));
                          else {
                              putChr('^');
                              put(ALWAYS,snd(e));
                          }
                          break;
        case OBSERVE    : if(printingObservations)
                              put(d, markedExpr(e));
                          else{
                              putChr('='); putChr('>');
                              put(ALWAYS,markedExpr(e));
                          }
                          break;
#else
	case INDIRECT   : putChr('^');
			  put(ALWAYS,snd(e));
			  break;
#endif
	default         : /*internal("put");*/
			  putChr('$');
			  putInt(e);
			  break;
    }
    putDepth--;
}



static Void local putFlds(exp,fs)       /* Output exp using labelled fields*/
Cell exp;
List fs; {
    put(ALWAYS,exp);
    putStr(" {");
    for (; nonNull(fs); fs=tl(fs)) {
	Cell v = hd(fs);
	if (isVar(v))
	    put(NEVER,v);
	else {
	    Cell f = fst(v);
	    Cell e = snd(v);
	    Text t = isName(f) ? name(f).text :
		     isVar(f)  ? textOf(f)    : inventText();
	    Text s = isName(e) ? name(e).text :
		     isVar(e)  ? textOf(e)    : inventText();

	    put(NEVER,f);
#if HASKELL_98_ONLY
        if (s!=t) {
#else
	    if (haskell98 || s!=t) {
#endif
		putStr(" = ");
		put(NEVER,e);
	    }
	}
	if (nonNull(tl(fs)))
	    putStr(", ");
    }
    putChr('}');
}

static Void local putComp(e,qs)         /* print comprehension             */
Cell e;
List qs; {
    putStr("[ ");
    put(NEVER,e);
    if (nonNull(qs)) {
	putStr(" | ");
	putQual(hd(qs));
	while (nonNull(qs=tl(qs))) {
	    putStr(", ");
	    putQual(hd(qs));
	}
    }
    putStr(" ]");
}

static Void local putQual(q)            /* print list comp qualifier       */
Cell q; {
    switch (whatIs(q)) {
	case BOOLQUAL : put(NEVER,snd(q));
			return;

	case QWHERE   : putStr("let {...}");
			return;

	case FROMQUAL : put(ALWAYS,fst(snd(q)));
			putStr("<-");
			put(NEVER,snd(snd(q)));
			return;
    }
}

static Bool local isDictVal(e)          /* Look for dictionary value       */
Cell e; {
#if !DEBUG_CODE || OBSERVATIONS         /* code definitely needed for obs. */
    Cell h = getHead(e);
    switch (whatIs(h)) {
	case DICTVAR : return TRUE;
	case NAME    : return isDfun(h);
    }
#endif
    return FALSE;
}

static Cell local maySkipDict(e)        /* descend function application,   */
Cell e; {                               /* ignoring dict aps               */
    while (isAp(e) && isDictVal(arg(e)))
	e = fun(e);
    return e;
}

static Void local putAp(d,e)            /* print application (args>=1)     */
Int  d;
Cell e; {
    Cell   h    = NIL;
    Text   t    = NIL;
    Syntax sy   = NIL;
    Int    args = 0;

    for (h=e; isAp(h); h=fun(h))        /* find head of expression, looking*/
	if (!isDictVal(arg(h)))         /* for dictionary arguments        */
	    args++;

    if (args==0) {                      /* Special case when *all* args    */
	put(d,h);                       /* are dictionary values           */
	return;
    }

    switch (whatIs(h)) {
#if NPLUSK
	case ADDPAT     : if (args==1)
			      putInfix(d,textPlus,syntaxOf(namePlus),
					 arg(e),mkInt(intValOf(fun(e))));
			  else
			      putStr("ADDPAT");
			  return;
#endif

	case TUPLE      : OPEN(args>tupleOf(h) && d>=FUN_PREC);
			  putTuple(tupleOf(h),e);
			  CLOSE(args>tupleOf(h) && d>=FUN_PREC);
			  return;

	case NAME       : if (args==1 &&
			      ((h==nameFromInt     && isInt(arg(e)))    ||
			       (h==nameFromInteger && isBignum(arg(e))) ||
			       (h==nameFromDouble  && isDouble(arg(e))))) {
			      put(d,arg(e));
			      return;
			  }
			  t  = name(h).text;
			  sy = syntaxOf(h);
			  break;

	case VARIDCELL  :
	case VAROPCELL  :
	case DICTVAR    :
	case CONIDCELL  :
	case CONOPCELL  : sy = defaultSyntax(t = textOf(h));
			  break;

#if TREX
	case EXT	: if (args==2) {
			      String punc = "(";
			      do {
				  putStr(punc);
				  punc = ", ";
				  putStr(textToStr(extText(h)));
				  putStr(" = ");
				  put(NEVER,extField(e));
				  args = 0;
				  e    = extRow(e);
				  for (h=e; isAp(h); h=fun(h))
				      if (!isDictVal(arg(h)))
					  args++;
			      } while (isExt(h) && args==2);
			      if (e!=nameNoRec) {
				  putStr(" | ");
				  put(NEVER,e);
			      }
			      putChr(')');
			      return;
			  }
			  else if (args<2)
			      internal("putExt");
			  else
			      args-=2;
			  break;
#endif

	default         : sy = APPLIC;
			  break;
    }

    e = maySkipDict(e);

    if (sy==APPLIC) {                   /* print simple application        */
	OPEN(d>=FUN_PREC);
	putSimpleAp(e,args);
	CLOSE(d>=FUN_PREC);
	return;
    }
    else if (args==1) {                 /* print section of the form (e+)  */
	putChr('(');
	put(FUN_PREC-1,arg(e));
	putChr(' ');
	unlexOp(t);
	putChr(')');
    }
    else if (args==2 && isStrConst(e))
        putStrConst(e);
    else if (args==2)                  /* infix expr of the form e1 + e2   */
	putInfix(d,t,sy,arg(maySkipDict(fun(e))),arg(e));
    else {                             /* o/w (e1 + e2) e3 ... en   (n>=3) */
	OPEN(d>=FUN_PREC);
	putOverInfix(args,t,sy,e);
	CLOSE(d>=FUN_PREC);
    }
}

static Void local putStrConst(e)
Cell e; {
    putChr('"'); 
    while (isAp(e) && isAp(fun(e)) && fun(fun(e))==nameCons) {
        putStr(unlexChar(getCellChar(arg(fun(e))),'"'));
        e = arg(e);
    }
    if (e!=nameNil)
        internal("putStrConst");
    putChr('"');
}

static Bool local isStrConst(e)
Cell e; {
    while (isAp(e) && isAp(fun(e)) && fun(fun(e))==nameCons) {
        if (!isCharCell(arg(fun(e))))
            return FALSE;
        e = arg(e);
    }
    return e==nameNil;
}

static Bool local isCharCell(e)
Cell e; {
    while(1)
        switch(whatIs(e)) {
	    case CHARCELL	: return 1;
	    case INDIRECT	: e = snd(e);
	    			  break;
#if OBSERVATIONS
	    case OBSERVE	: e = markedExpr(e);
	    			  break;
#endif
	    default		: return 0;
	}
}

static Char local getCellChar(e)
Cell e; {
    while(1)
        switch(whatIs(e)) {
	    case CHARCELL	: return charOf(e);
	    case INDIRECT	: e = snd(e);
	    			  break;
#if OBSERVATIONS
	    case OBSERVE	: e = markedExpr(e);
	    			  break;
#endif
	    default		: internal("error in getCellChar");
	}
}

static Void local putOverInfix(args,t,sy,e)
Int    args;                           /* infix applied to >= 3 arguments  */
Text   t;
Syntax sy;
Cell   e; {
    if (args>2) {
	putOverInfix(args-1,t,sy,maySkipDict(fun(e)));
	putChr(' ');
	put(FUN_PREC,arg(e));
    }
    else
	putInfix(ALWAYS,t,sy,arg(maySkipDict(fun(e))),arg(e));
}

static Void local putInfix(d,t,sy,e,f)  /* print infix expression          */
Int    d;
Text   t;                               /* Infix operator symbol           */
Syntax sy;                              /* with name t, syntax s           */
Cell   e, f; {                          /* Left and right operands         */
    Syntax a = assocOf(sy);
    Int    p = precOf(sy);
    OPEN(d>p);
    put((a==LEFT_ASS ? p : 1+p), e);
    putChr(' ');
    unlexOp(t);
    putChr(' ');
    put((a==RIGHT_ASS ? p : 1+p), f);
    CLOSE(d>p);
}

static Void local putSimpleAp(e,n)      /* print application e0 e1 ... en  */
Cell e; 
Int  n; {
    if (n>0) {
	putSimpleAp(maySkipDict(fun(e)),n-1);
	putChr(' ');
	put(FUN_PREC,arg(e));
    }
    else
	put(FUN_PREC,e);
}

static Void local putTuple(ts,e)        /* Print tuple expression, allowing*/
Int  ts;                                /* for possibility of either too   */
Cell e; {                               /* few or too many args to constr  */
    Int i;
    putChr('(');
    if ((i=unusedTups(ts,e))>0) {
	while (--i>0)
	    putChr(',');
	putChr(')');
    }
}

static Int local unusedTups(ts,e)       /* print first part of tuple expr  */
Int  ts;                                /* returning number of constructor */
Cell e; {                               /* args not yet printed ...        */
    if (isAp(e)) {
	if ((ts=unusedTups(ts,fun(e))-1)>=0) {
	    put(NEVER,arg(e));
	    putChr(ts>0?',':')');
	}
	else {
	    putChr(' ');
	    put(FUN_PREC,arg(e));
	}
    }
    return ts;
}

static Void local unlexVar(t)          /* print text as a variable name    */
Text t; {                              /* operator symbols must be enclosed*/
    String s = textToStr(t);           /* in parentheses... except [] ...  */
    if ((isascii(s[0]) && isalpha(s[0])) || s[0]=='_' || s[0]=='[' || s[0]=='(')
	putStr(s);
    else {
	putChr('(');
	putStr(s);
	putChr(')');
    }
}

static Void local unlexFullVar(n)  /* print text as a variable name    */
Name n; {                              
  Module m = name(n).mod;
  Text t = name(n).text;
  if (name(n).parent 
      && isName(name(n).parent)
      && isPair(name(name(n).parent).defn)
      && snd(name(name(n).parent).defn) == n) {
    /* Constructor with strict fields are handled in
     * a strange manner. Here we print the true construtor name
     */
    unlexFullVar(name(n).parent);  
  } else {
    if (name(n).primDef) {
      putStr("Prelude");
    } else {
      putStr(textToStr(module(m).text));
    }
    putChr('.');
    putStr(textToStr(t));
  }
}


static Void local unlexOp(t)           /* print text as operator name      */
Text t; {                              /* alpha numeric symbols must be    */
    String s = textToStr(t);           /* enclosed by backquotes           */

    if (isascii(s[0]) && isalpha(s[0])) {
	putChr('`');
	putStr(s);
	putChr('`');
    }
    else
	putStr(s);
}

static Void local unlexCharConst(c)
Char c; {
    putChr('\'');
    putStr(unlexChar(c,'\''));
    putChr('\'');
}

static Void local unlexStrConst(t)
Text t; {
    String s            = textToStr(t);
    static Char SO      = 14;          /* ASCII code for '\SO'             */
    Bool   lastWasSO    = FALSE;
    Bool   lastWasDigit = FALSE;
    Bool   lastWasEsc   = FALSE;

    putChr('\"');
    while (*s) {
      String ch;
	Char   c  = ' ';
	Char   sc = getStrChr(&s);

	ch = unlexChar(sc,'\"');

	if ((lastWasSO && *ch=='H') ||
		(lastWasEsc && lastWasDigit && isascii(*ch) && isdigit(*ch)))
	    putStr("\\&");

	lastWasEsc   = (*ch=='\\');
	lastWasSO    = (sc==SO);
	for (; *ch; c = *ch++)
	    putChr(*ch);
	lastWasDigit = (isascii(c) && isdigit(c));
    }
    putChr('\"');
}

/* --------------------------------------------------------------------------
 * Pretty printer for supercombinator definitions:
 * (i.e., for lambda-lifter output, immediately prior to code generation.)
 * ------------------------------------------------------------------------*/

#if DEBUG_SHOWSC
static Void local pIndent       Args((Int));
static Void local pPut          Args((Int,Cell,Int));
static Void local pPutAp        Args((Int,Cell,Int));
static Void local pPutSimpleAp  Args((Cell,Int));
static Void local pPutTuple     Args((Int,Cell,Int));
static Int  local punusedTups   Args((Int,Cell,Int));
static Void local pPutOffset    Args((Int));
static Int  local pPutLocals    Args((List,Int));
static Void local pLiftedStart  Args((Cell,Int,String));
static Void local pLifted       Args((Cell,Int,String));
static Int  local pDiscr        Args((Cell,Int));

static Void local pIndent(n)           /* indent to particular position    */
Int n; {
    outColumn = n;
    while (0<n--) {
	Putc(' ',outputStream);
    }
}

static Void local pPut(d,e,co)         /* pretty print expr in context of  */
Int  d;                                /* operator of precedence d         */
Cell e;                                /* with current offset co           */
Int  co; {
    switch (whatIs(e)) {
	case AP         : pPutAp(d,e,co);
			  break;

	case OFFSET     : pPutOffset(offsetOf(e));
			  break;

        case NAME       : 
                          unlexFullVar(e);
			  break;

#if TREX
	case RECSEL	: putStr("_recSel");
			  break;

	case EXT	: putStr("_recExt");
			  break;
#endif

	case TUPLE      : pPutTuple(tupleOf(e),e,co);
			  break;

	case CHARCELL   : unlexCharConst(charOf(e));
			  break;

	case INTCELL    : {   Int i = intOf(e);
			      if (i<0 && d>=UMINUS_PREC) putChr('(');
			      putInt(i);
			      if (i<0 && d>=UMINUS_PREC) putChr(')');
			  }
			  break;

	case DOUBLECELL : {   Double f = (Double)doubleOf(e);
			      if (f<0 && d>=UMINUS_PREC) putChr('(');
			      putStr(doubleToString(f));
			      if (f<0 && d>=UMINUS_PREC) putChr(')');
			  }
			  break;

	case STRCELL    : unlexStrConst(textOf(e));
			  break;
#if BIGNUMS
	case NEGNUM     :
	case ZERONUM    :
	case POSNUM     : {   List xs = bigOut(e,NIL,d>=UMINUS_PREC);
			      for (; nonNull(xs); xs=tl(xs))
				  putChr(charOf(arg(hd(xs))));
			  }
			  break;
#endif
	case LETREC     : OPEN(d>WHERE_PREC);
			  co += pPutLocals(fst(snd(e)),co);
			  pPut(WHERE_PREC+1, snd(snd(e)), co);
			  CLOSE(d>WHERE_PREC);
			  break;

        case COND       : { Int  left = outColumn;
	                    OPEN(d>COND_PREC);
			    putStr("case ");
			    pPut(COND_PREC+1,fst3(snd(e)),co);
			    putStr(" of");
			    pIndent(left+2);
			    putStr(" { Prelude.True -> ");
			    pPut(COND_PREC+1,snd3(snd(e)),co);
			    pIndent(left+2);
			    putStr(" ; Prelude.False ->  ");
			    pPut(COND_PREC+1,thd3(snd(e)),co);
			    pIndent(left+2);
			    putStr("}");
			    CLOSE(d>COND_PREC);
			    break;
	                   }
	default         : printf("[e = %d, whatIs(e) = %d]\n",e,whatIs(e));
	                  internal("pPut");
    }
}

static Void local pPutAp(d,e,co)        /* print application (args>=1)     */
Int  d;
Cell e;
Int  co; {
    Cell h = getHead(e);
    if (isTuple(h)) {
	Int args = argCount;
	OPEN(args>tupleOf(h) && d>=FUN_PREC);
	pPutTuple(tupleOf(h),e,co);
	CLOSE(args>tupleOf(h) && d>=FUN_PREC);
	return;
    }
    OPEN(d>=FUN_PREC);
    pPutSimpleAp(e,co);
    CLOSE(d>=FUN_PREC);
}

static Void local pPutSimpleAp(e,co)    /* print application e0 e1 ... en  */
Cell e;
Int  co; {
    if (isAp(e)) {
	pPutSimpleAp(fun(e),co);
	putChr(' ');
	pPut(FUN_PREC,arg(e),co);
    }
    else
	pPut(FUN_PREC,e,co);
}

static Void local pPutTuple(ts,e,co)    /* Print tuple expression, allowing*/
Int  ts;                                /* for possibility of either too   */
Cell e;                                 /* few or too many args to constr  */
Int  co; {
    Int i;
    putStr("(#");
    putInt(ts);
    putChr(' ');
    if ((i=punusedTups(ts,e,co))>0) {
	while (--i>0)
	    putChr(' ');
    }
    putChr(')');
}

static Int local punusedTups(ts,e,co)   /* print first part of tuple expr  */
Int  ts;                                /* returning number of constructor */
Cell e;                                 /* args not yet printed ...        */
Int  co; {
    if (isAp(e)) {
	if ((ts=punusedTups(ts,fun(e),co)-1)>=0) {
	    pPut(ALWAYS,arg(e),co);
	    putStr(ts>0?" ":"");
	}
	else {
	    putChr(' ');
	    pPut(FUN_PREC,arg(e),co);
	}
    }
    return ts;
}

static Void local pPutOffset(n)         /* pretty print offset number      */
Int n; {
    putChr('_');
    putInt(n);
}

static Int local pPutLocals(vs,co)      /* pretty print locals             */
List vs;
Int  co; {
    Int left = outColumn;
    Int n    = length(vs);
    Int i;

    putStr("let ");
    for (i=0; i<n; i++) {
	pPutOffset(co+i+1);
	putChr(' ');
	pLiftedStart(hd(vs),co+n,"=");
	vs = tl(vs);
	if (nonNull(vs))
	    pIndent(left+4);
    }
    pIndent(left);
    putStr("in ");
    return n;
}

static Void local pLiftedStart(e,co,eq) /* print start of definition       */
Cell   e;
Int    co;
String eq; {
    putStr(eq);
    putChr(' ');
    pLifted(e,co,eq);
}

static Void local pLifted(e,co,eq)      /* print lifted definition         */
Cell   e;
Int    co;
String eq; {
    switch (whatIs(e)) {
	case GUARDED : {   Int  left = outColumn;
	                   Int count = 0;
                           Int tmp;
			   List gs   = snd(e);
			   if (isNull(gs))
			       internal("pLifted");
			   for (;;) {
			       count++;
			       putStr("case ");
			       pPut(NEVER,fst(hd(gs)),co);
			       putStr(" of\n");
			       pIndent(left + 2);
			       putStr("{ Prelude.True -> ");
			       pPut(NEVER,snd(hd(gs)),co);
			       putStr("\n");
			       pIndent(left + 2);
			       putStr("; _ -> \n");
			       gs = tl(gs);
			       pIndent(left);
			       if (!nonNull(gs))
				   break;
			   }
			   putStr("_fatbar ");
			   for(tmp = 0;tmp < count;tmp++) {
			     putChr('}');
			   }
			   putStr(";\n");
		       }
		       break;

	case LETREC  : co += pPutLocals(fst(snd(e)),co);
		       pLifted(snd(snd(e)), co, eq);
		       break;

	case FATBAR  : {   Int left = outColumn;
                           putStr("let_ _fatbar = \n");
			   pIndent(left+2);
			   pLifted(snd(snd(e)),co,eq);
			   pIndent(left);
                           putStr("in\n");
			   pIndent(left+2);
			   pLifted(fst(snd(e)),co,eq);
		       }
		       break;

	case CASE    : {   Int  left = outColumn;
			   List cs   = snd(snd(e));
			   putStr("case ");
			   pPut(NEVER,fst(snd(e)),co);
			   putStr(" of {\n");
			   for (; nonNull(cs); cs=tl(cs)) {
			       Int arity;
			       pIndent(left+2);
			       arity = pDiscr(fst(hd(cs)),co);
			       putChr(' ');
			       pLiftedStart(snd(hd(cs)),co+arity,"->");
			   }
			   pIndent(left+2);
			   putStr("_ -> _fatbar\n");
			   pIndent(left);
			   putStr("};\n");
		       }
		       break;

#if TREX
	case EXTCASE :
#endif
	case NUMCASE : {   Int  left = outColumn;
			   Cell t    = snd(e);
			   Cell h     = getHead(snd3(t));
			   String eqInt     = "Prelude.primPmInt";
			   String eqInteger = "Prelude.primPmInteger";
			   String eqDouble  = "Prelude.primPmFlt";
			   String theEq     = "** BAD EQUALITY **";
			   Int  ar = 0;
			   putStr("case ");
			   switch (whatIs(h)) {
			   case NAME: 
			     if (h==nameFromInt) {
			       theEq = eqInt;
			     } else if (h == nameFromInteger) {
			       theEq = eqInteger;
			     } else if (h == nameFromDouble) {
			       theEq = eqDouble;
			     } else {
			       ERRMSG(0) "error in NUMCASE" EEND;
			     }
			     break;
			   case ADDPAT:
			       ERRMSG(0) "error in NUMCASE " EEND;
			     break;
			   default:
			       ERRMSG(0) "error in NUMCASE" EEND;
			     break;			     
			   }
			   putStr(theEq);
			   putStr(" (");
			   pPut(NEVER,arg(fun(snd3(t))),co);
			   putStr(") (");
			   pPut(NEVER,arg(snd3(t)),co);
			   putStr(") (");
			   pPut(NEVER,fst3(t),co);
			   putStr(") of\n");
			   pIndent(left+2);
			   putStr("{ Prelude.True ");
			   pLiftedStart(thd3(t),co+ar,"->");
			   putStr("\n");
			   pIndent(left+2);
			   putStr("  _ -> _fatbar\n");
			   pIndent(left+2);
			   putStr("};\n");
			   pIndent(left);
		       }
		       break;

	default      : pPut(NEVER,e,co);
		       putStr(";\n");
		       break;
    }
}

static Int local pDiscr(d,co)           /* pretty print discriminator      */
Cell d;
Int  co; {
    Int arity = 0;

    switch (whatIs(d)) {
#if NPLUSK
	case ADDPAT   : pPutOffset(co+1);
			putChr('+');
			putInt(intValOf(d));
			arity = 1;
			break;
#endif

	case NAME     : {   Int i = 0;
			    arity = name(d).arity;
			    unlexFullVar(d);
			    for (; i<arity; ++i) {
				putChr(' ');
				pPutOffset(co+arity-i);
			    }
			}
			break;

	case TUPLE    : {   Int i = 0;
			    arity = tupleOf(d);
			    putChr('#');
			    putInt(arity);
			    putChr(' ');
			    pPutOffset(co+arity);
			    while (++i<arity) {
				putChr(' ');
				pPutOffset(co+arity-i);
			    }
			}
			break;

#if TREX
	case AP       : if (isExt(fun(d))) {
			    putChr('(');
			    putStr(textToStr(extText(fun(d))));
			    putStr("=_|_)");
			    return 2;
			}
			/* intentional fall-thru */
#endif
	default       : pPut(NEVER,d,co);
			break;
    }
    return arity;
}

Void printSc(fp,t,arity,e)              /* Pretty print sc defn on fp      */
FILE *fp;
Text  t;
Int   arity;
Cell  e; {
    Int i;
    outputStream = fp;

    putChr('\n');
    outColumn = 0;
    unlexVar(t);
    for (i=0; i<arity; i++) {
	putChr(' ');
	pPutOffset(arity-i);
    }
    putChr(' ');
    pLiftedStart(e,arity,"=");
}
#endif /* defined( DEBUG_SHOWSC ) */

/* --------------------------------------------------------------------------
 * Print type expression:
 * ------------------------------------------------------------------------*/

static Void local putSigType(t)         /* print (possibly) generic type   */
Cell t; {
    Int fr = 0;
    if (isPolyType(t)) {
	Kinds ks = polySigOf(t);
	for (; isAp(ks); ks=tl(ks))
	    fr++;
	t = monotypeOf(t);
    }

    putType(t,NEVER,fr);		/* Finally, print rest of type ... */
}

static Void local putContext(ps,qs,fr)	/* print context list		   */
List ps;
List qs;
Int  fr; {
    Int len = length(ps) + length(qs);
    Int c   = len;
#if IPARAM
    Bool useParens = len!=1 || isIP(fun(hd(ps)));
#else
    Bool useParens = len!=1;
#endif
    if (useParens)
	putChr('(');
    for (; nonNull(ps); ps=tl(ps)) {
	putPred(hd(ps),fr);
	if (--c > 0) {
	    putStr(", ");
	}
    }
    for (; nonNull(qs); qs=tl(qs)) {
	putPred(hd(qs),fr);
	if (--c > 0) {
	    putStr(", ");
	}
    }
    if (useParens)
	putChr(')');
}

static Void local putPred(pi,fr)	/* Output predicate		   */
Cell pi;
Int  fr; {
    if (isAp(pi)) {
#if TREX
	if (isExt(fun(pi))) {
	    putType(arg(pi),ALWAYS,fr);
	    putChr('\\');
	    putStr(textToStr(extText(fun(pi))));
	    return;
	}
#endif
#if IPARAM
	if (isIP(fun(pi))) {
	    putChr('?');
	    putPred(fun(pi),fr);
	    putStr(" :: ");
	    putType(arg(pi),NEVER,fr);
	    return;
	}
#endif
	putPred(fun(pi),fr);
	putChr(' ');
	putType(arg(pi),ALWAYS,fr);
    }
    else if (isClass(pi)) {
        putModule(cclass(pi).mod);
        putStr(textToStr(cclass(pi).text));
    } else if (isCon(pi)) {
	putStr(textToStr(textOf(pi)));
    }
#if IPARAM
    else if (isIP(pi))
        unlexVar(textOf(pi));
#endif
    else
	putStr("<unknownPredicate>");
}

static Void local putType(t,prec,fr)	/* print nongeneric type expression*/
Cell t;
Int  prec;
Int  fr; {
    switch(whatIs(t)) {
	case TYCON     : 
	                 putModule(tycon(t).mod);
	                 putStr(textToStr(tycon(t).text));
			 break;

	case TUPLE     : {   Int n = tupleOf(t);
			     putChr('(');
			     while (--n > 0)
				 putChr(',');
			     putChr(')');
			 }
			 break;

	case POLYTYPE  : {   Kinds ks = polySigOf(t);
			     OPEN(prec>=ARROW_PREC);
			     putStr("forall ");
			     for (; isAp(ks); ks=tl(ks)) {
				 putTyVar(fr++);
				 if (isAp(tl(ks)))
				     putChr(' ');
			     }
			     putStr(". ");
			     putType(monotypeOf(t),NEVER,fr);
			     CLOSE(prec>=ARROW_PREC);
			 }
			 break;

	case CDICTS    :
	case QUAL      : OPEN(prec>=ARROW_PREC);
			 if (whatIs(snd(snd(t)))==CDICTS) {
			     putContext(fst(snd(t)),fst(snd(snd(snd(t)))),fr);
			     putStr(" => ");
			     putType(snd(snd(snd(snd(t)))),NEVER,fr);
			 } else {
			     putContext(fst(snd(t)),NIL,fr);
			     putStr(" => ");
			     putType(snd(snd(t)),NEVER,fr);
			 }
			 CLOSE(prec>=ARROW_PREC);
			 break;

	case EXIST     :
	case RANK2     : putType(snd(snd(t)),prec,fr);
			 break;

	case OFFSET    : putTyVar(offsetOf(t));
			 break;

	case VARIDCELL :
	case VAROPCELL : putChr('_');
			 unlexVar(textOf(t));
			 break;

	case INTCELL   : putChr('_');
			 putInt(intOf(t));
			 break;

	case AP       : {   Cell typeHead = getHead(t);
			    Bool brackets = (argCount!=0 && prec>=ALWAYS);
			    Int  args	 = argCount;

			    if (typeHead==typeList) {
				if (argCount==1) {
				    putChr('[');
				    putType(arg(t),NEVER,fr);
				    putChr(']');
				    return;
				}
			    }
			    else if (typeHead==typeArrow) {
				if (argCount==2) {
				    OPEN(prec>=ARROW_PREC);
				    putType(arg(fun(t)),ARROW_PREC,fr);
				    putStr(" -> ");
				    putType(arg(t),NEVER,fr);
				    CLOSE(prec>=ARROW_PREC);
				    return;
				}
/*
				else if (argCount==1) {
				    putChr('(');
				    putType(arg(t),ARROW_PREC,fr);
				    putStr("->)");
				    return;
				}
*/
			    }
			    else if (isTuple(typeHead)) {
				if (argCount==tupleOf(typeHead)) {
				    putChr('(');
				    putTupleType(t,fr);
				    putChr(')');
				    return;
				}
			    }
#if TREX
			    else if (isExt(typeHead)) {
				if (args==2) {
				    String punc = "(";
				    do {
					putStr(punc);
					punc = ", ";
					putStr(textToStr(extText(typeHead)));
					putStr(" :: ");
					putType(extField(t),NEVER,fr);
					t        = extRow(t);
					typeHead = getHead(t);
				    } while (isExt(typeHead) && argCount==2);
				    if (t!=typeNoRow) {
					putStr(" | ");
					putType(t,NEVER,fr);
				    }
				    putChr(')');
				    return;
				}
				else if (args<2)
				    internal("putExt");
				else
				    args-=2;
			    }
#endif
			    OPEN(brackets);
			    putApType(t,args,fr);
			    CLOSE(brackets);
			}
			break;

	default       : putStr("(bad type)");
    }
}

static Void local putTyVar(n)           /* print type variable             */
Int n; {
    static String alphabet              /* for the benefit of EBCDIC :-)   */
		="abcdefghijklmnopqrstuvwxyz";
    putChr(alphabet[n%26]);
    if (n /= 26)                        /* just in case there are > 26 vars*/
	putInt(n);
}

static Bool local putTupleType(e,fr)	/* print tuple of types, returning */
Cell e;					/* TRUE if something was printed,  */
Int  fr; {				/* FALSE otherwise; used to control*/
    if (isAp(e)) {			/* printing of intermed. commas    */
	if (putTupleType(fun(e),fr))
	    putChr(',');
	putType(arg(e),NEVER,fr);
	return TRUE;
    }
    return FALSE;
}

static Void local putApType(t,n,fr)	/* print type application          */
Cell t;
Int  n;
Int  fr; {
    if (n>0) {
	putApType(fun(t),n-1,fr);
	putChr(' ');
	putType(arg(t),ALWAYS,fr);
    }
    else
	putType(t,ALWAYS,fr);
}

/* --------------------------------------------------------------------------
 * Print kind expression:
 * ------------------------------------------------------------------------*/

static Void local putKind(k)            /* print kind expression           */
Kind k; {
    switch (whatIs(k)) {
	case AP      : if (isAp(fst(k))) {
			   putChr('(');
			   putKind(fst(k));
			   putChr(')');
		       }
		       else
			   putKind(fst(k));
		       if (whatIs(snd(k)) != NIL) {
			   putStr(" -> ");
			   putKind(snd(k));
		       }
		       break;

#if TREX
	case ROW     : putStr("row");
		       break;
#endif

	case STAR    : putChr('*');
		       break;

	case OFFSET  : putTyVar(offsetOf(k));
		       break;

	case INTCELL : putChr('_');
		       putInt(intOf(k));
		       break;

	default      : putStr("(bad kind)");
    }
}

static Void local putKinds(ks)		/* Print list of kinds		   */
Kinds ks; {
    if (isNull(ks))
	putStr("()");
    else if (nonNull(tl(ks))) {
	putChr('(');
	putKind(hd(ks));
	while (nonNull(ks=tl(ks))) {
	    putChr(',');
	    putKind(hd(ks));
	}
	putChr(')');
    }
    else
	putKind(hd(ks));
}

/* --------------------------------------------------------------------------
 * Print observations
 * ------------------------------------------------------------------------*/
#if OBSERVATIONS
#define DELTA 2

Bool printingObservations = FALSE;
Void newLine   Args((Int));

Void newLine(indent)
Int indent; {
    putChr('\n');
    outColumn = 0;
    while (indent--) putChr(' ');
}

Int countObsList   Args((Cell));
Int countObsList(header)
Cell header;
{
    Int seq, n=0;
    Cell j;

    for (j=firstObs(header); j!=header; j=nextObs(j)) {
        seq  = intOf(seqObs(j));
        if (seq < 0) {          /* non-functional value            */
            n++;    
        } else if (seq!=0) {    /* a function observation  */
	   if (whatIs(exprObs(j)) == OBSERVEHEAD) {
	       n += countObsList(exprObs(j));
           } else {
	       n++;
	   }
	}
    }
    return n;
}
Int countObserve(){
    Observe i;
    Int     n=0;

    i=firstObserve();
    while(i != NIL){
	    n += countObsList(observe(i).head); 
	    i =  nextObserve();
    }    
    return n;
}

Void printObserve(t)
String t;  {
    Observe i;
    String s;

    if (! (i=firstObserve())) return;

    printingObservations = TRUE;
    outputStream = stdout;
#if HUGS_FOR_WINDOWS
    { int svColor = SetForeColor(MAGENTA);
#endif
    putStr("\n>>>>>>> Observations <<<<<<");
    newLine(0);

    while(i != NIL){
          newLine(0);
     s = textToStr(observe(i).tag);
     if (*t==0 || strcmp(s,t)==0){
                putStr(s);
         newLine(2);
         if (printObsList(observe(i).head,2,FALSE))
             newLine(0);
     }
     i = nextObserve();
    }

    newLine(0);
    printingObservations = FALSE;
#if HUGS_FOR_WINDOWS
    SetForeColor(svColor); }
#endif
}

Bool printObsList(header, indent, funPrint)
Cell header;
Int  indent;
Bool funPrint; 
{
    Cell j, resultExp = 0;
    Int seq=0, appN, argN;
    Bool firstApp = 1;

    for (j=firstObs(header); j!=header; j=nextObs(j)){
        seq  = intOf(seqObs(j));
        appN = seqNum(seq);
        argN = argNum(seq);

        if ( seq < 0 ) {         /* non-functional value        */
            printArg(stdout, exprObs(j));
            if (!funPrint) newLine(indent);
        } else if (seq!=0) {     /* a function observation  */
            funPrint =1;
            if (argN == 0){      /* the result expr     */
                if (firstApp){   /* print previous result   */
                    firstApp = 0;
                    indent   = outColumn;
                    putStr("{ ");
                } else { 
                    putStr(" -> ");
                    printExp(stdout,resultExp);
                    newLine(indent); 
                    putStr(", ");
                }
                resultExp = exprObs(j);
                putStr ("\\ ");
            } else {          /* an arg expr         */
                if (whatIs(exprObs(j)) == OBSERVEHEAD)
                    printObsList(exprObs(j), indent, TRUE);
                else
                    printArg(stdout,exprObs(j));
                putStr(" ");
            }
        }
    }
    if (seq >= 0){  /* print result of last fun. obs. in list  */
        putStr(" -> ");
        printExp(stdout,resultExp);
        newLine(indent);
        putStr("}");
    }
    return(funPrint);
}
#endif

/* --------------------------------------------------------------------------
 * Print qualified module name (if wanted):
 * ------------------------------------------------------------------------*/
Bool useQualifiedNames = FALSE;

static Void local putModule(m)            /* print module qualifier        */
Module m; {
  if (useQualifiedNames && !isPrelude(m)) { /* leave out "Prelude." qualifiers, too noisy. */
       putStr(textToStr(module(m).text));
       putChr('.');
    }
}
/* --------------------------------------------------------------------------
 * Main drivers:
 * ------------------------------------------------------------------------*/

Void printExp(fp,e)			/* print expr on specified stream  */
FILE *fp;
Cell e; {
    outputStream = fp;
    putDepth     = 0;
    put(NEVER,e);
}

#if OBSERVATIONS
static Void printArg(fp,e)              /* print expr on specified stream  */
FILE *fp;
Cell e; {
    outputStream = fp;
    putDepth     = 0;
    put(ALWAYS,e);
}
#endif

Void printType(fp,t)			/* print type on specified stream  */
FILE *fp;
Cell t; {
    outputStream = fp;
    putSigType(t);
}

Void printContext(fp,qs)		/* print context on spec. stream   */
FILE *fp;
List qs; {
    outputStream = fp;
    putContext(qs,NIL,0);
}

Void printPred(fp,pi)			/* print predicate pi on stream    */
FILE *fp;
Cell pi; {
    outputStream = fp;
    putPred(pi,0);
}

Void printKind(fp,k)			/* print kind k on stream          */
FILE *fp;
Kind k; {
    outputStream = fp;
    putKind(k);
}

Void printKinds(fp,ks)			/* print list of kinds on stream   */
FILE  *fp;
Kinds ks; {
    outputStream = fp;
    putKinds(ks);
}

Void printFD(fp,fd)			/* print functional dependency	   */
FILE* fp;
Pair  fd; {
    List us;
    outputStream = fp;
    for (us=fst(fd); nonNull(us); us=tl(us)) {
        putTyVar(offsetOf(hd(us)));
	if (nonNull(tl(us))) {
	    putChr(' ');
	}
    }
    putStr(" -> ");
    for (us=snd(fd); nonNull(us); us=tl(us)) {
	putTyVar(offsetOf(hd(us)));
	if (nonNull(tl(us))) {
	    putChr(' ');
	}
    }
}


/*-------------------------------------------------------------------------*/
