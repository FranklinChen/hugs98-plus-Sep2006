/* --------------------------------------------------------------------------
 * Graph reduction engine, code generation and execution
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: machine.c,v $
 * $Revision: 1.26 $
 * $Date: 2006/02/14 16:12:19 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "char.h"
#include "opts.h"	/* needed for DEBUG_CODE */
#include <setjmp.h>

#if OBSERVATIONS
Bool   rootOpt       = TRUE;		/* TRUE => enable root optimisation*/
#endif
#if DEBUG_CODE
Bool   debugCode     = FALSE;		/* TRUE => print G-code to screen  */
#endif
#if DEBUG_SHOWSC
Bool   debugSC	     = FALSE;		/* TRUE => print SC-code to screen */
#endif

Int    evalDepth;			/* depth of nested eval()'s        */

/* --------------------------------------------------------------------------
 * Data structures for machine memory (program storage):
 * ------------------------------------------------------------------------*/

/* This list defines the sequence of all instructions that can be used in
 * the abstract machine code for Hugs.  The Ins() macro is used to
 * ensure that the correct mapping of instructions to labels is used when
 * compiling the GCC_THREADED version.
 */

#define INSTRLIST       Ins(iLOAD),  Ins(iCELL),   Ins(iCHAR),    \
			Ins(iINT),   Ins(iDOUBLE), Ins(iSTRING),  \
			Ins(iMKAP),  Ins(iUPDATE), Ins(iUPDAP),   \
			Ins(iEVAL),  Ins(iRETURN), Ins(iTEST),    \
			Ins(iGOTO),  Ins(iSETSTK), Ins(iROOT),    \
			Ins(iSLIDE), Ins(iSTAP),   Ins(iTABLE),   \
			Ins(iLEVAL), Ins(iRUPDAP), Ins(iRUPDATE), \
			Ins(iFAIL),  Ins(iALLOC)
  
#define Ins(x) x
typedef enum { INSTRLIST } Instr;
#undef  Ins

typedef Int Label;

typedef union {
    Int   mint;
#if !BREAK_FLOATS
    Float mfloat;
#endif
    Cell  cell;
    Text  text;
    Addr  addr;
    Instr instr;
    Label lab;
} MemCell;

typedef MemCell far *Memory;
static  Memory      memory;
#if !WANT_FIXED_SIZE_TABLES
/* (Dynamically) growable memory. */
DynTable* dynMemory = NULL;
#endif

#if !WANT_FIXED_SIZE_TABLES
void
growMemory(void)
{
    growDynTable(dynMemory);
    memory = (Memory)(dynMemory->data);
    if (memory==0)
	fatal("Cannot allocate program memory");
}
#endif

#define intAt(m)    memory[m].mint
#if !BREAK_FLOATS
#define floatAt(m)  memory[m].mfloat
#endif
#define cellAt(m)   memory[m].cell
#define textAt(m)   memory[m].text
#define setAddrAt(m,a) memory[m].addr = ((a)-(m))
#define addrAt(m)   (memory[m].addr+(m))
#define instrAt(m)  memory[m].instr
#define labAt(m)    memory[m].lab

#if BYTECODE_PRIMS

Int IntAt(m)
Addr m; {
    return intAt(m);
}

Float FloatAt(m)
Addr m; {
#if BREAK_FLOATS
    return floatFromParts(cellAt(m),cellAt(m+1));
#else
    return floatAt(m);
#endif
}

Double DoubleAt(m)
Addr m; {
    return doubleFromParts(cellAt(m),cellAt(m+1));
}

Cell CellAt(m)
Addr m; {
    return cellAt(m);
}

Text TextAt(m)
Addr m; {
    return textAt(m);
}

Addr AddrAt(m)
Addr m; {
    return addrAt(m);
}

Int InstrAt(m)
Addr m; {
    return instrAt(m);
}

#endif /* BYTECODE_PRIMS */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void  local instrNone    Args((Instr));
static Void  local instrInt     Args((Instr,Int));
static Void  local instrDouble  Args((Instr,DoublePro));
static Void  local instrCell    Args((Instr,Cell));
static Void  local instrText    Args((Instr,Text));
static Void  local instrLab     Args((Instr,Label));
static Void  local instrCellLab Args((Instr,Cell,Label));

static Void  local asSTART      Args((Void));
static Label local newLabel     Args((Label));
static Void  local asEND        Args((Void));
static Void  local asEVAL       Args((Void));
static Void  local asTEST       Args((Cell,Label));
static Void  local asSLIDE      Args((Int));
static Void  local asMKAP       Args((Int));
static Void  local asUPDATE     Args((Int));
static Void  local asRUPDATE    Args((Void));
static Void  local asGOTO       Args((Label));

#if DEBUG_CODE
static Void  local dissassemble Args((Addr,Addr));
static Addr  local dissInstr    Args((Addr));
static Void  local printCell    Args((Cell));
static Addr  local dissNone     Args((Addr,String));
static Addr  local dissInt      Args((Addr,String));
static Addr  local dissDouble   Args((Addr,String));
static Addr  local dissCell     Args((Addr,String));
static Addr  local dissText     Args((Addr,String));
static Addr  local dissAddr     Args((Addr,String));
static Addr  local dissCellAddr Args((Addr,String));
#endif

static Void  local build        Args((Cell,Int));
static Void  local buildGuards  Args((List,Int));
static Int   local buildLoc     Args((List,Int));
#if BIGNUMS
static Void  local buildBignum  Args((Bignum));
#endif
#if TREX
static Void  local buildName	Args((Cell));
#endif

static Void  local make         Args((Cell,Int,Label,Label));
static Void  local makeCond     Args((Cell,Cell,Cell,Int,Label,Label));
static Void  local makeNumcase  Args((Triple,Int,Label,Label));
#if TREX
static Void  local makeExtcase  Args((Triple,Int,Label,Label));
#endif
static Void  local testGuard    Args((Pair,Int,Label,Label,Label));
static Void  local testCase     Args((Pair,Int,Label,Label,Label));

static Void  local analyseAp    Args((Cell));
#if TREX
static Int   local fastBuildRec	Args((Cell,Int));
#endif
static Void  local buildAp      Args((Cell,Int,Label,Bool));

static Void  local evalString   Args((Cell));

/* --------------------------------------------------------------------------
 * Assembler: (Low level, instruction code storage)
 * ------------------------------------------------------------------------*/

static Addr  startInstr;                /* first instruction after START   */
static Addr  lastInstr;                 /* last instr written (for peephole*/
					/* optimisations etc.)             */
static Bool  newBasicBlock;		/* lastInstr ends a basic block    */
					/* (so no peeping at lastInstr)    */
static Addr  noMatch;                   /* address of a single FAIL instr  */
static Int   srsp;                      /* simulated runtime stack pointer */
static Int   offsPosn[NUM_OFFSETS];     /* mapping from logical to physical*/
					/* offset positions                */

static Void local instrNone(opc)        /* Opcode with no operands         */
Instr opc; {
    lastInstr          = getMem(1);
    instrAt(lastInstr) = opc;
    newBasicBlock      = FALSE;
}

static Void local instrInt(opc,n)       /* Opcode with integer operand     */
Instr opc;
Int   n; {
    lastInstr          = getMem(2);
    instrAt(lastInstr) = opc;
    intAt(lastInstr+1) = n;
    newBasicBlock      = FALSE;
}

static Void local instrDouble(opc,fl)    /* Opcode with Double operand     */
Instr    opc;
DoublePro fl; {
    lastInstr            = getMem(3);
    instrAt(lastInstr)   = opc;
    cellAt(lastInstr+1)  = part1Double(fl);
    cellAt(lastInstr+2)  = part2Double(fl);
    newBasicBlock        = FALSE;
}

static Void local instrCell(opc,c)      /* Opcode with Cell operand        */
Instr opc;
Cell  c; {
    lastInstr           = getMem(2);
    instrAt(lastInstr)  = opc;
    cellAt(lastInstr+1) = c;
    newBasicBlock       = FALSE;
}

static Void local instrText(opc,t)      /* Opcode with Text operand        */
Instr opc;
Text  t; {
    lastInstr           = getMem(2);
    instrAt(lastInstr)  = opc;
    textAt(lastInstr+1) = t;
    newBasicBlock       = FALSE;
}

static Void local instrLab(opc,l)       /* Opcode with label operand       */
Instr opc;
Label l; {
    lastInstr          = getMem(2);
    instrAt(lastInstr) = opc;
    labAt(lastInstr+1) = l;
    if (l<0)
	internal("bad Label");
    newBasicBlock      = FALSE;
}

static Void local instrCellLab(opc,c,l) /* Opcode with cell, label operands*/
Instr opc;
Cell  c;
Label l; {
    lastInstr           = getMem(3);
    instrAt(lastInstr)  = opc;
    cellAt(lastInstr+1) = c;
    labAt(lastInstr+2)  = l;
    if (l<0)
	internal("bad Label");
    newBasicBlock       = FALSE;
}

/* --------------------------------------------------------------------------
 * Main low level assembler control: (includes label assignment and fixup)
 *
 * Labels are used as a simple form of continuation during the code gen:
 *  RUNON    => produce code which does not make jump at end of construction
 *  UPDRET   => produce code which performs RUPDATE at end
 *  VALRET   => produce code which performs RETURN at end
 *  other(d) => produce code which branches to label d at end
 * ------------------------------------------------------------------------*/

static  Label         nextLab;         /* next label number to allocate    */
#define SHOULDNTFAIL  (-1)
#define RUNON         (-2)
#define UPDRET        (-3)
#define VALRET        (-4)
static  Addr          fixups[NUM_FIXUPS]; /* fixup table maps Label -> Addr*/
#define atLabel(n)    (newBasicBlock = TRUE, fixups[n] = getMem(0))
#define endLabel(d,l) if (d==RUNON) atLabel(l)
#define fix(a)        setAddrAt(a,fixups[labAt(a)])

static  Addr          lengthAddr;

static Void local asSTART() {          /* initialise assembler             */
    fixups[0]   = noMatch;
    nextLab     = 1;
    lengthAddr  = getMem(1);
    startInstr  = getMem(0);
    lastInstr   = startInstr-1;
    newBasicBlock = TRUE;
    srsp        = 0;
    offsPosn[0] = 0;
}

static Label local newLabel(d)         /* allocate new label               */
Label d; {
    if (d==RUNON) {
	if (nextLab>=NUM_FIXUPS) {
	    ERRMSG(0) "Compiled code too complex"
	    EEND;
	}
	return nextLab++;
    }
    return d;
}

static Void local asEND() {            /* Fix addresses in assembled code  */
    Addr pc = startInstr;

    intAt(lengthAddr) = lastInstr - startInstr + 1;
    while (pc<=lastInstr)
	switch (instrAt(pc)) {
	    case iEVAL   :             /* opcodes taking no arguments      */
	    case iFAIL   :
	    case iSTAP   :
	    case iRUPDATE:
	    case iRUPDAP :
	    case iRETURN : pc++;
			   break;

	    case iGOTO   : fix(pc+1);  /* opcodes taking one argument      */
	    case iSETSTK :
	    case iALLOC  :
	    case iSLIDE  :
	    case iROOT   :
	    case iLOAD   :
	    case iLEVAL  :
	    case iCELL   : 
	    case iCHAR   :
	    case iINT    :
	    case iSTRING :
	    case iMKAP   :
	    case iUPDATE :
	    case iUPDAP  : pc+=2;
			   break;

	    case iDOUBLE : pc+=3;
			   break;

	    case iTEST   : fix(pc+2);
			   pc+=3;
			   break;

	    default      : internal("fixAddrs");
	}
}

/* --------------------------------------------------------------------------
 * Assembler Opcodes: (includes simple peephole optimisations)
 * ------------------------------------------------------------------------*/

#define asINTEGER(n) instrInt(iINT,n);          srsp++
#define asDOUBLE(fl) instrDouble(iDOUBLE,fl);   srsp++
#define asSTRING(t)  instrText(iSTRING,t);      srsp++
#define asCHAR(n)    instrInt(iCHAR,n);         srsp++
#define asLOAD(n)    instrInt(iLOAD,n);         srsp++
#define asALLOC(n)   instrInt(iALLOC,n);        srsp+=n
#define asROOT(n)    instrInt(iROOT,n);         srsp++
#define asSETSTK(n)  instrInt(iSETSTK,n);       srsp=n
#define asSTAP()     instrNone(iSTAP);          srsp--
#define asRETURN()   instrNone(iRETURN)
#define asCELL(c)    instrCell(iCELL,c);        srsp++
#define asFAIL()     instrNone(iFAIL)

/* Peephole optimisations are unsafe if we're at the start of a new
   'basic block' (newBasicBlock is TRUE) -- e.g.,

	f a b = if (if a then True else b) then foo else bar

   After having emitted the code for the conditional, we want to evaluate
   it, BUT we better _not_ look at the last instruction of the code
   comprising the conditional expression and decide whether or not to
   peephole the EVAL into an LEVAL.
*/

static Void local asEVAL() {            /* load and eval stack element     */
    if (!newBasicBlock && instrAt(lastInstr)==iLOAD)
					/* Peephole optimisation:          */
	instrAt(lastInstr) = iLEVAL;	/* LOAD n; EVAL ===> LEVAL n       */
    else
	instrNone(iEVAL);
    srsp--;
}

static Void local asTEST(c,l)           /* test whnf and branch on mismatch*/
Cell  c;
Label l; {
    switch (whatIs(c)) {
	case TUPLE   : return;          /* typing guarantees that tags will*/
					/* match without further tests     */
	case NAME    : if (isCfun(c) && cfunOf(c)==0)
			   return;
    }
    instrCellLab(iTEST,c,l);
}

static Void local asSLIDE(n)            /* Slide results down stack        */
Int n; {
    if (!newBasicBlock && instrAt(lastInstr)==iSLIDE)
					/* Peephole optimisation:          */
	intAt(lastInstr+1)+=n;          /* SLIDE n;SLIDE m ===> SLIDE (n+m)*/
    else
	instrInt(iSLIDE,n);
    srsp -= n;
}

static Void local asMKAP(n)             /* Make application nodes ...      */
Int n; {
    if (!newBasicBlock && instrAt(lastInstr)==iMKAP)
					/* Peephole optimisation:          */
	intAt(lastInstr+1)+=n;          /* MKAP n; MKAP m  ===> MKAP (n+m) */
    else
	instrInt(iMKAP,n);
    srsp -= n;
}

static Void local asUPDATE(n)           /* Update node ...                 */
Int n; {
    if (!newBasicBlock && instrAt(lastInstr)==iMKAP) {
	Int m = intAt(lastInstr+1);	/* Peephole optimisations:         */
	nextInstr(lastInstr);
	if (m==1)                       /* MKAP 1; UPDATE p ===> UPDAP p   */
	    instrInt(iUPDAP,n);
	else {                          /* MKAP m; UPDATE p                */
	    instrInt(iMKAP,m-1);        /*       ===> MKAP (m-1); UPDAP p  */
	    instrInt(iUPDAP,n);
	}
    }
    else
	instrInt(iUPDATE,n);
    srsp--;
}

static Void local asRUPDATE() {         /* Update node and return ...      */
    if (!newBasicBlock && instrAt(lastInstr)==iMKAP) {
	Int m = intAt(lastInstr+1);	/* Peephole optimisations:         */
	nextInstr(lastInstr);
	if (m==1)                       /* MKAP 1; RUPDATE ===> RUPDAP     */
	    instrNone(iRUPDAP);
	else {                          /* MKAP m; RUPDATE                 */
	    instrInt(iMKAP,m-1);        /*       ===> MKAP (m-1); RUPDAP   */
	    instrNone(iRUPDAP);
	}
    }
    else
	instrNone(iRUPDATE);
}

static Void local asGOTO(l)             /* End evaluation of expr in manner*/
Label l; {                              /* indicated by label l            */
    switch (l) {                                        /* inaccurate srsp */
	case UPDRET : asRUPDATE();
		      break;
	case VALRET : asRETURN();
	case RUNON  : break;
	default     : instrLab(iGOTO,l);
		      break;
    }
}

/* --------------------------------------------------------------------------
 * Constructor function tables:
 *
 * Tables of constructor functions for enumerated types are needed to
 * produce derived instances.
 * ------------------------------------------------------------------------*/

Void addCfunTable(tc)                   /* Add a constructor fun table to  */
Tycon tc; {                             /* constructors for tycon tc       */
    if (isTycon(tc) && tycon(tc).what==DATATYPE) {
	List cs = tycon(tc).defn;
	if (hasCfun(cs) && hasCfun(tl(cs)) && name(hd(cs)).code<=0) {
	    Int  l     = length(cs);
	    Addr a     = getMem(2+l);
	    instrAt(a) = iTABLE;
	    intAt(a+1) = l;
	    for (l=0; nonNull(cs); l++, cs=tl(cs)) {
		cellAt(a+l+2)     = hd(cs);
		name(hd(cs)).code = a;
	    }
	}
    }
}

Name succCfun(n)                        /* get next constructor in sequence*/
Name n; {                               /* or NIL, if none                 */
    if (cfunOf(n)==0)
	return NIL;
    else {
	Int  d = cfunOf(n)+1;
	Addr a = name(n).code;
	return (d>intAt(a+1)) ? NIL : cellAt(a+d+1);
    }
}

Name nextCfun(n1,n2)                    /* get next constructor in series  */
Name n1, n2; {                          /* or NIL, if none                 */
    if (cfunOf(n1)==0)                  /* For product constructors, the   */
	return n1;                      /* only possibility is n1 == n2    */
    else {
	Int  d = 2*cfunOf(n2) - cfunOf(n1);
	Addr a = name(n1).code;
	return (d<=0 || d>intAt(a+1)) ? NIL : cellAt(a+d+1);
    }
}

Name cfunByNum(n,i)                     /* get ith constructor (0<=i<m)    */
Name n;                                 /* for enumerated datatype with a  */
Int  i; {                               /* representative cfun n           */
     if (cfunOf(n)==0)
	return i==0 ? n : NIL;
     else {
	 Addr a = name(n).code;
	 return (i>=0 && i<intAt(a+1)) ? cellAt(a+i+2) : NIL;
     }
}

/* --------------------------------------------------------------------------
 * Dissassembler:
 * ------------------------------------------------------------------------*/

#if DEBUG_CODE
#define printAddr(a) Printf("0x%04X",a)/* printable representation of Addr */

static Void local dissassemble(pc,end)    /* print dissassembly of code    */
Addr pc;
Addr end; {
    while (pc<=end) {
	printAddr(pc);
	Printf("\t");
	pc = dissInstr(pc);
    }
}

static Addr local dissInstr(pc)       /* print dissassembly of instruction */
Addr pc; {
    switch (instrAt(pc)) {
	case iLOAD   : pc = dissInt(pc,"LOAD");      break;
	case iLEVAL  : pc = dissInt(pc,"LEVAL");     break;
	case iCELL   : pc = dissCell(pc,"CELL");     break;
	case iCHAR   : pc = dissInt(pc,"CHAR");      break;
	case iINT    : pc = dissInt(pc,"INT");       break;
	case iDOUBLE : pc = dissDouble(pc,"DOUBLE"); break;
	case iSTRING : pc = dissText(pc,"STRING");   break;
	case iMKAP   : pc = dissInt(pc,"MKAP");      break;
	case iUPDATE : pc = dissInt(pc,"UPDATE");    break;
	case iRUPDATE: pc = dissNone(pc,"RUPDATE");  break;
	case iUPDAP  : pc = dissInt(pc,"UPDAP");     break;
	case iRUPDAP : pc = dissNone(pc,"RUPDAP");   break;
	case iEVAL   : pc = dissNone(pc,"EVAL");     break;
	case iSTAP   : pc = dissNone(pc,"STAP");     break;
	case iRETURN : pc = dissNone(pc,"RETURN");   break;
	case iTEST   : pc = dissCellAddr(pc,"TEST"); break;
	case iGOTO   : pc = dissAddr(pc,"GOTO");     break;
	case iSETSTK : pc = dissInt(pc,"SETSTK");    break;
	case iALLOC  : pc = dissInt(pc,"ALLOC");     break;
	case iSLIDE  : pc = dissInt(pc,"SLIDE");     break;
	case iROOT   : pc = dissInt(pc,"ROOT");      break;
	case iFAIL   : pc = dissNone(pc,"FAIL");     break;
	case iTABLE  : pc = dissNone(pc,"TABLE");
		       pc+= intAt(pc)+1;
		       break;
	default  : internal("unknown instruction");
    }
    return pc;
}

static Void local printCell(c)         /* printable representation of Cell */
Cell c; {
    if (isName(c))
	Printf("%s",textToStr(name(c).text));
    else
	Printf("$%d",c);
}

static Addr local dissNone(pc,s)       /* dissassemble instr no args       */
Addr   pc;
String s; {
    Printf("%s\n",s);
    return pc+1;
}

static Addr local dissInt(pc,s)        /* dissassemble instr with Int arg  */
Addr   pc;
String s; {
    Printf("%s\t%d\n",s,intAt(pc+1));
    return pc+2;
}

static Addr local dissDouble(pc,s)      /* dissassemble instr with Double arg*/
Addr   pc;
String s; {
    Printf("%s\t%s\n",s,
	doubleToString(doubleFromParts(cellAt(pc+1),cellAt(pc+2))));
    return pc+3;
}

static Addr local dissCell(pc,s)       /* dissassemble instr with Cell arg */
Addr   pc;
String s; {
    Printf("%s\t",s);
    printCell(cellAt(pc+1));
    Printf("\n");
    return pc+2;
}

static Addr local dissText(pc,s)       /* dissassemble instr with Text arg */
Addr   pc;
String s; {
    Printf("%s\t%s\n",s,textToStr(textAt(pc+1)));
    return pc+2;
}

static Addr local dissAddr(pc,s)       /* dissassemble instr with Addr arg */
Addr   pc;
String s; {
    Printf("%s\t",s);
    printAddr(addrAt(pc+1));
    Printf("\n");
    return pc+2;
}

static Addr local dissCellAddr(pc,s)   /* dissassemble instr with Cell/Addr*/
Addr   pc;
String s; {
    Printf("%s\t",s);
    printCell(cellAt(pc+1));
    Printf("\t");
    printAddr(addrAt(pc+2));
    Printf("\n");
    return pc+3;
}
#endif

/* --------------------------------------------------------------------------
 * Compile expression to code which will build expression without any
 * evaluation.
 * ------------------------------------------------------------------------*/

static Void local build(e,co)           /* Generate code which will build  */
Cell e;                                 /* instance of given expression but*/
Int  co; {                              /* perform no evaluation           */
    Int n;

    STACK_CHECK
    switch (whatIs(e)) {

	case LETREC    : n = buildLoc(fst(snd(e)),co);
			 build(snd(snd(e)),co+n);
			 asSLIDE(n);
			 break;

	case FATBAR    : build(snd(snd(e)),co);
			 build(fst(snd(e)),co);
			 asCELL(nameFatbar);
			 asMKAP(2);
			 break;

	case COND      : build(thd3(snd(e)),co);
			 build(snd3(snd(e)),co);
			 build(fst3(snd(e)),co);
			 asCELL(nameIf);
			 asMKAP(3);
			 break;

	case GUARDED   : buildGuards(snd(e),co);
			 break;

	case AP        : buildAp(e,co,SHOULDNTFAIL,FALSE);
			 break;

#if TREX
	case NAME      : buildName(e);
			 break;

	case TUPLE     : asCELL(e);
			 break;
#else
	case TUPLE     :
	case NAME      : asCELL(e);
			 break;
#endif

#if BIGNUMS
	case ZERONUM   :
	case POSNUM    :
	case NEGNUM    : buildBignum(e);
			 break;
#endif

	case INTCELL   : asINTEGER(intOf(e));
			 break;

	case DOUBLECELL : asDOUBLE(doubleOf(e));
			 break;

	case STRCELL   : asSTRING(textOf(e));
			 break;

	case CHARCELL  : asCHAR(charOf(e));
			 break;

	case OFFSET    : asLOAD(offsPosn[offsetOf(e)]);
			 break;

	default        : internal("build");
    }
}

static Void local buildGuards(gs,co)    /* Generate code to compile list   */
List gs;                                /* of guards to a conditional expr */
Int  co; {                              /* without evaluation              */
    if (isNull(gs)) {
	asCELL(nameFail);
    }
    else {
	buildGuards(tl(gs),co);
	build(snd(hd(gs)),co);
	build(fst(hd(gs)),co);
	asCELL(nameIf);
	asMKAP(3);
    }
}

static Int local buildLoc(vs,co)        /* Generate code to build local var*/
List vs;                                /* bindings on stack,  with no eval*/
Int  co; {
    Int n = length(vs);
    Int i;

    for (i=1; i<=n; i++)
	offsPosn[co+i] = srsp+i;
    asALLOC(n);
    for (i=1; i<=n; i++) {
	build(hd(vs),co+n);
	asUPDATE(offsPosn[co+i]);
	vs = tl(vs);
    }
    return n;
}

#if BIGNUMS
static Void local buildBignum(b)        /* Generate code to build bignum   */
Bignum b; {
    if (b==ZERONUM) {
	asCELL(ZERONUM);
    }
    else {
	List rs = snd(b) = rev(snd(b));
	asCELL(NIL);
	for (; nonNull(rs); rs=tl(rs)) {
	    asCELL(hd(rs));
	    asMKAP(1);
	}
	snd(b) = rev(snd(b));           /* put digits back in order        */
	asCELL(fst(b));
	asMKAP(1);
    }
}
#endif

#if TREX
static Void local buildName(e)		/* Build name, taking account of   */
Cell e; {				/* special cases, as necessary.	   */
    if (e==nameNoRec) {
	asCELL(nameNil);
	asCELL(RECORD);
	asMKAP(1);
    }
    else
	asCELL(e);
}
#endif

/* --------------------------------------------------------------------------
 * Compile expression to code which will build expression evaluating guards
 * and testing cases to avoid building complete graph.
 * ------------------------------------------------------------------------*/

#define makeTests(ct,tests,co,f,d)     {   Label l1 = newLabel(d);          \
					   List  xs = tests;                \
					   while (nonNull(tl(xs))) {        \
					       Label l2   = newLabel(RUNON);\
					       Int savesp = srsp;           \
					       ct(hd(xs),co,f,l2,l1);       \
					       atLabel(l2);                 \
					       srsp = savesp;               \
					       xs   = tl(xs);               \
					   }                                \
					   ct(hd(xs),co,f,f,d);             \
					   endLabel(d,l1);                  \
				       }

static Void local make(e,co,f,d)       /* Construct code to build e, given */
Cell  e;                               /* current offset co, and branch    */
Int   co;                              /* to f on failure, d on completion */
Label f;
Label d; {
    STACK_CHECK
    switch (whatIs(e)) {

	case LETREC    : {   Int n = buildLoc(fst(snd(e)),co);
			     if (d==UPDRET || d==VALRET)
				 make(snd(snd(e)),co+n,f,d);
			     else {
				 make(snd(snd(e)),co+n,f,RUNON);
				 asSLIDE(n);
				 asGOTO(d);
			     }
			 }
			 break;

	case FATBAR    : {   Label l1     = newLabel(RUNON);
			     Label l2     = newLabel(d);
			     Int   savesp = srsp;

			     make(fst(snd(e)),co,l1,l2);

			     atLabel(l1);
			     srsp = savesp;
			     asSETSTK(srsp);
			     make(snd(snd(e)),co,f,l2);

			     endLabel(d,l2);
			 }
			 break;

	case COND      : makeCond(fst3(snd(e)),
				  snd3(snd(e)),
				  thd3(snd(e)),co,f,d);
			 break;

#if TREX
	case EXTCASE   : makeExtcase(snd(e),co,f,d);
			 break;
#endif

	case NUMCASE   : makeNumcase(snd(e),co,f,d);
			 break;

	case CASE      : make(fst(snd(e)),co,SHOULDNTFAIL,RUNON);
			 asEVAL();
			 makeTests(testCase,snd(snd(e)),co,f,d);
			 break;

	case GUARDED   : makeTests(testGuard,snd(e),co,f,d);
			 break;

	case AP        : {   Cell h = getHead(e);
			     if (h==nameAnd && argCount==2) {
				 /* x && y ==> if x then y else False      */
				 makeCond(arg(fun(e)),arg(e),nameFalse,co,f,d);
				 break;
			     }
			     else if (h==nameOr && argCount==2) {
				 /* x || y ==> if x then True else y       */
				 makeCond(arg(fun(e)),nameTrue,arg(e),co,f,d);
				 break;
			     }
			 }
			 buildAp(e,co,f,TRUE);
			 asGOTO(d);
			 break;

#if TREX
	case NAME      : buildName(e);
			 asGOTO(d);
			 break;

	case TUPLE     : asCELL(e);
			 asGOTO(d);
			 break;
#else
	case TUPLE     :
	case NAME      : asCELL(e);
			 asGOTO(d);
			 break;
#endif

#if BIGNUMS
	case ZERONUM   :
	case POSNUM    :
	case NEGNUM    : buildBignum(e);
			 asGOTO(d);
			 break;
#endif

	case INTCELL   : asINTEGER(intOf(e));
			 asGOTO(d);
			 break;

	case DOUBLECELL : asDOUBLE(doubleOf(e));
			 asGOTO(d);
			 break;

	case STRCELL   : asSTRING(textOf(e));
			 asGOTO(d);
			 break;

	case CHARCELL  : asCHAR(charOf(e));
			 asGOTO(d);
			 break;

	case OFFSET    : asLOAD(offsPosn[offsetOf(e)]);
			 asGOTO(d);
			 break;

	default        : internal("make");
    }
}

static Void local makeCond(i,t,e,co,f,d)/* Build code for conditional      */
Cell  i,t,e;
Int   co;
Label f;
Label d; {
    Label l1 = newLabel(RUNON);
    Label l2 = newLabel(d);
    Int   savesp;

    make(i,co,f,RUNON);
    asEVAL();

    savesp = srsp;
    asTEST(nameTrue,l1);
    make(t,co,f,l2);

    srsp = savesp;
    atLabel(l1);
    make(e,co,f,(d==RUNON?d:l2));

    endLabel(d,l2);
}

static Void local makeNumcase(nc,co,f,d)/* Build code for numcase          */
Triple nc;
Int    co;
Label  f, d; {
    Cell discr = snd3(nc);
    Cell h     = getHead(discr);
    make(fst3(nc),co,SHOULDNTFAIL,RUNON);
    switch (whatIs(h)) {
	case NAME   : if (h==nameFromInt) {
			  asINTEGER(intOf(arg(discr)));
			  make(arg(fun(discr)),co,SHOULDNTFAIL,RUNON);
			  asCELL(namePmInt);
		      }
#if BIGNUMS
		      else if (h==nameFromInteger) {
			  buildBignum(arg(discr));
			  make(arg(fun(discr)),co,SHOULDNTFAIL,RUNON);
			  asCELL(namePmInteger);
		      }
#else
		      /* ToDo: should this be the same as fromInt? */
#endif
		      else if (h==nameFromDouble) {
			  asDOUBLE(doubleOf(arg(discr)));
			  make(arg(fun(discr)),co,SHOULDNTFAIL,RUNON);
			  asCELL(namePmFlt);
		      }
		      asMKAP(3);
		      asEVAL();
		      asTEST(nameTrue,f);
		      make(thd3(nc),co,f,d);
		      break;
#if NPLUSK
	case ADDPAT : asINTEGER(snd(h));
		      make(arg(discr),co,SHOULDNTFAIL,RUNON);
		      asCELL(namePmNpk);
		      asMKAP(3);
		      asEVAL();
		      asTEST(nameJust,f);
		      offsPosn[co+1] = ++srsp;
		      make(thd3(nc),co+1,f,d);
		      --srsp;
		      break;
#endif
    }
}

#if TREX
static Void local makeExtcase(ec,co,f,d)/* Build code for extcase	   */
Triple ec;
Int    co;
Label  f, d; {
    make(ap(ap(nameRecBrk,arg(snd3(ec))),fst3(ec)),co,SHOULDNTFAIL,RUNON);
    asEVAL();
    offsPosn[co+1] = ++srsp;
    offsPosn[co+2] = ++srsp;
    make(thd3(ec),co+2,f,d);
}
#endif

static Void local testGuard(g,co,f,cf,d)/* Produce code for guard          */
Pair  g;
Int   co;
Label f;
Label cf;
Label d; {
    if (fst(g)!=nameTrue) {
	make(fst(g),co,SHOULDNTFAIL,RUNON);
	asEVAL();
	asTEST(nameTrue,cf);
    }
    make(snd(g),co,f,d);
}

static Void local testCase(c,co,f,cf,d) /* Produce code for guard          */
Pair  c;
Int   co;                               /* labels determine where to go if:*/
Label f;                                /* match succeeds, but rest fails  */
Label cf;                               /* this match fails                */
Label d; {
    Int n = discrArity(fst(c));
    Int i;
    asTEST(fst(c),cf);
    for (i=1; i<=n; i++)
	offsPosn[co+i] = ++srsp;
    make(snd(c),co+n,f,d);
}

/* --------------------------------------------------------------------------
 * We frequently encounter functions which call themselves recursively with
 * a number of initial arguments preserved:
 * e.g.  (map f) []     = []
 *       (map f) (x:xs) = f x : (map f) xs
 * Lambda lifting, in particular, is likely to introduce such functions.
 * Rather than reconstructing a new instance of the recursive function and
 * its arguments, we can extract the relevant portion of the root of the
 * current redex.
 *
 * The following functions implement this optimisation.
 * ------------------------------------------------------------------------*/

static Int  nonRoots;                  /* #args which can't get from root  */
static Int  rootPortion;               /* portion of root used ...         */
static Name definingName;              /* name of func being defined,if any*/
static Int  definingArity;             /* arity of definingName            */

static Void local analyseAp(e)         /* Determine if any portion of an   */
Cell e; {                              /* application can be built using a */
    if (isAp(e)) {                     /* portion of the root              */
	analyseAp(fun(e));
	if (nonRoots==0 && rootPortion>1
			&& isOffset(arg(e))
			&& offsetOf(arg(e))==rootPortion-1)
	    rootPortion--;
	else
	    nonRoots++;
    }
    else if (e==definingName)
	rootPortion = definingArity+1;
    else
	rootPortion = 0;
}

#if TREX
static Int local fastBuildRec(e,co)	/* Try to build record, returning  */
Cell e;					/* no of elements, or (-1) if not a*/
Int  co; {				/* simple record		   */
    if (e==nameNoRec) {
	asCELL(NIL);
	return 0;
    }
    else if (isExt(getHead(e)) && argCount==3) {
	Int c = fastBuildRec(extRow(e),co);
	if (c>=0) {
	    build(extField(e),co);
	    asMKAP(1);
	    return (c+1);
	}
    }
    return (-1);
}
#endif

static Void local buildAp(e,co,f,str)  /* Build application, making use of */
Cell  e;                               /* root optimisation if poss.       */
Int   co;
Label f;
Bool  str; {
    Int nr, rp, i;

#if TREX
    if ((i=fastBuildRec(e,co))>=0) {	/* Fast build for records whose	   */
	asCELL(RECORD);			/* structure is known at compile   */
	asMKAP(1);			/* time ... but the savings are	   */
	return;				/* pretty small.		   */
    }
#endif
#if OBSERVATIONS
    if (!rootOpt){
        rp = 0;
        nr = 0;
        while (isAp(e)){
            build(arg(e),co);
	    e = fun(e);
	    nr++;
        }
    }
    else {
#endif
    nonRoots = 0;
    analyseAp(e);
    nr = nonRoots;
    rp = rootPortion;

    for (i=0; i<nr; ++i) {
        build(arg(e),co);
        e = fun(e);
    }
#if OBSERVATIONS
    }
#endif

#if TREX
    if (isExt(e))
	e = nameRecExt;
#endif
    if (0<rp && rp<=definingArity) {
	asROOT(rp-1);
    }
    else if (str)
	make(e,co,f,RUNON);
    else
	build(e,co);

    if (nr>0) {
	asMKAP(nr);
    }
}

/* --------------------------------------------------------------------------
 * Code generator entry points:
 * ------------------------------------------------------------------------*/

Addr codeGen(n,arity,e)                /* Generate code for expression e,  */
Name n;                                /* treating return value of CAFs    */
Int  arity;                            /* differently to functs with args  */
Cell e; {
    definingName  = n;
    definingArity = arity;
#if DEBUG_CODE
    if (debugCode) {
	Printf("------------------\n");
	if (nonNull(n)) Printf("name=%s\n",textToStr(name(n).text));
	Printf("Arity   = %d\n",arity);
	Printf("codeGen = "); printExp(stdout,e); Putchar('\n');
    }
#endif
    asSTART();
    if (nonNull(n)) {
	Int i;
	for (i=1; i<=arity; i++)
	    offsPosn[i] = ++srsp;
	make(e,arity,noMatch,(arity>0 ? UPDRET : VALRET));
    }
    else {
	build(e,0);
	asRETURN();
    }
    asEND();
#if DEBUG_CODE
    if (debugCode) {
	if (nonNull(n))
	    Printf("name=%s\n",textToStr(name(n).text));
	dissassemble(startInstr,lastInstr);
	Printf("------------------\n");
    }
#endif
    return startInstr;
}

Void implementCfun(c,scs)               /* Build implementation for constr */
Name c;                                 /* fun c.  scs lists integers (1..)*/
List scs; {                             /* in incr order of strict comps.  */
    Int a = name(c).arity;
    if (a==0 || isNull(scs))
	name(c).defn = c;               /* Name ==> no special imp.        */
    else {
	Name n          = newName(inventText(),c);
	Int  i          = 1;
	name(c).defn    = pair(scs,n);  /* (scs,n) => strict components    */
	name(n).arity   = a;            /* Initialize name data as approp. */

	asSTART();                      /* inaccurate srsp doesn't matter  */
	asCELL(c);
	for (; i<=a; i++)
	    if (nonNull(scs) && intOf(hd(scs))==i) {
		asSTAP();
		scs = tl(scs);
	    }
	    else
		asMKAP(1);
	asRUPDATE();
	asEND();
	name(n).code = startInstr;
#if DEBUG_CODE
	if (debugCode) {
	    Printf("Implement constructor ");
	    printExp(stdout,c);
	    Printf(" using ");
	    printExp(stdout,n);
	    Printf(" with code:\n");
	    dissassemble(startInstr,lastInstr);
	    Printf("------------------\n");
	}
#endif
    }
}

#if TREX
Name nameInsFld;			/* Hooks to Trex library	   */
Name nameShowRecRow;                    /* static.c:trexLoad() binds them  */
Name nameEqRecRow;

Name implementRecShw(t,parent)		/* Build implementation for record */
Text t; 				/* display function.		   */
Cell parent; {
    Name n          = newName(inventText(),parent);
    name(n).arity   = 0;
    name(n).number  = DFUNNAME;
    asSTART();
    asSTRING(t);
    asCELL(nameRecShw);
    asMKAP(1);
    asRETURN();
    asEND();
    name(n).code    = startInstr;
    return n;
}

Name implementRecEq(t,parent)		/* Build implementation for record */
Text t;					/* compare function.		   */
Cell parent; {
    Name n         = newName(inventText(),parent);
    name(n).arity  = 0;
    name(n).number = DFUNNAME;
    asSTART();
    asSTRING(t);
    asCELL(nameRecEq);
    asMKAP(1);
    asRETURN();
    asEND();
    name(n).code   = startInstr;
    return n;
}
#endif

/* --------------------------------------------------------------------------
 * Evaluator:
 * ------------------------------------------------------------------------*/

Int   whnfArgs;                         /* number of arguments of whnf term*/
Cell  whnfHead;                         /* head cell of term in whnf       */
Int   whnfInt;                          /* value of INTCELL (in whnf)      */
Float whnfFloat;                        /* value of FLOATCELL (in whnf)    */
Double whnfDouble;                      /* value of DOUBLECELL (in whnf)   */
Long  numReductions;                    /* number of reductions counted    */
#if PROFILING
#define saveProducer(n)                 { Name old = producer; producer = n
#define restoreProducer()               producer = old;                      \
					if ((numReductions%profInterval)==0) \
					    garbageCollect();                \
					}
#else
#define saveProducer(n)                 /* nothing */
#define restoreProducer()               /* nothing */
#endif

static Cell    exception;               /* Exception thrown                */
static jmp_buf *evalError = 0;          /* jump buffer for eval errors     */

#if GIMME_STACK_DUMPS
Int  rootsp = (-1);
Cell evalRoots[NUM_STACK];
#endif

#if OBSERVATIONS
#define obsMarker(p) (whatIs(stack(p))==OBSERVESTK)
#define isFree(n) (whatIs(n)==OBSERVE && whatIs(markedExpr(n))==FREECELL)
Int appNum;
Int obsCount=0;
#endif

Void eval(n)                            /* Graph reduction evaluator       */
Cell n; {
    StackPtr base = sp;
    Int      ar;

    STACK_CHECK
    if (++evalDepth == MAX_EVAL_DEPTH)
	hugsStackOverflow();
#if GIMME_STACK_DUMPS
    evalRoots[++rootsp] = n;		/* Save pointer to root expression */
					/* should probably test that rootsp*/
					/* is in interval 0..NUM_STACK-1   */
#endif

unw:switch (whatIs(n)) {                /* unwind spine of application     */

	case AP        : push(n);
			 n = fun(n);
			 goto unw;

	case INDIRECT  : n = arg(n);
			 allowBreak();
			 goto unw;

#if OBSERVATIONS
        case OBSERVE   : push(pair(OBSERVESTK,markedObs(n)));
                         obsCount++;
                         n = markedExpr(n);
                         goto unw;
#endif

	case NAME      : allowBreak();
			 {
#if DEBUG_CODE
			 Name saveName = n;
			 if (debugCode) {
			     Printf("%*sEntering name(%d): %s\n", base, "", 
				    n - NAMEMIN, textToStr(name(n).text));
			 }
#endif
#if OBSERVATIONS
    if (!obsCount) {
#endif
			 if (!isCfun(n) && (ar=name(n).arity)<=(sp-base)) {
			    if (ar>0) {                     /* fn with args*/
				 StackPtr root;

				 push(NIL);                 /* rearrange   */
				 root = sp;
				 do {
				     stack(root) = arg(stack(root-1));
				     --root;
				 } while (--ar>0);

				 saveProducer(n);
#ifdef DOTNET
				 if (name(n).foreignInfo != NIL) 
				    primInvoker(root,n);
				 else
#endif
				 if (name(n).primDef)       /* reduce      */
				     (*name(n).primDef)(root);
				 else
				     run(name(n).code,root);
				 numReductions++;
				 restoreProducer();

				 sp = root;                 /* continue... */
				 n      = pop();
			     }
			     else {                         /* CAF         */
				 if (isNull(name(n).defn)) {/* build CAF   */
				     StackPtr root = sp;
				     push(n);               /* save CAF    */
				     saveProducer(n);
#ifdef DOTNET
				     if (name(n).foreignInfo != NIL) 
				       primInvoker(root,n);
				     else
#endif
				     if (name(n).primDef)
					 (*name(n).primDef)(sp);
				     else
					 run(name(n).code,sp);
				     numReductions++;
				     restoreProducer();
				     name(n).defn = top();
				     sp       = root;   /* drop CAF    */
				 }
				 n = name(n).defn;          /*already built*/
				 if (sp>base)
				     fun(top()) = n;
			     }
#if DEBUG_CODE
			     if (debugCode) {
				 Printf("%*sLeaving name(%d): %s\n", base, "", 
					saveName - NAMEMIN, textToStr(name(saveName).text));
			     }
#endif
			     goto unw;
			 }
#if OBSERVATIONS
    }
    else { 		/* handle reduction in presence of observations	   */
        StackPtr p, dest;
        Int      args;
        Cell     newCell, arg, newHead;

        args = 0;                      /* count arguments */
        for (p=sp; p>base; p--)
           if (!obsMarker(p)) args++;  
        
        if (!isCfun(n) && (ar=name(n).arity)<=args) {
           if (ar>0) {                     /* fn with args*/
                StackPtr root;
                StackPtr q;
                Int      argNum, markers, i;
                Cell     insCell;

                push(NIL);
                /* conv. AP to arg; count markers         */
                for (p=sp-1, args=0, markers=0; args<ar; p--)
                  if (obsMarker(p)) {
                    stack(p+1) = stack(p);
                    markers++;
                  }
                  else {
                    stack(p+1) = arg(stack(p));
                    args++;
                  }
                root = p+1;            /* posn of last app*/
                /* add obs cells for marked args          */
                if (markers){
                  appNum++;
                  for (p=sp; p>=root+1; p--)
                    if (obsMarker(p)) {
                       /* set function return value cell   */
                       /* new result list elem    */
                       insCell = triple(NIL,mkInt(appId(appNum,0)),NIL); 
                       insertAfterObs(snd(stack(p)),insCell);
                       seqObs(snd(stack(p))) = mkInt(0); 
                       snd(stack(p)) = insCell;
                       for (q=p-1, argNum=1; q>=root+1; q--)
                         if (!obsMarker(q)) {
                           arg     = triple(OBSERVE,stack(q), NIL);
                           stack(q)=arg;
                           newCell = triple(NIL, mkInt(-1), stack(q));
                           newHead = triple(OBSERVEHEAD,newCell,newCell);
                           /* fix back pointers */
                           nextObs(newCell) = newHead;
                           markedObs(arg)   = newCell;
                           /* reuse newCell as inderect obs. list item */
                           newCell = triple(NIL, mkInt(appId(appNum,argNum++)), newHead);
                           insertAfterObs(insCell, newCell);
                           insCell = newCell;
                         }
                    }
                  /* reorganise stack (move markers)      */
                  for (p=dest=sp; p>=root; p--)
                    if (obsMarker(p))
                      push(stack(p));
                    else
                      stack(dest--) = stack(p);
                  for (i=1, p=root; i<=markers; i++)
                    stack(p++) = pop();
                  root = p;
                }

                saveProducer(n);
#ifdef DOTNET
		if (name(n).foreignInfo != NIL) 
		  primInvoker(root,n);
		else
#endif
                if (name(n).primDef){      /* reduce      */
                    (*name(n).primDef)(root);
                }   
                else
                    run(name(n).code,root);
                numReductions++;
                restoreProducer();

                sp = root;                 /* continue... */
                n      = pop();

                if (markers) {         /* observe results */
                  for (p=root-1, i=1; i<=markers; i++){
                    exprObs(snd(stack(p--))) = n;
                  }
                  sp = sp - markers;
		  obsCount -= markers;
                }

            }
            else {                         /* CAF         */
                if (isNull(name(n).defn)   /* build CAF   */
                   /* || isFree(name(n).defn) */){
                    StackPtr root = sp;
                    push(n);               /* save CAF    */
                    saveProducer(n);
#ifdef DOTNET
		    if (name(n).foreignInfo != NIL)
		      primInvoker(root,n);
		    else
#endif
                    if (name(n).primDef)
                        (*name(n).primDef)(sp);
                    else
                        run(name(n).code,sp);
                    numReductions++;
                    restoreProducer();
                    name(n).defn = top();
                    sp       = root;   /* drop CAF    */
                }
                
                n = name(n).defn;          /*already built*/

                /* move OBSERVESTK markers                */
		for (p=sp; p > base  && obsMarker(p); p--)  ;
		if (p > base)
		    fun(stack(p)) = n;
            }
#if DEBUG_CODE
            if (debugCode) {
                Printf("%*sLeaving name(%d): %s\n", base, "", 
                       saveName - NAMEMIN, textToStr(name(saveName).text));
            }
#endif
            goto unw;
        }
    }
#endif
			 }
			 break;

	case INTCELL   : whnfInt = intOf(n);
			 break;

	case FLOATCELL : whnfFloat = (Float)floatOf(n);
			 break;

	case DOUBLECELL : whnfDouble = (Double)doubleOf(n);
			 break;

	case STRCELL   : evalString(n);
			 goto unw;
    }

#if OBSERVATIONS
    /* remove observation markers due to non-fun observations              */
    if (obsCount){
        StackPtr p, dest;
        for (p=dest=base+1; p<=sp; p++)
            if (!obsMarker(p))
                stack(dest++) = stack(p);
            else{
                obsCount--;
            }
        sp = dest - 1;
    }
#endif

    whnfHead = n;                      /* rearrange components of term on  */
    whnfArgs = sp - base;              /* stack, now in whnf ...           */
    for (ar=whnfArgs; ar>0; ar--) {
	fun(stack(base+ar)) = n;
	n                   = stack(base+ar);
	stack(base+ar)      = arg(n);
    }
#if GIMME_STACK_DUMPS
    rootsp--;
#endif
    evalDepth--;
}

#if OBSERVATIONS
Bool isWhnf(n)                            /* is graph expr in WHNF      */
Cell n; {
    Int      ar;
    Int      args=0;

unw:switch (whatIs(n)) {                /* unwind spine of application     */
	case AP        : args++; 
			 n = fun(n);
			 goto unw;

	case INDIRECT  : n = arg(n);
			 goto unw;

	case OBSERVE   : n = markedExpr(n);
			 goto unw;

	case NAME      : return isCfun(n) 
	                        || (ar=name(n).arity) > args
				|| ar == 0; 

	case STRCELL   : evalString(n);
			 goto unw;
    }
    return TRUE;
}

Cell getCaf(n)
Cell n; {
    while (whatIs(n) == INDIRECT) n = arg(n);
    if (whatIs(n) == NAME && !isCfun(n) && name(n).arity == 0)
        return n;
    else
        return 0;
}
#endif

Void unwind(n)                         /* unwind spine of application;     */
Cell n; {                              /* like eval except that we always  */
    whnfArgs = 0;                      /* treat the expression n as if it  */
				       /* were already in whnf.            */
unw:switch (whatIs(n)) {
	case AP        : push(arg(n));
			 whnfArgs++;
			 n = fun(n);
			 goto unw;

	case INDIRECT  : n = arg(n);
			 allowBreak();
			 goto unw;

	case INTCELL   : whnfInt = intOf(n);
			 break;

	case FLOATCELL : whnfFloat = (Float)floatOf(n);
			 break;

	case DOUBLECELL : whnfDouble = (Double)doubleOf(n);
			 break;

	case STRCELL   : evalString(n);
			 goto unw;
    }
    whnfHead = n;
}

static Void local evalString(n)        /* expand STRCELL at node n         */
Cell n; {
    Text t = textOf(n);
    String s = textToStr(t);
    Char c;
    if (*s==0) {
	fst(n) = INDIRECT;
	snd(n) = nameNil;
	return;
    }
    c = getStrChr(&s);
    push(n);                           /* protect n during mkStr           */
    fst(n) = consChar(c);
    snd(n) = mkStr(t + (s-textToStr(t)));
    drop();
}

Void run(start,root)                   /* execute code beginning at given  */
Addr     start;                        /* address with local stack starting*/
StackPtr root; {                       /* at given root offset             */
    register Memory pc = memory+start;

#if     !DEBUG_CODE && HAVE_LABELS_AS_VALUES
#define Ins(x)          &&l##x
static  void *labs[] = { INSTRLIST };
#undef  Ins
#define Case(x)         l##x
#define Continue        goto *labs[(pc++)->instr]
#define Dispatch        Continue;
#define EndDispatch
#else
#if DEBUG_CODE
#define Dispatch        for (;;) {                                        \
			    if (debugCode) {                              \
				Printf("%*s0x%04X: ", root, "", pc-memory);  \
				dissInstr(pc-memory);                     \
			    }                                             \
			    switch((pc++)->instr) {
#else
#define Dispatch        for (;;) { switch((pc++)->instr) {
#endif
#define Case(x)         case x
#define Continue        continue
#define EndDispatch     default : internal("illegal instruction"); \
				  break;                           \
			}}
#endif

    Dispatch

	Case(iLOAD)   : push(stack(root+pc->mint));      /* load from stack*/
			pc++;
			Continue;

	Case(iCELL)   : push(pc->cell);                  /* load const Cell*/
			pc++;
			Continue;

	Case(iCHAR)   : push(mkChar(pc->mint));          /* load char const*/
			pc++;
			Continue;

	Case(iINT)    : push(mkInt(pc->mint));           /* load int const */
			pc++;
			Continue;

	Case(iDOUBLE)  : push(mkDouble(doubleFromParts   /* load dbl const */
				(pc->cell,(pc+1)->cell)));
			pc+=2;
			Continue;

	Case(iSTRING) : push(mkStr(pc->text));           /* load str const */
			pc++;
			Continue;

	Case(iMKAP)   : {   Int i = pc->mint;            /* make AP nodes  */
			    while (0<i--) {
				pushed(1) = ap(pushed(0),pushed(1));
				drop();
			    }
			}
			pc++;
			Continue;

	Case(iUPDATE) : {   Cell t = stack(root          /* update cell ...*/
					     + pc->mint);
			    Cell r = pop();
			    while (isPair(r) && fst(r)==INDIRECT)
				r = snd(r);
			    fst(t) = INDIRECT;
			    snd(t) = r;
			}
			pc++;
			Continue;

	Case(iRUPDATE): {   Cell t = stack(root);        /* update and ret */
			    Cell r = top();
			    while (isPair(r) && fst(r)==INDIRECT)
				r = snd(r);
			    fst(t) = INDIRECT;
			    snd(t) = r;
			}
			return;

	Case(iUPDAP)  : {   Cell t = stack(root          /* update AP node */
					     + pc->mint);
			    fst(t) = pop();
			    snd(t) = pop();
			}
			pc++;
			Continue;

	Case(iRUPDAP) : {   Cell t = stack(root);        /* updap and ret  */
			    fst(t) = pop();
			    snd(t) = top();
			}
			return;

	Case(iEVAL)   : eval(pop());                     /* evaluate top() */
			Continue;

	Case(iLEVAL)  : eval(stack(root+pc->mint));      /* eval from stack*/
			pc++;
			Continue;

	Case(iSTAP)   : eval(pushed(1));                 /* strict apply   */
			sp   -= whnfArgs;
			pushed(1) = ap(top(),pushed(1));
			drop();
			Continue;

	Case(iRETURN) : return;                          /* terminate      */

	Case(iTEST)   : if (whnfHead==pc->cell)          /* test for cell  */
			    pc += 2;
			else
			    pc += 1 + (pc+1)->addr;
			Continue;

	Case(iGOTO)   : pc += pc->addr;                  /* goto label     */
			Continue;

	Case(iSETSTK) : sp = root + pc->mint;            /* set stack ptr  */
			pc++;
			Continue;

	Case(iALLOC)  : {   Int i = pc->mint;            /* alloc loc vars */
			    chkStack(i);
			    while (0<i--)
				onto(ap(NIL,NIL));
			}
			pc++;
			Continue;

#if OBSERVATIONS
	Case(iROOT)   : {   Cell t = stack(root);        /* partial root   */
			    Int  i = pc->mint;
			    Cell c;
			    while ((c = fst(t))==INDIRECT || c==OBSERVE) {
				allowBreak();
				t = c == INDIRECT ? arg(t) : markedExpr(t);
			    }
			    while (0<i--) {
				t = fun(t);
			        while ((c = fst(t))==INDIRECT || c==OBSERVE) {
				    allowBreak();
				    t = c == INDIRECT ? arg(t) : markedExpr(t);
				}
			    }
			    push(t);
			}
			pc++;
			Continue;
#else
	Case(iROOT)   : {   Cell t = stack(root);        /* partial root   */
			    Int  i = pc->mint;
			    while (fst(t)==INDIRECT) {
				allowBreak();
				t = arg(t);
			    }
			    while (0<i--) {
				t = fun(t);
				while (fst(t)==INDIRECT) {
				    allowBreak();
				    t = arg(t);
				}
			    }
			    push(t);
			}
			pc++;
			Continue;
#endif
	Case(iSLIDE)  : pushed(pc->mint) = top();        /* remove loc vars*/
			sp -= pc->mint;
			pc++;
			Continue;

	Case(iTABLE)  :
	Case(iFAIL)   : evalFails(root);		 /* cannot reduce  */
			return;/*NOT REACHED*/

    EndDispatch

#undef Dispatch
#undef Case
#undef Continue
#undef EndDispatch
}

Cell evalWithNoError(e)                /* Evaluate expression, returning   */
Cell e; {                              /* NIL if successful,               */
    Cell caughtEx;                     /* Exception value if not...        */
    jmp_buf *oldCatch = evalError;

#if JMPBUF_ARRAY
    jmp_buf catcherr[1];
    evalError = catcherr;
    if (setjmp(catcherr[0])==0) {
	eval(e);
	caughtEx = NIL;
    }
    else
	caughtEx = exception;
#else
    jmp_buf catcherr;
    evalError = &catcherr;
    if (setjmp(catcherr)==0) {
	eval(e);
	caughtEx = NIL;
    }
    else
	caughtEx = exception;
#endif
    evalError = oldCatch;
    return caughtEx;
}

Void evalFails(root)                   /* Eval of current redex fails     */
StackPtr root; {
    Cell errorRedex = stack(root);     /* get error & bypass indirections */
    while (isPair(errorRedex) && fst(errorRedex)==INDIRECT)
	errorRedex = snd(errorRedex);
    throwException(ap(namePatternMatchFail,
	ap(ap(ap(nameNPrint, mkInt(MIN_PREC)), errorRedex), nameNil)));
}

Void throwException(ex)
Cell ex; {
    exception = ex;
#if OBSERVATIONS
    obsCount=0;
#endif
    if (evalError)
	longjmp(*evalError,1);
    else
	internal("uncaught exception");
}

/* --------------------------------------------------------------------------
 * Machine control:
 * ------------------------------------------------------------------------*/

Void machine(what)
Int what; {
    switch (what) {
	case INSTALL : 
#if WANT_FIXED_SIZE_TABLES
	               memory  = (Memory)farCalloc(NUM_ADDRS,sizeof(MemCell));
#else
		       /* prelude.h defines NUM_ADDRS; 0 is the upper bound (=> unbounded.) */
		       dynMemory = allocDynTable(sizeof(Memory), NUM_ADDRS, 0, "memory");
		       memory    = (Memory)(dynMemory->data);
#endif
		       if (memory==0)
			   fatal("Cannot allocate program memory");
		       instrNone(iFAIL);
		       noMatch = lastInstr;
		       break;

	case MARK    : break;

	case RESET   : evalError = 0;
		       evalDepth = 0;
#if GIMME_STACK_DUMPS
		       rootsp = (-1);
#endif
		       break;

#if OBSERVATIONS
	case BREAK   : obsCount = 0;
	               break;
#endif

        case EXIT    : 
#if WANT_FIXED_SIZE_TABLES
	               free(memory);
#else
		       if (dynMemory) freeDynTable(dynMemory);
#endif
	               break;
    }
}

/* ------------------------------------------------------------------------*/
