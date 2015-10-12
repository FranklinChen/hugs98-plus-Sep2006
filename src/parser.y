/* --------------------------------------------------------------------------
 * Hugs parser (included as part of input.c)
 *
 * Expect 16 shift/reduce conflicts when passing this grammar through yacc,
 * but don't worry; they should all be resolved in an appropriate manner.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: parser.y,v $
 * $Revision: 1.50 $
 * $Date: 2006/08/30 18:57:13 $
 * ------------------------------------------------------------------------*/

%{
#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define fixdecl(l,ops,a,p)	 ap(FIXDECL,\
				    triple(l,ops,mkInt(mkSyntax(a,intOf(p)))))
#define grded(gs)		 ap(GUARDED,gs)
#define bang(t)			 ap(BANG,t)
#define only(t)			 ap(ONLY,t)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define qualify(ps,t)		 (nonNull(ps) ? ap(QUAL,pair(ps,t)) : t)
#define exportSelf()		 singleton(ap(MODULEENT,mkCon(module(currentModule).text)))
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

#ifdef YYBISON
# if !defined(__GNUC__) || __GNUC__ <= 1
static void __yy_memcpy Args((char*,char*, unsigned int));
# endif
#endif

#ifdef _MANAGED
static void yymemcpy (char *yyto, const char *yyfrom, size_t yycount);
#endif

static Cell   local gcShadow	 Args((Int,Cell));
static Void   local syntaxError	 Args((String));
static String local unexpected	 Args((Void));
static Cell   local checkPrec	 Args((Cell));
static Cell   local buildTuple	 Args((List));
static List   local checkCtxt	 Args((List));
static Cell   local checkPred	 Args((Cell));
static Pair   local checkDo	 Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local checkConstr	 Args((Cell));

#if MUDO
static Pair   local checkMDo	 Args((List));
#endif

#if !TREX
static Void   local noTREX	 Args((String));
#endif
#if !IPARAM
static Void   local noIP	 Args((String));
#endif
#if !MUDO
static Void   local noMDo	 Args((String));
#endif

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Hugs stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl & begin
 * do not leave any values on the Hugs stack.  The same is true for the
 * terminals EXPR and SCRIPT.  At the end of a successful parse, there
 * should only be one element left on the stack, containing the result
 * of the parse.
 */

#define gc0(e)			gcShadow(0,e)
#define gc1(e)			gcShadow(1,e)
#define gc2(e)			gcShadow(2,e)
#define gc3(e)			gcShadow(3,e)
#define gc4(e)			gcShadow(4,e)
#define gc5(e)			gcShadow(5,e)
#define gc6(e)			gcShadow(6,e)
#define gc7(e)			gcShadow(7,e)

%}

%token EXPR       CTXT       SCRIPT
%token CASEXP     OF         DATA       TYPE       IF
%token THEN       ELSE       WHERE      LET        IN
%token INFIXN     INFIXL     INFIXR     PRIMITIVE  TNEWTYPE
%token DEFAULT    DERIVING   DO         TCLASS     TINSTANCE
/*#if MUDO*/
%token MDO
/*#endif*/
%token REPEAT	  ALL        NUMLIT     CHARLIT	   STRINGLIT
%token VAROP      VARID	     CONOP      CONID
%token QVAROP     QVARID     QCONOP     QCONID
/*#if TREX*/
%token RECSELID	  IPVARID
/*#endif*/
%token COCO       '='        UPTO       '@'        '\\'
%token '|'        '-'        FROM       ARROW      '~'
%token '!'        IMPLIES    '('        ','        ')'
%token '['        ';'        ']'        '`'	   '.'
%token TMODULE    IMPORT     HIDING     QUALIFIED  ASMOD
%token NEEDPRIMS
%token FOREIGN

%%
/*- Top level script/module structure -------------------------------------*/

start	  : EXPR exp lwherePart		{inputExpr = letrec($3,$2); sp-=2;}
	  | CTXT context		{inputContext = $2;	    sp-=1;}
	  | SCRIPT topModule		{valDefns  = $2;	    sp-=1;}
	  | error			{syntaxError("input");}
	  ;

/*- Haskell module header/import parsing: -----------------------------------
 * Syntax for Haskell modules (module headers and imports) is parsed but
 * most of it is ignored.  However, module names in import declarations
 * are used, of course, if import chasing is turned on.
 *-------------------------------------------------------------------------*/

/* In Haskell 1.2, the default module header was "module Main where"
 * In 1.3, this changed to "module Main(main) where".
 * We use the 1.2 header because it breaks much less pre-module code.
 */
topModule : startMain begin modBody end {
					 setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
					 $$ = gc3($3);
					}
          | startMain '{' modBody '}'   {
					 setExportList(singleton(ap(MODULEENT,mkCon(module(currentModule).text))));
					 $$ = gc4($3);
					}
	  | TMODULE modname expspec WHERE '{' modBody end
					{setExportList($3);   $$ = gc7($6);}
	  | TMODULE modname expspec WHERE error
					{syntaxError("declaration");}
	  | TMODULE error		{syntaxError("module definition");}
	  ;
/* To implement the Haskell module system, we have to keep track of the
 * current module.  We rely on the use of LALR parsing to ensure that this 
 * side effect happens before any declarations within the module.
 */
startMain : /* empty */			{startModule(conMain); 
					 $$ = gc0(NIL);}
	  ;
modname	  : qconid			{startModule(mkCon(mkNestedQual($1))); $$ = gc1(NIL);}
	  ;
modid	  : qconid			{$$ = mkCon(mkNestedQual($1));}
	  | STRINGLIT			{ String modName = findPathname(textToStr(textOf($1)));
					  if (modName) { /* fillin pathname if known */
					      $$ = mkStr(findText(modName));
					  } else {
					      $$ = $1;
					  }
					}
	  ;
modBody	  : /* empty */                 {$$ = gc0(NIL); }
          | ';' modBody 		{$$ = gc2($2);}
          | topDecls			{$$ = gc1($1);}
	  | impDecls chase		{$$ = gc2(NIL);}
	  | impDecls ';' chase topDecls	{$$ = gc4($4);}
	  ;

/*- Exports: --------------------------------------------------------------*/

expspec	  : /* empty */			{$$ = gc0(exportSelf());}
	  | '(' ')'			{$$ = gc2(NIL);}
	  | '(' ',' ')'			{$$ = gc3(NIL);}
	  | '(' exports ')'		{$$ = gc3($2);}
	  | '(' exports ',' ')'		{$$ = gc4($2);}
	  ;
exports	  : exports ',' export		{$$ = gc3(cons($3,$1));}
	  | export			{$$ = gc1(singleton($1));}
	  ;
/* The qcon should be qconid.  
 * Relaxing the rule lets us explicitly export (:) from the Prelude.
 */
export	  : qvar			{$$ = $1;}
	  | qcon			{$$ = $1;}
	  | qconid '(' UPTO ')'		{$$ = gc4(pair($1,DOTDOT));}
	  | qconid '(' qnames ')'	{$$ = gc4(pair($1,$3));}
	  | TMODULE modid		{$$ = gc2(ap(MODULEENT,$2));}
	  ;
qnames	  : /* empty */			{$$ = gc0(NIL);}
	  | ','				{$$ = gc1(NIL);}
	  | qnames1			{$$ = $1;}
	  | qnames1 ','			{$$ = gc2($1);}
	  ;
qnames1	  : qnames1 ',' qname		{$$ = gc3(cons($3,$1));}
	  | qname			{$$ = gc1(singleton($1));}
	  ;
qname	  : qvar			{$$ = $1;}
	  | qcon			{$$ = $1;}
	  ;

/*- Import declarations: --------------------------------------------------*/

impDecls  : impDecls ';' impDecl	{imps = cons($3,imps); $$=gc3(NIL);}
	  | impDecls ';'		{$$   = gc2(NIL); }
	  | impDecl			{imps = singleton($1); $$=gc1(NIL);}
	  ;
chase	  : /* empty */			{if (chase(imps)) {
					     clearStack();
					     onto(imps);
					     done();
					     closeAnyInput();
					     return 0;
					 }
					 $$ = gc0(NIL);
					}
	  ;
/* Note that qualified import ignores the import list. */
impDecl	  : IMPORT modid impspec	{addUnqualImport($2,NIL,$3);
					 $$ = gc3($2);}
	  | IMPORT modid ASMOD modid impspec
					{addUnqualImport($2,$4,$5);
					 $$ = gc5($2);}
	  | IMPORT QUALIFIED modid ASMOD modid impspec
					{addQualImport($3,$5,$6);
					 $$ = gc6($3);}
	  | IMPORT QUALIFIED modid impspec
					{addQualImport($3,$3,$4);
					 $$ = gc4($3);}
	  | IMPORT error		{syntaxError("import declaration");}
	  ;
impspec	  : /* empty */			{$$ = gc0(DOTDOT);}
	  | HIDING '(' imports ')'	{$$ = gc4(ap(HIDDEN,$3));}
	  | '(' imports ')'		{$$ = gc3($2);}
	  ;
imports	  : /* empty */			{$$ = gc0(NIL);}
	  | ','				{$$ = gc1(NIL);}
	  | imports1			{$$ = $1;}
	  | imports1 ','		{$$ = gc2($1);}
	  ;
imports1  : imports1 ',' import		{$$ = gc3(cons($3,$1));}
	  | import			{$$ = gc1(singleton($1));}
	  ;
import	  : var				{$$ = $1;}
	  | CONID			{$$ = gc1(pair($1,NONE));}
	  | CONID '(' UPTO ')'		{$$ = gc4(pair($1,DOTDOT));}
	  | CONID '(' names ')'		{$$ = gc4(pair($1,$3));}
	  ;
names	  : /* empty */			{$$ = gc0(NIL);}
	  | ','				{$$ = gc1(NIL);}
	  | names1			{$$ = $1;}
	  | names1 ','			{$$ = gc2($1);}
	  ;
names1	  : names1 ',' name		{$$ = gc3(cons($3,$1));}
	  | name			{$$ = gc1(singleton($1));}
	  ;
name	  : var				{$$ = $1;}
	  | con				{$$ = $1;}
	  ;

/*- Top-level declarations: -----------------------------------------------*/

topDecls  : topDecls ';'		{$$ = gc2($1);}
          | topDecls ';' topDecl	{$$ = gc2($1);}
	  | topDecls ';' decl		{$$ = gc3(cons($3,$1));}
	  | topDecl			{$$ = gc0(NIL);}
	  | decl			{$$ = gc1(cons($1,NIL));}
	  ;

/*- Type declarations: ----------------------------------------------------*/

topDecl	  : TYPE tyLhs '=' type		{defTycon(4,$3,$2,$4,SYNONYM);}
	  | TYPE tyLhs '=' type IN invars
					{defTycon(6,$3,$2,
						    ap($4,$6),RESTRICTSYN);}
	  | TYPE error			{syntaxError("type declaration");}
	  | DATA btype2 '=' constrs deriving
					{defTycon(5,$3,checkTyLhs($2),
						    ap(rev($4),$5),DATATYPE);}
	  | DATA context IMPLIES tyLhs '=' constrs deriving
					{defTycon(7,$5,$4,
						  ap(qualify($2,rev($6)),
						     $7),DATATYPE);}
	  | DATA btype2			{defTycon(2,$1,checkTyLhs($2),
						    ap(NIL,NIL),DATATYPE);}
	  | DATA context IMPLIES tyLhs  {defTycon(4,$1,$4,
						  ap(qualify($2,NIL),
						     NIL),DATATYPE);}
	  | DATA error			{syntaxError("data declaration");}
	  | TNEWTYPE btype2 '=' nconstr deriving
					{defTycon(5,$3,checkTyLhs($2),
						    ap($4,$5),NEWTYPE);}
	  | TNEWTYPE context IMPLIES tyLhs '=' nconstr deriving
					{defTycon(7,$5,$4,
						  ap(qualify($2,$6),
						     $7),NEWTYPE);}
	  | TNEWTYPE error		{syntaxError("newtype declaration");}
	  | NEEDPRIMS NUMLIT		{if (isInt($2)) {
					     needPrims(intOf($2), NULL);
					 } else {
					     syntaxError("needprims decl");
					 }
					 sp-=2;}
	  | NEEDPRIMS error		{syntaxError("needprims decl");}
	  ;
tyLhs	  : tyLhs varid			{$$ = gc2(ap($1,$2));}
	  | CONID			{$$ = $1;}
	  | error			{syntaxError("type defn lhs");}
	  ;
invars	  : invars ',' invar		{$$ = gc3(cons($3,$1));}
	  | invar			{$$ = gc1(cons($1,NIL));}
	  ;
invar	  : var COCO topType		{$$ = gc3(sigdecl($2,singleton($1),
									$3));}
	  | var				{$$ = $1;}
	  ;
constrs	  : constrs '|' pconstr		{$$ = gc3(cons($3,$1));}
	  | pconstr			{$$ = gc1(cons($1,NIL));}
	  ;
pconstr	  : ALL varids '.' qconstr	{$$ = gc4(ap(POLYTYPE,
						     pair(rev($2),$4)));}
	  | constr			{$$ = $1;}
	  ;
qconstr	  : context IMPLIES constr	{$$ = gc3(qualify($1,$3));}
	  | constr			{$$ = $1;}
	  ;
constr	  : '!' btype conop bbtype	{$$ = gc4(ap(ap($3,bang($2)),$4));}
	  | btype1    conop bbtype	{$$ = gc3(ap(ap($2,$1),$3));}
	  | btype2    conop bbtype	{$$ = gc3(ap(ap($2,$1),$3));}
	  | bpolyType conop bbtype	{$$ = gc3(ap(ap($2,$1),$3));}
	  | btype2			{$$ = checkConstr($1);}
	  | btype3			{$$ = checkConstr($1);}
	  | con '{' fieldspecs '}'	{$$ = gc4(ap(LABC,pair($1,rev($3))));}
	  | con '{' '}'			{$$ = gc3(ap(LABC,pair($1,NIL)));}
	  | error			{syntaxError("data type declaration");}
	  ;
btype3	  : btype2 '!' atype		{$$ = gc3(ap($1,bang($3)));}
	  | btype2 bpolyType		{$$ = gc2(ap($1,$2));}
	  | btype3 atype		{$$ = gc2(ap($1,$2));}
	  | btype3 '!' atype		{$$ = gc3(ap($1,bang($3)));}
	  | btype3 bpolyType		{$$ = gc2(ap($1,$2));}
	  | '(' CONOP ')'		{$$ = gc3($2);}
	  ;
bbtype	  : '!' btype			{$$ = gc2(bang($2));}
	  | btype			{$$ = $1;}
	  | bpolyType			{$$ = $1;}
	  ;
nconstr	  : pconstr			{$$ = gc1(singleton($1));}
	  ;
fieldspecs: fieldspecs ',' fieldspec	{$$ = gc3(cons($3,$1));}
	  | fieldspec			{$$ = gc1(cons($1,NIL));}
	  ;
fieldspec : vars COCO polyType		{$$ = gc3(pair(rev($1),$3));}
	  | vars COCO type		{$$ = gc3(pair(rev($1),$3));}
	  | vars COCO '!' type		{$$ = gc4(pair(rev($1),bang($4)));}
	  ;
deriving  : /* empty */			{$$ = gc0(NIL);}
	  | DERIVING qconid		{$$ = gc2(singleton($2));}
	  | DERIVING '(' derivs0 ')'	{$$ = gc4($3);}
	  ;
derivs0	  : /* empty */			{$$ = gc0(NIL);}
	  | derivs			{$$ = gc1(rev($1));}
	  ;
derivs	  : derivs ',' qconid		{$$ = gc3(cons($3,$1));}
	  | qconid			{$$ = gc1(singleton($1));}
	  ;

/*- Processing definitions of primitives ----------------------------------*/

topDecl	  : PRIMITIVE prims COCO topType{primDefn($1,$2,$4); sp-=4;}
	  ;
prims	  : prims ',' prim		{$$ = gc3(cons($3,$1));}
	  | prim			{$$ = gc1(cons($1,NIL));}
	  | error			{syntaxError("primitive defn");}
	  ;
prim	  : var STRINGLIT		{$$ = gc2(pair($1,$2));}
	  | var				{$$ = $1;}
	  ;

/*- Foreign Function Interface --------------------------------------------*/

topDecl   : FOREIGN IMPORT var STRINGLIT var COCO topType 
               {foreignImport($1,$3,NIL,$4,$5,$7); sp-=7;}
          | FOREIGN IMPORT var var COCO topType 
               {foreignImport($1,$3,NIL,$4,$4,$6); sp-=6;}
          | FOREIGN IMPORT var var STRINGLIT var COCO topType 
               {foreignImport($1,$3,$4,$5,$6,$8); sp-=8;}
          | FOREIGN IMPORT var var var COCO topType 
               {foreignImport($1,$3,$4,$5,$5,$7); sp-=7;}
          | FOREIGN var    var STRINGLIT var COCO topType 
               {foreignExport($1,$2,$3,$4,$5,$7); sp-=7;}
	  ;

/*- Class declarations: ---------------------------------------------------*/

topDecl	  : TCLASS crule fds wherePart	{classDefn(intOf($1),$2,$4,$3); sp-=4;}
	  | TINSTANCE irule wherePart	{instDefn(intOf($1),$2,$3);  sp-=3;}
	  | DEFAULT '(' dtypes ')'	{defaultDefn(intOf($1),$3);  sp-=4;}
	  | TCLASS error		{syntaxError("class declaration");}
	  | TINSTANCE error		{syntaxError("instance declaration");}
	  | DEFAULT error		{syntaxError("default declaration");}
	  ;
crule	  : context IMPLIES btype2	{$$ = gc3(pair($1,checkPred($3)));}
	  | btype2			{$$ = gc1(pair(NIL,checkPred($1)));}
	  ;
irule	  : context IMPLIES btype2	{$$ = gc3(pair($1,checkPred($3)));}
	  | btype2			{$$ = gc1(pair(NIL,checkPred($1)));}
	  ;
dtypes	  : /* empty */			{$$ = gc0(NIL);}
	  | dtypes1			{$$ = gc1(rev($1));}
	  ;
dtypes1	  : dtypes1 ',' type		{$$ = gc3(cons($3,$1));}
	  | type			{$$ = gc1(cons($1,NIL));}
	  ;
fds	  : /* empty */			{$$ = gc0(NIL);}
	  | '|' fds1			{h98DoesntSupport(row,"dependent parameters");
					 $$ = gc2(rev($2));}
	  ;
fds1	  : fds1 ',' fd			{$$ = gc3(cons($3,$1));}
	  | fd				{$$ = gc1(cons($1,NIL));}
	  ;
fd	  : varids0 ARROW varids0	{$$ = gc3(pair(rev($1),rev($3)));}
	  | error			{syntaxError("functional dependency");}
	  ;
varids0   : /* empty */			{$$ = gc0(NIL);}
	  | varids0 varid		{$$ = gc2(cons($2,$1));}
	  ;

/*- Type expressions: -----------------------------------------------------*/

topType	  : ALL varids '.' topType0	{$$ = gc4(ap(POLYTYPE,
						     pair(rev($2),$4)));}
	  | topType0			{$$ = $1;}
	  ;
topType0  : context IMPLIES topType1	{$$ = gc3(qualify($1,$3));}
	  | topType1			{$$ = $1;}
	  ;
topType1  : bpolyType ARROW topType1	{$$ = gc3(fn($1,$3));}
	  | btype1    ARROW topType1	{$$ = gc3(fn($1,$3));}
	  | btype2    ARROW topType1	{$$ = gc3(fn($1,$3));}
	  | btype			{$$ = $1;}
	  ;
polyType  : ALL varids '.' sigType	{$$ = gc4(ap(POLYTYPE,
						     pair(rev($2),$4)));}
	  | bpolyType			{$$ = $1;}
	  ;
bpolyType : '(' polyType ')'		{$$ = gc3($2);}
	  | '(' lcontext IMPLIES type ')' {$$ = gc5(qualify($2,$4));}
	  ;
varids	  : varids varid		{$$ = gc2(cons($2,$1));}
	  | varid			{$$ = gc1(singleton($1));}
	  ;
sigType   : context IMPLIES type	{$$ = gc3(qualify($1,$3));}
	  | type			{$$ = $1;}
	  ;
context	  : '(' ')'			{$$ = gc2(NIL);}
	  | btype2			{$$ = gc1(singleton(checkPred($1)));}
	  | '(' btype2 ')'		{$$ = gc3(singleton(checkPred($2)));}
	  | '(' btypes2 ')'		{$$ = gc3(checkCtxt(rev($2)));}
 	  | lacks			{$$ = gc1(singleton($1));}
 	  | '(' lacks1 ')'		{$$ = gc3(checkCtxt(rev($2)));}
 	  ;
lcontext  : lacks			{$$ = gc1(singleton($1));}
 	  | '(' lacks1 ')'		{$$ = gc3(checkCtxt(rev($2)));}
	  ;
lacks	  : varid '\\' varid		{
#if TREX
					 $$ = gc3(ap(mkExt(textOf($3)),$1));
#else
					 noTREX("a type context");
#endif
					}
          | IPVARID COCO type		{
#if IPARAM
					 $$ = gc3(pair(mkIParam($1),$3));
#else
					 noIP("a type context");
#endif
					}
	  ;
lacks1    : btypes2 ',' lacks		{$$ = gc3(cons($3,$1));}
	  | lacks1  ',' btype2		{$$ = gc3(cons($3,$1));}
	  | lacks1  ',' lacks		{$$ = gc3(cons($3,$1));}
	  | btype2  ',' lacks		{$$ = gc3(cons($3,cons($1,NIL)));}
	  | lacks			{$$ = gc1(singleton($1));}
	  ;

type	  : type1			{$$ = $1;}
	  | btype2			{$$ = $1;}
	  ;
type1	  : btype1			{$$ = $1;}
	  | bpolyType ARROW type	{$$ = gc3(fn($1,$3));}
	  | btype1    ARROW type	{$$ = gc3(fn($1,$3));}
	  | btype2    ARROW type	{$$ = gc3(fn($1,$3));}
	  | error			{syntaxError("type expression");}
	  ;
btype	  : btype1			{$$ = $1;}
	  | btype2			{$$ = $1;}
	  ;
btype1	  : btype1 atype		{$$ = gc2(ap($1,$2));}
	  | atype1			{$$ = $1;}
	  ;
btype2	  : btype2 atype		{$$ = gc2(ap($1,$2));}
	  | qconid			{$$ = $1;}
	  ;
atype	  : atype1			{$$ = $1;}
	  | qconid			{$$ = $1;}
	  ;
atype1	  : varid			{$$ = $1;}
	  | '(' ')'			{$$ = gc2(typeUnit);}
	  | '(' ARROW ')'		{$$ = gc3(typeArrow);}
	  | '(' type1 ')'		{$$ = gc3($2);}
	  | '(' btype2 ')'		{$$ = gc3($2);}
	  | '(' tupCommas ')'		{$$ = gc3($2);}
	  | '(' btypes2 ')'		{$$ = gc3(buildTuple($2));}
	  | '(' typeTuple ')'		{$$ = gc3(buildTuple($2));}
	  | '(' tfields ')'		{
#if TREX
					 $$ = gc3(revOnto($2,typeNoRow));
#else
					 noTREX("a type");
#endif
					}
	  | '(' tfields '|' type ')'	{
#if TREX
					 $$ = gc5(revOnto($2,$4));
#else
					 noTREX("a type");
#endif
					}
	  | '[' type ']'		{$$ = gc3(ap(typeList,$2));}
	  | '[' ']'			{$$ = gc2(typeList);}
	  | '_'				{h98DoesntSupport(row,"anonymous type variables");
					 $$ = gc1(inventVar());}
	  ;
btypes2	  : btypes2 ',' btype2		{$$ = gc3(cons($3,$1));}
	  | btype2  ',' btype2		{$$ = gc3(cons($3,cons($1,NIL)));}
	  ;
typeTuple : type1     ',' type		{$$ = gc3(cons($3,cons($1,NIL)));}
	  | btype2    ',' type1		{$$ = gc3(cons($3,cons($1,NIL)));}
	  | btypes2   ',' type1		{$$ = gc3(cons($3,$1));}
	  | typeTuple ',' type		{$$ = gc3(cons($3,$1));}
	  ;
/*#if TREX*/
tfields	  : tfields ',' tfield		{$$ = gc3(cons($3,$1));}
	  | tfield			{$$ = gc1(singleton($1));}
	  ;
tfield	  : varid COCO type		{h98DoesntSupport(row,"extensible records");
					 $$ = gc3(ap(mkExt(textOf($1)),$3));}
	  ;
/*#endif*/

/*- Value declarations: ---------------------------------------------------*/

gendecl	  : INFIXN optDigit ops		{$$ = gc3(fixdecl($1,$3,NON_ASS,$2));}
	  | INFIXN error		{syntaxError("fixity decl");}
	  | INFIXL optDigit ops		{$$ = gc3(fixdecl($1,$3,LEFT_ASS,$2));}
	  | INFIXL error		{syntaxError("fixity decl");}
	  | INFIXR optDigit ops		{$$ = gc3(fixdecl($1,$3,RIGHT_ASS,$2));}
	  | INFIXR error		{syntaxError("fixity decl");}
	  | vars COCO topType		{$$ = gc3(sigdecl($2,$1,$3));}
	  | vars COCO error		{syntaxError("type signature");}
	  ;
optDigit  : NUMLIT			{$$ = gc1(checkPrec($1));}
	  | /* empty */			{$$ = gc0(mkInt(DEF_PREC));}
	  ;
ops	  : ops ',' op			{$$ = gc3(cons($3,$1));}
	  | op				{$$ = gc1(singleton($1));}
	  ;
vars	  : vars ',' var		{$$ = gc3(cons($3,$1));}
	  | var				{$$ = gc1(singleton($1));}
	  ;
decls	  : '{' decls0 end		{$$ = gc3($2);}
	  | '{' decls1 end		{$$ = gc3($2);}
	  ;
decls0	  : /* empty */			{$$ = gc0(NIL);}
	  | decls0 ';'			{$$ = gc2($1);}
	  | decls1 ';'			{$$ = gc2($1);}
	  ;
decls1	  : decls0 decl			{$$ = gc2(cons($2,$1));}
	  ;
decl	  : gendecl			{$$ = $1;}
	  | funlhs rhs			{$$ = gc2(ap(FUNBIND,pair($1,$2)));}
	  | funlhs COCO type rhs	{$$ = gc4(ap(FUNBIND,
						     pair($1,ap(RSIGN,
								ap($4,$3)))));}
	  | pat0 rhs			{$$ = gc2(ap(PATBIND,pair($1,$2)));}
	  ;
funlhs	  : funlhs0			{$$ = $1;}
	  | funlhs1			{$$ = $1;}
	  | npk				{$$ = $1;}
	  ;
funlhs0	  : pat10_vI varop    pat0	{$$ = gc3(ap2($2,$1,$3));}
	  | infixPat varop    pat0	{$$ = gc3(ap2($2,$1,$3));}
	  | NUMLIT   varop    pat0	{$$ = gc3(ap2($2,$1,$3));}
	  | var      varop_pl pat0	{$$ = gc3(ap2($2,$1,$3));}
	  | var      '+'      pat0_INT	{$$ = gc3(ap2(varPlus,$1,$3));}
	  ;
funlhs1	  : '(' funlhs0 ')' apat	{$$ = gc4(ap($2,$4));}
	  | '(' funlhs1 ')' apat	{$$ = gc4(ap($2,$4));}
	  | '(' npk     ')' apat	{$$ = gc4(ap($2,$4));}
	  | var     apat		{$$ = gc2(ap($1,$2));}
	  | funlhs1 apat		{$$ = gc2(ap($1,$2));}
	  ;
rhs	  : rhs1 wherePart		{$$ = gc2(letrec($2,$1));}
	  | error			{syntaxError("declaration");}
	  ;
rhs1	  : '=' exp			{$$ = gc2(pair($1,$2));}
	  | gdrhs			{$$ = gc1(grded(rev($1)));}
	  ;
gdrhs	  : gdrhs gddef			{$$ = gc2(cons($2,$1));}
	  | gddef			{$$ = gc1(singleton($1));}
	  ;
gddef	  : '|' exp0 '=' exp		{$$ = gc4(pair($3,pair($2,$4)));}
	  ;
wherePart : /* empty */			{$$ = gc0(NIL);}
	  | WHERE decls			{$$ = gc2($2);}
	  ;
/* lwherePart and ldecls permit the binding of both 'normal' 
 * and implicit parameter bindings.
 */
lwherePart : /* empty */		{$$ = gc0(NIL);}
	  | WHERE ldecls		{$$ = gc2($2);}
	  ;

ldecls	  : '{' ldecls0 end		{$$ = gc3($2);}
	  | '{' ldecls1 end		{$$ = gc3($2);}
	  ;

ldecls0	  : /* empty */			{$$ = gc0(NIL);}
	  | ldecls0 ';'			{$$ = gc2($1);}
	  | ldecls1 ';'			{$$ = gc2($1);}
	  ;

ldecls1	  : ldecls0 ldecl		{$$ = gc2(cons($2,$1));}
	  ;
ldecl	  : IPVARID '=' exp		{
#if IPARAM
				         $$ = gc3(pair($1,$3));
#else
					 noIP("a binding");
#endif
					}
	  | IPVARID error		{syntaxError("a binding");}
          | decl 			{$$ = $1;} 
	  ;

/*- Patterns: -------------------------------------------------------------*/

pat	  : npk				{$$ = $1;}
	  | pat_npk			{$$ = $1;}
	  ;
pat_npk	  : pat0 COCO type		{$$ = gc3(ap(ESIGN,pair($1,$3)));}
	  | pat0			{$$ = $1;}
	  ;
npk	  : var '+' NUMLIT		{$$ = gc3(ap2(varPlus,$1,$3));}
	  ;
pat0	  : var				{$$ = $1;}
	  | NUMLIT			{$$ = $1;}
	  | pat0_vI			{$$ = $1;}
	  ;
pat0_INT  : var				{$$ = $1;}
	  | pat0_vI			{$$ = $1;}
	  ;
pat0_vI	  : pat10_vI			{$$ = $1;}
	  | infixPat			{$$ = gc1(ap(INFIX,$1));}
	  ;
infixPat  : '-' pat10			{$$ = gc2(ap(NEG,only($2)));}
	  | '-' error			{syntaxError("pattern");}
	  | var qconop pat10		{$$ = gc3(ap(ap($2,only($1)),$3));}
	  | var qconop '-' pat10	{$$ = gc4(ap(NEG,ap2($2,only($1),$4)));}
	  | NUMLIT qconop pat10		{$$ = gc3(ap(ap($2,only($1)),$3));}
	  | NUMLIT qconop '-' pat10	{$$ = gc4(ap(NEG,ap2($2,only($1),$4)));}
	  | pat10_vI qconop pat10	{$$ = gc3(ap(ap($2,only($1)),$3));}
	  | pat10_vI qconop '-' pat10	{$$ = gc4(ap(NEG,ap2($2,only($1),$4)));}
	  | infixPat qconop pat10	{$$ = gc3(ap(ap($2,$1),$3));}
	  | infixPat qconop '-' pat10	{$$ = gc4(ap(NEG,ap(ap($2,$1),$4)));}
	  ;
pat10	  : fpat			{$$ = $1;}
	  | apat			{$$ = $1;}
	  ;
pat10_vI  : fpat			{$$ = $1;}
	  | apat_vI			{$$ = $1;}
	  ;
fpat	  : fpat apat			{$$ = gc2(ap($1,$2));}
	  | gcon apat			{$$ = gc2(ap($1,$2));}
	  ;
apat	  : NUMLIT			{$$ = $1;}
	  | var				{$$ = $1;}
	  | apat_vI			{$$ = $1;}
	  ;
apat_vI	  : var '@' apat		{$$ = gc3(ap(ASPAT,pair($1,$3)));}
	  | gcon			{$$ = $1;}
	  | qcon '{' patbinds '}'	{$$ = gc4(ap(CONFLDS,pair($1,$3)));}
	  | CHARLIT			{$$ = $1;}
	  | STRINGLIT			{$$ = $1;}
	  | '_'				{$$ = gc1(WILDCARD);}
	  | '(' pat_npk ')'		{$$ = gc3($2);}
	  | '(' npk ')'			{$$ = gc3($2);}
	  | '(' pats2 ')'		{$$ = gc3(buildTuple($2));}
	  | '[' pats1 ']'		{$$ = gc3(ap(FINLIST,rev($2)));}
	  | '~' apat			{$$ = gc2(ap(LAZYPAT,$2));}
/*#if TREX*/
	  | '(' patfields ')'		{
#if TREX
					 $$ = gc3(revOnto($2,nameNoRec));
#else
					 $$ = gc3(NIL);
#endif
					}
	  | '(' patfields '|' pat ')'	{$$ = gc5(revOnto($2,$4));}
/*#endif TREX*/
	  ;
pats2	  : pats2 ',' pat		{$$ = gc3(cons($3,$1));}
	  | pat ',' pat			{$$ = gc3(cons($3,singleton($1)));}
	  ;
pats1	  : pats1 ',' pat		{$$ = gc3(cons($3,$1));}
	  | pat				{$$ = gc1(singleton($1));}
	  ;
patbinds  : /* empty */			{$$ = gc0(NIL);}
	  | patbinds1			{$$ = gc1(rev($1));}
	  ;
patbinds1 : patbinds1 ',' patbind	{$$ = gc3(cons($3,$1));}
	  | patbind			{$$ = gc1(singleton($1));}
	  ;
patbind	  : qvar '=' pat		{$$ = gc3(pair($1,$3));}
	  | var				{$$ = $1;}
	  ;
/*#if TREX*/
patfields : patfields ',' patfield	{$$ = gc3(cons($3,$1));}
	  | patfield			{$$ = gc1(singleton($1));}
	  ;
patfield  : varid '=' pat		{
#if TREX
					 $$ = gc3(ap(mkExt(textOf($1)),$3));
#else
					 noTREX("a pattern");
#endif
					}
	  ;
/*#endif TREX*/

/*- Expressions: ----------------------------------------------------------*/

exp	  : exp_err			{$$ = $1;}
	  | error			{syntaxError("expression");}
	  ;
exp_err	  : exp0a COCO sigType		{$$ = gc3(ap(ESIGN,pair($1,$3)));}
	  | exp0			{$$ = $1;}
	  ;
exp0	  : exp0a			{$$ = $1;}
	  | exp0b			{$$ = $1;}
	  ;
exp0a	  : infixExpa			{$$ = gc1(ap(INFIX,$1));}
	  | exp10a			{$$ = $1;}
	  ;
exp0b	  : infixExpb			{$$ = gc1(ap(INFIX,$1));}
	  | exp10b			{$$ = $1;}
	  ;
infixExpa : infixExpa qop '-' exp10a	{$$ = gc4(ap(NEG,ap(ap($2,$1),$4)));}
	  | infixExpa qop exp10a	{$$ = gc3(ap(ap($2,$1),$3));}
	  | '-' exp10a			{$$ = gc2(ap(NEG,only($2)));}
	  | exp10a qop '-' exp10a	{$$ = gc4(ap(NEG,
						     ap(ap($2,only($1)),$4)));}
	  | exp10a qop exp10a		{$$ = gc3(ap(ap($2,only($1)),$3));}
	  ;
infixExpb : infixExpa qop '-' exp10b	{$$ = gc4(ap(NEG,ap(ap($2,$1),$4)));}
	  | infixExpa qop exp10b	{$$ = gc3(ap(ap($2,$1),$3));}
	  | '-' exp10b			{$$ = gc2(ap(NEG,only($2)));}
	  | exp10a qop '-' exp10b	{$$ = gc4(ap(NEG,
						     ap(ap($2,only($1)),$4)));}
	  | exp10a qop exp10b		{$$ = gc3(ap(ap($2,only($1)),$3));}
	  ;
exp10a	  : CASEXP exp OF '{' alts end	{$$ = gc6(ap(CASE,pair($2,rev($5))));}
	  | DO '{' stmts end		{$$ = gc4(ap(DOCOMP,checkDo($3)));}
          | MDO '{' stmts end		{
#if MUDO
					 $$ = gc4(ap(MDOCOMP, checkMDo($3)));
#else
					 noMDo("an expression");
#endif
					}
	  | appExp			{$$ = $1;}
	  ;
exp10b	  : '\\' pats ARROW exp		{$$ = gc4(ap(LAMBDA,      
						     pair(rev($2),
							  pair($3,$4))));}
	  | LET ldecls IN exp		{$$ = gc4(letrec($2,$4));}
	  | IF exp then_exp else_exp	{$$ = gc4(ap(COND,triple($2,$3,$4)));}
	  ;
/* Allow optional semicolons before 'then' and 'else' (as suggested by
   John Meacham), to remove a common pitfall when using if-then-else
   inside do expressions with implicit layout. */
then_exp  : ';' THEN exp		{$$ = gc3($3);}
	  | THEN exp			{$$ = gc2($2);}
	  ;
else_exp  : ';' ELSE exp		{$$ = gc3($3);}
	  | ELSE exp			{$$ = gc2($2);}
	  ;

pats      : pats apat			{$$ = gc2(cons($2,$1));}
	  | apat			{$$ = gc1(cons($1,NIL));}
	  ;
appExp    : appExp aexp			{$$ = gc2(ap($1,$2));}
	  | aexp			{$$ = $1;}
	  ;
aexp	  : qvar			{$$ = $1;}
	  | qvar '@' aexp		{$$ = gc3(ap(ASPAT,pair($1,$3)));}
	  | '~' aexp			{$$ = gc2(ap(LAZYPAT,$2));}
	  | IPVARID			{$$ = $1;}
	  | '_'				{$$ = gc1(WILDCARD);}
	  | gcon			{$$ = $1;}
	  | qcon '{' fbinds '}'		{$$ = gc4(ap(CONFLDS,pair($1,$3)));}
	  | aexp '{' fbinds '}'		{$$ = gc4(ap(UPDFLDS,
						     triple($1,NIL,$3)));}
	  | NUMLIT			{$$ = $1;}
	  | CHARLIT			{$$ = $1;}
	  | STRINGLIT			{$$ = $1;}
	  | REPEAT			{$$ = $1;}
	  | '(' exp ')'			{$$ = gc3($2);}
	  | '(' exps2 ')'		{$$ = gc3(buildTuple($2));}
/*#if TREX*/
	  | '(' vfields ')'		{
#if TREX
					 $$ = gc3(revOnto($2,nameNoRec));
#else
					 $$ = gc3(NIL);
#endif
					}
	  | '(' vfields '|' exp ')'	{$$ = gc5(revOnto($2,$4));}
	  | RECSELID			{$$ = $1;}
/*#endif*/
	  | '[' list ']'		{$$ = gc3($2);}
	  | '(' exp10a qop ')'		{$$ = gc4(ap($3,$2));}
	  | '(' qvarop_mi exp0 ')'	{$$ = gc4(ap(ap(nameFlip,$2),$3));}
	  | '(' qconop exp0 ')'		{$$ = gc4(ap(ap(nameFlip,$2),$3));}
	  ;
exps2	  : exps2 ',' exp		{$$ = gc3(cons($3,$1));}
	  | exp ',' exp			{$$ = gc3(cons($3,cons($1,NIL)));}
	  ;
/*#if TREX*/
vfields	  : vfields ',' vfield		{$$ = gc3(cons($3,$1));}
	  | vfield			{$$ = gc1(singleton($1));}
	  ;
vfield	  : varid '=' exp		{
#if TREX
					 $$ = gc3(ap(mkExt(textOf($1)),$3));
#else
					 noTREX("an expression");
#endif
					}
	  ;
/*#endif*/
alts      : alts1                       {$$ = $1;}
          | ';' alts                    {$$ = gc2($2);}
          ;
alts1     : alts1 ';' alt               {$$ = gc3(cons($3,$1));}
          | alts1 ';'                   {$$ = gc2($1);}
          | alt                         {$$ = gc1(cons($1,NIL));}
	  ;
alt	  : pat altRhs wherePart	{$$ = gc3(pair($1,letrec($3,$2)));}
	  ;
altRhs	  : guardAlts			{$$ = gc1(grded(rev($1)));}
	  | ARROW exp			{$$ = gc2(pair($1,$2));}
	  | error			{syntaxError("case expression");}
	  ;
guardAlts : guardAlts guardAlt		{$$ = gc2(cons($2,$1));}
	  | guardAlt			{$$ = gc1(cons($1,NIL));}
	  ;
guardAlt  : '|' exp0 ARROW exp		{$$ = gc4(pair($3,pair($2,$4)));}
	  ;

stmts     : stmts1                      {$$ = $1;}
          | ';' stmts                   {$$ = gc2($2);}
	  ;
stmts1	  : stmts1 ';' stmt		{$$ = gc3(cons($3,$1));}
          | stmts1 ';'			{$$ = gc2($1);}
	  | stmt			{$$ = gc1(cons($1,NIL));}
	  ;

stmt      : exp_err FROM exp		{$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
	  | LET ldecls			{$$ = gc2(ap(QWHERE,$2));}
/*	  | IF exp			{$$ = gc2(ap(BOOLQUAL,$2));}*/
	  | exp_err			{$$ = gc1(ap(DOQUAL,$1));}
	  ;
fbinds	  : /* empty */			{$$ = gc0(NIL);}
	  | fbinds1			{$$ = gc1(rev($1));}
	  ;
fbinds1	  : fbinds1 ',' fbind		{$$ = gc3(cons($3,$1));}
	  | fbind			{$$ = gc1(singleton($1));}
	  ;
fbind	  : var				{$$ = $1;}
	  | qvar '=' exp		{$$ = gc3(pair($1,$3));}
	  ;

/*- List Expressions: -------------------------------------------------------*/

list	  : exp				{$$ = gc1(ap(FINLIST,cons($1,NIL)));}
	  | exps2			{$$ = gc1(ap(FINLIST,rev($1)));}
	  | exp zipquals		{
#if ZIP_COMP
					 if (length($2)==1) {
					     $$ = gc2(ap(COMP,pair($1,hd($2))));
					 } else {
					     if (haskell98)
						 syntaxError("list comprehension");
					     $$ = gc2(ap(ZCOMP,pair($1,rev($2))));
					 }
#else
					 if (length($2)!=1) {
					     syntaxError("list comprehension");
					 }
					 $$ = gc2(ap(COMP,pair($1,hd($2))));
#endif
					}
	  | exp         UPTO exp	{$$ = gc3(ap(ap(nameFromTo,$1),$3));}
	  | exp ',' exp UPTO		{$$ = gc4(ap(ap(nameFromThen,$1),$3));}
	  | exp         UPTO		{$$ = gc2(ap(nameFrom,$1));}
	  | exp ',' exp UPTO exp	{$$ = gc5(ap(ap(ap(nameFromThenTo,
								$1),$3),$5));}
	  ;
zipquals  : zipquals '|' quals		{$$ = gc3(cons(rev($3),$1));}
	  | '|' quals			{$$ = gc2(cons(rev($2),NIL));}
	  ;
quals	  : quals ',' qual		{$$ = gc3(cons($3,$1));}
	  | qual			{$$ = gc1(cons($1,NIL));}
	  ;
qual	  : exp FROM exp		{$$ = gc3(ap(FROMQUAL,pair($1,$3)));}
	  | exp				{$$ = gc1(ap(BOOLQUAL,$1));}
	  | LET ldecls			{$$ = gc2(ap(QWHERE,$2));}
	  ;

/*- Identifiers and symbols: ----------------------------------------------*/

gcon	  : qcon			{$$ = $1;}
	  | '(' ')'			{$$ = gc2(nameUnit);}
	  | '[' ']'			{$$ = gc2(nameNil);}
	  | '(' tupCommas ')'		{$$ = gc3($2);}
	  ;
tupCommas : tupCommas ','		{$$ = gc2(mkTuple(tupleOf($1)+1));}
	  | ','				{$$ = gc1(mkTuple(2));}
	  ;
varid	  : VARID			{$$ = $1;}
	  | HIDING			{$$ = gc1(varHiding);}
	  | QUALIFIED			{$$ = gc1(varQualified);}
	  | ASMOD			{$$ = gc1(varAsMod);}
	  ;
qconid	  : QCONID			{$$ = $1;}
	  | CONID			{$$ = $1;}
	  ;
var	  : varid			{$$ = $1;}
	  | '(' VAROP ')'		{$$ = gc3($2);}
	  | '(' '+' ')'			{$$ = gc3(varPlus);}
	  | '(' '-' ')'			{$$ = gc3(varMinus);}
	  | '(' '!' ')'			{$$ = gc3(varBang);}
	  | '(' '.' ')'			{$$ = gc3(varDot);}
	  ;
qvar	  : QVARID			{$$ = $1;}
	  | '(' QVAROP ')'		{$$ = gc3($2);}
	  | var				{$$ = $1;}
	  ;
con	  : CONID			{$$ = $1;}
	  | '(' CONOP ')'		{$$ = gc3($2);}
	  ;
qcon	  : QCONID			{$$ = $1;}
	  | '(' QCONOP ')'		{$$ = gc3($2);}
	  | con				{$$ = $1;}
	  ;
varop	  : '+'				{$$ = gc1(varPlus);}
	  | '-'				{$$ = gc1(varMinus);}
	  | varop_mipl			{$$ = $1;}
	  ;
varop_mi  : '+'				{$$ = gc1(varPlus);}
	  | varop_mipl			{$$ = $1;}
	  ;
varop_pl  : '-'				{$$ = gc1(varMinus);}
	  | varop_mipl			{$$ = $1;}
	  ;
varop_mipl: VAROP			{$$ = $1;}
	  | '`' varid '`'		{$$ = gc3($2);}
	  | '!'				{$$ = gc1(varBang);}
	  | '.'				{$$ = gc1(varDot);}
	  ;
qvarop	  : '-'				{$$ = gc1(varMinus);}
	  | qvarop_mi			{$$ = $1;}
	  ;
qvarop_mi : QVAROP			{$$ = $1;}
	  | '`' QVARID '`'		{$$ = gc3($2);}
	  | varop_mi			{$$ = $1;}
	  ;

conop	  : CONOP 			{$$ = $1;}
	  | '`' CONID  '`'		{$$ = gc3($2);}
	  ;
qconop	  : QCONOP			{$$ = $1;}
	  | '`' QCONID '`'		{$$ = gc3($2);}
	  | conop			{$$ = $1;}
	  ;
op	  : varop			{$$ = $1;}
	  | conop			{$$ = $1;}
	  ;
qop	  : qvarop			{$$ = $1;}
	  | qconop			{$$ = $1;}
	  ;

/*- Tricks to force insertion of leading and closing braces ---------------*/

begin	  : /* empty */			{goOffside(startColumn);}
	  ;
					/* deal with trailing semicolon    */
end	  : '}'				{$$ = $1;}
	  | error			{yyerrok; 
					 if (canUnOffside()) {
					     unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
					 else
					     syntaxError("declaration");
					}
	  ;

/*-------------------------------------------------------------------------*/

%%

static Cell local gcShadow(n,e)		/* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
	pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)	/* report on syntax error	   */
String s; {
    ERRMSG(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {     /* find name for unexpected token   */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";

    switch (yychar) {
	case 0         : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIXN    : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case TNEWTYPE  : keyword("newtype");
	case LET       : keyword("let");
	case IN        : keyword("in");
	case DERIVING  : keyword("deriving");
	case DEFAULT   : keyword("default");
	case IMPORT    : keyword("import");
	case TMODULE   : keyword("module");
	case ALL       : keyword("forall");
#undef keyword

	case ARROW     : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case '!'       : return "`!'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '{'       : return "`{', possibly due to bad layout";
	case '}'       : return "`}', possibly due to bad layout";
	case '_'       : return "`_'";
	case '|'       : return "`|'";
	case '.'       : return "`.'";
	case ';'       : return "`;', possibly due to bad layout";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
#if TREX
	case RECSELID  : sprintf(buffer,"selector \"#%s\"",
				 textToStr(extText(snd(yylval))));
			 return buffer;
#endif
#if IPARAM
	case IPVARID   : sprintf(buffer,"implicit parameter \"?%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
#endif
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case QVAROP    :
	case QVARID    :
	case QCONOP    : 
	case QCONID    : sprintf(buffer,"symbol \"%s\"",
				 identToStr(yylval));
			 return buffer;
	case HIDING    : return "symbol \"hiding\"";
	case QUALIFIED : return "symbol \"qualified\"";
	case ASMOD     : return "symbol \"as\"";
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>'";
	default        : return "token";
    }
}

static Cell local checkPrec(p)		/* Check for valid precedence value*/
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
	ERRMSG(row) "Precedence value must be an integer in the range [%d..%d]",
		    MIN_PREC, MAX_PREC
	EEND;
    }
    return p;
}

static Cell local buildTuple(tup)	/* build tuple (x1,...,xn) from	   */
List tup; {				/* list [xn,...,x1]		   */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {				/*    .                    .	   */
	x      = fst(t);		/*   / \                  / \	   */
	fst(t) = snd(t);		/*  xn  .                .   xn	   */
	snd(t) = x;			/*       .    ===>      .	   */
	x      = t;			/*        .            .	   */
	t      = fun(x);		/*         .          .		   */
	n++;				/*        / \        / \	   */
    } while (nonNull(t));		/*       x1  NIL   (n)  x1	   */
    fst(x) = mkTuple(n);
    return tup;
}

static List local checkCtxt(con)	/* validate context		   */
Type con; {
    mapOver(checkPred, con);
    return con;
}

static Cell local checkPred(c)		/* check that type expr is a valid */
Cell c; {				/* constraint			   */
    Cell cn = getHead(c);
#if TREX
    if (isExt(cn) && argCount==1)
	return c;
#endif
#if IPARAM
    if (isIP(cn))
	return c;
#endif
    if (!isQCon(cn) /*|| argCount==0*/)
	syntaxError("class expression");
    return c;
}

static Pair local checkDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in do {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
    return dqs;
}

#if MUDO
static Pair local checkMDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to an (expr,quals) pair         */
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERRMSG(row) "Last generator in mdo {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
    return dqs;
}
#endif

static Cell local checkTyLhs(c)		/* check that lhs is of the form   */
Cell c; {				/* T a1 ... a			   */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL) {
	tlhs = fun(tlhs);
    }
    if (whatIs(tlhs)!=CONIDCELL) {
	ERRMSG(row) "Illegal left hand side in data type declaration"
	EEND;
    }
    return c;
}

static Cell local checkConstr(c)	/* check that data constructor has */
Cell c; {				/* an unqualified conid as head    */
    Cell chd = c;
    while (isAp(chd)) {
	chd = fun(chd);
    }
    if (whatIs(chd)==QUALIDENT) {
	ERRMSG(row) "Qualified constructor in data type declaration"
	EEND;
    }
    return c;
}

#if !TREX
static Void local noTREX(where)
String where; {
    ERRMSG(row) "Attempt to use TREX records while parsing %s.\n", where ETHEN
    ERRTEXT     "(TREX is disabled in this build of Hugs)"
    EEND;
}
#endif
#if !IPARAM
static Void local noIP(where)
String where; {
    ERRMSG(row) "Attempt to use Implicit Parameters while parsing %s.\n", where ETHEN
    ERRTEXT     "(Implicit Parameters are disabled in this build of Hugs)"
    EEND;
}
#endif

#if !MUDO
/***
   Due to the way we implement this stuff, this function will actually
   never be called. When MUDO is not defined, the lexer thinks that mdo
   is just another identifier, and hence the MDO token is never returned
   to the parser: consequently the mdo production is never reduced, making 
   this code unreachable. The alternative is to let the lexer to 
   recognize "mdo" all the time, but that's not Haskell compliant. In any 
   case we keep this function here, even if just for documentation purposes.
***/
static Void local noMDo(where)
String where; {
    ERRMSG(row) "Attempt to use MDO while parsing %s.\n", where ETHEN
    ERRTEXT     "(Recursive monadic bindings are disabled in this build of Hugs)"
    EEND;
}
#endif

/*-------------------------------------------------------------------------*/
