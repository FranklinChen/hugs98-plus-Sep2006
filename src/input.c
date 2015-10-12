/* --------------------------------------------------------------------------
 * Input functions, lexical analysis parsing etc...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: input.c,v $
 * $Revision: 1.91 $
 * $Date: 2006/09/09 09:37:48 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "command.h"
#include "errors.h"
#include "module.h"
#include "script.h"
#include "opts.h"
#include "goal.h"
#include "machdep.h"
#include "char.h"
#include <ctype.h>

#if HAVE_WINDOWS_H
#include <windows.h>
#undef IN
#endif

/* --------------------------------------------------------------------------
 * Global data:
 * ------------------------------------------------------------------------*/

List tyconDefns      = NIL;             /* type constructor definitions    */
List typeInDefns     = NIL;             /* type synonym restrictions       */
List valDefns        = NIL;             /* value definitions in script     */
List classDefns      = NIL;             /* class defns in script           */
List instDefns       = NIL;             /* instance defns in script        */
List selDefns	     = NIL;             /* list of selector lists          */
List genDefns	     = NIL;             /* list of generated names	   */
List primDefns       = NIL;             /* primitive definitions           */
List unqualImports   = NIL;             /* unqualified import list         */
Int  foreignCount    = 0;               /* count of foreigns in this module*/
List foreignImports  = NIL;             /* foreign import declarations     */
List foreignExports  = NIL;             /* foreign export declarations     */
List foreignLabels   = NIL;             /* foreign label  declarations     */
List defaultDefns    = NIL;             /* default definitions (if any)    */
Int  defaultLine     = 0;               /* line in which default defs occur*/
List evalDefaults    = NIL;             /* defaults for evaluator          */

Cell inputExpr       = NIL;             /* input expression                */
Cell inputContext    = NIL;             /* input context                   */
Bool literateScripts = FALSE;           /* TRUE => default to lit scripts  */

String repeatStr     = 0;               /* Repeat last expr                */

#if SUPPORT_PREPROCESSOR
String preprocessor  = 0;
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Bool local fileInput       Args((String,Long));
static Bool local literateMode    Args((String));
static Bool local linecmp         Args((String,ShortChar *));
static Int  local nextLine        Args((Void));
static Void local skip            Args((Void));
static Void local thisLineIs      Args((Int));
static Void local newlineSkip     Args((Void));
static Void local closeAnyInput   Args((Void));

       Int  yyparse         Args((Void)); /* can't stop yacc making this   */
					  /* public, but don't advertise   */
					  /* it in a header file.          */

static Void local endToken        Args((Void));
static Text local readOperator    Args((Void));
static Text local readIdent       Args((Void));
static Cell local readRadixNumber Args((Int));
static Cell local readNumber      Args((Void));
static Cell local readChar        Args((Void));
static Cell local readString      Args((Void));
static Void local saveStrChr      Args((Char));
static Cell local readAChar       Args((Bool));

static Bool local lazyReadMatches Args((String));
static Cell local readEscapeChar  Args((Bool,Bool));
static Void local skipGap         Args((Void));
static Cell local readCtrlChar    Args((Void));
static Cell local readOctChar     Args((Void));
static Cell local readHexChar     Args((Void));
static Int  local readHexDigit    Args((Char));
static Cell local readDecChar     Args((Void));

static Void local goOffside       Args((Int));
static Void local unOffside       Args((Void));
static Bool local canUnOffside    Args((Void));

static Void local skipWhitespace  Args((Void));
static Int  local yylex           Args((Void));
static Int  local repeatLast      Args((Void));

static Void local parseInput      Args((Int));

/* --------------------------------------------------------------------------
 * Text values for reserved words and special symbols:
 * ------------------------------------------------------------------------*/

static Text textCase,    textOfK,      textData,   textType,   textIf;
static Text textThen,    textElse,     textWhere,  textLet,    textIn;
static Text textInfix,   textInfixl,   textInfixr, textPrim,   textNewtype;
static Text textDefault, textDeriving, textDo,     textClass,  textInstance;
#if IPARAM
static Text textWith,  textDlet;
#endif

#if MUDO
static Text textMDo;
#endif

static Text textCoco,    textEq,       textUpto,   textAs,     textLambda;
static Text textBar,     textMinus,    textFrom,   textArrow,  textLazy;
static Text textBang,    textDot,      textAll,	   textImplies;

static Text textModule,  textImport;
static Text textHiding,  textQualified, textAsMod;
static Text textWildcard;
static Text textNeedPrims;
static Text textForeign;

Text   textCCall;                       /* ccall                           */
Text   textSafe;                        /* safe                            */
Text   textUnsafe;                      /* unsafe                          */
Text   textThreadsafe;                  /* threadsafe                      */
Text   textExport;                      /* export                          */

/* Platform-specific calling conventions */
#if STDCALL_SUPPORTED
Text   textStdcall;                     /* stdcall                         */
#endif
#ifdef DOTNET
Text   textDotnet;                      /* dotnet                          */
#endif

Text   textNum;                         /* Num                             */
Text   textPrelude;                     /* Prelude / Hugs.Prelude          */
Text   textUserPrelude;                 /* Prelude                         */
Text   textPlus;			/* (+)				   */

static Cell conMain;                    /* Main                            */
static Cell varMain;                    /* main                            */

static Cell varMinus;                   /* (-)                             */
static Cell varPlus;			/* (+)				   */
static Cell varBang;                    /* (!)                             */
static Cell varDot;			/* (.)				   */
static Cell varHiding;                  /* hiding                          */
static Cell varQualified;               /* qualified                       */
static Cell varAsMod;                   /* as                              */

static List imps;                       /* List of imports to be chased    */

#if HERE_DOC
Bool hereDocs = FALSE;
enum { START = 0,
       KEEP_GOING,
       BEGIN_VAR,
       ISSUE_QUOTE,
       HERE_VAR,
       END_VAR,
       CLOSE_PAREN };
static Int hereState = START;
#endif

#if MULTI_LINEFEED
#define FOPEN_MODE                "rb"
#else
#define FOPEN_MODE                "r"
#endif

/* --------------------------------------------------------------------------
 * Single character input routines:
 *
 * At the lowest level of input, characters are read one at a time, with the
 * current character held in c0 and the following (lookahead) character in
 * c1.  The corrdinates of c0 within the file are held in (column,row).
 * The input stream is advanced by one character using the skip() function.
 * ------------------------------------------------------------------------*/

#define TABSIZE    8                   /* spacing between tabstops         */

#define NOTHING    0                   /* what kind of input is being read?*/
#define KEYBOARD   1                   /* - keyboard/console?              */
#define SCRIPTFILE 2                   /* - script file                    */
#define STRING     3                   /* - string buffer?                 */
#define NOKEYBOARD 4                   /* - standard input, but not a tty  */


static Int    reading   = NOTHING;

static Target readSoFar;
static Int    row, column, startColumn;
static int    c0, c1;
static FILE   *inputStream = 0;
static Bool   thisLiterate;
static String nextStringChar;          /* next char in string buffer       */

#if     USE_READLINE                   /* for command line editors         */
static  String currentLine;            /* editline or GNU readline         */
static  String nextChar;
#define nextConsoleChar() (*nextChar=='\0' ? '\n' : ExtractChar(nextChar))
extern  Void add_history  Args((String));
extern  String readline   Args((String));
#else
#define nextConsoleChar() FGetChar(stdin)
#endif

static  Int litLines;                  /* count defn lines in lit script   */
#define DEFNCHAR  '>'                  /* definition lines begin with this */
static  Int lastLine;                  /* records type of last line read:  */
#define STARTLINE 0                    /* - at start of file, none read    */
#define BLANKLINE 1                    /* - blank (may preceed definition) */
#define TEXTLINE  2                    /* - text comment                   */
#define DEFNLINE  3                    /* - line containing definition     */
#define CODELINE  4                    /* - line inside code block         */

#define BEGINCODE "\\begin{code}"
#define ENDCODE   "\\end{code}"

Bool startsQual(c)
Char c; {
    return isIn(c,LARGE);
}

#define LINEBUFFER_SIZE 1000
static ShortChar lineBuffer[LINEBUFFER_SIZE];
static int lineLength = 0;
static int inCodeBlock = FALSE; /* Inside \begin{code}..\end{code} */
static int linePtr = 0;

Void consoleInput(prompt)              /* prepare to input characters from */
String prompt; {                       /* standard in (i.e. console/kbd)   */
    reading     = KEYBOARD;            /* keyboard input is Line oriented, */
    c0          =                      /* i.e. input terminated by '\n'    */
    c1          = ' ';
    column      = (-1);
    row         = 0;
#if HERE_DOC
    hereState   = START;
#endif

#if HAVE_ISATTY && USE_READLINE
    if (!isatty(fileno(stdin))) { /* not reading from a tty: */
	reading = NOKEYBOARD;     /* don't try readline      */
	Printf("%s",prompt);FlushStdout();
	return;
    }
#endif
#if USE_READLINE
    /* Paranoid freeing code supplied by Sverker Nilsson (sverker@opq.se) 
     * avoids accidentally freeing currentLine twice. 
     */
    if (currentLine) {
	String oldCurrentLine = currentLine;
	currentLine = 0;           /* We may lose the space of currentLine */
	free(oldCurrentLine);      /* if interrupted here - unlikely       */
    }
    currentLine = readline(prompt);
    nextChar    = currentLine;
    if (currentLine) {
	if (*currentLine)
	    add_history(currentLine);
    }
    else
	c0 = c1 = EOF;
#else
#if HUGS_FOR_WINDOWS
    {
    INT svColor = SetForeColor(GREEN);
#endif
    Printf("%s",prompt);
    FlushStdout();
#if HUGS_FOR_WINDOWS
    SetForeColor(svColor);
    }
#endif
#endif
}

#if HUGS_FOR_WINDOWS
/* These variables and functions are used to save the current */
/* state of the input, to implement auto load of files        */
static Int    	saveReading;
static int    	savec0, savec1;

Void saveInputState(Void)
{
    saveReading = reading;
    savec0 = c0;
    savec1 = c1;
}

Void restoreInputState(Void)
{
    reading = saveReading;
    c0 = savec0;
    c1 = savec1;
}
#endif

static Bool local fileInput(nm,len)     /* prepare to input characters from*/
String nm;                              /* named file (specified length is */
Long   len; {                           /* used to set target for reading) */
#if SUPPORT_PREPROCESSOR
    if (!readable(nm,FALSE)) { /* file not there */
	inputStream = NULL;
    } else if (preprocessor) {
	Int reallen = strlen(preprocessor) + 1 + strlen(nm) + 1;
	char *cmd = malloc(reallen+1);
	if (cmd == NULL) {
	    ERRMSG(0) "Unable to allocate memory for filter command."
	    EEND_NORET;
	    return FALSE;
	}
	if (snprintf(cmd,reallen, "%s %s", preprocessor, nm) < 0) {
	    ERRMSG(0) "Unable to allocate memory for filter command."
	    EEND_NORET;
	    return FALSE;
	} else {
	    cmd[reallen] = '\0';
	}
	inputStream = popen(cmd,"r");
	free(cmd);
    } else {
	inputStream = fopen(nm,FOPEN_MODE);
    }
#else
    if (nm[0] == '\0')
	inputStream = NULL;
    else
	inputStream = fopen(nm,FOPEN_MODE);
#endif
    if (inputStream) {
	reading      = SCRIPTFILE;
	c0           = ' ';
	c1           = '\n';
	column       = 1;
	row          = 0;

	lastLine     = STARTLINE;       /* literate file processing */
	litLines     = 0;
	linePtr      = 0;
	lineLength   = 0;
	thisLiterate = literateMode(nm);
	inCodeBlock  = FALSE;

	readSoFar    = 0;
	setGoal("Parsing", (Target)len);
    }
    else {
	ERRMSG(0) "Unable to open file \"%s\"", nm
	EEND_NORET;
	return FALSE;
    }
    return TRUE;
}

Void stringInput(s)             /* prepare to input characters from string */
String s; {                
    reading      = STRING;            
    c0           = EOF;
    c1           = EOF;
    if (*s) c0 = *s++;
    if (*s) c1 = *s++;
    column       = 1;
    row          = 1;

    nextStringChar = s;
    if (!charTabBuilt)
	initCharTab();
}

static Bool local literateMode(nm)      /* Select literate mode for file   */
String nm; {
    char *dot = strrchr(nm,'.');        /* look for last dot in file name  */
    if (dot) {
	if (filenamecmp(dot+1,"hs")==0) /* .hs files are never literate    */
	    return FALSE;
	if (filenamecmp(dot+1,"lhs") ==0 || /* .lhs, .verb files are always*/
	    filenamecmp(dot+1,"verb")==0) /* literate scripts              */
	    return TRUE;
    }
    return literateScripts;             /* otherwise, use the default      */
}


/* This code originally came from Sigbjorn Finne (sof@dcs.gla.ac.uk).
 * I've removed the loop (since newLineSkip contains a loop too) and
 * replaced the warnings with errors. ADR
 */
/*
 * To deal with literate \begin{code}...\end{code} blocks,
 * add a line buffer that rooms the current line. The old c0 and c1  
 * stream pointers are used as before within that buffer -- sof
 *
 * Upon reading a new line into the line buffer, we check to see if
 * we're reading in a line containing \begin{code} or \end{code} and
 * take appropriate action. 
 */

static Bool local linecmp(s,line)       /* compare string with line        */
String s;                               /* line may end in whitespace      */
ShortChar *line; {
    Int i=0;
    while (s[i] != '\0' && s[i] == line[i]) {
	++i;
    }
    /* s[0..i-1] == line[0..i-1] */
    if (s[i] != '\0') {                 /* check s `isPrefixOf` line       */
	return FALSE;
    }
    while (isIn(line[i], SPACE)) {      /* allow whitespace at end of line */
	++i;
    }
    return (line[i] == '\0');
}

/* Returns line length (including \n) or 0 upon EOF. */
static Int local nextLine()
{
    int ch;

    for (lineLength = 0; lineLength < LINEBUFFER_SIZE-1; lineLength++) {
        lineBuffer[lineLength] = (ch = FGetChar(inputStream));
        if (ch == EOF)
            break;
#if MULTI_LINEFEED
        if ((char)ch == '\r') {
            ch = fgetc(inputStream);
	    /* ToDo: verify that this behaves correctly re EOF */
            if ((char)ch != '\n') 
                ungetc(ch, inputStream);
            lineBuffer[lineLength] = '\n';
            lineLength++;
            break;
        } else 
#endif
        if ((char)ch == '\n') {
            lineLength++;
            break;
        }
    }
    lineBuffer[lineLength] = '\0';

    if (lineLength <= 0) { /* EOF / IO error, who knows.. */
	return lineLength;
    }
    else if (lineLength >= 2 && lineBuffer[0] == '#' && lineBuffer[1] == '!') {
	lineBuffer[0]='\n'; /* pretend it's a blank line */
	lineBuffer[1]='\0';
	lineLength=1;
    } else if (thisLiterate) {
	if (linecmp(BEGINCODE, lineBuffer)) {
	    if (!inCodeBlock) {             /* Entered a code block        */
		inCodeBlock = TRUE;
		lineBuffer[0]='\n'; /* pretend it's a blank line */
		lineBuffer[1]='\0';
		lineLength=1;
	    }
	    else {
		ERRMSG(row) "\\begin{code} encountered inside code block"
		EEND;
	    }
	}
	else if (linecmp(ENDCODE, lineBuffer)) {
	    if (inCodeBlock) {              /* Finished code block         */
		inCodeBlock = FALSE;
		lineBuffer[0]='\n'; /* pretend it's a blank line */
		lineBuffer[1]='\0';
		lineLength=1;
	    }
	    else {
		ERRMSG(row) "\\end{code} encountered outside code block"
		EEND;
	    }
	}
    }
    return lineLength;
}
    
static Void local skip() {              /* move forward one char in input  */
    if (c0!=EOF) {                      /* stream, updating c0, c1, ...    */
	if (c0=='\n') {                 /* Adjusting cursor coords as nec. */
	    row++;
	    column=1;
	    if (reading==SCRIPTFILE)
		soFar(readSoFar);
	}
	else if (c0=='\t')
	    column += TABSIZE - ((column-1)%TABSIZE);
	else
	    column++;

	c0 = c1;
	readSoFar++;

	if (c0==EOF) {
	    column = 0;
	    if (reading==SCRIPTFILE)
		done();
	    closeAnyInput();
	}
	else if (reading==KEYBOARD) {
	    allowBreak();
	    if (c0=='\n')
		c1 = EOF;
	    else {
		c1 = nextConsoleChar();
#if HAVE_WINDOWS_H && !HUGS_FOR_WINDOWS
		Sleep(0);
#endif
		/* On Win32, hitting ctrl-C causes the next getchar to
		 * fail - returning "-1" to indicate an error.
		 * This is one of the rare cases where "-1" does not mean EOF.
		 */
		if (EOF == c1 && (!feof(stdin) || broken==TRUE)) {
		    c1 = ' ';
		}
	    }
	} else if (reading==NOKEYBOARD) {
	    c1 = c0=='\n' ? EOF : FGetChar(stdin);
	} else if (reading==STRING) {
	    c1 = ExtractChar(nextStringChar);
	    if (c1 == '\0')
		c1 = EOF;
	}
	else {
	    if (lineLength <=0 || linePtr == lineLength) {
		/* Current line, exhausted - get new one */
		if (nextLine() <= 0) { /* EOF */
		    c1 = EOF;
		}
		else {
		    linePtr = 0;
		    c1 = lineBuffer[linePtr++];
		}
	    }
	    else {
		c1 = lineBuffer[linePtr++];
	    }
	}

    }
}

static Void local thisLineIs(kind)     /* register kind of current line    */
Int kind; {                            /* & check for literate script errs */
    if ((kind==DEFNLINE && lastLine==TEXTLINE) ||
	(kind==TEXTLINE && lastLine==DEFNLINE)) {
	ERRMSG(row) "Program line next to comment"
	EEND;
    }
    lastLine = kind;
}

static Void local newlineSkip() {      /* skip `\n' (supports lit scripts) */
    /* assert(c0=='\n'); */
    if (reading==SCRIPTFILE && thisLiterate) {
	do {
	    skip();
	    if (inCodeBlock) {         /* pass chars on definition lines   */
		thisLineIs(CODELINE);  /* to lexer (w/o leading DEFNCHAR)  */
		litLines++;
		return;
	    }
	    if (c0==DEFNCHAR) {        /* pass chars on definition lines   */
		thisLineIs(DEFNLINE);  /* to lexer (w/o leading DEFNCHAR)  */
		skip();
		litLines++;
		return;
	    }
	    while (c0!='\n' && isIn(c0,SPACE)) /* maybe line is blank?   */
		skip();
	    if (c0=='\n' || c0==EOF)
		thisLineIs(BLANKLINE);
	    else {
		thisLineIs(TEXTLINE);  /* otherwise it must be a comment   */
		while (c0!='\n' && c0!=EOF)
		    skip();
	    }                          /* by now, c0=='\n' or c0==EOF      */
	} while (c0!=EOF);             /* if new line, start again         */

	if (litLines==0) {
	    ERRMSG(row) "Empty script - perhaps you forgot the `%c's?",
			DEFNCHAR
	    EEND;
	}
	return;
    }
    skip();
}

static Void local closeAnyInput() {    /* Close input stream, if open,     */
    switch (reading) {                 /* or skip to end of console line   */
	case SCRIPTFILE : if (inputStream) {
#if SUPPORT_PREPROCESSOR
			      if (preprocessor) {
				  pclose(inputStream);
			      } else {
				  fclose(inputStream);
			      }
#else
			      fclose(inputStream);
#endif
			      inputStream = 0;
			  }
			  break;
	case KEYBOARD   : while (c0!=EOF)
			      skip();
			  break;
    }
    reading=NOTHING;
}

/* --------------------------------------------------------------------------
 * Parser: Uses table driven parser generated from parser.y using yacc
 * ------------------------------------------------------------------------*/

# if __MWERKS__ && macintosh
#include "parser.tab.c"
#else
#include "parser.c"
#endif

/* --------------------------------------------------------------------------
 * Single token input routines:
 *
 * The following routines read the values of particular kinds of token given
 * that the first character of the token has already been located in c0 on
 * entry to the routine.
 * ------------------------------------------------------------------------*/

#define MAX_TOKEN           4000
#define startToken()        tokPtr = tokenStr
#define saveTokenChar(c)    if (tokPtr<=tokenStr+MAX_TOKEN-MAX_CHAR_ENCODING) saveChar(c); else ++tokPtr
#define saveChar(c)         AddChar((c), tokPtr)
#define overflows(n,b,d,m)  (n > ((m)-(d))/(b))

static char tokenStr[MAX_TOKEN+1];     /* token buffer                     */
static String tokPtr;                  /* input position in buffer         */
static Int  identType;                 /* identifier type: CONID / VARID   */
static Int  opType;                    /* operator type  : CONOP / VAROP   */
									   
static Void local endToken() {         /* check for token overflow         */
    if (tokPtr>tokenStr+MAX_TOKEN) {
	ERRMSG(row) "Maximum token length (%d) exceeded", MAX_TOKEN	   
	EEND;								   
    }									   
    *tokPtr = '\0';						   
}									   
									   
static Text local readOperator() {     /* read operator symbol             */
    startToken();
    do {
	saveTokenChar(c0);
	skip();
    } while (isLatin1(c0) && isIn(c0,SYMBOL));
    opType = (tokenStr[0]==':' ? CONOP : VAROP);
    endToken();
    return findText(tokenStr);
}

static Text local readIdent() {        /* read identifier                  */
    startToken();
    do {
	saveTokenChar(c0);
	skip();
    } while (isLatin1(c0) && isIn(c0,IDAFTER));
    endToken();
    identType = isIn(tokenStr[0],LARGE) ? CONID : VARID;
    return findText(tokenStr);
}

static Cell local readRadixNumber(r)   /* Read literal in specified radix  */
Int r; {                               /* from input of the form 0c{digs}  */
    Int d;			       					   
    skip();                            /* skip leading zero                */
    if ((d=readHexDigit(c1))<0 || d>=r)/* Special case; no digits, lex as  */
	return mkInt(0);               /* if it had been written "0 c..."  */
    else {
	Int  n = 0;
#if BIGNUMS
	Cell big = NIL;
#endif
	skip();
	do {
#if BIGNUMS
	    if (nonNull(big))
		big = bigShift(big,d,r);
	    else if (overflows(n,r,d,MAXPOSINT))
		big = bigShift(bigInt(n),d,r);
	    else
#else
	    if (overflows(n,r,d,MAXPOSINT)) {
		ERRMSG(row) "Integer literal out of range"
		EEND;
	    }
	    else
#endif
		n = r*n + d;
	    skip();
	    d = readHexDigit(c0);
	} while (d>=0 && d<r);
#if BIGNUMS
	return nonNull(big) ? big : mkInt(n);
#else
	return mkInt(n);
#endif
    }
}

static Cell local readNumber() {        /* read numeric constant           */
    Int   n           = 0;
    Bool  intTooLarge = FALSE;
    Bool  floatingPt  = FALSE;		/* floating point literal?         */

    if (c0=='0') {
	if (c1=='x' || c1=='X')         /* Maybe a hexadecimal literal?    */
	    return readRadixNumber(16);
	if (c1=='o' || c1=='O')         /* Maybe an octal literal?         */
	    return readRadixNumber(8);
    }

    startToken();
    do {
	if (overflows(n,10,(c0-'0'),MAXPOSINT))
	    intTooLarge = TRUE;
	n  = 10*n  + (c0-'0');
	saveTokenChar(c0);
	skip();
    } while (isLatin1(c0) && isIn(c0,DIGIT));

    if (c0=='.' && isLatin1(c1) && isIn(c1,DIGIT)) {	/* decimal part */
	floatingPt = TRUE;
	saveTokenChar(c0);                  /* save decimal point          */
	skip();
	do {                                /* process fractional part ... */
	    saveTokenChar(c0);
	    skip();
	} while (isLatin1(c0) && isIn(c0,DIGIT));
    }

    /* Look for exponent part.  BUG: since we don't use 2 characters of
     * lookahead, we mis-scan things like 9e+a and 9.0e+a.
     */
    if ((c0=='e' || c0=='E') &&
	isLatin1(c1) && (isIn(c1,DIGIT) || c1=='-' || c1=='+')) {
	floatingPt = TRUE;
	saveTokenChar('e');
	skip();
	if (c0=='-') {
	    saveTokenChar('-');
	    skip();
	}
	else if (c0=='+')
	    skip();

	if (!isLatin1(c0) || !isIn(c0,DIGIT)) {
	    ERRMSG(row) "Missing digits in exponent"
	    EEND;
	}
	else {
	    do {
		saveTokenChar(c0);
		skip();
	    } while (isLatin1(c0) && isIn(c0,DIGIT));
	}
    }

    endToken();

    if (floatingPt) {
#if FLOATS_SUPPORTED
	return mkDouble(stringToDouble(tokenStr));
#else
	ERRMSG(row) "No floating point numbers in this implementation"
	EEND;
	return NIL;
#endif
    } else if (intTooLarge) {
#if BIGNUMS
	return bigStr(tokenStr);
#else
	ERRMSG(row) "Integer literal out of range"
	EEND;
	return NIL;
#endif
    } else
	return mkInt(n);
}

static Cell local readChar() {         /* read character constant          */
    Cell charRead;

    skip(/* '\'' */);
    if (c0=='\'' || c0=='\n' || c0==EOF) {
	ERRMSG(row) "Illegal character constant"
	EEND;
    }

    charRead = readAChar(FALSE);

    if (c0=='\'')
	skip(/* '\'' */);
    else {
	ERRMSG(row) "Improperly terminated character constant"
	EEND;
    }
    return charRead;
}

static Cell local readString() {       /* read string literal              */
    Cell c;

    startToken();
    skip(/* '\"' */);
    while (c0!='\"' && c0!='\n' && c0!=EOF) {
	c = readAChar(TRUE);
	if (nonNull(c))
	    saveStrChr(charOf(c));
    }

    if (c0=='\"')
	skip(/* '\"' */);
    else {
	ERRMSG(row) "Improperly terminated string"
	EEND;
    }
    endToken();
    return mkStr(findText(tokenStr));
}

#if HERE_DOC
static Void local readHereString() {       /* read fragment of here doc    */
    startToken();
    while ((c0!='\'' || c1!='\'') && c0!=EOF) {
	if (c0 == '$') {
	    skip();
	    switch (c0) {
	    case '$' :
		break;
	    case '(' :
		skip();
		hereState = BEGIN_VAR;
		goto urgh;
	    default :
		ERRMSG(row) "Singleton $ in here document"
		EEND;
	    }
	}
	saveStrChr(c0);
	skip();
    }
    if (c0==EOF) {
	ERRMSG(row) "Improperly terminated here document"
	EEND;
    }
    /* eat the closing ticks */
    skip(); skip();
    hereState = CLOSE_PAREN;

  urgh:
    endToken();
    push(yylval = mkStr(findText(tokenStr)));
}

static Void local hereJoin() {
    Text plusplus = findText("++");
    push(yylval = ap(VAROPCELL,plusplus));
}

static Void local hereQuote() {
    Text quote = findText("quote");
    push(yylval = ap(VARIDCELL,quote));
}
#endif

static Void local saveStrChr(c)        /* save character in string         */
Char c; {
#if UNICODE_CHARS && !CHAR_ENCODING_UTF8
    if (!charIsRepresentable(c)) {
	/* Kludge piled on kludge (using the external encoding for strings):
	 * Such Chars are coded as \\ followed by a sequence of 7-bit
	 * chunks, least significant first, with all but the last having
	 * their top bit set.
	 */
	if (tokPtr<tokenStr+MAX_TOKEN-6) {
	    *tokPtr++ = '\\';
	    while (c > 0x7f) {
		*tokPtr++ = 0x80 | (c&0x7f);
		c >>= 7;
	    }
	    *tokPtr++ = c;
	}
    } else
#endif
    if (c!='\0' && c!='\\') {          /* save non null char as single char*/
	saveTokenChar(c);
    }
    else {                             /* save null char as TWO null chars */
	if (tokPtr<tokenStr+MAX_TOKEN-1) {
	    saveChar('\\');
	    if (c=='\\')
		saveChar('\\');
	    else
		saveChar('0');
	}
    }
}

/* Extract a Char from a string built with saveStrChr(), advancing the ptr */
Char getStrChr(String *sp) {
    Char c = ExtractChar(*sp);
    if (c=='\\') {
#if UNICODE_CHARS && !CHAR_ENCODING_UTF8
	Int b;
	b = *(*sp)++;
	if ((b&0x80)==0)
       	    c = b;
	else {
	    Int shift = 0;
	    c = b&0x7f;
	    while ((b&0x80)!=0) {
		c |= (b&0x7f)<<shift;
		b = *(*sp)++;
		shift += 7;
	    }
	    c |= b<<shift;
	}
#else
	c = ExtractChar(*sp);
#endif
	if (c=='0')
	    c = '\0';
    }
    return c;
}

static Cell local readAChar(isStrLit)  /* read single char constant        */
Bool isStrLit; {                       /* TRUE => enable \& and gaps       */
    Cell c = mkChar(c0);

    if (c0=='\\') {                    /* escape character?                */
	return readEscapeChar(isStrLit,TRUE);
    }
#if !UNICODE_CHARS
    if (!isLatin1(c0)) {
	ERRMSG(row) "Non Latin-1 character `\\%d' in constant", ((int)c0)
	EEND;
    }
#endif
    skip();                            /* normal character?                */
    return c;
}

/* --------------------------------------------------------------------------
 * Character escape code sequences:
 * ------------------------------------------------------------------------*/

static struct {                        /* table of special escape codes    */
    char *codename;
    int  codenumber;
} escapes[] = {
   {"a",    7}, {"b",    8}, {"f",   12}, {"n",   10},  /* common escapes  */
   {"r",   13}, {"t",    9}, {"\\",'\\'}, {"\"",'\"'},
   {"\'",'\''}, {"v",   11},
   {"NUL",  0}, {"SOH",  1}, {"STX",  2}, {"ETX",  3},  /* ascii codenames */
   {"EOT",  4}, {"ENQ",  5}, {"ACK",  6}, {"BEL",  7},
   {"BS",   8}, {"HT",   9}, {"LF",  10}, {"VT",  11},
   {"FF",  12}, {"CR",  13}, {"SO",  14}, {"SI",  15},
   {"DLE", 16}, {"DC1", 17}, {"DC2", 18}, {"DC3", 19},
   {"DC4", 20}, {"NAK", 21}, {"SYN", 22}, {"ETB", 23},
   {"CAN", 24}, {"EM",  25}, {"SUB", 26}, {"ESC", 27},
   {"FS",  28}, {"GS",  29}, {"RS",  30}, {"US",  31},
   {"SP",  32}, {"DEL", 127},
   {0,0}
};

static Int  alreadyMatched;            /* Record portion of input stream   */
static char alreadyRead[10];           /* that has been read w/o a match   */

static Bool local lazyReadMatches(s)   /* compare input stream with string */
String s; {                            /* possibly using characters that   */
    int i;                             /* have already been read           */

    for (i=0; i<alreadyMatched; ++i)
	if (alreadyRead[i]!=s[i])
	    return FALSE;

    while (s[i] && s[i]==c0) {
	alreadyRead[alreadyMatched++]=(char)c0;
	skip();
	i++;
    }

    return s[i]=='\0';
}

static Cell local readEscapeChar(isStrLit,skipEsc)/* read escape character         */
Bool isStrLit;
Bool skipEsc; {
    int i=0;

    if (skipEsc) skip(/* '\\' */);
    switch (c0) {
	case '&'  : if (isStrLit) {
			skip();
			return NIL;
		    }
		    ERRMSG(row) "Illegal use of `\\&' in character constant"
		    EEND;
		    break;/*NOTREACHED*/

	case '^'  : return readCtrlChar();

	case 'o'  : return readOctChar();
	case 'x'  : return readHexChar();

	default   : if (!isLatin1(c0)) {
			ERRMSG(row) "Illegal escape sequence"
			EEND;
		    }
		    else if (isIn(c0,SPACE)) {
			if (isStrLit) {
			    skipGap();
			    return NIL;
			}
			ERRMSG(row) "Illegal use of gap in character constant"
			EEND;
			break;
		    }
		    else if (isIn(c0,DIGIT))
			return readDecChar();
    }

    for (alreadyMatched=0; escapes[i].codename; i++)
	if (lazyReadMatches(escapes[i].codename))
	    return mkChar(escapes[i].codenumber);

    alreadyRead[alreadyMatched++] = (char)c0;
    alreadyRead[alreadyMatched++] = '\0';
    ERRMSG(row) "Illegal character escape sequence \"\\%s\"",
                 alreadyRead
    EEND;
    return NIL;/*NOTREACHED*/
}

static Void local skipGap() {          /* skip over gap in string literal  */
    do                                 /* (simplified in Haskell 1.1)      */
	if (c0=='\n')
	    newlineSkip();
	else
	    skip();
    while (isLatin1(c0) && isIn(c0,SPACE));
    if (c0!='\\') {
	ERRMSG(row) "Missing `\\' terminating string literal gap"
	EEND;
    }
    skip(/* '\\' */);
}

static Cell local readCtrlChar() {     /* read escape sequence \^x         */
    static String controls = "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";
    String which;

    skip(/* '^' */);
    if ((which = strchr(controls,c0))==NULL) {
	ERRMSG(row) "Unrecognised escape sequence `\\^%c'", c0
	EEND;
    }
    skip();
    return mkChar(which-controls);
}

static Cell local readOctChar() {      /* read octal character constant    */
    Int n = 0;
    Int d;

    skip(/* 'o' */);
    if ((d = readHexDigit(c0))<0 || d>=8) {
	ERRMSG(row) "Empty octal character escape"
	EEND;
    }
    do {
	if (overflows(n,8,d,MAXCHARVAL)) {
	    ERRMSG(row) "Octal character escape out of range"
	    EEND;
	}
	n = 8*n + d;
	skip();
    } while ((d = readHexDigit(c0))>=0 && d<8);

    return mkChar(n);
}

static Cell local readHexChar() {      /* read hex character constant      */
    Int n = 0;
    Int d;

    skip(/* 'x' */);
    if ((d = readHexDigit(c0))<0) {
	ERRMSG(row) "Empty hexadecimal character escape"
	EEND;
    }
    do {
	if (overflows(n,16,d,MAXCHARVAL)) {
	    ERRMSG(row) "Hexadecimal character escape out of range"
	    EEND;
	}
	n = 16*n + d;
	skip();
    } while ((d = readHexDigit(c0))>=0);

    return mkChar(n);
}

static Int local readHexDigit(c)       /* read single hex digit            */
Char c; {
    if ('0'<=c && c<='9')
	return c-'0';
    if ('A'<=c && c<='F')
	return 10 + (c-'A');
    if ('a'<=c && c<='f')
	return 10 + (c-'a');
    return -1;
}

static Cell local readDecChar() {      /* read decimal character constant  */
    Int n = 0;

    do {
	if (overflows(n,10,(c0-'0'),MAXCHARVAL)) {
	    ERRMSG(row) "Decimal character escape out of range"
	    EEND;
	}
	n = 10*n + (c0-'0');
	skip();
    } while (c0!=EOF && isIn(c0,DIGIT));

    return mkChar(n);
}

/* --------------------------------------------------------------------------
 * Produce printable representation of character:
 * ------------------------------------------------------------------------*/

String unlexChar(c,quote)              /* return string representation of  */
Char c;                                /* character...                     */
Char quote; {                          /* protect quote character          */
    static char buffer[12];	       					   

    assert(c >= 0);
    if (isascii(c) && isIn(c,PRINT)) { /* normal printable character       */
	if (c==quote || c=='\\') {     /* look for quote of approp. kind   */
	    buffer[0] = '\\';           
	    buffer[1] = (char)c;
	    buffer[2] = '\0';
	}
	else {
	    buffer[0] = (char)c;
	    buffer[1] = '\0';
	}
    }
    else {                             /* look for escape code             */
	Int escs;
	for (escs=0; escapes[escs].codename; escs++)
	    if (escapes[escs].codenumber==c) {
		sprintf(buffer,"\\%s",escapes[escs].codename);
		return buffer;
	    }
	sprintf(buffer,"\\%d",c);      /* otherwise use numeric escape     */
    }
    return buffer;
}

Void printString(s)                    /* print string s, using quotes and */
String s; {                            /* escapes if any parts need them   */
    if (s) {			       
	String t = s;		       
	Char   c;		       
	while ((c = *t)!=0 && isLatin1(c)
			   && isIn(c,PRINT) && c!='"' && !isIn(c,SPACE)) {
	    t++;		       
	}
	if (*t) {		       
	    Putchar('"');	       
	    for (t=s; *t; )
		Printf("%s",unlexChar(ExtractChar(t),'"'));
	    Putchar('"');	       
	}			       
	else			       
	    Printf("%s",s);	       
    }				       
}				       
				       
/* -------------------------------------------------------------------------
 * Handle special types of input for use in interpreter:
 * -----------------------------------------------------------------------*/
				       
Command readCommand(cmds,start,sys)    /* read command at start of input   */
struct cmd *cmds;                      /* line in interpreter              */
Char   start;                          /* characters introducing a cmd     */
Char   sys; {                          /* character for shell escape       */
    while (c0==' ' || c0 =='\t')      					   
	skip();			      					   
									   
    if (c0=='\n')                      /* look for blank command lines     */
	return NOCMD;		      					   
    if (c0==EOF)                       /* look for end of input stream     */
#if HUGS_FOR_WINDOWS
	return NOCMD;
#else
	return QUIT;
#endif
    if (c0==sys) {                     /* single character system escape   */
	skip();			      					   
	return SYSTEM;		      					   
    }				      					   
    if (c0==start && c1==sys) {        /* two character system escape      */
	skip();
	skip();
	return SYSTEM;
    }

    startToken();                      /* All cmds start with start        */
#if OBSERVATIONS
    if (c0==start || (start==0 && c0!=EOF))
                                       /* except cmds without start char   */
#else
    if (c0==start)                     /* except default (usually EVAL)    */
#endif
	do {                           /* which is empty                   */
	    saveTokenChar(c0);
	    skip();
	} while (c0!=EOF && !isIn(c0,SPACE));
    endToken();

    for (; cmds->cmdString; ++cmds)
	if (strcmp((cmds->cmdString),tokenStr)==0 ||
	    (tokenStr[0]==start &&
	     tolower(tokenStr[1])==(cmds->cmdString)[1] &&
	     tokenStr[2]=='\0'))
	    return (cmds->cmdCode);
    return BADCMD;
}

String readFilename() {                /* Read filename from input (if any)*/
    while (c0==' ' || c0=='\t')
	skip();

    if (c0=='\n' || c0==EOF)           /* return null string at end of line*/
	return 0;

    startToken();
    while (c0!=EOF && !isIn(c0,SPACE)) {
	if (c0=='"') {
	    skip();
	    while (c0!=EOF && c0!='\"') {
		Cell c;
		/* Permit and treat an escaped space as legal character
		   in a filename. Some environments include these when
		   pasting filenames (MacOS X one, according to reports).
		   
		   Also, we no longer support the full array of escape chars
		   in filename / option strings; apart from '\ ', only '\"'
		   and '\\' are recognised. This is done to have lone backslashes
		   (as is common in filenames on certain platforms) be interpreted
		   as just that. As was, such backslashes would either cause
		   the interpreter to fall over (and fail to start up) or
		   be interpreted as some (unintended) escaped character.
		*/
		if (c0 == '\\') {
		  skip();
		  if (c0 == '"' || c0 == ' ' || c0 == '\\') {
		    saveTokenChar(c0);
		    skip();
		    continue;
		  } else {
		    saveTokenChar('\\');
		    continue;
		  }
		} else {
		  c = readAChar(TRUE);
		}
		if (nonNull(c)) {
		    saveTokenChar(charOf(c));
		}
	    }
	    if (c0=='"')
		skip();
	    else {
		ERRMSG(row) "a closing quote, '\"', was expected"
		EEND;
	    }
	}
	else {
  	    Int savedChar = c0;
	    skip();
	    /* Handle escaped spaces - see above comment. */
	    if (savedChar == '\\' && c0 == ' ') {
	      saveTokenChar(' ');
	      skip();
	    } else {
	      saveTokenChar(savedChar);
	    }
	}
    }
    endToken();
    return tokenStr;
}

String readLine() {                    /* Read command line from input     */
    while (c0==' ' || c0=='\t')        /* skip leading whitespace          */
	skip();

    startToken();
    while (c0!='\n' && c0!=EOF) {
	saveTokenChar(c0);
	skip();
    }
    endToken();

    return tokenStr;
}

/* --------------------------------------------------------------------------
 * This lexer supports the Haskell layout rule:
 *
 * - Layout area bounded by { ... }, with `;'s in between.
 * - A `{' is a HARD indentation and can only be matched by a corresponding
 *   HARD '}'
 * - Otherwise, if no `{' follows the keywords WHERE/LET or OF, a SOFT `{'
 *   is inserted with the column number of the first token after the
 *   WHERE/LET/OF keyword.
 * - When a soft indentation is uppermost on the indentation stack with
 *   column col' we insert:
 *    `}'  in front of token with column<col' and pop indentation off stack,
 *    `;'  in front of token with column==col'.
 * ------------------------------------------------------------------------*/

#define MAXINDENT  100                 /* maximum nesting of layout rule   */
static  Int        layout[MAXINDENT+1];/* indentation stack                */
#define HARD       (-1)                /* indicates hard indentation       */
static  Int        indentDepth = (-1); /* current indentation nesting      */

static Void local goOffside(col)       /* insert offside marker            */
Int col; {                             /* for specified column             */
    if (indentDepth>=MAXINDENT) {
	ERRMSG(row) "Too many levels of program nesting"
	EEND;
    }
    layout[++indentDepth] = col;
}

static Void local unOffside() {        /* leave layout rule area           */
    indentDepth--;
}

static Bool local canUnOffside() {     /* Decide if unoffside permitted    */
    return indentDepth>=0 && layout[indentDepth]!=HARD;
}

/* --------------------------------------------------------------------------
 * Main tokeniser:
 * ------------------------------------------------------------------------*/

static Void local skipWhitespace() {   /* Skip over whitespace/comments    */
    for (;;)                           /* Strictly speaking, this code is  */
	if (c0==EOF)                   /* a little more liberal than the   */
	    return;                    /* report allows ...                */
	else if (c0=='\n')	       					   
	    newlineSkip();	       					   
	else if (isIn(c0,SPACE))       					   
	    skip();		       					   
	else if (c0=='{' && c1=='-') { /* (potentially) nested comment     */
	    Int nesting = 1;	       					   
	    Int origRow = row;         /* Save original row number         */
	    skip();
	    skip();
	    while (nesting>0 && c0!=EOF)
		if (c0=='{' && c1=='-') {
		    skip();
		    skip();
		    nesting++;
		}
		else if (c0=='-' && c1=='}') {
		    skip();
		    skip();
		    nesting--;
		}
		else if (c0=='\n')
		    newlineSkip();
		else
		    skip();
	    if (nesting>0) {
		ERRMSG(origRow) "Unterminated nested comment {- ..."
		EEND;
	    }
	}
	else if (c0=='-' && c1=='-') {  /* One line comment                */
	    do
		skip();
	    while (c0!='\n' && c0!=EOF);
	    if (c0=='\n')
		newlineSkip();
	}
	else
	    return;
}

static Bool allDashes(char* s) {
  char* ptr = s;
  while ( *ptr != '\0' ) {
    if (*ptr != '-') return FALSE;
    ptr++;
  }
  return TRUE;
}

static Bool foundDashedOp = FALSE;

/*
 * Haskell98 makes it harder to detect one-line comment markup,
 * "--" is now only the start of a one-line comment if it isn't
 * followed by symbol chars other than "-". i.e., need to tokenise
 * anything that starts with "--" as an operator and check whether
 * the lexeme consists of all dashes or not.
 */
static Void local skipWhitespaceTok() { /* Skip over whitespace/comments    */
    for (;;)                            /* Strictly speaking, this code is  */
	if (c0==EOF)                    /* a little more liberal than the   */
	    return;                     /* report allows ...                */
	else if (c0=='\n')	       					   
	    newlineSkip();	       					   
	else if (isIn(c0,SPACE))       					   
	    skip();		       					   
	else if (c0=='{' && c1=='-') { /* (potentially) nested comment     */
	    Int nesting = 1;	       					   
	    Int origRow = row;         /* Save original row number         */
	    skip();
	    skip();
	    while (nesting>0 && c0!=EOF)
		if (c0=='{' && c1=='-') {
		    skip();
		    skip();
		    nesting++;
		}
		else if (c0=='-' && c1=='}') {
		    skip();
		    skip();
		    nesting--;
		}
		else if (c0=='\n')
		    newlineSkip();
		else
		    skip();
	    if (nesting>0) {
		ERRMSG(origRow) "Unterminated nested comment {- ..."
		EEND;
	    }
	}
	else if (c0=='-' && c1=='-') {  /* One line comment...   */
	    /* ..possibly, could also be the start of an operator, so 
	       tokenise the operator symbol & check whether it
	       consists of all dashes or not. */
   	    readOperator();
	    if (!allDashes(tokenStr)) {
	      /* Yep, return (with the operator in the token buffer). */
	      foundDashedOp = TRUE;
	      return;
	    } else {
	      /* Reset token buffer */
	      startToken();
	    }

	    while (c0!='\n' && c0!=EOF) {
	      skip();
	    }
	    if (c0=='\n')
		newlineSkip();
	}
	else
	    return;
}



static Bool firstToken;                /* Set to TRUE for first token      */
static Int  firstTokenIs;              /* ... with token value stored here */

static Int local yylex() {             /* Read next input token ...        */
    static Bool insertOpen    = FALSE;
    static Bool insertClose   = FALSE;
    static Bool insertedToken = FALSE;
    static Bool inADo         = FALSE;
    static Text textRepeat;
    Bool readingRepeat = repeatStr && (reading==KEYBOARD || reading==STRING);

#define lookAhead(t) {skipWhitespace(); insertOpen = (c0!='{'); inADo = (t==DO); return t;}

    if (firstToken) {                  /* Special case for first token     */
	indentDepth   = (-1);
	firstToken    = FALSE;
	insertOpen    = FALSE;
	insertedToken = FALSE;
	inADo         = FALSE;
	if (readingRepeat)
	    textRepeat = findText(repeatStr);
	return firstTokenIs;
    }
    
    if ( insertOpen ) { /* insert `soft' opening brace      */
	insertOpen    = FALSE;
	insertedToken = TRUE;
	/* If the indentation of a nested layout context is
	   not more indented than the current/enclosing, 
	   empty braces are inserted.
	   
	   cf. of Section B.3 (note 2) of the Haskell98 report.

	   Extension: adopt GHC's special (and useful) handling of
	   the following (comment straight out of ghc/compiler/parser/Lex.lhs):
	   
	   There's also a special hack in here to deal with

	       do
	        ....
		e $ do
		blah

   	   i.e. the inner context is at the same indentation level as the outer
	   context.  This is strictly illegal according to Haskell 98, but
	   there's a lot of existing code using this style and it doesn't make
	   any sense to disallow it, since empty 'do' lists don't make sense.
	*/
	insertClose = 
	    (indentDepth >=0 && (inADo ? column <  layout[indentDepth] :
 				         column <= layout[indentDepth]));
	if (!insertClose) {
	    goOffside(column);
	}
	push(yylval = mkInt(row));
	return '{';
    }
    
    if ( insertClose ) {
	insertOpen    = FALSE;
	insertClose   = FALSE;
	insertedToken = FALSE;
	push(yylval = mkInt(row));
	return '}';
    }

#if HERE_DOC
    if (hereState) {
	switch (hereState) {
        case KEEP_GOING :
	    readHereString();
	    return STRINGLIT;
	case BEGIN_VAR :
	    hereJoin();
	    hereState = ISSUE_QUOTE;
	    return VAROP;
	case ISSUE_QUOTE :
	    hereQuote();
	    hereState = HERE_VAR;
	    return VARID;
	case HERE_VAR :
	    hereState = END_VAR;
	    /* will parse and return id, and come back in the right state */
	    break;
        case END_VAR :
	    skipWhitespace();
	    if (c0!=')') {
		ERRMSG(row) "Improperly escaped variable in here document"
		EEND;
	    }
	    skip();
	    hereJoin();
	    hereState = KEEP_GOING;
	    return VAROP;
	case CLOSE_PAREN :
	    push(yylval = mkInt(row));
	    hereState = START;
	    return ')';
	}
    }
#endif

    /* ----------------------------------------------------------------------
     * Skip white space, and insert tokens to support layout rules as reqd.
     * --------------------------------------------------------------------*/

    skipWhitespaceTok();
    startColumn = column;
    push(yylval = mkInt(row));         /* default token value is line no.  */
    /* subsequent changes to yylval must also set top() to the same value  */

    if (indentDepth>=0) {              /* layout rule(s) active ?          */
	if (insertedToken) {           /* avoid inserting multiple `;'s    */
	    insertedToken = FALSE;     /* or putting `;' after `{'         */
	} else if (layout[indentDepth]!=HARD) {
	    if (column<layout[indentDepth]) {
		unOffside();
		return '}';
	    }
	    else if (column==layout[indentDepth] && c0!=EOF) {
		insertedToken = TRUE;
		return ';';
	    }
	}
    }

#if HERE_DOC && !HASKELL_98_ONLY
    if (c0=='`' && c1=='`' && hereDocs) {
	skip(); skip();
	hereState = KEEP_GOING;
	return '(';
    }
#endif

    /* ----------------------------------------------------------------------
     * Now try to identify token type:
     * --------------------------------------------------------------------*/
     
    if (foundDashedOp) {
      /* as it turns out, skipping whitespace turned up an operator. */
      top() = yylval = ap(VAROPCELL,findText(tokenStr));
      foundDashedOp = FALSE;
      return VAROP;
    }

    switch (c0) {
	case EOF  : return 0;                   /* End of file/input       */

	/* The next 10 characters make up the `special' category in 1.3    */
	case '('  : skip(); return '(';
	case ')'  : skip(); return ')';
	case ','  : skip(); return ',';
	case ';'  : skip(); return ';'; 
	case '['  : skip(); return '['; 
	case ']'  : skip(); return ']';
	case '`'  : skip(); return '`';
	case '{'  : goOffside(HARD);
		    skip();
		    return '{';
	case '}'  : if (indentDepth<0) {
			ERRMSG(row) "Misplaced `}'"
			EEND;
		    }
		    if (layout[indentDepth]==HARD)      /* skip over hard }*/
			skip();
		    unOffside();        /* otherwise, we have to insert a }*/
		    return '}';         /* to (try to) avoid an error...   */

	/* Character and string literals                                   */
	case '\'' : top() = yylval = readChar();
		    return CHARLIT;

	case '\"' : top() = yylval = readString();
		    return STRINGLIT;
    }

#if IPARAM && !HASKELL_98_ONLY
    if (c0=='?' && isIn(c1,SMALL) && !haskell98) {
	Text it;			/* Look for implicit param name    */
	skip();
	it    = readIdent();
	top() = yylval = ap(IPVAR,it);
	return identType=IPVARID;
    }
#endif
#if TREX && !HASKELL_98_ONLY
    if (c0=='#' && isIn(c1,SMALL) && !haskell98) {
	Text it;			/* Look for record selector name   */
	skip();
	it    = readIdent();
	top() = yylval = ap(RECSEL,mkExt(it));
	return identType=RECSELID;
    }
#endif
    if (isIn(c0,LARGE)) {               /* Look for qualified name         */
	Text it = readIdent();          /* No keyword begins with LARGE ...*/
	if (c0=='.' && isIn(c1,(SMALL|LARGE|SYMBOL))) {
	    Text it2 = NIL;
loop:	    skip();                     /* Skip qualifying dot             */
	    if (isIn(c0,SYMBOL)) { /* Qualified operator */
		it2 = readOperator();
		if (opType==CONOP) {
		    top() = yylval = mkQConOp(it,it2);
		    return QCONOP;
		} else {
		    top() = yylval = mkQVarOp(it,it2);
		    return QVAROP;
		}
	    } else {               /* Qualified identifier */
		it2 = readIdent();
		if (identType==CONID) {
		    top() = yylval = mkQCon(it,it2);
                    if (c0=='.' && isIn(c0,(SMALL|LARGE|SYMBOL))) {
                        it = mkNestedQual(yylval);
                        goto loop;
                    }
		    return QCONID;
		} else {
		    top() = yylval = mkQVar(it,it2);
		    return QVARID;
		}
	    }
	} else {
	    top() = yylval = mkCon(it);
	    return identType;
	}
    }
    if (isIn(c0,(SMALL|LARGE))) {
	Text it = readIdent();

	if (it==textCase)              return CASEXP;
	if (it==textOfK)               lookAhead(OF);
	if (it==textData)              return DATA;
	if (it==textType)              return TYPE;
	if (it==textIf)                return IF;
	if (it==textThen)              return THEN;
	if (it==textElse)              return ELSE;
	if (it==textWhere)             lookAhead(WHERE);
	if (it==textLet)               lookAhead(LET);
	if (it==textIn)                return IN;
	if (it==textInfix)             return INFIXN;
	if (it==textInfixl)            return INFIXL;
	if (it==textInfixr)            return INFIXR;
	if (it==textPrim)              return PRIMITIVE;
	if (it==textNewtype)           return TNEWTYPE;
	if (it==textDefault)           return DEFAULT;
	if (it==textDeriving)          return DERIVING;
	if (it==textDo)                lookAhead(DO);
	if (it==textClass)             return TCLASS;
	if (it==textInstance)          return TINSTANCE;
	if (it==textModule)            return TMODULE;
	if (it==textImport)            return IMPORT;
	if (it==textHiding)            return HIDING;
	if (it==textQualified)         return QUALIFIED;
	if (it==textNeedPrims)         return NEEDPRIMS;
        if (it==textForeign)           return FOREIGN;
	if (it==textAsMod)             return ASMOD;
	if (it==textWildcard)	       return '_';
#if !HASKELL_98_ONLY
	if (it==textAll && !haskell98) return ALL;

#if MUDO
	if (it==textMDo && !haskell98) lookAhead(MDO);
#endif
#endif
	if (it==textRepeat && readingRepeat)
	    return repeatLast();

	top() = yylval = ap((identType==CONID ? CONIDCELL : VARIDCELL),it);
	return identType;
    }

    if (isIn(c0,SYMBOL)) {
	Text it = readOperator();

	if (it==textCoco)    return COCO;
	if (it==textEq)      return '=';
	if (it==textUpto)    return UPTO;
	if (it==textAs)      return '@';
	if (it==textLambda)  return '\\';
	if (it==textBar)     return '|';
	if (it==textFrom)    return FROM;
	if (it==textMinus)   return '-';
	if (it==textPlus)    return '+';
	if (it==textBang)    return '!';
	if (it==textDot)     return '.';
	if (it==textArrow)   return ARROW;
	if (it==textLazy)    return '~';
	if (it==textImplies) return IMPLIES;
	if (it==textRepeat && readingRepeat)
	    return repeatLast();

	top() = yylval = ap((opType==CONOP ? CONOPCELL : VAROPCELL),it);
	return opType;
    }

    if (isIn(c0,DIGIT)) {
	top() = yylval = readNumber();
	return NUMLIT;
    }

    ERRMSG(row) "Unrecognised character `\\%d' in column %d", ((int)c0), column
    EEND;
    return 0; /*NOTREACHED*/
}

Bool isModuleId(s)
String s; {
    Char c;
    do {
	c = ExtractChar(s);
	if (!isIn(c,LARGE))
	    return FALSE;
	do {
	    c = ExtractChar(s);
	} while (isIn(c,IDAFTER));
    } while (c == '.');
    return (c == '\0');
}

static Int local repeatLast() {         /* Obtain last expression entered  */
    if (isNull(yylval=getLastExpr())) {
	ERRMSG(row) "Cannot use %s without any previous input", repeatStr
	EEND;
    }
    return REPEAT;
}

Syntax defaultSyntax(t)			/* Find default syntax of var named*/
Text t; {				/* by t ...			   */
    String s = textToStr(t);
    Char c = ExtractChar(s);
    return isIn(c,SYMBOL) ? DEF_OPSYNTAX : APPLIC;
}

Syntax syntaxOf(n)			/* Find syntax for name		   */
Name n; {
    if (name(n).syntax==NO_SYNTAX)	/* Return default if no syntax set */
	return defaultSyntax(name(n).text);
    return name(n).syntax;
}

/* --------------------------------------------------------------------------
 * main entry points to parser/lexer:
 * ------------------------------------------------------------------------*/

static Void local parseInput(startWith)/* Parse input with given first tok,*/
Int startWith; {                       /* determining whether to read a    */
    firstToken   = TRUE;               /* script or an expression          */
    firstTokenIs = startWith;

    clearStack();
    if (yyparse()) {                   /* This can only be parser overflow */
	ERRMSG(row) "Parser overflow"  /* as all syntax errors are caught  */
	EEND;                          /* in the parser...                 */
    }
    drop();
    if (!stackEmpty())                 /* stack should now be empty        */
	internal("parseInput");
}

Void parseScriptString(buf)		/* Read a script from a string buffer */
String buf; {
    input(RESET);
    stringInput(buf);
    parseInput(SCRIPT);
}

Bool parseScript(nm,len)               /* Read a script                    */
String nm;
Long   len; {                          /* Used to set a target for reading */
    input(RESET);
    if (fileInput(nm,len)) {
       /* File successfully located, change 'scriptFile' so that error messages
        * are reported relative to this file.
        */
        scriptFile = nm;
	parseInput(SCRIPT);
	return TRUE;
    }
    return FALSE;
}

Void parseExp() {                      /* Read an expression to evaluate   */
    parseInput(EXPR);
    setLastExpr(inputExpr);
}

#if EXPLAIN_INSTANCE_RESOLUTION
Void parseContext() {                  /* Read a context to prove   */
    parseInput(CTXT);
}
#endif

/* --------------------------------------------------------------------------
 * Input control:
 * ------------------------------------------------------------------------*/

Void input(what)
Int what; {
    switch (what) {
	case INSTALL : textCase       = findText("case");
		       textOfK        = findText("of");
		       textData       = findText("data");
		       textType       = findText("type");
		       textIf         = findText("if");
		       textThen       = findText("then");
		       textElse       = findText("else");
		       textWhere      = findText("where");
		       textLet        = findText("let");
		       textIn         = findText("in");
		       textInfix      = findText("infix");
		       textInfixl     = findText("infixl");
		       textInfixr     = findText("infixr");
		       textPrim       = findText("primitive");
		       textNewtype    = findText("newtype");
		       textDefault    = findText("default");
		       textDeriving   = findText("deriving");
		       textDo         = findText("do");
#if MUDO
		       textMDo        = findText("mdo");
#endif
		       textClass      = findText("class");
		       textInstance   = findText("instance");
#if IPARAM
		       textWith       = findText("with");
		       textDlet       = findText("dlet");
#endif
		       textCoco       = findText("::");
		       textEq         = findText("=");
		       textUpto       = findText("..");
		       textAs         = findText("@");
		       textLambda     = findText("\\");
		       textBar        = findText("|");
		       textMinus      = findText("-");
		       textPlus       = findText("+");
		       textFrom       = findText("<-");
		       textArrow      = findText("->");
		       textLazy       = findText("~");
		       textBang       = findText("!");
		       textDot	      = findText(".");
		       textImplies    = findText("=>");
		       textPrelude    = findText(STD_PRELUDE_HUGS);
		       textUserPrelude= findText(STD_PRELUDE);
		       textNum        = findText("Num");
		       textModule     = findText("module");
		       textImport     = findText("import");
		       textHiding     = findText("hiding");
		       textQualified  = findText("qualified");
		       textNeedPrims  = findText("needPrims_hugs");
		       textForeign    = findText("foreign");
		       textExport     = findText("export");
		       textCCall      = findText("ccall");
		       textSafe       = findText("safe");
		       textUnsafe     = findText("unsafe");
		       textThreadsafe = findText("threadsafe");
#if STDCALL_SUPPORTED		       
		       textStdcall    = findText("stdcall");
#endif
#ifdef DOTNET
		       textDotnet     = findText("dotnet");
#endif
		       textAsMod      = findText("as");
		       textWildcard   = findText("_");
		       textAll	      = findText("forall");
		       varMinus       = mkVar(textMinus);
		       varPlus	      = mkVar(textPlus);
		       varBang        = mkVar(textBang);
		       varDot	      = mkVar(textDot);
		       varHiding      = mkVar(textHiding);
		       varQualified   = mkVar(textQualified);
		       varAsMod       = mkVar(textAsMod);
		       conMain        = mkCon(findText("Main"));
		       varMain        = mkVar(findText("main"));
		       evalDefaults   = NIL;

		       input(RESET);
		       break;

	case RESET   : tyconDefns   = NIL;
		       typeInDefns  = NIL;
		       valDefns     = NIL;
		       classDefns   = NIL;
		       instDefns    = NIL;
		       selDefns     = NIL;
		       genDefns	    = NIL;
		       primDefns    = NIL;
		       unqualImports= NIL;
                       foreignCount = 0;
                       foreignImports= NIL;
                       foreignExports= NIL;
		       defaultDefns = NIL;
		       defaultLine  = 0;
		       inputExpr    = NIL;
		       imps         = NIL;
		       closeAnyInput();
		       break;

	case BREAK   : if (reading==KEYBOARD)
			   c0 = EOF;
		       break;

	case MARK    : mark(tyconDefns);
		       mark(typeInDefns);
		       mark(valDefns);
		       mark(classDefns);
		       mark(instDefns);
		       mark(selDefns);
		       mark(genDefns);
		       mark(primDefns);
		       mark(unqualImports);
                       mark(foreignImports);
                       mark(foreignExports);
		       mark(defaultDefns);
		       mark(evalDefaults);
		       mark(inputExpr);
		       mark(varMinus);
		       mark(varPlus);
		       mark(varBang);
		       mark(varDot);
		       mark(varHiding);
		       mark(varQualified);
		       mark(varAsMod);
		       mark(varMain);
		       mark(conMain);
		       mark(imps);
		       break;
    }
}

/*-------------------------------------------------------------------------*/
