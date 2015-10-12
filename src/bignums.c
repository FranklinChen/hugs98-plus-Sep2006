/* --------------------------------------------------------------------------
 * Functions for manipulating Haskell Integers (bignums).
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: bignums.c,v $
 * $Revision: 1.15 $
 * $Date: 2004/10/29 12:43:09 $
 * ------------------------------------------------------------------------*/

/*#define DEBUG_BIGNUMS*/

#if DEBUG_BIGNUMS
static Void local bigDump(List ds, Int n) {
    while (nonNull(ds) && 0<n--) {
	Printf(":%04d",digitOf(hd(ds)));
	ds = tl(ds);
    }
}
#endif

static Cell bn;			/* Bignum accumulator; used in bignum	   */
				/* calcs, visited by the garbage collector.*/
static List bigRem = NIL;	/* used in bigQrm to return remainder	   */


/*-------------------------------------------------------------------------*/

PROTO_PRIM(primPlusInteger);
PROTO_PRIM(primMinusInteger);
PROTO_PRIM(primMulInteger);
PROTO_PRIM(primQrmInteger);
PROTO_PRIM(primNegInteger);
PROTO_PRIM(primIntToInteger);
PROTO_PRIM(primIntegerToInt);
PROTO_PRIM(primWordToInteger);
PROTO_PRIM(primIntegerToWord);
PROTO_PRIM(primIntegerToFloat);
PROTO_PRIM(primIntegerToDouble);
PROTO_PRIM(primEqInteger);
PROTO_PRIM(primCmpInteger);

static struct primitive bignumPrimTable[] = {
  {"primPlusInteger",	2, primPlusInteger},
  {"primMinusInteger",	2, primMinusInteger},
  {"primMulInteger",	2, primMulInteger},
  {"primQrmInteger",	2, primQrmInteger},
  {"primNegInteger",	1, primNegInteger},
  {"primIntToInteger",	1, primIntToInteger},
  {"primIntegerToInt",	1, primIntegerToInt},
  {"primWordToInteger",	1, primWordToInteger},
  {"primIntegerToWord",	1, primIntegerToWord},
  {"primIntegerToFloat",1, primIntegerToFloat},
  {"primIntegerToDouble",1,primIntegerToDouble},
  {"primEqInteger",	2, primEqInteger},
  {"primCmpInteger",	2, primCmpInteger},
  {0,			0, 0}
};

/* --------------------------------------------------------------------------
 * Bignum control:
 * ------------------------------------------------------------------------*/

#if SHORT_CIRCUIT_COERCIONS
static Name nameIntToInteger;
#endif

static Void bignumControl Args((Int));
static Void bignumControl(what)
Int what; {
    switch (what) {
	case MARK    : mark(bn);
		       mark(bigRem);
		       break;
	case INSTALL : 
#if SHORT_CIRCUIT_COERCIONS
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       predef(nameIntToInteger, "primIntToInteger");
#undef predef
#endif
	case RESET   : bn     = NIL;
		       bigRem = NIL;
		       break;
    }
}

static struct primInfo bignumPrims = { bignumControl, bignumPrimTable, 0 };

/* Used by the IO implementation, so not 'static' like the rest. */
extern Bignum bigWord   Args((Unsigned));

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Int    local digitsCmp Args((List,List));
static Bignum local bigAdd    Args((Bignum,Bignum));
static Bignum local bigSub    Args((Bignum,Bignum));
static Bignum local digitsAdd Args((Cell,List,List));
static Bignum local digitsSub Args((List,List));
static Bignum local bigMul    Args((Bignum,Bignum));
static Bignum local bigQrm    Args((Bignum,Bignum));
static List   local digitsQrm Args((List,List));

/*---------------------------------------------------------------------------
 * Simple bignum primitives:
 *-------------------------------------------------------------------------*/

Bignum bigInt(n)			/* convert Int to bignum	   */
Int n; {
    if (n==0)
	return ZERONUM;
    else {
	unsigned long no;
	Cell nx;
	if (n<0) {
	    no = (unsigned long)(-n);
	    bn = pair(NEGNUM,NIL);
	}
	else {
	    no = (unsigned long)(n);
	    bn = pair(POSNUM,NIL);
	}
	for (nx=bn; no>0; no/=BIGBASE, nx=tl(nx)) {
	    tl(nx) = pair(mkDigit(no%BIGBASE),NIL);
	}
	return bn;
    }
}

Bignum bigWord(n)		      /* convert Word to bignum	   */
Unsigned n; {
    if (n==0)
	return ZERONUM;
    else {
	unsigned long no;
	Cell nx;
	no = (unsigned long)(n);
	bn = pair(POSNUM,NIL);
	for (nx=bn; no>0; no/=BIGBASE, nx=tl(nx)) {
	    tl(nx) = pair(mkDigit(no%BIGBASE),NIL);
	}
	return bn;
    }
}

Bignum bigDouble(a)			/* convert double to bignum	   */
double a; {
    if (a==0) {
	return ZERONUM;
    } else {
	double b; /* Obscure use of a and b is to avoid a bug in Symantec C*/
	Cell nx;
	if (a<0) {
	    b  = (-a);
	    bn = pair(NEGNUM,NIL);
	} else {
	    b = a;
	    bn = pair(POSNUM,NIL);
	}
	b = floor(b);
	/* NB: in IEEE floating point, !(b>0) is not the same as b<=0 */
	if (!(b>0))			/* happens on IEEE NaN */
	    throwException(ap(nameArithException, nameOverflow));
	for (nx=bn; b>0; nx=tl(nx)) {
	    double n = fmod(b,(double)(BIGBASE));
	    Int d    = (Int)n;
	    if (d<0)			/* happens on IEEE inf and -inf */
		throwException(ap(nameArithException, nameOverflow));
	    tl(nx)   = pair(mkDigit(d),NIL);
	    b        = (b - n) / BIGBASE;
	}
	return bn;
    }
}

/* The library documentation says that conversion between signed and
 * unsigned types preserves representation, not sign.  Since this
 * conversion uses fromInteger, and we do note know the original type,
 * fromInteger is the same for Int and Word, and must accept values
 * in the range MINNEGINT..MAXHUGSWORD.
 */

Cell bigToInt(n)			/* convert bignum to Int	   */
Bignum n; {
    if (n!=ZERONUM) {
	List ds = snd(n);
	if (nonNull(ds)) {
	    Int m;
	    Int b = 1;
	    if (fst(n)==POSNUM) {
		m = digitOf(hd(ds));
		while (nonNull(ds=tl(ds))) {
		    Int d = digitOf(hd(ds));
		    if (b > (Int)(MAXHUGSWORD/BIGBASE))
			return NIL;
		    b *= BIGBASE;
		    if (d > (Int)((MAXHUGSWORD - m)/b))
			return NIL;
		    m += b*d;
		}
	    } else { /* fst(n)==NEGNUM */
		m = - digitOf(hd(ds));
		while (nonNull(ds=tl(ds))) {
		    Int d = - digitOf(hd(ds));
		    if (b > (MAXPOSINT/BIGBASE))
			return NIL;
		    b *= BIGBASE;
		    if (d < (MINNEGINT - m)/b)
			return NIL;
		    m += b*d;
		}
	    }
	    return mkInt(m);
	}
    }
    return mkInt(0);
}

double bigToDouble(n)			/* convert bignum to double	  */
Bignum n; {
    if (n==ZERONUM)
	return 0.0;
    else {
	double m  = 0.0;
	double b  = 1.0;
	List  ds = snd(n);
	for (; nonNull(ds); ds=tl(ds)) {
	    m += b*digitOf(hd(ds));
	    b *= BIGBASE;
	}
	return fst(n)==POSNUM ? m : (-m);
    }
}

Bignum bigStr(s)			/* convert String to bignum	   */
String s; {				/* Surprisingly, this is GC safe   */
    List   ds = NIL;			/* because ds is the only variable */
    String t  = s;			/* that needs marking, and always  */
    Int    i;				/* appears as the snd of each cons */

    if (*t == '-')			/* look for leading minus	   */
	t++;				/* and ignore for time being	   */
    if ((i=(size_t)strlen(t)%BIGEXP)!=0) {
	Int d = 0;
	while (0<i--)
	    d = d*10 + (*t++ - '0');
	if (d)
	    ds = cons(mkDigit(d),NIL);
    }
    while (*t) {			/* now scan rest of string	   */
	Int d = 0;
	for (i=BIGEXP; i>0; i--)
	    d = d*10 + (*t++ - '0');
	if (nonNull(ds) || d)
	    ds = cons(mkDigit(d),ds);
    }

    return isNull(ds) ? ZERONUM : pair((*s=='-' ? NEGNUM : POSNUM), ds);
}

Cell bigOut(a,s,b)			/* bignum output, prepend digits to*/
Bignum a;				/* stream s			   */
Cell   s;				/* GC safe; s is snd in each cons  */
Bool   b; {				/* TRUE => wrap neg int in parens  */
    if (a==ZERONUM)
	return ap(consChar('0'),s);
    else {
	Bool isNeg = fst(a)==NEGNUM;	/* keep record of sign		   */
	bn         = snd(a);		/* list of digits		   */

	if (b && isNeg)			/* print closing paren		   */
	    s = ap(consChar(')'),s);

	for (;;) {
	    Int d = digitOf(hd(bn));	/* get next digit		   */
	    bn    = tl(bn);		/* move to next digit		   */
	    if (nonNull(bn)) {		/* more digits to come after this  */
		Int i = BIGEXP;
		for (; i>0; i--, d/=10)
		    s = ap(consChar('0'+(d%10)),s);
	    }
	    else {			/* leading (non-zero) digits	   */
		for (; d; d/=10)
		    s = ap(consChar('0'+(d%10)),s);
		break;
	    }
	    allowBreak();
	}

	if (isNeg)			/* print minus sign		   */
	    s = ap(consChar('-'),s);
	if (b && isNeg)			/* print open paren		   */
	    s = ap(consChar('('),s);
	return s;
    }
}

Bignum bigShift(big,c,mult)		/* Digits 0 <= c, mult < BIGBASE   */
Bignum big;				/* Calculate big*mult+c, if big>=0 */
Int    c;				/*           big*mult-c, if big<0  */
Int    mult; {				/* UPDATE big, DESTRUCTIVELY!	   */
    if (big==ZERONUM)
	return (c==0) ? ZERONUM : pair(POSNUM,singleton(mkDigit(c)));
    else {
	Cell nx = bn = big;
	while (nonNull(tl(nx))) {
	    nx     = tl(nx);
	    c      = digitOf(hd(nx))*mult + c;
	    hd(nx) = mkDigit(c % BIGBASE);
	    c      = c / BIGBASE;	/* N.B. new carry is < BIGBASE	   */
	}
	if (c>0)
	    tl(nx) = singleton(mkDigit(c));
    }
    return big;
}

/*---------------------------------------------------------------------------
 * Simple Bignum operations:
 *-------------------------------------------------------------------------*/

primFun(primIntToInteger) {		/* Conversion :: Int -> Integer	   */
    eval(primArg(1));
    updateRoot(bigInt(whnfInt));
}

primFun(primIntegerToInt) {		/* Conversion :: Integer -> Int	   */
#if SHORT_CIRCUIT_COERCIONS
    /* Optimisation: we try to short-circuit trivial conversions */
    Cell x = followInd(primArg(1));
    if (isAp(x) && followInd(fun(x)) == nameIntToInteger) {
	updateRoot(arg(x));
	return;
    } else
#endif
    {
	eval(primArg(1));
	whnfHead = bigToInt(whnfHead);
	if (nonNull(whnfHead)) {
	    updateRoot(whnfHead);
	} else {
	    throwException(ap(nameArithException, nameOverflow));
	}
    }
}

primFun(primWordToInteger) {		/* Conversion :: Word -> Integer   */
    eval(primArg(1));
    updateRoot(bigWord(whnfInt));
}

primFun(primIntegerToWord) {		/* Conversion :: Integer -> Word   */
    eval(primArg(1));
    whnfHead = bigToInt(whnfHead);
    if (nonNull(whnfHead))
	updateRoot(whnfHead);
    else
	throwException(ap(nameArithException, nameOverflow));
}

primFun(primIntegerToFloat) {		/* Conversion :: Integer -> Float  */
    eval(primArg(1));
    updateRoot(mkFloat((FloatPro)bigToDouble(whnfHead)));
}

primFun(primIntegerToDouble) {		/* Conversion :: Integer -> Double */
    eval(primArg(1));
    updateRoot(mkDouble((DoublePro)bigToDouble(whnfHead)));
}

primFun(primNegInteger) {		/* Integer unary negate		   */
    eval(primArg(1));
    updateRoot(bigNeg(whnfHead));
}

Bignum bigNeg(a)		        /* unary negation		   */
Bignum a; {
    if (a==ZERONUM)
	return ZERONUM;
    else
	return pair(((fst(a)==POSNUM) ? NEGNUM : POSNUM), snd(a));
}

/*---------------------------------------------------------------------------
 * Bignum comparison routines:
 *-------------------------------------------------------------------------*/

primFun(primEqInteger) {		/* Integer equality test	   */
    eval(primArg(2));
    primArg(2) = whnfHead;
    eval(primArg(1));
    updateRoot(bigCmp(primArg(2),whnfHead)==0 ? nameTrue : nameFalse);
}

primFun(primCmpInteger) {		/* Integer comparison		   */
    eval(primArg(2));
    primArg(2) = whnfHead;
    eval(primArg(1));
    switch (bigCmp(primArg(2),whnfHead)) {
	case (-1) : updateRoot(nameLT); break;
	case   0  : updateRoot(nameEQ); break;
	case   1  : updateRoot(nameGT); break;
    }
}

Int bigCmp(a,b)				/* Compare bignums returning:	   */
Bignum a, b; {				/* -1 if a<b,  +1 if a>b,  0 o/w   */
    if (a==ZERONUM)
	return (b==ZERONUM) ? 0 : ((fst(b)==POSNUM) ? (-1) : 1);
    else if (fst(a)==NEGNUM)
	if (b==ZERONUM || fst(b)==POSNUM)
	    return (-1);
	else
	    return digitsCmp(snd(b),snd(a));
    else
	if (b==ZERONUM || fst(b)==NEGNUM)
	    return 1;
	else
	    return digitsCmp(snd(a),snd(b));
}

static Int local digitsCmp(xs,ys)	/* Compare positive digit streams  */
List xs, ys; {				/* -1 if xs<ys, +1 if xs>ys, 0 if= */
    Int s = 0;
    for (; nonNull(xs) && nonNull(ys); xs=tl(xs), ys=tl(ys)) {
	Int x = hd(xs);
	Int y = hd(ys);
	if (x<y)
	    s = (-1);
	else if (x>y)
	    s = 1;
    }
    return (nonNull(xs) ? 1 : (nonNull(ys) ? (-1) : s));
}

/*---------------------------------------------------------------------------
 * Addition and subtraction:
 *-------------------------------------------------------------------------*/

static Bignum local bigAdd(a,b)		/* Bignum addition		   */
Bignum a, b; {
    if (a==ZERONUM)
	return b;
    else if (b==ZERONUM)
	return a;
    else if (fst(a)==POSNUM)
	if (fst(b)==POSNUM)
	    return digitsAdd(POSNUM,snd(a),snd(b));
	else
	    return digitsSub(snd(a),snd(b));
    else /* fst(a)==NEGNUM */
	if (fst(b)==NEGNUM)
	    return digitsAdd(NEGNUM,snd(a),snd(b));
	else
	    return digitsSub(snd(b),snd(a));
}

static Bignum local bigSub(a,b)		/* Bignum subtraction		   */
Bignum a, b; {
    if (a==ZERONUM)
	return bigNeg(b);
    else if (b==ZERONUM)
	return a;
    else if (fst(a)==POSNUM)
	if (fst(b)==NEGNUM)
	    return digitsAdd(POSNUM,snd(a),snd(b));
	else
	    return digitsSub(snd(a),snd(b));
    else /* fst(a)==NEGNUM */
	if (fst(b)==POSNUM)
	    return digitsAdd(NEGNUM,snd(a),snd(b));
	else
	    return digitsSub(snd(b),snd(a));
}

static Bignum local digitsAdd(sign,xs,ys)/* Addition of digit streams	   */
Cell sign;
List xs, ys; {
    Cell nx = bn = pair(sign,NIL);
    Int  c  = 0;
    for (;;) {
	if (nonNull(xs)) {			/* Add any digits to carry */
	    if (nonNull(ys)) {
		c += digitOf(hd(xs)) + digitOf(hd(ys));
		xs = tl(xs);
		ys = tl(ys);
	    }
	    else if (c==0) {			/* look for short cut when */
		tl(nx) = xs;			/* a stream ends and there */
		break;				/* is no outstanding carry */
	    }
	    else {
		c += digitOf(hd(xs));
		xs = tl(xs);
	    }
	}
	else if (c==0) {
	    tl(nx) = ys;
	    break;
	}
	else if (nonNull(ys)) {
	    c += digitOf(hd(ys));
	    ys = tl(ys);
	}

	if (c>=BIGBASE) {			/* Calculate output digit  */
	    nx = tl(nx) = cons(mkDigit(c-BIGBASE),NIL);
	    c  = 1;
	}
	else {					/* Carry will always be >0 */
	    nx = tl(nx) = cons(mkDigit(c),NIL);	/* at this point	   */
	    c  = 0;
	}
	allowBreak();
    }
    return bn;
}

static Bignum local digitsSub(xs,ys)	/* Subtraction of digit streams    */
List xs, ys; {
    Cell nx;
    Int  b  = 0;
    Int  lz = 0;

    switch (digitsCmp(xs,ys)) {
	case (-1) : nx = xs;		/* if xs<ys, return -(ys-xs)	   */
		    xs = ys;
		    ys = nx;
		    bn = pair(NEGNUM,NIL);
		    break;

	case   0  : return ZERONUM;	/* if xs=ys, return 0		   */

	case   1  : bn = pair(POSNUM,NIL);
		    break;		/* if xs>ys, return +(xs-ys)	   */
    }

    nx = bn;				/* Now we can assume that xs>ys    */
    for (;;) {				/* Scan each digit		   */
	Int y = b;
	if (nonNull(ys)) {
	    y += digitOf(hd(xs)) - digitOf(hd(ys));
	    xs = tl(xs);
	    ys = tl(ys);
	}
	else if (y==0) {
	    if (nonNull(xs))
		for (; lz>0; lz--)
		    nx = tl(nx) = cons(mkDigit(0),NIL);
	    tl(nx) = xs;
	    break;
	}
	else {
	    y += digitOf(hd(xs));	/* xs>ys, so we can't run out of   */
	    xs = tl(xs);		/* digits of xs while y!=0	   */
	}

	if (y<0) {			/* Calculate output digit	   */
	    y += BIGBASE;
	    b  = (-1);
	}
	else
	    b  = 0;

	if (y==0)			/* Don't insert leading zeros	   */
	    lz++;
	else {
	    for (; lz>0; lz--)
		nx = tl(nx) = cons(mkDigit(0),NIL);
	    nx = tl(nx) = cons(mkDigit(y),NIL);
	}
	allowBreak();
    }
    return bn;
}

/*---------------------------------------------------------------------------
 * Multiplication:
 *-------------------------------------------------------------------------*/

static Bignum local bigMul(a,b)		/* Bignum multiply		   */
Bignum a, b; {
    if (a==ZERONUM || b==ZERONUM)	/* if either operand is zero, then */
	return ZERONUM;			/* so is the result ...		   */
    else {				/* otherwise, use rule of signs:   */
	Cell nx = bn = ap((hd(a)==hd(b) ? POSNUM : NEGNUM), NIL);
	for (; nonNull(b=tl(b)); nx=tl(nx)) {	/* loop through digits of b*/
	    List as = nx;		/* At each stage of the loop, add  */
	    List xs = tl(a);		/* y * xs to the value in result,  */
	    Int  y  = digitOf(hd(b));	/* using c as carry		   */
	    Int  c  = 0;
	    for (; nonNull(xs); xs=tl(xs)) {	/* loop through digits of a*/
		c += digitOf(hd(xs)) * y;
		if (nonNull(tl(as))) {
		    as = tl(as);
		    c += digitOf(hd(as));
		}
		else
		    as = tl(as) = cons(NIL,NIL);
		hd(as) = mkDigit(c % BIGBASE);
		c     /= BIGBASE;
	    }
	    if (c>0)			/* add carry digit, if required	   */
		tl(as) = cons(mkDigit(c),NIL);
	    allowBreak();
	}
	return bn;
    }
}

/*---------------------------------------------------------------------------
 * Division:
 *-------------------------------------------------------------------------*/

static Cell local bigQrm(a,b)		/* bignum quotient and remainder   */
Bignum a, b; {
    if (b==ZERONUM)			/* division by zero?		   */
	return NIL;
    else if (a==ZERONUM)   		/* 0 `div` x == 0 `mod` x == 0	   */
	return ap(ap(mkTuple(2),ZERONUM),ZERONUM);
    else {
	/* The sign of the quotient is positive if numerator and denominator
	 * have the same sign, negative if the signs differ.  The sign of the
	 * remainder is always the same as the sign of the numerator.
	 */
	Cell qsign = (fst(a)==fst(b) ? POSNUM : NEGNUM);
	Cell rsign = fst(a);
	bn         = digitsQrm(snd(a),snd(b));
	bn         = isNull(bn) ? ZERONUM : pair(qsign,bn);
	bn         = pair(mkTuple(2),bn);
	bigRem     = isNull(bigRem) ? ZERONUM : pair(rsign,bigRem);
	bn         = ap(bn,bigRem);
	bigRem     = NIL;
	return bn;
    }
}

static List local digitsQrm(us,vs)	/* digits quotient and remainder   */
List us, vs; {
    Bool gc = consGC;
    consGC  = TRUE;
    if (isNull(tl(vs))) {		/* single digit divisor		   */
	Int  v   = digitOf(hd(vs));
	Int  r   = 0;
	List us1 = NIL;			/* first, copy and reverse us	   */
	for (; nonNull(us); us=tl(us))
	    us1 = cons(hd(us),us1);
	while (nonNull(us1)) {		/* then do division, MSD first	   */
	    Cell tmp = tl(us1);
	    Int  u   = r * BIGBASE + digitOf(hd(us1));
	    r        = u % v;
	    u	     = u / v;
	    if (nonNull(us) || u) {	/* output quotient digit	   */
		hd(us1) = mkDigit(u);
		tl(us1) = us;
		us 	= us1;
	    }
	    us1	     = tmp;
	}
	bigRem = r ? singleton(mkDigit(r)) : NIL;
	consGC = gc;
	return us;
    }
    else {				/* at least two digits in divisor  */

	/* The division algorithm used here is, inevitably, based on the
	 * description in Knuth's volume 2 on Seminumerical algorithms,
	 * and is probably at least as incomprehensible as the MIX
	 * implementation given there :-)
	 */

	List us1 = NIL;
	List vs1 = NIL;
	List ds  = us;
	Int  v1  = 0, v2  = 0;
	Int  uj  = 0, uj1 = 0, uj2 = 0;
	Int  n   = 0;
	List qs  = NIL;
	Int  sc;

	while (nonNull(us) && nonNull(vs)) {
	    v2  = v1;
	    v1  = digitOf(hd(vs));
	    vs1 = cons(hd(vs),vs1);
	    vs  = tl(vs);

	    uj2 = uj1;
	    uj1 = digitOf(hd(us));
	    us1 = cons(hd(us),us1);
	    us  = tl(us);

	    n++;
	}

	if (nonNull(vs)) {		/* if us is shorter than vs, then  */
	    bigRem = ds;		/* remainder is us, quotient zero  */
	    consGC = gc;
	    return NIL;
	}
	vs = rev(vs1);

	/* Now we have:
	 *   n      = number of digits in vs which is at least two (we
	 *	      also know that us has at least n digits),
	 *   v1, v2 = most significant digits of vs
	 *   vs     = digits of vs with least significant digit first
	 */

#if DEBUG_BIGNUMS
Printf("initial vs (n=%d, v1=%d, v2=%d): ",n,v1,v2);
bigDump(vs,n);
Putchar('\n');
#endif
	while (nonNull(us)) {
	    uj2 = uj1;
	    uj1 = digitOf(hd(us));
	    us1 = cons(hd(us),us1);
	    us  = tl(us);
	}

	us = cons(mkDigit(uj=0),NIL);
	ds = us1;
	for (vs1=tl(vs); nonNull(vs1); vs1=tl(vs1)) {
	    us1    = tl(ds);
	    tl(ds) = us;
	    us     = ds;
	    ds     = us1;
	}

	/* And, at this point, we have:
	 *   us = first (n-1) significant digits of original numerator,
	 *	  with least significant digit first, and a zero at the
	 *	  end (MSD) of the list, so that length us == n.
	 *   ds = remaining digits of the numerator, most significant
	 *	  digit first.
	 *   uj, uj1, uj2
	 *      = first three significant digits of us.  (At this point,
	 *	  uj is actually zero.)
	 */

#if DEBUG_BIGNUMS
Printf("initial us (uj=%d, uj1=%d, uj2=%d): ",uj,uj1,uj2);
bigDump(us,n);
Putchar('\n');
Printf("initial ds: ");
bigDump(ds,1000);
Putchar('\n');
#endif
	sc = BIGBASE / (v1+1);
#if DEBUG_BIGNUMS
Printf("scaling factor %d\n",sc);
#endif
	if (sc!=1) {			/* Scale numerator and denominator */
	    Int c = 0;
	    v1 = v2 = 0;
	    for (vs1=vs; nonNull(vs1); vs1=tl(vs1)) {
		v2 = v1;
		v1 = sc * digitOf(hd(vs1)) + c;
		c  = v1 / BIGBASE;
		hd(vs1) = mkDigit(v1%=BIGBASE);
	    }				/* no carry here, guaranteed	   */

	    c = uj = uj1 = uj2 = 0;
	    for (us1=ds=rev(ds); nonNull(us1); us1=tl(us1)) {
		uj2 = uj1;
		uj1 = uj;
		uj  = sc * digitOf(hd(us1)) + c;
		c   = uj / BIGBASE;
		hd(us1) = mkDigit(uj%=BIGBASE);
	    }
	    for (ds=rev(ds), us1=us; nonNull(us1); us1=tl(us1)) {
		uj2 = uj1;
		uj1 = uj;
		uj  = sc * digitOf(hd(us1)) + c;
		c   = uj / BIGBASE;
		hd(us1) = mkDigit(uj%=BIGBASE);
	    }				/* no carry here, guaranteed	   */
	}

#if DEBUG_BIGNUMS
Printf("scaled vs (n=%d, v1=%d, v2=%d): ",n,v1,v2);
bigDump(vs,n);
Putchar('\n');
Printf("scaled us (uj=%d, uj1=%d, uj2=%d): ",uj,uj1,uj2);
bigDump(us,n);
Putchar('\n');
Printf("scaled ds: ");
bigDump(ds,1000);
Putchar('\n');
#endif
	/* Most of the conditions above are still valid, except that both
	 * the numerator and denominator have been multiplied by the scaling
	 * factor sc, and the values of the various digit positions have been
	 * updated accordingly.
	 *
	 * Now we can start the division algorithm proper:
	 */

	while (nonNull(ds)) {
	    Int nd, c;			/* Guess a value for quotient digit*/
	    Int qhat = (uj==v1) ? (BIGBASE-1) : (uj*BIGBASE+uj1)/v1;
	    while (v2*qhat > (uj*BIGBASE+uj1-qhat*v1)*BIGBASE+uj2)
		qhat--;			/* and then try to improve it	   */

	    us1    = tl(ds);		/* take digit off off front of ds  */
	    tl(ds) = us;		/* and add to front of us	   */
	    us     = ds;
	    ds	   = us1;
	    nd     = isNull(ds) ? 0 : digitOf(hd(ds));	/* next digit of ds*/

#if DEBUG_BIGNUMS
Printf("To divide us (uj=%d, uj1=%d, uj2=%d): ",uj,uj1,uj2);
bigDump(us,n+1);
Printf(" by vs (v1=%d, v2=%d): ",v1,v2);
bigDump(vs,n);
Printf(", guess qhat=%d\n",qhat);
#endif
	    uj  = nd;			/* us := us - qhat * vs		   */
	    uj1 = uj2 = c = 0;
	    us1 = us;
	    vs1 = vs;
	    do {
		uj2 = uj1;
		uj1 = uj;
		uj  = digitOf(hd(us1)) - qhat*digitOf(hd(vs1)) - c;
		if (uj>=0)
		    c = 0;
		else {
		    c   = (BIGBASE - 1 - uj) / BIGBASE;
		    uj += c*BIGBASE;
		}
		hd(us1) = mkDigit(uj);
		us1     = tl(us1);
		vs1     = tl(vs1);
	    } while (nonNull(vs1));

	    if (digitOf(hd(us1))<c) {	/* maybe we overestimated qhat?	   */
#if DEBUG_BIGNUMS
Printf("overestimated qhat by one!\n");
#endif
		qhat--;			/* so reduce guess		   */
		uj  = nd;		/* and set us := us + vs	   */
		uj1 = uj2 = c = 0;	/* (we can't have overestimated by */
		us1 = us;		/* more than one thanks to the	   */
		vs1 = vs;		/* initial scaling of us, vs by sc)*/
		do {
		    uj2 = uj1;
		    uj1 = uj;
		    uj  = digitOf(hd(us1)) + digitOf(hd(vs1)) + c;
		    if (uj>=BIGBASE)
			c = 1, uj -= BIGBASE;
		    else
			c = 0;
		    hd(us1) = mkDigit(uj);
		    us1     = tl(us1);
		    vs1     = tl(vs1);
		} while (nonNull(vs1));
	    }
#if DEBUG_BIGNUMS
Printf("There remains (uj=%d, uj1=%d, uj2=%d): ",uj,uj1,uj2);
bigDump(us,n);
Putchar('\n');
#endif

	    if (nonNull(qs) || qhat)	/* output quotient digit, without  */
		qs = cons(mkDigit(qhat),qs);	/* leading zeros	   */

	    allowBreak();
	}

#if DEBUG_BIGNUMS
Printf("done quotient\n");
#endif
	/* Now we have the quotient digits (if any) with least significant
	 * digit first in qs, and sc times the remainder is the first n
	 * digits of us.  All that remains is to adjust the remainder:
	 */

	us1 = rev(take(n,us));
	us  = NIL;
	uj  = 0;			/* reuse variable uj as a carry	   */
	while (nonNull(us1)) {
	    Int y = uj * BIGBASE + digitOf(hd(us1));
	    uj    = y % sc;
	    y    /= sc;
	    if (nonNull(us) || y) {
		vs1     = tl(us1);
		tl(us1) = us;
		hd(us1) = mkDigit(y);
		us      = us1;
		us1	= vs1;
	    }
	    else
		us1 = tl(us1);
	}
	bigRem = us;
	consGC = gc;
	return qs;
    }
}

/*-------------------------------------------------------------------------*/

/* e is a constant expression with no live pointers */
#define CAFBignum(nm,e)                   \
  primCAF(nm) {                           \
    push(e);                              \
    bn = NIL;                             \
  }

/* e is an expression with free variables x and y */
#define BignumBignum2Bignum(nm,e)         \
  primFun(nm) {                           \
    Bignum x, y;                          \
    eval(primArg(2)); push(whnfHead);     \
    eval(primArg(1)); y = whnfHead;  	  \
    x = pop();                    	  \
    if (!isBignum(x) || !isBignum(y))     \
	internal("Bignum expected");      \
    updateRoot(e);                   	  \
    bn = NIL;                             \
}

/* e is an expression with free variable x */
#define Bignum2Bignum(nm,e)      	  \
  primFun(nm) {                     	  \
    Bignum x;                       	  \
    eval(primArg(1)); x = whnfHead; 	  \
    if (!isBignum(x))                     \
	internal("Bignum expected");      \
    updateRoot(e);                  	  \
    bn = NIL;                             \
}

/* e is an expression with free variable x that does no heap allocation */
#define Bignum2Bool(nm,e)                 \
  primFun(nm) {                           \
    Bignum x;                             \
    eval(primArg(1)); x = whnfHead;       \
    if (!isBignum(x))                     \
	internal("Bignum expected");      \
    updateRoot(e ? nameTrue : nameFalse); \
    bn = NIL;                             \
}

/* e is an expression with free variable x that does no heap allocation */
#define BignumBignum2Bool(nm,e)           \
  primFun(nm) {                           \
    Bignum x, y;                          \
    eval(primArg(2)); push(whnfHead);     \
    eval(primArg(1)); y = whnfHead;       \
    x = pop();                            \
    if (!isBignum(x) || !isBignum(y))     \
	internal("Bignum expected");      \
    updateRoot((e) ? nameTrue : nameFalse); \
    bn = NIL;                             \
}

/*-------------------------------------------------------------------------*/

BignumBignum2Bignum(primPlusInteger,bigAdd(x,y))  /* Integer addition	   */
BignumBignum2Bignum(primMinusInteger,bigSub(x,y)) /* Integer subtraction   */
BignumBignum2Bignum(primMulInteger,bigMul(x,y))   /* Integer multiply	   */

primFun(primQrmInteger) {		/* Integer quotient and remainder  */
    Bignum x;
    eval(primArg(2));
    primArg(2) = whnfHead;
    eval(primArg(1));
    x = bigQrm(primArg(2),primArg(1)=whnfHead);
    if (isNull(x)) {
	throwException(ap(nameArithException, nameDivideByZero));
    }
    else {
	updateRoot(x);
    }
    bn = NIL;
}

/*-------------------------------------------------------------------------*/

