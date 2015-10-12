/* --------------------------------------------------------------------------
 * Haskell array primitives.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: array.c,v $
 * $Revision: 1.10 $
 * $Date: 2004/01/06 10:02:51 $
 * ------------------------------------------------------------------------*/

static Name nameEltUndef;		/* undefined element in array	   */

PROTO_PRIM(primArray);
PROTO_PRIM(primUpdate);
PROTO_PRIM(primAccum);
PROTO_PRIM(primAccumArray);
PROTO_PRIM(primAmap);
PROTO_PRIM(primSubscript);
PROTO_PRIM(primBounds);
PROTO_PRIM(primElems);
PROTO_PRIM(primEltUndef);
#if IO_MONAD
PROTO_PRIM(primIONewArr);
PROTO_PRIM(primIOReadArr);
PROTO_PRIM(primIOWriteArr);
PROTO_PRIM(primIOFreeze);
PROTO_PRIM(primIOArrEq);
#endif

static struct primitive arrayPrimTable[] = {
  {"primArray",		3, primArray},
  {"primUpdate",	2, primUpdate},
  {"primAccum",		3, primAccum},
  {"primAccumArray",	5, primAccumArray},
  {"primAmap",		2, primAmap},
  {"primSubscript",	2, primSubscript},
  {"primBounds",	1, primBounds},
  {"primElems",		1, primElems},
  {"eltUndef",		0, primEltUndef},
#if IO_MONAD
  {"IONewArr",		3+IOArity, primIONewArr},
  {"IOReadArr",		2+IOArity, primIOReadArr},
  {"IOWriteArr",	3+IOArity, primIOWriteArr},
  {"IOFreeze",		1+IOArity, primIOFreeze},
  {"IOBounds",	        1, primBounds},
  {"IOArrEq",	        2, primIOArrEq},
#endif
  {0,			0, 0}
};

static Void outOfBounds Args((void));
static void outOfBounds() {
    throwException(ap(nameArrayException, ap(nameIndexOutOfBounds, nameNil)));
}

/* --------------------------------------------------------------------------
 * Array control:
 * ------------------------------------------------------------------------*/

static Void arrayControl Args((Int));
static Void arrayControl(what)
Int what; {
    switch (what) {
	case INSTALL : 
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(nameEltUndef,  "_undefined_array_element",
							"eltUndef");
#undef pFun
		       break;
    }
}

static struct primInfo arrayPrims = { arrayControl, arrayPrimTable, 0 };

/*-------------------------------------------------------------------------*/

/* The implementation of arrays is heavily parameterized to allow the
 * use of different implementations.  Non-conservative GC is also an
 * important goal, which is also why so much of this was originally done
 * using macros rather than procedures.  As it happens, this probably could
 * have been avoided, but there don't seem to be sufficiently good reasons
 * to warrant changing it.
 *
 * The result, however, is a torture-test for the C preprocessor!
 *
 * A description of the various `parameters' follows:
 *
 * Primitives that build a new array use the macro:
 * declArr;		Allocate slot on stack to hold a freshly created
 *			array that will be seen by the garbage collector.
 *			The value of the array can subsequently be referred
 *			to using the `arr' macro.  The declArr macro also
 *			declares a local Int variable, alen, to hold the
 *			length of the array.
 *
 * There are four methods for creating a new array, all of which return
 * the intermediate array in arr and its length in alen:
 *
 * aNewSet(b,s,v);	Allocate new array with bounds b and size s.
 *			Data elements set to v.
 * aNewNil(b,s);	Equivalent to aNewSet(b,s,NIL), treated separately
 *			because it is possible to use more efficient code
 *			for this special case in some implementations.
 * aNewCopy(a);		Builds an exact copy of array a, which can then be
 *			modified destructively, without changing a.
 *			Note that this forces evaluation of a.
 * aNewLike(a,v);	Builds an array of the same size and bounds as a
 *			with each element initialized to v.
 *			Note that this forces evaluation of a.
 *
 * All four of these methods are implemented using macros; the b, r, a
 * parameters are integers, identifying particular primArg(x) slots.
 * The v parameters should be constants, unmovable by GC, or primArg(x)
 * references that can be safely modified during GC.
 *
 * Other functions are:
 *
 * aEvalModel(a);	Evaluate model array primArg(a), and overwrite it
 *			on stack with an indirection free pointer to the
 *			resulting array.
 * aAssocs(as,p);	Move list of assocs -- (offset,value) pairs -- from
 *			primArg(as) (which is NIL'd to prevent space leak)
 *			to top of stack and evaluate, in sequence, until all
 *			assocs have been processed.  For each pair, we
 *			run procedure p with the offset in whnfInt and the
 *			associated value in top(), to be popped before p is
 *			done.
 * aSetElt;		To be used with aAssocs: if arr[whnfInt] is NIL,
 *			set it to top(), otherwise set to undefined.
 * aAddElt(f);		To be used with aAssocs: replace whnfInt element e
 *			of arr with ap(ap(primArg(f),e),top())
 * aNullElts;		Set any null elements in arr to nameEltUndef.
 * aCopyNull(a);	Replace any null elements in arr with corresponding
 *			values in array primArg(a).
 * aMapElts(f);		Replace every element e in arr with ap(primArg(f),e).
 * aGetElt(a);		Push value of whnfInt'th element of primArg(a).
 * aPutElt(a,v);	Put v into whnfInt'th slot of primArg(a).
 * aElems(a);		Evaluate array at primArg(a), and return its list of
 *			elements on top of stack in reverse order, backed onto
 *			NIL (ready for revOnto(top(),nameNil)).
 * aBounds()		Extract bounds from arr.
 * aGetBounds(a)	Extract bounds from primArg(a).
 *
 * There is no guarantee that the representation used for arr will be the
 * same as for any other array.  The following methods do however ensure
 * that the standard representation is used when a value is finally returned:
 *
 * updarrRoot();	Updates root of redex with array represented by arr.
 *			(Should also reset arr to avoid space leaks.)
 * aRetForIO();		Update root to return an array from IO monad;
 *			i.e. pass arr to the continuation.
 */

#define declArr		StackPtr arrPos=sp+1; Int alen; push(NIL)
#define arr		stack(arrPos)

#define aNewNil(b,s)	aNewSet(b,s,NIL)
#define aNewSet(b,s,v)	{   Int i;				\
			    eval(primArg(s));			\
			    alen = (whnfInt>0)?whnfInt:0;	\
			    for (arr=NIL, i=alen; i>0; i--)	\
				arr = ap(v,arr);		\
			    arr = ap(primArg(b),arr);		\
			}
#define aNewCopy(a)	{   Cell es = snd(primArg(a));		\
			    for (arr=ap(hd(es),NIL), alen=0;	\
				 nonNull(es=tl(es)); ++alen)	\
				arr = ap(hd(es),arr);		\
			    arr = rev(arr);			\
			}
#define aNewLike(a,v)	{   Cell es = snd(primArg(a));		\
			    for (arr=ap(hd(es),NIL), alen=0;	\
				 nonNull(es=tl(es)); ++alen)	\
				arr = ap(v,arr);		\
			    arr = rev(arr);			\
			}
#define aEvalModel(a)	eval(primArg(a)); primArg(a)=whnfHead
#define aSetElt		{   List us = snd(arr);			\
			    for (; 0<whnfInt--; us=tl(us));	\
			    hd(us) = isNull(hd(us))?top():nameEltUndef;\
			    drop();				\
			}
#define aAddElt(f)	{   List us = snd(arr);			\
			    for (; 0<whnfInt--; us=tl(us));	\
			    hd(us) = ap(primArg(f),hd(us));	\
			    hd(us) = ap(hd(us),pop());		\
			}
#define aNullElts	{   List us = snd(arr);			\
			    for (; nonNull(us); us=tl(us))	\
				if (isNull(hd(us)))		\
				    hd(us) = nameEltUndef;	\
			}
#define aCopyNull(a)	{   List us = snd(snd(primArg(a)));	\
			    List vs = snd(arr);			\
			    for (; nonNull(vs); vs=tl(vs), us=tl(us))\
				if (isNull(hd(vs)))		\
				    hd(vs) = hd(us);		\
			}
#define aMapElts(f)	{   List us = snd(arr);			\
			    for (; nonNull(us); us=tl(us))	\
				hd(us) = ap(primArg(f),hd(us));	\
			}
#define aGetElt(a)	{   List es = snd(snd(primArg(a)));	\
			    while (0<whnfInt--)			\
				es = tl(es);			\
			    push(hd(es));			\
			}
#define aPutElt(a,v)	{   List es = snd(snd(primArg(a)));	\
			    while (0<whnfInt--)			\
				es = tl(es);			\
			    hd(es) = v;				\
			}
#define aElems(a)	{   List us;				\
			    eval(primArg(a));			\
			    us = snd(snd(primArg(a)));		\
			    chkStack(2); onto(NIL); onto(NIL);	\
			    for(; nonNull(us); us=tl(us)) {	\
				top()     = ap(nameCons,hd(us));\
				pushed(1) = ap(top(),pushed(1));\
			    }					\
			    drop();				\
			}
#define aBounds()	fst(arr)
#define aGetBounds(a)	fst(snd(primArg(a)))
#define updarrRoot()	updapRoot(ARRAY,arr); arr=NIL
#define aRetForIO()	arr = ap(ARRAY,arr);			\
			updapRoot(primArg(1),arr);		\
			arr = NIL;

/* The implementation of aAssocs(as,p) should be independent of the
 * representation for arrays:
 */
#define aAssocs(as,p)	push(primArg(as)); primArg(as)=NIL;	\
			eval(pop());				\
			while (whnfHead==nameCons) {		\
			    eval(pop());			\
			    eval(top());			\
			    if (whnfInt<0 || whnfInt>=alen)	\
				outOfBounds();			\
			    drop(); p; eval(pop());		\
			}

/* Finally, we come to the implementation of the Haskell array primitives: */

primFun(primArray) {			/* :: (a,a)			   */
    declArr;				/*    -> Int			   */
    aNewNil(3,2);			/*	 -> [(Int,b)]		   */
    aAssocs(1,aSetElt);			/*	    -> Array a b	   */
    aNullElts;
    updarrRoot();
}

primFun(primUpdate) {			/* :: [(Int,b)]			   */
    declArr;				/*    -> Array a b		   */
    aEvalModel(1);			/*       -> Array a b		   */
    aNewLike(1,NIL);
    aAssocs(2,aSetElt);
    aCopyNull(1);
    updarrRoot();
}

primFun(primAccum) {			/* :: [(Int,c)] -> Array a b	   */
    declArr;				/*    -> (b -> c -> b)		   */
    aEvalModel(2);			/*	 -> Array a b		   */
    aNewCopy(2);
    aAssocs(3,aAddElt(1));
    updarrRoot();
}

primFun(primAccumArray) {		/* :: (a,a) -> Int		   */
    declArr;				/*    -> (b -> c -> b) -> b	   */
    aNewSet(5,4,primArg(2));		/*	 -> [(Int,c)]		   */
    aAssocs(1,aAddElt(3));		/*	    -> Array a b	   */
    updarrRoot();
}

primFun(primAmap) {			/* :: (a -> b)			   */
    declArr;				/*    -> Array c a		   */
    aEvalModel(1);			/*       -> Array c b		   */
    aNewCopy(1);
    aMapElts(2);
    updarrRoot();
}

primFun(primSubscript) {		/* :: Array a b -> Int -> b        */
    aEvalModel(2);
    eval(primArg(1));
    aGetElt(2);
    updateRoot(top());
}

primFun(primBounds) {			/* :: Array a b -> (a,a)	   */
    aEvalModel(1);
    updateRoot(aGetBounds(1));
}

primFun(primElems) {			/* :: Array a b -> [b]		   */
    aEvalModel(1);
    aElems(1);
    updateRoot(revOnto(top(),nameNil));
}

primFun(primEltUndef) {
    throwException(ap(nameArrayException, ap(nameUndefinedElement, nameNil)));
}

#if IO_MONAD
primFun(primIONewArr) {			/* :: (a,a)			   */
    declArr;				/*    -> Int			   */
    aNewSet(3+IOArity,2+IOArity,IOArg(1));/*	 -> b			   */
    aRetForIO();			/*	    -> IO (IOArray a b)	   */
}

primFun(primIOReadArr) {		/* :: IOArray a b -> Int -> IO b   */
    aEvalModel(2+IOArity);
    eval(primArg(1+IOArity));
    aGetElt(2+IOArity);
    IOReturn(top());
}

primFun(primIOWriteArr) {		/* :: IOArray a b -> Int -> b	   */
    aEvalModel(3+IOArity);		/*    -> IO ()			   */
    eval(primArg(2+IOArity));
    aPutElt(3+IOArity,IOArg(1));
    IOReturn(nameUnit);
}

primFun(primIOFreeze) {			/* :: IOArray a b		   */
    declArr;				/*    -> IO (Array a b)		   */
    aEvalModel(1+IOArity);
    aNewCopy(1+IOArity);
    aRetForIO();
}

primFun(primIOArrEq) {		        /* :: IOArray a b                 */
    aEvalModel(1);                      /*    -> IOArray a b -> Bool      */
    aEvalModel(2);
    BoolResult(primArg(1)==primArg(2));
}

#endif /* IO_MONAD */

/* Retire macros used in the implementation of arrays -------------------- */

#undef aNewSet
#undef aNewNil
#undef aNewCopy
#undef aNewLike
#undef aEvalModel
#undef aAssocs
#undef aSetElt
#undef aAddElt
#undef aNullElts
#undef aCopyNull
#undef aMapElts
#undef aGetElt
#undef aPutElt
#undef aElems
#undef aBounds
#undef aGetBounds
#undef updarrRoot
#undef aRetForIO

/*-------------------------------------------------------------------------*/
