#include "BlueHead.h"
#include <stdio.h>


File hOut; //THIS MUST BE SET BY MY PROGRAM
File hIn;

#undef malloc
#undef free

void Neil_Init(File Out, File In)
{
	hOut = Out;
	hIn = In;
}



#include "inflate.h" //Used to be globals.h
struct Globals  G;


//From consts.h
const u16 mask_bits[] = {
    0x0000,
    0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff,
    0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff
};

//From unzpriv.h
struct huft {
    u8 e;                /* number of extra bits or operation */
    u8 b;                /* number of bits in this code or subcode */
    union {
        u16 n;            /* literal, length base, or distance base */
        struct huft *t;   /* pointer to next level of table */
    } v;
};
#  define redirSlide G.area.Slide





//NEIL SPECIFIC END


#define PKZIP_BUG_WORKAROUND    /* PKZIP 1.93a problem--live with it */

#define __INFLATE_C     /* identifies this source module */

/* #define DEBUG */
#define INFMOD          /* tell inflate.h to include code to be compiled */
//#include "inflate.h"

#if (defined(DLL) && !defined(NO_SLIDE_REDIR))
#  define wsize G._wsize    /* wsize is a variable */
#else
#  define wsize WSIZE       /* wsize is a constant */
#endif


inline int NextByte()
{
	unsigned char c;
	FileRead(hIn, &c, 1);
	return c;
}

#ifndef NEXTBYTE        /* default is to simply get a byte from stdin */
#  define NEXTBYTE NextByte()
#endif



#ifndef MESSAGE   /* only used twice, for fixed strings--NOT general-purpose */
#  define MESSAGE(str,len,flag)  fprintf(stderr,(char *)(str))
#endif


void inline FLUSH(int n)
{
	FileWrite(hOut, redirSlide, n);
	CRC(redirSlide, n);}
//#ifndef FLUSH           /* default is to simply write the buffer to stdout */
//#  define FLUSH(n) \
//#endif
/* Warning: the fwrite above might not work on 16-bit compilers, since
   0x8000 might be interpreted as -32,768 by the library function. */

#ifndef Trace
#  ifdef DEBUG
#    define Trace(x) fprintf x
#  else
#    define Trace(x)
#  endif
#endif


/*---------------------------------------------------------------------------*/

/* Function prototypes */
#ifndef OF
#  ifdef __STDC__
#    define OF(a) a
#  else
#    define OF(a) ()
#  endif
#endif /* !OF */
int inflate_codes OF((__GPRO__ struct huft *tl, struct huft *td,
                      int bl, int bd));
static int inflate_stored OF((__GPRO));
static int inflate_fixed OF((__GPRO));
static int inflate_dynamic OF((__GPRO));
static int inflate_block OF((__GPRO__ int *e));


/* The inflate algorithm uses a sliding 32K byte window on the uncompressed
   stream to find repeated byte strings.  This is implemented here as a
   circular buffer.  The index is updated simply by incrementing and then
   and'ing with 0x7fff (32K-1). */
/* It is left to other modules to supply the 32K area.  It is assumed
   to be usable as if it were declared "uch slide[32768];" or as just
   "uch *slide;" and then malloc'ed in the latter case.  The definition
   must be in unzip.h, included above. */


/* unsigned wp;  moved to globals.h */     /* current position in slide */

#define INVALID_CODE 99
#define IS_INVALID_CODE(c)  ((c) == INVALID_CODE)

/* Tables for deflate from PKZIP's appnote.txt. */
static const uint border[] = { /* Order of the bit length code lengths */
        16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15};
static const u16 cplens[] = {  /* Copy lengths for literal codes 257..285 */
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0};
        /* note: see note #13 above about the 258 in this list. */
static const u16 cplext[] = {  /* Extra bits for literal codes 257..285 */
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, INVALID_CODE, INVALID_CODE};
static const u16 cpdist[] = {  /* Copy offsets for distance codes 0..29 */
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
#ifdef USE_DEFLATE64
        8193, 12289, 16385, 24577, 32769, 49153};
#else
        8193, 12289, 16385, 24577};
#endif
static const u16 cpdext[] = {  /* Extra bits for distance codes */
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
#ifdef USE_DEFLATE64
        12, 12, 13, 13, 14, 14};
#else
        12, 12, 13, 13};
#endif
#ifdef USE_DEFLATE64
#  define NUMDISTS 32
#else
#  define NUMDISTS 30
#endif


/* moved to consts.h (included in unzip.c), resp. funzip.c */
#if 0
/* And'ing with mask_bits[n] masks the lower n bits */
ZCONST u16 near mask_bits[] = {
    0x0000,
    0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff,
    0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff
};
#endif /* 0 */


/* Macros for inflate() bit peeking and grabbing.
   The usage is:

        NEEDBITS(j)
        x = b & mask_bits[j];
        DUMPBITS(j)

   where NEEDBITS makes sure that b has at least j bits in it, and
   DUMPBITS removes the bits from b.  The macros use the variable k
   for the number of bits in b.  Normally, b and k are register
   variables for speed and are initialized at the begining of a
   routine that uses these macros from a global bit buffer and count.

   In order to not ask for more bits than there are in the compressed
   stream, the Huffman tables are constructed to only ask for just
   enough bits to make up the end-of-block code (value 256).  Then no
   bytes need to be "returned" to the buffer at the end of the last
   block.  See the huft_build() routine.
 */

/* These have been moved to globals.h */
#if 0
ulg bb;                         /* bit buffer */
unsigned bk;                    /* bits in bit buffer */
#endif

//EOF is not a valid
//#ifndef CHECK_EOF
//#  define CHECK_EOF   /* default as of 5.13/5.2 */
//#endif

#ifndef CHECK_EOF
#  define NEEDBITS(n) {while(k<(n)){b|=((u32)NEXTBYTE)<<k;k+=8;}}
#else
#  define NEEDBITS(n) {while(k<(n)){int c=NEXTBYTE;\
    if(c==EOF){retval=1;goto cleanup_and_exit;}\
    b|=((u32)c)<<k;k+=8;}}
#endif

#define DUMPBITS(n) {b>>=(n);k-=(n);}


/*
   Huffman code decoding is performed using a multi-level table lookup.
   The fastest way to decode is to simply build a lookup table whose
   size is determined by the longest code.  However, the time it takes
   to build this table can also be a factor if the data being decoded
   are not very long.  The most common codes are necessarily the
   shortest codes, so those codes dominate the decoding time, and hence
   the speed.  The idea is you can have a shorter table that decodes the
   shorter, more probable codes, and then point to subsidiary tables for
   the longer codes.  The time it costs to decode the longer codes is
   then traded against the time it takes to make longer tables.

   This results of this trade are in the variables lbits and dbits
   below.  lbits is the number of bits the first level table for literal/
   length codes can decode in one step, and dbits is the same thing for
   the distance codes.  Subsequent tables are also less than or equal to
   those sizes.  These values may be adjusted either when all of the
   codes are shorter than that, in which case the longest code length in
   bits is used, or when the shortest code is *longer* than the requested
   table size, in which case the length of the shortest code in bits is
   used.

   There are two different values for the two tables, since they code a
   different number of possibilities each.  The literal/length table
   codes 286 possible values, or in a flat code, a little over eight
   bits.  The distance table codes 30 possible values, or a little less
   than five bits, flat.  The optimum values for speed end up being
   about one bit more than those, so lbits is 8+1 and dbits is 5+1.
   The optimum values may differ though from machine to machine, and
   possibly even between compilers.  Your mileage may vary.
 */


static const int lbits = 9;    /* bits in base literal/length lookup table */
static const int dbits = 6;    /* bits in base distance lookup table */


#ifndef ASM_INFLATECODES

int inflate_codes(__G__ __GDEF            huft* tl, huft* td, int bl, int bd)
//     __GDEF
//struct huft *tl, *td;   /* literal/length and distance decoder tables */
//int bl, bd;             /* number of bits decoded by tl[] and td[] */
/* inflate (decompress) the codes in a deflated (compressed) block.
   Return an error code or zero if it all goes ok. */
{
  register unsigned e;  /* table entry flag/number of extra bits */
  unsigned n, d;        /* length and index for copy */
  unsigned w;           /* current window position */
  struct huft *t;       /* pointer to table entry */
  unsigned ml, md;      /* masks for bl and bd bits */
  u32 b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */
  int retval = 0;       /* error code returned: initialized to "no error" */


  /* make local copies of globals */
  b = G.bb;                       /* initialize bit buffer */
  k = G.bk;
  w = G.wp;                       /* initialize window position */


  /* inflate the coded data */
  ml = mask_bits[bl];           /* precompute masks for speed */
  md = mask_bits[bd];
  while (1)                     /* do until end of block */
  {
    NEEDBITS((unsigned)bl)
    if ((e = (t = tl + ((unsigned)b & ml))->e) > 16)
      do {
        if (IS_INVALID_CODE(e))
          return 1;
        DUMPBITS(t->b)
        e -= 16;
        NEEDBITS(e)
      } while ((e = (t = t->v.t + ((unsigned)b & mask_bits[e]))->e) > 16);
    DUMPBITS(t->b)
    if (e == 16)                /* then it's a literal */
    {
      redirSlide[w++] = (u8)t->v.n;
      if (w == wsize)
      {
        FLUSH(w);
        w = 0;
      }
    }
    else                        /* it's an EOB or a length */
    {
      /* exit if end of block */
      if (e == 15)
        break;

      /* get length of block to copy */
      NEEDBITS(e)
      n = t->v.n + ((unsigned)b & mask_bits[e]);
      DUMPBITS(e)
#if (defined(USE_DEFLATE64) && !defined(FUNZIP))
      if (n == 258 && G.lrec.compression_method == ENHDEFLATED) {
        /* fetch length bits */
        NEEDBITS(16)
        n = ((unsigned)b & 0xffff) + 3;
        DUMPBITS(16)
      }
#endif

      /* decode distance of block to copy */
      NEEDBITS((unsigned)bd)
      if ((e = (t = td + ((unsigned)b & md))->e) > 16)
        do {
          if (IS_INVALID_CODE(e))
            return 1;
          DUMPBITS(t->b)
          e -= 16;
          NEEDBITS(e)
        } while ((e = (t = t->v.t + ((unsigned)b & mask_bits[e]))->e) > 16);
      DUMPBITS(t->b)
      NEEDBITS(e)
      d = w - t->v.n - ((unsigned)b & mask_bits[e]);
      DUMPBITS(e)

      /* do the copy */
      do {
#if (defined(DLL) && !defined(NO_SLIDE_REDIR))
        if (G.redirect_slide) {/* &= w/ wsize unnecessary & wrong if redirect */
          if (d >= wsize)
            return 1;           /* invalid compressed data */
          n -= (e = (e = wsize - (d > w ? d : w)) > n ? n : e);
        }
        else
#endif
          n -= (e = (e = wsize - ((d &= wsize-1) > w ? d : w)) > n ? n : e);
#ifndef NOMEMCPY
        if (w - d >= e)         /* (this test assumes unsigned comparison) */
        {
          memcpy(redirSlide + w, redirSlide + d, e);
          w += e;
          d += e;
        }
        else                    /* do it slowly to avoid memcpy() overlap */
#endif /* !NOMEMCPY */
          do {
            redirSlide[w++] = redirSlide[d++];
          } while (--e);
        if (w == wsize)
        {
          FLUSH(w);
          w = 0;
        }
      } while (n);
    }
  }


  /* restore the globals from the locals */
  G.wp = w;                       /* restore global window pointer */
  G.bb = b;                       /* restore global bit buffer */
  G.bk = k;

  return retval;
}

#endif /* ASM_INFLATECODES */



static int inflate_stored(__G)
     __GDEF
/* "decompress" an inflated type 0 (stored) block. */
{
  unsigned n;           /* number of bytes in block */
  unsigned w;           /* current window position */
  register u32 b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */
  int retval = 0;       /* error code returned: initialized to "no error" */


  /* make local copies of globals */
  Trace((stderr, "\nstored block"));
  b = G.bb;                       /* initialize bit buffer */
  k = G.bk;
  w = G.wp;                       /* initialize window position */


  /* go to byte boundary */
  n = k & 7;
  DUMPBITS(n);


  /* get the length and its complement */
  NEEDBITS(16)
  n = ((unsigned)b & 0xffff);
  DUMPBITS(16)
  NEEDBITS(16)
  if (n != (unsigned)((~b) & 0xffff))
    return 1;                   /* error in compressed data */
  DUMPBITS(16)


  /* read and output the compressed data */
  while (n--)
  {
    NEEDBITS(8)
    redirSlide[w++] = (u8)b;
    if (w == wsize)
    {
      FLUSH(w);
      w = 0;
    }
    DUMPBITS(8)
  }


  /* restore the globals from the locals */
  G.wp = w;                       /* restore global window pointer */
  G.bb = b;                       /* restore global bit buffer */
  G.bk = k;

  return retval;
}


/* Globals for literal tables (built once) */
/* Moved to globals.h                      */
#if 0
struct huft *fixed_tl = (struct huft *)NULL;
struct huft *fixed_td;
int fixed_bl, fixed_bd;
#endif

int huft_build(__GPRO__ const uint *b, uint n, uint s, const u16* d, const u16* e, struct huft **t, int *m);
int huft_free(huft* t);          /* inflate.c */


static int inflate_fixed(__G)
     __GDEF
/* decompress an inflated type 1 (fixed Huffman codes) block.  We should
   either replace this with a custom decoder, or at least precompute the
   Huffman tables. */
{
  /* if first time, set up tables for fixed blocks */
  Trace((stderr, "\nliteral block"));
  if (G.fixed_tl == (struct huft *)NULL)
  {
    int i;                /* temporary variable */
    unsigned l[288];      /* length list for huft_build */

    /* literal table */
    for (i = 0; i < 144; i++)
      l[i] = 8;
    for (; i < 256; i++)
      l[i] = 9;
    for (; i < 280; i++)
      l[i] = 7;
    for (; i < 288; i++)          /* make a complete, but wrong code set */
      l[i] = 8;
    G.fixed_bl = 7;
    if ((i = huft_build(__G__ l, 288, 257, cplens, cplext,
                        &G.fixed_tl, &G.fixed_bl)) != 0)
    {
      G.fixed_tl = (struct huft *)NULL;
      return i;
    }

    /* distance table */
    for (i = 0; i < NUMDISTS; i++)      /* make an incomplete code set */
      l[i] = 5;
    G.fixed_bd = 5;
    if ((i = huft_build(__G__ l, NUMDISTS, 0, cpdist, cpdext,
                        &G.fixed_td, &G.fixed_bd)) > 1)
    {
      huft_free(G.fixed_tl);
      G.fixed_td = G.fixed_tl = (struct huft *)NULL;
      return i;
    }
  }

  /* decompress until an end-of-block code */
  return inflate_codes(__G__ G.fixed_tl, G.fixed_td,
                             G.fixed_bl, G.fixed_bd);
}



static int inflate_dynamic(__G)
  __GDEF
/* decompress an inflated type 2 (dynamic Huffman codes) block. */
{
  int i;                /* temporary variables */
  unsigned j;
  unsigned l;           /* last length */
  unsigned m;           /* mask for bit lengths table */
  unsigned n;           /* number of lengths to get */
  struct huft *tl;      /* literal/length code table */
  struct huft *td;      /* distance code table */
  int bl;               /* lookup bits for tl */
  int bd;               /* lookup bits for td */
  unsigned nb;          /* number of bit length codes */
  unsigned nl;          /* number of literal/length codes */
  unsigned nd;          /* number of distance codes */
#ifdef PKZIP_BUG_WORKAROUND
  unsigned ll[288+32]; /* literal/length and distance code lengths */
#else
  unsigned ll[286+NUMDISTS]; /* literal/length and distance code lengths */
#endif
  register u32 b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */
  int retval = 0;       /* error code returned: initialized to "no error" */


  /* make local bit buffer */
  Trace((stderr, "\ndynamic block"));
  b = G.bb;
  k = G.bk;


  /* read in table lengths */
  NEEDBITS(5)
  nl = 257 + ((unsigned)b & 0x1f);      /* number of literal/length codes */
  DUMPBITS(5)
  NEEDBITS(5)
  nd = 1 + ((unsigned)b & 0x1f);        /* number of distance codes */
  DUMPBITS(5)
  NEEDBITS(4)
  nb = 4 + ((unsigned)b & 0xf);         /* number of bit length codes */
  DUMPBITS(4)
#ifdef PKZIP_BUG_WORKAROUND
  if (nl > 288 || nd > 32)
#else
  if (nl > 286 || nd > NUMDISTS)
#endif
    return 1;                   /* bad lengths */


  /* read in bit-length-code lengths */
  for (j = 0; j < nb; j++)
  {
    NEEDBITS(3)
    ll[border[j]] = (unsigned)b & 7;
    DUMPBITS(3)
  }
  for (; j < 19; j++)
    ll[border[j]] = 0;


  /* build decoding table for trees--single level, 7 bit lookup */
  bl = 7;
  retval = huft_build(__G__ ll, 19, 19, NULL, NULL, &tl, &bl);
  if (bl == 0)                  /* no bit lengths */
    retval = 1;
  if (retval)
  {
    if (retval == 1)
      huft_free(tl);
    return retval;              /* incomplete code set */
  }


  /* read in literal and distance code lengths */
  n = nl + nd;
  m = mask_bits[bl];
  i = l = 0;
  while ((unsigned)i < n)
  {
    NEEDBITS((unsigned)bl)
    j = (td = tl + ((unsigned)b & m))->b;
    DUMPBITS(j)
    j = td->v.n;
    if (j < 16)                 /* length of code in bits (0..15) */
      ll[i++] = l = j;          /* save last length in l */
    else if (j == 16)           /* repeat last length 3 to 6 times */
    {
      NEEDBITS(2)
      j = 3 + ((unsigned)b & 3);
      DUMPBITS(2)
      if ((unsigned)i + j > n)
        return 1;
      while (j--)
        ll[i++] = l;
    }
    else if (j == 17)           /* 3 to 10 zero length codes */
    {
      NEEDBITS(3)
      j = 3 + ((unsigned)b & 7);
      DUMPBITS(3)
      if ((unsigned)i + j > n)
        return 1;
      while (j--)
        ll[i++] = 0;
      l = 0;
    }
    else                        /* j == 18: 11 to 138 zero length codes */
    {
      NEEDBITS(7)
      j = 11 + ((unsigned)b & 0x7f);
      DUMPBITS(7)
      if ((unsigned)i + j > n)
        return 1;
      while (j--)
        ll[i++] = 0;
      l = 0;
    }
  }


  /* free decoding table for trees */
  huft_free(tl);


  /* restore the global bit buffer */
  G.bb = b;
  G.bk = k;


  /* build the decoding tables for literal/length and distance codes */
  bl = lbits;
  retval = huft_build(__G__ ll, nl, 257, cplens, cplext, &tl, &bl);
  if (bl == 0)                  /* no literals or lengths */
    retval = 1;
  if (retval)
  {
    if (retval == 1) {
      huft_free(tl);
    }
    return retval;              /* incomplete code set */
  }
  bd = dbits;
  retval = huft_build(__G__ ll + nl, nd, 0, cpdist, cpdext, &td, &bd);
#ifdef PKZIP_BUG_WORKAROUND
  if (retval == 1)
    retval = 0;
#endif
  if (bd == 0 && nl > 257)    /* lengths but no distances */
    retval = 1;
  if (retval)
  {
    if (retval == 1) {
      huft_free(td);
    }
    huft_free(tl);
    return retval;
  }

  /* decompress until an end-of-block code */
  retval = inflate_codes(__G__ tl, td, bl, bd);

  /* free the decoding tables, return */
  huft_free(tl);
  huft_free(td);
  return retval;
}



static int inflate_block(__G__ __GDEF      int* e)
               /* last block flag */
/* decompress an inflated block */
{
  unsigned t;           /* block type */
  register u32 b;       /* bit buffer */
  register unsigned k;  /* number of bits in bit buffer */
  int retval = 0;       /* error code returned: initialized to "no error" */


  /* make local bit buffer */
  b = G.bb;
  k = G.bk;


  /* read in last block bit */
  NEEDBITS(1)
  *e = (int)b & 1;
  DUMPBITS(1)


  /* read in block type */
  NEEDBITS(2)
  t = (unsigned)b & 3;
  DUMPBITS(2)


  /* restore the global bit buffer */
  G.bb = b;
  G.bk = k;


  /* inflate that block type */
  if (t == 2)
    return inflate_dynamic(__G);
  if (t == 0)
    return inflate_stored(__G);
  if (t == 1)
    return inflate_fixed(__G);


  /* bad block type */
  retval = 2;

  return retval;
}



int inflate(__G)
     __GDEF
/* decompress an inflated entry */
{
  int e;                /* last block flag */
  int r;                /* result code */
#ifdef DEBUG
  unsigned h = 0;       /* maximum struct huft's malloc'ed */
#endif

#if (defined(DLL) && !defined(NO_SLIDE_REDIR))
  if (G.redirect_slide)
    wsize = G.redirect_size, redirSlide = G.redirect_buffer;
  else
    wsize = WSIZE, redirSlide = slide;   /* how they're #defined if !DLL */
#endif

  /* initialize window, bit buffer */
  G.wp = 0;
  G.bk = 0;
  G.bb = 0;


  /* decompress until the last block */
  do {
#ifdef DEBUG
    G.hufts = 0;
#endif
    if ((r = inflate_block(__G__ &e)) != 0)
      return r;
#ifdef DEBUG
    if (G.hufts > h)
      h = G.hufts;
#endif
  } while (!e);

  Trace((stderr, "\n%u bytes in Huffman tables (%d/entry)\n",
         h * sizeof(struct huft), sizeof(struct huft)));

  /* flush out redirSlide and return (success, unless final FLUSH failed) */
  FLUSH(G.wp);
  return 0;
}



int inflate_free(__G)
     __GDEF
{
  if (G.fixed_tl != (struct huft *)NULL)
  {
    huft_free(G.fixed_td);
    huft_free(G.fixed_tl);
    G.fixed_td = G.fixed_tl = (struct huft *)NULL;
  }
  return 0;
}


/*
 * GRR:  moved huft_build() and huft_free() down here; used by explode()
 *       and fUnZip regardless of whether USE_ZLIB defined or not
 */


/* If BMAX needs to be larger than 16, then h and x[] should be ulg. */
#define BMAX 16         /* maximum bit length of any code (16 for explode) */
#define N_MAX 288       /* maximum number of codes in any set */


int huft_build(__G__ __GDEF   const uint* b, uint n, uint s, const u16* d, const u16* e, huft** t, int* m)
//  __GDEF
//  const unsigned *b;   /* code lengths in bits (all assumed <= BMAX) */
//  unsigned n;           /* number of codes (assumed <= N_MAX) */
//  unsigned s;           /* number of simple-valued codes (0..s-1) */
//  const u16 *d;        /* list of base values for non-simple codes */
//  const u16 *e;        /* list of extra bits for non-simple codes */
//  struct huft **t;      /* result: starting table */
//  int *m;               /* maximum lookup bits, returns actual */
/* Given a list of code lengths and a maximum table size, make a set of
   tables to decode that set of codes.  Return zero on success, one if
   the given code set is incomplete (the tables are still built in this
   case), two if the input is invalid (all zero length codes or an
   oversubscribed set of lengths), and three if not enough memory.
   The code with value 256 is special, and the tables are constructed
   so that no bits beyond that code are fetched when that code is
   decoded. */
{
  unsigned a;                   /* counter for codes of length k */
  unsigned c[BMAX+1];           /* bit length count table */
  unsigned el;                  /* length of EOB code (value 256) */
  unsigned f;                   /* i repeats in table every f entries */
  int g;                        /* maximum code length */
  int h;                        /* table level */
  register unsigned i;          /* counter, current code */
  register unsigned j;          /* counter */
  register int k;               /* number of bits in current code */
  int lx[BMAX+1];               /* memory for l[-1..BMAX-1] */
  int *l = lx+1;                /* stack of bits per table */
  register unsigned *p;         /* pointer into c[], b[], or v[] */
  register struct huft *q;      /* points to current table */
  struct huft r;                /* table entry for structure assignment */
  struct huft *u[BMAX];         /* table stack */
  unsigned v[N_MAX];            /* values in order of bit length */
  register int w;               /* bits before this table == (l * h) */
  unsigned x[BMAX+1];           /* bit offsets, then code stack */
  unsigned *xp;                 /* pointer into x */
  int y;                        /* number of dummy codes added */
  unsigned z;                   /* number of entries in current table */


  /* Generate counts for each bit length */
  el = n > 256 ? b[256] : BMAX; /* set length of EOB code, if any */
  memset((char *)c, 0, sizeof(c));
  p = (unsigned *)b;  i = n;
  do {
    c[*p]++; p++;               /* assume all entries <= BMAX */
  } while (--i);
  if (c[0] == n)                /* null input--all zero length codes */
  {
    *t = (struct huft *)NULL;
    *m = 0;
    return 0;
  }


  /* Find minimum and maximum length, bound *m by those */
  for (j = 1; j <= BMAX; j++)
    if (c[j])
      break;
  k = j;                        /* minimum code length */
  if ((unsigned)*m < j)
    *m = j;
  for (i = BMAX; i; i--)
    if (c[i])
      break;
  g = i;                        /* maximum code length */
  if ((unsigned)*m > i)
    *m = i;


  /* Adjust last length count to fill out codes, if needed */
  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j]) < 0)
      return 2;                 /* bad input: more codes than bits */
  if ((y -= c[i]) < 0)
    return 2;
  c[i] += y;


  /* Generate starting offsets into the value table for each length */
  x[1] = j = 0;
  p = c + 1;  xp = x + 2;
  while (--i) {                 /* note that i == g from above */
    *xp++ = (j += *p++);
  }


  /* Make a table of values in order of bit lengths */
  memset((char *)v, 0, sizeof(v));
  p = (unsigned *)b;  i = 0;
  do {
    if ((j = *p++) != 0)
      v[x[j]++] = i;
  } while (++i < n);
  n = x[g];                     /* set n to length of v */


  /* Generate the Huffman codes and for each, make the table entries */
  x[0] = i = 0;                 /* first Huffman code is zero */
  p = v;                        /* grab values in bit order */
  h = -1;                       /* no tables yet--level -1 */
  w = l[-1] = 0;                /* no bits decoded yet */
  u[0] = (struct huft *)NULL;   /* just to keep compilers happy */
  q = (struct huft *)NULL;      /* ditto */
  z = 0;                        /* ditto */

  /* go through the bit lengths (k already is bits in shortest code) */
  for (; k <= g; k++)
  {
    a = c[k];
    while (a--)
    {
      /* here i is the Huffman code of length k bits for value *p */
      /* make tables up to required level */
      while (k > w + l[h])
      {
        w += l[h++];            /* add bits already decoded */

        /* compute minimum size table less than or equal to *m bits */
        z = (z = g - w) > (unsigned)*m ? *m : z;        /* upper limit */
        if ((f = 1 << (j = k - w)) > a + 1)     /* try a k-w bit table */
        {                       /* too few codes for k-w bit table */
          f -= a + 1;           /* deduct codes from patterns left */
          xp = c + k;
          while (++j < z)       /* try smaller tables up to z bits */
          {
            if ((f <<= 1) <= *++xp)
              break;            /* enough codes to use up j bits */
            f -= *xp;           /* else deduct codes from patterns */
          }
        }
        if ((unsigned)w + j > el && (unsigned)w < el)
          j = el - w;           /* make EOB code end at table */
        z = 1 << j;             /* table entries for j-bit table */
        l[h] = j;               /* set table size in stack */

        /* allocate and link in new table */
        if ((q = (struct huft *)malloc((z + 1)*sizeof(struct huft))) ==
            (struct huft *)NULL)
        {
          if (h)
            huft_free(u[0]);
          return 3;             /* not enough memory */
        }
#ifdef DEBUG
        G.hufts += z + 1;         /* track memory usage */
#endif
        *t = q + 1;             /* link to list for huft_free() */
        *(t = &(q->v.t)) = (struct huft *)NULL;
        u[h] = ++q;             /* table starts after link */

        /* connect to last table, if there is one */
        if (h)
        {
          x[h] = i;             /* save pattern for backing up */
          r.b = (u8)l[h-1];    /* bits to dump before this table */
          r.e = (u8)(16 + j);  /* bits in this table */
          r.v.t = q;            /* pointer to this table */
          j = (i & ((1 << w) - 1)) >> (w - l[h-1]);
          u[h-1][j] = r;        /* connect to last table */
        }
      }

      /* set up table entry in r */
      r.b = (u8)(k - w);
      if (p >= v + n)
        r.e = INVALID_CODE;     /* out of values--invalid code */
      else if (*p < s)
      {
        r.e = (u8)(*p < 256 ? 16 : 15);  /* 256 is end-of-block code */
        r.v.n = (u16)*p++;                /* simple code is just the value */
      }
      else
      {
        r.e = (u8)e[*p - s];   /* non-simple--look up in lists */
        r.v.n = d[*p++ - s];
      }

      /* fill code-like entries with r */
      f = 1 << (k - w);
      for (j = i >> w; j < z; j += f)
        q[j] = r;

      /* backwards increment the k-bit code i */
      for (j = 1 << (k - 1); i & j; j >>= 1)
        i ^= j;
      i ^= j;

      /* backup over finished tables */
      while ((i & ((1 << w) - 1)) != x[h])
        w -= l[--h];            /* don't need to update q */
    }
  }


  /* return actual size of base table */
  *m = l[0];


  /* Return true (1) if we were given an incomplete table */
  return y != 0 && g != 1;
}



int huft_free(huft* t)
//struct huft *t;         /* table to free */
/* Free the malloc'ed tables built by huft_build(), which makes a linked
   list of the tables it made, with the links in a dummy first entry of
   each table. */
{
  register struct huft *p, *q;


  /* Go through linked list, freeing from the malloced (t[-1]) address. */
  p = t;
  while (p != (struct huft *)NULL)
  {
    q = (--p)->v.t;
    free((void*) p);
    p = q;
  }
  return 0;
}
