#ifndef CHAR_H
#define CHAR_H

/* --------------------------------------------------------------------------
 * Character set handling:
 *
 * Hugs follows Haskell 1.3 in assuming that input uses the ISO-8859-1
 * (Latin-1) character set.  The following code provides methods for
 * classifying input characters according to the lexical structure
 * specified by the report.  Hugs should still accept older programs
 * because ASCII is just a subset of the Latin-1 character set.
 *
 * Extended to Unicode by Dimitry Golubovsky <dimitry@golubovsky.org>.
 * ------------------------------------------------------------------------*/

/* Possibly shorter version of Char for use in arrays. */
#if UNICODE_CHARS
typedef	Char ShortChar;
#else
typedef	unsigned char ShortChar;
#endif

/* --------------------------------------------------------------------------
 * Character classification and other primitives.
 * ------------------------------------------------------------------------*/

extern	Bool		charTabBuilt;
extern  unsigned char   charTable[];

#if UNICODE_CHARS
/* cf HS_CHAR_MAX in HsFFI.h */
#define	MAXCHARVAL	0x10FFFF
#else
#define	MAXCHARVAL	(NUM_LAT1_CHARS-1)
#endif

#define isIn(c,x)       (charTable[(unsigned char)(c)]&(x))
#define isLatin1(c)     (0<=(c) && (c)<NUM_LAT1_CHARS)

/* character classes for lexical analysis */
#define DIGIT           0x01
#define SMALL           0x02
#define LARGE           0x04
#define SYMBOL          0x08
#define IDAFTER         0x10
#define SPACE           0x20
#define PRINT           0x40

/* predicates for the Char module */

#define	isLowerLat1(c)	(isIn((c),SMALL) && (c)!='_')
#define	isUpperLat1(c)	isIn((c),LARGE)
#define	isAlphaLat1(c)	(isIn((c),(SMALL|LARGE)) && (c)!='_')
#define	isAlphaNumLat1(c) (isIn((c),IDAFTER) && (c)!='_' && (c)!='\'')
#define	isPrintLat1(c)	isIn((c),PRINT)

#if UNICODE_CHARS

extern	Bool	isLower		Args((Char));
extern	Bool	isUpper		Args((Char));
extern	Bool	isAlpha		Args((Char));
extern	Bool	isAlphaNum	Args((Char));
extern	Bool	isPrint 	Args((Char));

#else

#define	isLower(c)	isLowerLat1(c)
#define	isUpper(c)	isUpperLat1(c)
#define	isAlpha(c)	isAlphaLat1(c)
#define	isAlphaNum(c)	isAlphaNumLat1(c)
#define	isPrint(c)	isPrintLat1(c)

#endif

extern	Void	initCharTab	Args((void));
extern	Char	toUpper		Args((Char));
extern	Char	toLower		Args((Char));
extern	Char	toTitle		Args((Char));
extern	Int	uni_gencat	Args((Char));

/* --------------------------------------------------------------------------
 * Encoding of Chars as sequences of bytes.
 *
 * The byte encoding is assumed to be an extension of ASCII.  These macros
 * should be used unless you're sure you're dealing with ASCII.
 *
 * We use one of the following encodings for input and output, selected
 * at configuration time:
 *
 * CHAR_ENCODING_LOCALE: the encoding determined by the current setting
 *	of LC_CTYPE.  The encodings in some locales cannot represent all
 *	Unicode characters.  Note that this works only for stateless
 *	encodings; the most widely used exception is ISO 2022-JP.
 *
 * CHAR_ENCODING_UTF8: the UTF-8 encoding, which covers all of Char.
 *
 * (default): Chars in the ISO 8859-1 (Latin-1) subset are encoded directly
 *	as bytes; other Chars are unrepresentable.
 *
 * In an ideal world, we would use Unicode characters uniformly inside
 * the program.  However, to minimize changes to the rest of the program,
 * and to save space, we also encode strings in the text table with the
 * same encoding as we use for I/O (but with special treatment of chars
 * not representable in that encoding: see saveStrChr()/getStrChr().)
 *
 * The interface is:
 *
 *	int MAX_CHAR_ENCODING
 *		Maximum number of bytes to encode a Char.
 *	Char FPutChar(Char c, FILE *f)
 *		Output the encoding of the c, returning c if successful,
 *		EOF if not.
 *	Char FGetChar(FILE *f)
 *		Read and decode a Char, returning the Char if successful,
 *		or EOF if end-of-file or BAD_CHAR on a coding error.
 *	void AddChar(Char c, String &sp)
 *		Add the encoding of c to the string, moving the pointer.
 *		At least MAX_CHAR_ENCODING bytes should be available.
 *	Char ExtractChar(String &sp)
 *		Decode a Char from the string, moving the pointer.
 *		Returns '\0' on end of string or BAD_CHAR on a coding error.
 *	Bool charIsRepresentable(Char c)
 *		Tests whether s can be represented without loss using
 *		the selected encoding.
 *
 * There is in general no way to recover from encoding errors.
 * ------------------------------------------------------------------------*/

#define	BAD_CHAR	0xFFFD

#if CHAR_ENCODING_LOCALE
#  include <wchar.h>
#  include <limits.h>
#  define MAX_CHAR_ENCODING MB_LEN_MAX
#elif CHAR_ENCODING_UTF8
#  define MAX_CHAR_ENCODING 6
#else
#  define MAX_CHAR_ENCODING 1
#endif

#if CHAR_ENCODING
extern	int	fputc_mb	Args((Char, FILE *));
extern	int	fgetc_mb	Args((FILE *));
extern	Void	addc_mb		Args((Char, String *));
extern	Char	extc_mb		Args((String *));

#define	FPutChar(c,f)	fputc_mb(c,f)
#define	FGetChar(f)	fgetc_mb(f)
#define	AddChar(c,s)	addc_mb(c,&s)
#define	ExtractChar(s)	extc_mb(&s)

#if CHAR_ENCODING_UTF8
#define	charIsRepresentable(c)	TRUE
#else
extern	Bool	charIsRepresentable	Args((Char));
#endif

#else /* !CHAR_ENCODING */

#define	FPutChar(c,f)	(fputc(c, f))
#define	FGetChar(f)	(getc(f))
#define	AddChar(c,s)	(*(s)++ = (c))
#define	ExtractChar(s)	(*(unsigned char *)(s)++)

#define	charIsRepresentable(c)	isLatin1(c)

#endif /* !CHAR_ENCODING */

#endif /* CHAR_H */
