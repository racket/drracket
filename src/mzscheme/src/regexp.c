/*
 * regcomp and regexec -- regsub and regerror are elsewhere
 * @(#)regexp.c	1.3 of 18 April 87
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 *
 * from Vladimir Tsyshevsky:
 *  additional optional parameter `offset' in `regexp-match'
 *  and `regexp-match-positions'
 */

#include "schpriv.h"

#ifndef NO_REGEXP_UTILS

#include <stdio.h>
#include <string.h>

typedef struct regexp {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  long nsubexp;
  long regsize;
  char regstart;		/* Internal use only. */
  char reganch;			/* Internal use only. */
  long regmust;                 /* Internal use only: => pointer relative to self */
  long regmlen;			/* Internal use only. */
  char program[1];		/* Unwarranted chumminess with compiler. */
} regexp;

static regexp *regcomp(char *, int);
static int regexec(regexp *, char *, int, char **, char **);

/* 156 is octal 234: */
#define MAGIC   156

#ifdef MZ_PRECISE_GC
# define MZREGISTER /**/
#else
# define MZREGISTER register
#endif

/*
 * The "internal use only" fields in regexp.h are present to pass info from
 * compile to execute that permits the execute phase to run lots faster on
 * simple cases.  They are:
 *
 * regstart	char that must begin a match; '\0' if none obvious
 * reganch	is the match anchored (at beginning-of-line only)?
 * regmust	string (pointer into program) that match must include, or NULL
 * regmlen	length of regmust string
 *
 * Regstart and reganch permit very fast decisions on suitable starting points
 * for a match, cutting down the work a lot.  Regmust permits fast rejection
 * of lines that cannot possibly match.  The regmust tests are costly enough
 * that regcomp() supplies a regmust only if the r.e. contains something
 * potentially expensive (at present, the only such thing detected is * or +
 * at the start of the r.e., which can involve a lot of backup).  Regmlen is
 * supplied because the test in regexec() needs it and regcomp() is computing
 * it anyway.
 */

/*
 * Structure for regexp "program".  This is essentially a linear encoding
 * of a nondeterministic finite-state machine (aka syntax charts or
 * "railroad normal form" in parsing technology).  Each node is an opcode
 * plus a "next" pointer, possibly plus an operand.  "Next" pointers of
 * all nodes except BRANCH implement concatenation; a "next" pointer with
 * a BRANCH on both ends of it is connecting two alternatives.  (Here we
 * have one of the subtle syntax dependencies:  an individual BRANCH (as
 * opposed to a collection of them) is never concatenated with anything
 * because of operator precedence.)  The operand of some types of node is
 * a literal string; for others, it is a node leading into a sub-FSM.  In
 * particular, the operand of a BRANCH node is the first node of the branch.
 * (NB this is *not* a tree structure:  the tail of the branch connects
 * to the thing following the set of BRANCHes.)  The opcodes are:
 */

/* definition	number	opnd?	meaning */
#define	END	0	/* no	End of program. */
#define	BOL	1	/* no	Match "" at beginning of line. */
#define	EOL	2	/* no	Match "" at end of line. */
#define	ANY	3	/* no	Match any one character. */
#define	ANYOF	4	/* str	Match any character in this string. */
#define	ANYBUT	5	/* str	Match any character not in this string. */
#define	BRANCH	6	/* node	Match this alternative, or the next... */
#define	BACK	7	/* no	Match "", "next" ptr points backward. */
#define	EXACTLY	8	/* str	Match this string. */
#define	NOTHING	9	/* no	Match empty string. */
#define	STAR	10	/* node	Match this (simple) thing 0 or more times. */
#define	PLUS	11	/* node	Match this (simple) thing 1 or more times. */
#define OPENN   12      /* like OPEN, but with an n >= 50 */
#define CLOSEN  13      /* like CLOSE, but with an n >= 50 */
#define	OPEN	20	/* no	Mark this point in input as start of #n. */
/*	OPEN+1 is number 1, etc. */
#define	CLOSE	70	/* no	Analogous to OPEN. */

# define OPSTR(o) (o + 2)
# define OPLEN(o) ((int)(((unsigned char *)o)[0] << 8) | (((unsigned char *)o)[1]))

/*
 * Opcode notes:
 *
 * BRANCH	The set of branches constituting a single choice are hooked
 *		together with their "next" pointers, since precedence prevents
 *		anything being concatenated to any individual branch.  The
 *		"next" pointer of the last BRANCH in a choice points to the
 *		thing following the whole choice.  This is also where the
 *		final "next" pointer of each individual branch points; each
 *		branch starts with the operand node of a BRANCH node.
 *
 * BACK		Normal "next" pointers all implicitly point forward; BACK
 *		exists to make loop structures possible.
 *
 * STAR,PLUS	'?', and complex '*' and '+', are implemented as circular
 *		BRANCH structures using BACK.  Simple cases (one character
 *		per match) are implemented with STAR and PLUS for speed
 *		and to minimize recursive plunges.
 *
 * OPEN,CLOSE	...are numbered at compile time.
 */

/*
 * A node is one char of opcode followed by two chars of "next" pointer.
 * "Next" pointers are stored as two 8-bit pieces, high order first.  The
 * value is a positive offset from the opcode of the node containing it.
 * An operand, if any, simply follows the node.  (Note that much of the
 * code generation knows about this implicit relationship.)
 *
 * Using two bytes for the "next" pointer is vast overkill for most things,
 * but allows patterns to get big without disasters.
 */
#define	OP(p)	(*(p))
#define	NEXT(p)	(((*((p)+1)&255)<<8) + (*((p)+2)&255))
#define	OPERAND(p)	((p) + 3)

/*
 * See regmagic.h for one further detail of program structure.
 */


/*
 * Utility definitions.
 */
#ifndef CHARBITS
#define	UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define	UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

#define	FAIL(m)	{ regerror(m); return(NULL); }
#define	ISMULT(c)	((c) == '*' || (c) == '+' || (c) == '?')
#define	META	"^$.[()|?+*\\"

/*
 * Flags to be passed up and down.
 */
#define	HASWIDTH	01	/* Known never to match null string. */
#define	SIMPLE		02	/* Simple enough to be STAR/PLUS operand. */
#define	SPSTART		04	/* Starts with * or +. */
#define	WORST		0	/* Worst case. */

/*
 * Global work variables for regcomp().
 */
static char *regparse, *regparse_end; /* Input-scan pointer. */
static int regnpar;		/* () count. */
static char regdummy;
static char *regcode;		/* Code-emit pointer; &regdummy = don't. */
static long regsize;		/* Code size. */

/*
 * Forward declarations for regcomp()'s friends.
 */
#ifndef STATIC
#define	STATIC	static
#endif
STATIC char *reg(int, int *);
STATIC char *regbranch(int *);
STATIC char *regpiece(int *);
STATIC char *regatom(int *);
STATIC char *regnode(char);
STATIC char *regnext(char *);
STATIC void regc(char);
STATIC void reginsert(char, char *);
STATIC void regtail(char *, char *);
STATIC void regoptail(char *, char *);
STATIC int regstrcspn(char *, char *, char *);

#ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
#endif

static void
regerror(char *s)
{
  scheme_raise_exn(MZEXN_MISC,
		   "regexp: %s", s);
}

/*
 - regcomp - compile a regular expression into internal code
 *
 * We can't allocate space until we know how big the compiled form will be,
 * but we can't compile it (and thus know how big it is) until we've got a
 * place to put the code.  So we cheat:  we compile it twice, once with code
 * generation turned off and size counting turned on, and once "for real".
 * This also means that we don't allocate space until we are sure that the
 * thing really will compile successfully, and we never have to move the
 * code and thus invalidate pointers into it.  (Note that it has to be in
 * one piece because free() must be able to free it all.)
 *
 * Beware that the optimization-preparation code in here knows about some
 * of the structure of the compiled regexp.
 */
static regexp *
regcomp(char *exp, int explen)
{
  MZREGISTER regexp *r;
  MZREGISTER char *scan;
  MZREGISTER char *longest;
  MZREGISTER int len;
  int flags;
  
  if (exp == NULL)
    FAIL("NULL argument");
  
  /* First pass: determine size, legality. */
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
  regsize = 0L;
  regcode = &regdummy;
  regc(MAGIC);
  if (reg(0, &flags) == NULL)
    return(NULL);
  
  /* Small enough for pointer-storage convention? */
  if (regsize >= 32767L)		/* Probably could be 65535L. */
    FAIL("regexp too big");
  
  /* Allocate space. */
  r = (regexp *)scheme_malloc_tagged(sizeof(regexp) + (unsigned)regsize);

  /* Senora/Precise GC: reset the globals */
  regparse = exp;
  regparse_end = exp + explen;

  if (r == NULL)
    FAIL("out of space");
  
  r->type = scheme_regexp_type;
  
  r->regsize = regsize;

  r->nsubexp = regnpar;
  
  /* Second pass: emit code. */
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
  regcode = r->program;
  regc(MAGIC);
  if (reg(0, &flags) == NULL)
    return(NULL);
  
  /* Dig out information for optimizations. */
  r->regstart = '\0';	/* Worst-case defaults. */
  r->reganch = 0;
  r->regmust = -1;
  r->regmlen = 0;
  scan = r->program+1;			/* First BRANCH. */
  if (OP(regnext(scan)) == END) {	/* Only one top-level choice. */
    scan = OPERAND(scan);
    
    /* Starting-point info. */
    if (OP(scan) == EXACTLY)
      r->regstart = *(OPSTR(OPERAND(scan)));
    else if (OP(scan) == BOL)
      r->reganch++;
    
    /*
     * If there's something expensive in the r.e., find the
     * longest literal string that must appear and make it the
     * regmust.  Resolve ties in favor of later strings, since
     * the regstart check works with the beginning of the r.e.
     * and avoiding duplication strengthens checking.  Not a
     * strong reason, but sufficient in the absence of others.
     */
    if (flags&SPSTART) {
      longest = NULL;
      len = 0;
      for (; scan != NULL; scan = regnext(scan)) {
	if (OP(scan) == EXACTLY && OPLEN(OPERAND(scan)) >= len) {
	  /* Skip regmust if it contains a null character: */
	  char *ls = OPSTR(OPERAND(scan));
	  int ll = OPLEN(OPERAND(scan)), i;
	  for (i = 0; i < ll; i++) {
	    if (!ls[i])
	      break;
	  }
	  if (i >= ll) {
	    longest = ls;
	    len = ll;
	  }
	}
      }
      if (longest)
	r->regmust = longest - (char *)r;
      r->regmlen = len;
    }
  }
  
  return(r);
}

/*
   - reg - regular expression, i.e. main body or parenthesized thing
   *
   * Caller must absorb opening parenthesis.
   *
   * Combining parenthesis handling with the base level of regular expression
   * is a trifle forced, but the need to tie the tails of the branches to what
   * follows makes it hard to avoid.
   */
static char *
reg(int paren, int *flagp)
{
  MZREGISTER char *ret;
  MZREGISTER char *br;
  MZREGISTER char *ender;
  MZREGISTER int parno = 0;
  int flags;

  *flagp = HASWIDTH;		/* Tentatively. */

  /* Make an OPEN node, if parenthesized. */
  if (paren) {
    parno = regnpar;
    regnpar++;
    if (OPEN + parno >= CLOSE) {
      ret = regcode;
      regc(parno >> 8);
      regc(parno & 255);
      reginsert(OPENN, ret);
    } else {
      ret = regnode(OPEN+parno);
    }
  } else
    ret = NULL;

  /* Pick up the branches, linking them together. */
  br = regbranch(&flags);
  if (br == NULL)
    return(NULL);
  if (ret != NULL)
    regtail(ret, br);		/* OPEN -> first. */
  else
    ret = br;
  if (!(flags&HASWIDTH))
    *flagp &= ~HASWIDTH;
  *flagp |= flags&SPSTART;
  while (*regparse == '|') {
    regparse++;
    br = regbranch(&flags);
    if (br == NULL)
      return(NULL);
    regtail(ret, br);		/* BRANCH -> BRANCH. */
    if (!(flags&HASWIDTH))
      *flagp &= ~HASWIDTH;
    *flagp |= flags&SPSTART;
  }

  /* Make a closing node, and hook it on the end. */
  if (paren) {
    if (OPEN + parno >= CLOSE) {
      ender = regcode;
      regc(parno >> 8);
      regc(parno & 255);
      reginsert(CLOSEN, ender);
    } else
      ender = regnode(CLOSE+parno);
  } else {
    ender = regnode(END);	
  }
  regtail(ret, ender);

  /* Hook the tails of the branches to the closing node. */
  for (br = ret; br != NULL; br = regnext(br)) {
    regoptail(br, ender);
  }

  /* Check for proper termination. */
  if (paren && *regparse++ != ')') {
    FAIL("missing closing parenthesis in pattern");
  } else if (!paren && regparse != regparse_end) {
    if (*regparse == ')') {
      FAIL("extra closing parenthesis in pattern");
    } else
      FAIL("junk on end");	/* "Can't happen". */
    /* NOTREACHED */
  }

  return(ret);
}

/*
   - regbranch - one alternative of an | operator
   *
   * Implements the concatenation operator.
   */
static char *
regbranch(int *flagp)
{
  MZREGISTER char *ret;
  MZREGISTER char *chain;
  MZREGISTER char *latest;
  int flags;

  *flagp = WORST;		/* Tentatively. */

  ret = regnode(BRANCH);
  chain = NULL;
  while (regparse != regparse_end && *regparse != '|' && *regparse != ')') {
    latest = regpiece(&flags);
    if (latest == NULL)
      return(NULL);
    *flagp |= flags&HASWIDTH;
    if (chain == NULL)		/* First piece. */
      *flagp |= flags&SPSTART;
    else
      regtail(chain, latest);
    chain = latest;
  }
  if (chain == NULL)		/* Loop ran zero times. */
    (void) regnode(NOTHING);

  return(ret);
}

/*
   - regpiece - something followed by possible [*+?]
   *
   * Note that the branching code sequences used for ? and the general cases
   * of * and + are somewhat optimized:  they use the same NOTHING node as
   * both the endmarker for their branch list and the body of the last branch.
   * It might seem that this node could be dispensed with entirely, but the
   * endmarker role is not redundant.
   */
static char *
regpiece(int *flagp)
{
  MZREGISTER char *ret;
  MZREGISTER char op;
  MZREGISTER char *next;
  int flags;

  ret = regatom(&flags);
  if (ret == NULL)
    return(NULL);

  op = *regparse;
  if (!ISMULT(op)) {
    *flagp = flags;
    return(ret);
  }

  if (!(flags&HASWIDTH) && op != '?')
    FAIL("* or + operand could be empty");
  *flagp = (op != '+') ? (WORST|SPSTART) : (WORST|HASWIDTH);

  if (op == '*' && (flags&SIMPLE))
    reginsert(STAR, ret);
  else if (op == '*') {
    /* Emit x* as (x&|), where & means "self". */
    reginsert(BRANCH, ret);	/* Either x */
    regoptail(ret, regnode(BACK)); /* and loop */
    regoptail(ret, ret);	/* back */
    regtail(ret, regnode(BRANCH)); /* or */
    regtail(ret, regnode(NOTHING)); /* null. */
  } else if (op == '+' && (flags&SIMPLE))
    reginsert(PLUS, ret);
  else if (op == '+') {
    /* Emit x+ as x(&|), where & means "self". */
    next = regnode(BRANCH);	/* Either */
    regtail(ret, next);
    regtail(regnode(BACK), ret); /* loop back */
    regtail(next, regnode(BRANCH)); /* or */
    regtail(ret, regnode(NOTHING)); /* null. */
  } else if (op == '?') {
    /* Emit x? as (x|) */
    reginsert(BRANCH, ret);	/* Either x */
    regtail(ret, regnode(BRANCH)); /* or */
    next = regnode(NOTHING);	/* null. */
    regtail(ret, next);
    regoptail(ret, next);
  }
  regparse++;
  if (ISMULT(*regparse))
    FAIL("nested *, ?, or + in pattern");

  return(ret);
}

/*
   - regatom - the lowest level
   *
   * Optimization:  gobbles an entire sequence of ordinary characters so that
   * it can turn them into a single node, which is smaller to store and
   * faster to run.  Backslashed characters are exceptions, each becoming a
   * separate node; the code is simpler that way and it's not worth fixing.
 */
static char *
regatom(int *flagp)
{
	MZREGISTER char *ret;
	int flags;

	*flagp = WORST;		/* Tentatively. */

	switch (*regparse++) {
	case '^':
	  ret = regnode(BOL);
	  break;
	case '$':
	  ret = regnode(EOL);
	  break;
	case '.':
	  ret = regnode(ANY);
	  *flagp |= HASWIDTH|SIMPLE;
	  break;
	case '[': {
	  MZREGISTER int xclass;
	  MZREGISTER int classend, len;
	  char *l0, *l1;

	  if (*regparse == '^') { /* Complement of range. */
	    ret = regnode(ANYBUT);
	    regparse++;
	  } else
	    ret = regnode(ANYOF);
	  len = 0;
	  l0 = regcode;
	  regc(0);
	  l1 = regcode;
	  regc(0);
	  if (*regparse == ']' || *regparse == '-') {
	    regc(*regparse++);
	    len++;
	  }
	  while (regparse != regparse_end && *regparse != ']') {
	    if (*regparse == '-') {
	      regparse++;
	      if (*regparse == ']' || regparse == regparse_end) {
		regc('-');
		len++;
	      } else {
		xclass = UCHARAT(regparse-2)+1;
		classend = UCHARAT(regparse);
		if (xclass > classend+1)
		  FAIL("invalid range within square brackets in pattern");
		for (; xclass <= classend; xclass++) {
		  regc(xclass);
		  len++;
		}
		regparse++;
	      }
	    } else {
	      regc(*regparse++);
	      len++;
	    }
	  }
	  if (*regparse != ']')
	    FAIL("missing closing square bracket in pattern");
	  regparse++;
	  *l0 = (len >> 8);
	  *l1 = (len & 255);
	  *flagp |= HASWIDTH|SIMPLE;
	}
	  break;
	case '(':
	  ret = reg(1, &flags);
	  if (ret == NULL)
	    return(NULL);
	  *flagp |= flags&(HASWIDTH|SPSTART);
	  break;
	case '|':
	case ')':
	  FAIL("internal urp");	/* Supposed to be caught earlier. */
	  break;
	case '?':
	case '+':
	case '*':
	  FAIL("?, +, or * follows nothing in pattern");
	  break;
	case '\\':
	  if (regparse == regparse_end)
	    FAIL("trailing backslash in pattern");
	  ret = regnode(EXACTLY);
	  regc(0);
	  regc(1);
	  regc(*regparse++);
	  *flagp |= HASWIDTH|SIMPLE;
	  break;
	default: {
	  MZREGISTER int len;
	  MZREGISTER char ender;

	  regparse--;
	  len = regstrcspn(regparse, regparse_end, META);
	  if (len <= 0)
	    FAIL("internal disaster");
	  ender = *(regparse+len);
	  if (len > 1 && ISMULT(ender))
	    len--;		/* Back off clear of ?+* operand. */
	  *flagp |= HASWIDTH;
	  if (len == 1)
	    *flagp |= SIMPLE;
	  ret = regnode(EXACTLY);
	  regc(len >> 8); regc(len & 255);
	  while (len > 0) {
	    regc(*regparse++);
	    len--;
	  }
	}
	  break;
	}
	
	return(ret);
      }

/*
   - regnode - emit a node
   */
static char *			/* Location. */
regnode(char op)
{
  MZREGISTER char *ret;
  MZREGISTER char *ptr;

  ret = regcode;
  if (ret == &regdummy) {
    regsize += 3;
    return(ret);
  }

  ptr = ret;
  *ptr++ = op;
  *ptr++ = '\0';		/* Null "next" pointer. */
  *ptr++ = '\0';
  regcode = ptr;

  return(ret);
}

/*
   - regc - emit (if appropriate) a byte of code
   */
static void
regc(char b)
{
  if (regcode != &regdummy)
    *regcode++ = b;
  else
    regsize++;
}

/*
   - reginsert - insert an operator in front of already-emitted operand
   *
   * Means relocating the operand.
   */
static void
reginsert(char op, char *opnd)
{
  MZREGISTER char *src;
  MZREGISTER char *dst;
  MZREGISTER char *place;

  if (regcode == &regdummy) {
    regsize += 3;
    return;
  }

  src = regcode;
  regcode += 3;
  dst = regcode;
  while (src > opnd) {
    *--dst = *--src;
  }

  place = opnd;			/* Op node, where operand used to be. */
  *place++ = op;
  *place++ = '\0';
  *place++ = '\0';
}

/*
   - regtail - set the next-pointer at the end of a node chain
   */
static void
regtail(char *p, char *val)
{
  MZREGISTER char *scan;
  MZREGISTER char *temp;
  MZREGISTER int offset;

  if (p == &regdummy)
    return;

  /* Find last node. */
  scan = p;
  for (;;) {
    temp = regnext(scan);
    if (temp == NULL)
      break;
    scan = temp;
  }

  if (OP(scan) == BACK)
    offset = scan - val;
  else
    offset = val - scan;
  *(scan+1) = (offset>>8)&255;
  *(scan+2) = offset&255;
}

/*
   - regoptail - regtail on operand of first argument; nop if operandless
   */
static void
regoptail(char *p, char *val)
{
  /* "Operandless" and "op != BRANCH" are synonymous in practice. */
  if (p == NULL || p == &regdummy || OP(p) != BRANCH)
    return;
  regtail(OPERAND(p), val);
}

static char *l_strchr(char *a, int l, int c)
{
  int i;

  for (i = 0; i < l; i++) {
    if (a[i] == c)
      return a + i;
  }

  return NULL;
}

/*
 * regexec and friends
 */

/*
 * Global work variables for regexec().
 */
static char *reginput, *reginput_start, *reginput_end; /* String-input pointer. */
static char *regbol;		/* Beginning of input, for ^ check. */
static char **regstartp;	/* Pointer to startp array. */
static char **regendp;		/* Ditto for endp. */

/*
 * Forwards.
 */
STATIC int regtry(regexp *, char *, int, char **, char **);
STATIC int regmatch(char *);
STATIC int regrepeat(char *);

#ifdef DEBUG
int regnarrate = 0;
void regdump();
STATIC char *regprop();
#endif

/*
   - regexec - match a regexp against a string
   */
static int
regexec(regexp *prog, char *string, int stringlen, char **startp, char **endp)
{
  MZREGISTER char *s;
  MZREGISTER int slen;
 
  /* Be paranoid... */
  if (prog == NULL || string == NULL) {
    regerror("NULL parameter");
    return(0);
  }

  /* Check validity of program. */
  if (UCHARAT(prog->program) != MAGIC) {
    regerror("corrupted program");
    return(0);
  }

  /* If there is a "must appear" string, look for it. */
  if (prog->regmust >= 0) {
    s = string;
    slen = stringlen;
    while ((s = l_strchr(s, slen, ((char *)prog + prog->regmust)[0])) != NULL) {
      int i, l = prog->regmlen;
      char *p = ((char *)prog + prog->regmust);
      slen = stringlen - (s - string);
      for (i = 0; (i < l) && (i < slen); i++) {
	if (s[i] != p[i])
	  break;
      }
      if (i >= l)
	break; /* Found it. */
      s++;
      slen--;
    }
    if (s == NULL)		/* Not present. */
      return(0);
  }

  /* Mark beginning of line for ^ . */
  regbol = string;

  /* Simplest case:  anchored match need be tried only once. */
  if (prog->reganch)
    return(regtry(prog, string, stringlen, startp, endp));

  /* Messy cases:  unanchored match. */
  s = string;
  if (prog->regstart != '\0')
    /* We know what char it must start with. */
    while ((s = l_strchr(s, stringlen - (s - string), prog->regstart)) != NULL) {
      if (regtry(prog, s, stringlen - (s - string), startp, endp))
	return(1);
      s++;
    }
  else {
    /* We don't -- general case. */
    char *e = string + stringlen;
    do {
      if (regtry(prog, s, stringlen - (s - string), startp, endp))
	return (1);
    } while (s++ != e);
  }

  /* Failure. */
  return(0);
}

/*
   - regtry - try match at specific point
   */
static int			/* 0 failure, 1 success */
regtry(regexp *prog, char *string, int stringlen, char **startp, char **endp)
{
  MZREGISTER int i;
  MZREGISTER char **sp;
  MZREGISTER char **ep;

  reginput = string;
  reginput_start = string;
  reginput_end = string + stringlen;
  regstartp = startp;
  regendp = endp;

  sp = startp;
  ep = endp;
  for (i = prog->nsubexp; i > 0; i--) {
    *sp++ = NULL;
    *ep++ = NULL;
  }
  if (regmatch(prog->program + 1)) {
    startp[0] = string;
    endp[0] = reginput;
    return(1);
  } else
    return(0);
}

/*
   - regmatch - main matching routine
   *
   * Conceptually the strategy is simple:  check to see whether the current
   * node matches, call self recursively to see whether the rest matches,
   * and then act accordingly.  In practice we make some effort to avoid
   * recursion, in particular by going through "ordinary" nodes (that don't
   * need to know whether the rest of the match failed) by a loop instead of
   * by recursion.
   */
static int			/* 0 failure, 1 success */
regmatch(char *prog)
{
  MZREGISTER char *scan;		/* Current node. */
  char *next;			/* Next node. */

  scan = prog;
#ifdef DEBUG
  if (scan != NULL && regnarrate)
    fprintf(stderr, "%s(\n", regprop(scan));
#endif
  while (scan != NULL) {
#ifdef DEBUG
    if (regnarrate)
      fprintf(stderr, "%s...\n", regprop(scan));
#endif
    next = regnext(scan);

    switch (OP(scan)) {
    case BOL:
      if (reginput != regbol)
	return(0);
      break;
    case EOL:
      if (reginput != reginput_end)
	return(0);
      break;
    case ANY:
      if (reginput == reginput_end)
	return(0);
      reginput++;
      break;
    case EXACTLY: {
      MZREGISTER int len, i;
      MZREGISTER char *opnd;

      opnd = OPSTR(OPERAND(scan));
      len = OPLEN(OPERAND(scan));
      if (len > reginput_end - reginput)
	return 0;
      for (i = 0; i < len; i++) {
	if (opnd[i] != reginput[i])
	  return 0;
      }
      reginput += len;
    }
      break;
    case ANYOF:
      if (reginput == reginput_end || !l_strchr(OPSTR(OPERAND(scan)), OPLEN(OPERAND(scan)), *reginput))
	return(0);
      reginput++;
      break;
    case ANYBUT:
      if (reginput == reginput_end || l_strchr(OPSTR(OPERAND(scan)), OPLEN(OPERAND(scan)), *reginput))
	return(0);
      reginput++;
      break;
    case NOTHING:
      break;
    case BACK:
      break;
    case BRANCH: {
      MZREGISTER char *save;

      if (OP(next) != BRANCH)	/* No choice. */
	next = OPERAND(scan);	/* Avoid recursion. */
      else {
	do {
	  save = reginput;
	  if (regmatch(OPERAND(scan)))
	    return(1);
	  reginput = save;
	  scan = regnext(scan);
	} while (scan != NULL && OP(scan) == BRANCH);
	return(0);
	/* NOTREACHED */
      }
    }
      break;
    case STAR:
    case PLUS: {
      MZREGISTER char nextch;
      MZREGISTER int no;
      MZREGISTER char *save;
      MZREGISTER int min;

      /*
       * Lookahead to avoid useless match attempts
       * when we know what character comes next.
       */
      nextch = '\0';
      if (OP(next) == EXACTLY)
	nextch = *OPSTR(OPERAND(next));
      min = (OP(scan) == STAR) ? 0 : 1;
      save = reginput;
      no = regrepeat(OPERAND(scan));
      while (no >= min) {
	/* If it could work, try it. */
	if (nextch == '\0' || *reginput == nextch)
	  if (regmatch(next))
	    return(1);
	/* Couldn't or didn't -- back up. */
	no--;
	reginput = save + no;
      }
      return(0);
    }
      break;
    case END:
      return(1);		/* Success! */
      break;
    default:
      {
	int isopen;
	int no;
	char *save;

	switch (OP(scan)) {
	case OPENN:
	  isopen = 1;
	  no = OPLEN(OPERAND(scan));
	  break;
	case CLOSEN:
	  isopen = 0;
	  no = OPLEN(OPERAND(scan));
	  break;
	default:
	  if (OP(scan) < CLOSE) {
	    isopen = 1;
	    no = OP(scan) - OPEN;
	  } else {
	    isopen = 0;
	    no = OP(scan) - CLOSE;
	  }
	}

	save = reginput;

	if (isopen) {
	  if (regmatch(next)) {
	    /*
	     * Don't set startp if some later
	     * invocation of the same parentheses
	     * already has.
	     */
	    if (regstartp[no] == NULL)
	      regstartp[no] = save;
	    return(1);
	  } else
	    return(0);
	} else {
	  if (regmatch(next)) {
	    /*
	     * Don't set endp if some later
	     * invocation of the same parentheses
	     * already has.
	     */
	    if (regendp[no] == NULL)
	      regendp[no] = save;
	    return(1);
	  } else
	    return(0);
	}
      }
      break;
    }

    scan = next;
  }

  /*
   * We get here only if there's trouble -- normally "case END" is
   * the terminating point.
   */
  regerror("corrupted pointers");
  return(0);
}

/*
   - regrepeat - repeatedly match something simple, report how many
   */
static int
regrepeat(char *p)
{
  MZREGISTER int count = 0;
  MZREGISTER char *scan;
  MZREGISTER char *opnd;

  scan = reginput;
  opnd = OPERAND(p);
  switch (OP(p)) {
  case ANY:
    count = reginput_end - scan;
    scan += count;
    break;
  case EXACTLY:
    {
      char *opnd2 = OPSTR(opnd);
      while (*opnd2 == *scan) {
	count++;
	scan++;
      }
    }
    break;
  case ANYOF:
    while (scan != reginput_end && l_strchr(OPSTR(opnd), OPLEN(opnd), *scan)) {
      count++;
      scan++;
    }
    break;
  case ANYBUT:
    while (scan != reginput_end && !l_strchr(OPSTR(opnd), OPLEN(opnd), *scan)) {
      count++;
      scan++;
    }
    break;
  default:			/* Oh dear.  Called inappropriately. */
    regerror("internal foulup");
    count = 0;			/* Best compromise. */
    break;
  }
  reginput = scan;

  return(count);
}

/*
   - regnext - dig the "next" pointer out of a node
   */
static char *
regnext(char *p)
{
  MZREGISTER int offset;

  if (p == &regdummy)
    return(NULL);

  offset = NEXT(p);
  if (offset == 0)
    return(NULL);

  if (OP(p) == BACK)
    return(p-offset);
  else
    return(p+offset);
}

#ifdef DEBUG

STATIC char *regprop();

/*
   - regdump - dump a regexp onto stdout in vaguely comprehensible form
   */
static void
regdump(r)
     regexp *r;
{
  MZREGISTER char *s;
  MZREGISTER char op = EXACTLY;	/* Arbitrary non-END op. */
  MZREGISTER char *next;
  extern char *strchr();


  s = r->program + 1;
  while (op != END) {		/* While that wasn't END last time... */
    op = OP(s);
    printf("%2d%s", s-r->program, regprop(s)); /* Where, what. */
    next = regnext(s);
    if (next == NULL)		/* Next ptr. */
      printf("(0)");
    else 
      printf("(%d)", (s-r->program)+(next-s));
    s += 3;
    if (op == ANYOF || op == ANYBUT || op == EXACTLY) {
      /* Literal string, where present. */
#  error This needs to be fixed to deal with OPLEN and OPSTR
      while (*s != '\0') {
	putchar(*s);
	s++;
      }
      s++;
    }
    putchar('\n');
  }

  /* Header fields of interest. */
  if (r->regstart != '\0')
    printf("start `%c' ", r->regstart);
  if (r->reganch)
    printf("anchored ");
  if (r->regmust >= 0)
    printf("must have \"%s\"", (char *)r + r->regmust);
  printf("\n");
}

/*
   - regprop - printable representation of opcode
   */
static char *
regprop(op)
     char *op;
{
  MZREGISTER char *p;
  static char buf[50];

  (void) strcpy(buf, ":");

  switch (OP(op)) {
  case BOL:
    p = "BOL";
    break;
  case EOL:
    p = "EOL";
    break;
  case ANY:
    p = "ANY";
    break;
  case ANYOF:
    p = "ANYOF";
    break;
  case ANYBUT:
    p = "ANYBUT";
    break;
  case BRANCH:
    p = "BRANCH";
    break;
  case EXACTLY:
    p = "EXACTLY";
    break;
  case NOTHING:
    p = "NOTHING";
    break;
  case BACK:
    p = "BACK";
    break;
  case END:
    p = "END";
    break;
  case OPEN+1:
  case OPEN+2:
  case OPEN+3:
  case OPEN+4:
  case OPEN+5:
  case OPEN+6:
  case OPEN+7:
  case OPEN+8:
  case OPEN+9:
    sprintf(buf+strlen(buf), "OPEN%d", OP(op)-OPEN);
    p = NULL;
    break;
  case CLOSE+1:
  case CLOSE+2:
  case CLOSE+3:
  case CLOSE+4:
  case CLOSE+5:
  case CLOSE+6:
  case CLOSE+7:
  case CLOSE+8:
  case CLOSE+9:
    sprintf(buf+strlen(buf), "CLOSE%d", OP(op)-CLOSE);
    p = NULL;
    break;
  case STAR:
    p = "STAR";
    break;
  case PLUS:
    p = "PLUS";
    break;
  default:
    regerror("corrupted opcode");
    break;
  }
  if (p != NULL)
    (void) strcat(buf, p);
  return(buf);
}
#endif

/*
 * strcspn - find length of initial segment of s1 consisting entirely
 * of characters not from s2
 */

static int
regstrcspn(char *s1, char *e1, char *s2)
{
  MZREGISTER char *scan1;
  MZREGISTER char *scan2;
  MZREGISTER int count;

  count = 0;
  for (scan1 = s1; scan1 != e1; scan1++) {
    for (scan2 = s2; *scan2 != '\0';) { /* ++ moved down. */
      if (*scan1 == *scan2++)
	return(count);
    }
    count++;
  }
  return(count);
}

#ifdef MZ_PRECISE_GC
START_XFORM_ARITH;
#endif

#ifndef strncpy
  extern char *strncpy();
#endif

/*
   - regsub - perform substitutions after a regexp match
   */
static 
char *regsub(regexp *prog, char *src, int sourcelen, long *lenout, char *insrc, long iso, unsigned long srcbase, char **startp, char **endp)
{
  char *dest;
  MZREGISTER char c;
  MZREGISTER long no;
  MZREGISTER long len;
  long destalloc, destlen, srcpos;
	
  destalloc = 2 * sourcelen;
  destlen = 0;
  dest = (char *)scheme_malloc_atomic(destalloc + 1);
	

  if (prog == NULL || src == NULL || dest == NULL) {
    regerror("NULL parm to regsub");
    return "";
  }
	
  srcpos = 0;
  while (srcpos < sourcelen) {
    c = src[srcpos++];
    if (c == '&')
      no = 0;
    else if (c == '\\') {
      if (src[srcpos] == '\\' || src[srcpos] == '&')
	no = -1;
      else if (src[srcpos] == '$') {
	no = prog->nsubexp + 1; /* Gives the empty string */
	srcpos++;
      } else {
	no = 0;
	while ('0' <= src[srcpos] && src[srcpos] <= '9') {
	  no = (no * 10) + (src[srcpos++] - '0');
	}
      }
    } else
      no = -1;


    if (no < 0) {		/* Ordinary character. */
      if (c == '\\' && (src[srcpos] == '\\' || src[srcpos] == '&'))
	c = src[srcpos++];
      if (destlen + 1 >= destalloc) {
	char *old = dest;
	destalloc *= 2;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      dest[destlen++] = c;
    } else if (no >= prog->nsubexp) {
      /* Number too big; prentend it's the empty string */
    } else if (startp[no] != NULL && endp[no] != NULL) {
      len = endp[no] - startp[no];
      if (len + destlen >= destalloc) {
	char *old = dest;
	destalloc = 2 * destalloc + len + destlen;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      memcpy(dest + destlen, insrc + iso + ((unsigned long)startp[no] - srcbase), len);
      destlen += len;
    }
  }
  dest[destlen] = '\0';

  if (lenout)
    *lenout = destlen;

  return dest;
}

static Scheme_Object *make_regexp(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->regexp", "string", 0, argc, argv);

  return (Scheme_Object *)regcomp(SCHEME_STR_VAL(argv[0]), SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *gen_compare(char *name, int pos, 
				  int argc, Scheme_Object *argv[])
{
  regexp *r;
  char *s, *full_s, **startp, **endp;
  unsigned long srcbase;
  int offset = 0, endset;
  
  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(name, "regexp-or-string", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type(name, "string", 1, argc, argv);

  endset = SCHEME_STRLEN_VAL(argv[1]);
  if (argc > 2) {
    int len = endset;

    offset = scheme_extract_index(name, 2, argc, argv, len + 1);

    if (offset > len)
      scheme_out_of_string_range(name, "offset ", argv[2], argv[1], 0, len);

    if (argc > 3) {
      endset = scheme_extract_index(name, 3, argc, argv, len + 1);

      if (endset < offset || endset > len)
	scheme_out_of_string_range(name, "ending ", argv[3], argv[1], offset, len);
    }
  }

  if (SCHEME_STRINGP(argv[0]))
    r = regcomp(SCHEME_STR_VAL(argv[0]), SCHEME_STRTAG_VAL(argv[0]));
  else
    r = (regexp *)argv[0];

  full_s = SCHEME_STR_VAL(argv[1]);

  startp = MALLOC_N_ATOMIC(char *, r->nsubexp);
  endp = MALLOC_N_ATOMIC(char *, r->nsubexp);

  /* s might be mis-aligned, so with MZ_PRECISE_GC we rely on 
     GC not happening for a while... */
  s = full_s + offset;

  srcbase = (unsigned long)s; /* precise gc: srcbase isn't moved */
  /* No GCs during regexc... */

  if (regexec(r, s, endset - offset, startp, endp)) {
    int i;
    Scheme_Object *l = scheme_null;

    s = NULL; /* Cleart mis-aligned pointer */

    /* GC may happen now. But startp[*], endp[*], and srcbase are atomic */

    for (i = r->nsubexp; i--; ) {
      if (startp[i]) {
	if (pos) {
	  long startpd, endpd;

	  startpd = ((unsigned long)startp[i] - srcbase) + offset;
	  endpd = ((unsigned long)endp[i] - srcbase) + offset;
	
	  l = scheme_make_pair(scheme_make_pair(scheme_make_integer(startpd),
						scheme_make_integer(endpd)),
			       l);
	} else {
	  l = scheme_make_pair(scheme_make_sized_offset_string(full_s,
							       offset + ((unsigned long)startp[i] - srcbase), 
							       (endp[i] - startp[i]),
							       1),
			       l);
	}
      } else
	l = scheme_make_pair(scheme_false, l);
    }

    return l;
  } else {
    return scheme_false;
  }
}

static Scheme_Object *compare(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match", 0, argc, argv);
}

static Scheme_Object *positions(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-positions", 1, argc, argv);
}

static Scheme_Object *gen_replace(int argc, Scheme_Object *argv[], int all)
{
  regexp *r;
  char *source, *prefix = NULL, **startp, **endp;
  unsigned long srcbase;
  int prefix_len = 0, sourcelen, srcoffset = 0;

  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("regexp-replace", "regexp-or-string", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("regexp-replace", "string", 1, argc, argv);
  if (!SCHEME_STRINGP(argv[2]))
    scheme_wrong_type("regexp-replace", "string", 2, argc, argv);

  if (SCHEME_STRINGP(argv[0]))
    r = regcomp(SCHEME_STR_VAL(argv[0]), SCHEME_STRTAG_VAL(argv[0]));
  else
    r = (regexp *)argv[0];

  source = SCHEME_STR_VAL(argv[1]);
  sourcelen = SCHEME_STRTAG_VAL(argv[1]);

  startp = MALLOC_N_ATOMIC(char *, r->nsubexp);
  endp = MALLOC_N_ATOMIC(char *, r->nsubexp);

  while (1) {
    srcbase = (unsigned long)source + srcoffset; /* precise gc: srcbase isn't moved */
    /* No GCs during regexc... */
    if (regexec(r, source + srcoffset, sourcelen - srcoffset, startp, endp)) {
      char *insert;
      long len, end, startpd, endpd;
      
      /* GC may happen now. But startp[*], endp[*], and srcbase are atomic */

      insert = regsub(r, SCHEME_STR_VAL(argv[2]), SCHEME_STRTAG_VAL(argv[2]), &len, source, srcoffset, srcbase, startp, endp);
      
      end = SCHEME_STRTAG_VAL(argv[1]);
      
      startpd = ((unsigned long)startp[0] - srcbase);
      endpd = ((unsigned long)endp[0] - srcbase);
      
      if (!startpd && (endpd == end) && !prefix)
	return scheme_make_sized_string(insert, len, 0);
      else if (!all) {
	char *result;
	long total;
	
	total = len + startpd + (end - endpd);
	
	result = (char *)scheme_malloc_atomic(total + 1);
	memcpy(result, source + srcoffset, startpd);
	memcpy(result + startpd, insert, len);
	memcpy(result + startpd + len, source + srcoffset + endpd, (end - endpd) + 1);
	
	return scheme_make_sized_string(result, total, 0);
      } else {
	char *naya;
	long total;
	
	total = len + prefix_len + startpd;
	
	naya = (char *)scheme_malloc_atomic(total + 1);
	memcpy(naya, prefix, prefix_len);
	memcpy(naya + prefix_len, source + srcoffset, startpd);
	memcpy(naya + prefix_len + startpd, insert, len);

	prefix = naya;
	prefix_len = total;

	srcoffset += endpd;
      }
    } else if (!prefix)
      return argv[1];
    else {
      char *result;
      long total, slen;
      
      slen = sourcelen - srcoffset;
      total = prefix_len + slen;
      
      result = (char *)scheme_malloc_atomic(total + 1);
      memcpy(result, prefix, prefix_len);
      memcpy(result + prefix_len, source + srcoffset, slen);
      result[prefix_len + slen] = 0;
      
      return scheme_make_sized_string(result, total, 0);
    }

    SCHEME_USE_FUEL(1);
  }
}

static Scheme_Object *replace(int argc, Scheme_Object *argv[])
{
  return gen_replace(argc, argv, 0);
}

static Scheme_Object *replace_star(int argc, Scheme_Object *argv[])
{
  return gen_replace(argc, argv, 1);
}

static Scheme_Object *regexp_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_TYPE(argv[0]) == scheme_regexp_type) ? scheme_true : scheme_false;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#define MARKS_FOR_REGEXP_C
#include "mzmark.c"
END_XFORM_SKIP;
#endif

void scheme_regexp_initialize(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  if (scheme_starting_up) {
    GC_REG_TRAV(scheme_regexp_type, mark_regexp);
  }
#endif

  scheme_add_global_constant("regexp", 
			     scheme_make_prim_w_arity(make_regexp, 
						      "regexp", 
						      1, 1), 
			     env);
  scheme_add_global_constant("regexp-match",
			     scheme_make_prim_w_arity(compare,
						      "regexp-match",
						      2, 4),
			     env);
  scheme_add_global_constant("regexp-match-positions", 
			     scheme_make_prim_w_arity(positions, 
						      "regexp-match-positions", 
						      2, 4),
			     env);
  scheme_add_global_constant("regexp-replace", 
			     scheme_make_prim_w_arity(replace, 
						      "regexp-replace", 
						      3, 3), 
			     env);
  scheme_add_global_constant("regexp-replace*", 
			     scheme_make_prim_w_arity(replace_star, 
						      "regexp-replace*", 
						      3, 3), 
			     env);
  scheme_add_global_constant("regexp?", 
			     scheme_make_prim_w_arity(regexp_p, 
						      "regexp?", 
						      1, 1), 
			     env);
}

#endif
/* NO_REGEXP_UTILS */
