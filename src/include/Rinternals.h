/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2013   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_INTERNALS_H_
#define R_INTERNALS_H_

#ifdef __cplusplus
# include <cstdio>
# ifdef __SUNPRO_CC
using std::FILE;
# endif
# include <climits>
extern "C" {
#else
# include <stdio.h>
# include <limits.h> /* for INT_MAX */
#endif

/* These are all required by C99 */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif

#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>
#include <R_ext/Error.h>
#include <R_ext/Memory.h>
#include <R_ext/Utils.h>
#include <R_ext/Print.h>

#include <R_ext/libextern.h>

typedef unsigned char Rbyte;

/* type for length of (standard, not long) vectors etc */
typedef int R_len_t;
#define R_LEN_T_MAX INT_MAX

/* both config.h and Rconfig.h set SIZEOF_SIZE_T, but Rconfig.h is
   skipped if config.h has already been included. */
#ifndef R_CONFIG_H
# include <Rconfig.h>
#endif

#if ( SIZEOF_SIZE_T > 4 )
# define LONG_VECTOR_SUPPORT
#endif

#ifdef LONG_VECTOR_SUPPORT
    typedef ptrdiff_t R_xlen_t;
    typedef struct { R_xlen_t lv_length, lv_truelength; } R_long_vec_hdr_t;
# define R_XLEN_T_MAX 4503599627370496
# define R_SHORT_LEN_MAX 2147483647
# define R_LONG_VEC_TOKEN -1
#else
    typedef int R_xlen_t;
# define R_XLEN_T_MAX R_LEN_T_MAX
#endif

#ifndef TESTING_WRITE_BARRIER
# define INLINE_PROTECT
#endif

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.
 *
 *			--> TypeTable[] in ../main/util.c for  typeof()
 */

/*  These exact numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.c, and they are serialized.
*/

/* this number of types is used in type tables, utils.c; should be kept in sync with SEXPTYPE below */
#define NTYPES 32

#ifndef enum_SEXPTYPE
/* NOT YET using enum:
 *  1)	The SEXPREC struct below has 'SEXPTYPE type : 5'
 *	(making FUNSXP and CLOSXP equivalent in there),
 *	giving (-Wall only ?) warnings all over the place
 * 2)	Many switch(type) { case ... } statements need a final `default:'
 *	added in order to avoid warnings like [e.g. l.170 of ../main/util.c]
 *	  "enumeration value `FUNSXP' not handled in switch"
 */
typedef unsigned int SEXPTYPE;

#define NILSXP	     0	  /* nil = NULL */
#define SYMSXP	     1	  /* symbols */
#define LISTSXP	     2	  /* lists of dotted pairs */
#define CLOSXP	     3	  /* closures */
#define ENVSXP	     4	  /* environments */
#define PROMSXP	     5	  /* promises: [un]evaluated closure arguments */
#define LANGSXP	     6	  /* language constructs (special lists) */
#define SPECIALSXP   7	  /* special forms */
#define BUILTINSXP   8	  /* builtin non-special forms */
#define CHARSXP	     9	  /* "scalar" string type (internal only)*/
#define LGLSXP	    10	  /* logical vectors */
/* 11 and 12 were factors and ordered factors in the 1990s */
#define INTSXP	    13	  /* integer vectors */
#define REALSXP	    14	  /* real variables */
#define CPLXSXP	    15	  /* complex variables */
#define STRSXP	    16	  /* string vectors */
#define DOTSXP	    17	  /* dot-dot-dot object */
#define ANYSXP	    18	  /* make "any" args work.
			     Used in specifying types for symbol
			     registration to mean anything is okay  */
#define VECSXP	    19	  /* generic vectors */
#define EXPRSXP	    20	  /* expressions vectors */
#define BCODESXP    21    /* byte code */
#define EXTPTRSXP   22    /* external pointer */
#define WEAKREFSXP  23    /* weak reference */
#define RAWSXP      24    /* raw bytes */
#define S4SXP       25    /* S4, non-vector */

/* used for detecting PROTECT issues in memory.c */
#define NEWSXP      30    /* fresh node creaed in new page */
#define FREESXP     31    /* node released by GC */

#define FUNSXP      99    /* Closure or Builtin or Special */


#else /* NOT YET */
/*------ enum_SEXPTYPE ----- */
typedef enum {
    NILSXP	= 0,	/* nil = NULL */
    SYMSXP	= 1,	/* symbols */
    LISTSXP	= 2,	/* lists of dotted pairs */
    CLOSXP	= 3,	/* closures */
    ENVSXP	= 4,	/* environments */
    PROMSXP	= 5,	/* promises: [un]evaluated closure arguments */
    LANGSXP	= 6,	/* language constructs (special lists) */
    SPECIALSXP	= 7,	/* special forms */
    BUILTINSXP	= 8,	/* builtin non-special forms */
    CHARSXP	= 9,	/* "scalar" string type (internal only)*/
    LGLSXP	= 10,	/* logical vectors */
    INTSXP	= 13,	/* integer vectors */
    REALSXP	= 14,	/* real variables */
    CPLXSXP	= 15,	/* complex variables */
    STRSXP	= 16,	/* string vectors */
    DOTSXP	= 17,	/* dot-dot-dot object */
    ANYSXP	= 18,	/* make "any" args work */
    VECSXP	= 19,	/* generic vectors */
    EXPRSXP	= 20,	/* expressions vectors */
    BCODESXP	= 21,	/* byte code */
    EXTPTRSXP	= 22,	/* external pointer */
    WEAKREFSXP	= 23,	/* weak reference */
    RAWSXP	= 24,	/* raw bytes */
    S4SXP	= 25,	/* S4 non-vector */

    NEWSXP      = 30,   /* fresh node creaed in new page */
    FREESXP     = 31,   /* node released by GC */

    FUNSXP	= 99	/* Closure or Builtin */
} SEXPTYPE;
#endif

#ifdef USE_RINTERNALS
/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */

/* Flags */
struct sxpinfo_struct {
    SEXPTYPE type      :  5;/* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int obj   :  1;
    unsigned int named :  2;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;  /* functions and memory tracing */
    unsigned int spare :  1;  /* currently unused */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
}; /*		    Tot: 32 */

struct vecsxp_struct {
    R_len_t	length;
    R_len_t	truelength;
};

struct primsxp_struct {
    int offset;
};

struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};

struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};

struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};

struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};

struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */

/* Define SWITH_TO_REFCNT to use reference counting instead of the
   'NAMED' mechanism. This uses the R-devel binary layout. The two
   'named' field bits are used for the REFCNT, so REFCNTMAX is 3. */
//#define SWITCH_TO_REFCNT

#if defined(SWITCH_TO_REFCNT) && ! defined(COMPUTE_REFCNT_VALUES)
# define COMPUTE_REFCNT_VALUES
#endif
#define REFCNTMAX (4 - 1)

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    struct SEXPREC *attrib; \
    struct SEXPREC *gengc_next_node, *gengc_prev_node

/* The standard node structure consists of a header followed by the
   node data. */
typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
	struct primsxp_struct primsxp;
	struct symsxp_struct symsxp;
	struct listsxp_struct listsxp;
	struct envsxp_struct envsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
    } u;
} SEXPREC, *SEXP;

/* The generational collector uses a reduced version of SEXPREC as a
   header in vector nodes.  The layout MUST be kept consistent with
   the SEXPREC definition.  The standard SEXPREC takes up 7 words on
   most hardware; this reduced version should take up only 6 words.
   In addition to slightly reducing memory use, this can lead to more
   favorable data alignment on 32-bit architectures like the Intel
   Pentium III where odd word alignment of doubles is allowed but much
   less efficient than even word alignment. */
typedef struct VECTOR_SEXPREC {
    SEXPREC_HEADER;
    struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;

typedef union { VECTOR_SEXPREC s; double align; } SEXPREC_ALIGN;

/* General Cons Cell Attributes */
#define ATTRIB(x)	((x)->attrib)
#define OBJECT(x)	((x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.mark)
#define TYPEOF(x)	((x)->sxpinfo.type)
#define NAMED(x)	((x)->sxpinfo.named)
#define RTRACE(x)	((x)->sxpinfo.trace)
#define LEVELS(x)	((x)->sxpinfo.gp)
#define SET_OBJECT(x,v)	(((x)->sxpinfo.obj)=(v))
#define SET_TYPEOF(x,v)	(((x)->sxpinfo.type)=(v))
#define SET_NAMED(x,v)	(((x)->sxpinfo.named)=(v))
#define SET_RTRACE(x,v)	(((x)->sxpinfo.trace)=(v))
#define SETLEVELS(x,v)	(((x)->sxpinfo.gp)=((unsigned short)v))

#if defined(COMPUTE_REFCNT_VALUES)
# define REFCNT(x) ((x)->sxpinfo.named)
# define TRACKREFS(x) (TYPEOF(x) == CLOSXP ? TRUE : ! (x)->sxpinfo.spare)
#else
# define REFCNT(x) 0
# define TRACKREFS(x) FALSE
#endif

#ifdef SWITCH_TO_REFCNT
# undef NAMED
# undef SET_NAMED
# define NAMED(x) REFCNT(x)
# define SET_NAMED(x, v) do {} while (0)
#endif

/* S4 object bit, set by R_do_new_object for all new() calls */
#define S4_OBJECT_MASK ((unsigned short)(1<<4))
#define IS_S4_OBJECT(x) ((x)->sxpinfo.gp & S4_OBJECT_MASK)
#define SET_S4_OBJECT(x) (((x)->sxpinfo.gp) |= S4_OBJECT_MASK)
#define UNSET_S4_OBJECT(x) (((x)->sxpinfo.gp) &= ~S4_OBJECT_MASK)

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
    R_len_t R_BadLongVector(SEXP, const char *, int);
# define IS_LONG_VEC(x) (SHORT_VEC_LENGTH(x) == R_LONG_VEC_TOKEN)
# define SHORT_VEC_LENGTH(x) (((VECSEXP) (x))->vecsxp.length)
# define SHORT_VEC_TRUELENGTH(x) (((VECSEXP) (x))->vecsxp.truelength)
# define LONG_VEC_LENGTH(x) ((R_long_vec_hdr_t *) (x))[-1].lv_length
# define LONG_VEC_TRUELENGTH(x) ((R_long_vec_hdr_t *) (x))[-1].lv_truelength
# define XLENGTH(x) (IS_LONG_VEC(x) ? LONG_VEC_LENGTH(x) : SHORT_VEC_LENGTH(x))
# define XTRUELENGTH(x)	(IS_LONG_VEC(x) ? LONG_VEC_TRUELENGTH(x) : SHORT_VEC_TRUELENGTH(x))
# define LENGTH(x) (IS_LONG_VEC(x) ? R_BadLongVector(x, __FILE__, __LINE__) : SHORT_VEC_LENGTH(x))
# define TRUELENGTH(x) (IS_LONG_VEC(x) ? R_BadLongVector(x, __FILE__, __LINE__) : SHORT_VEC_TRUELENGTH(x))
# define SET_SHORT_VEC_LENGTH(x,v) (SHORT_VEC_LENGTH(x) = (v))
# define SET_SHORT_VEC_TRUELENGTH(x,v) (SHORT_VEC_TRUELENGTH(x) = (v))
# define SET_LONG_VEC_LENGTH(x,v) (LONG_VEC_LENGTH(x) = (v))
# define SET_LONG_VEC_TRUELENGTH(x,v) (LONG_VEC_TRUELENGTH(x) = (v))
# define SETLENGTH(x,v) do { \
      SEXP sl__x__ = (x); \
      R_xlen_t sl__v__ = (v); \
      if (IS_LONG_VEC(sl__x__)) \
	  SET_LONG_VEC_LENGTH(sl__x__,  sl__v__); \
      else SET_SHORT_VEC_LENGTH(sl__x__, (R_len_t) sl__v__); \
  } while (0)
# define SET_TRUELENGTH(x,v) do { \
      SEXP sl__x__ = (x); \
      R_xlen_t sl__v__ = (v); \
      if (IS_LONG_VEC(sl__x__)) \
	  SET_LONG_VEC_TRUELENGTH(sl__x__, sl__v__); \
      else SET_SHORT_VEC_TRUELENGTH(sl__x__, (R_len_t) sl__v__); \
  } while (0)
# define IS_SCALAR(x, type) (TYPEOF(x) == (type) && SHORT_VEC_LENGTH(x) == 1)
#else
# define LENGTH(x)	(((VECSEXP) (x))->vecsxp.length)
# define TRUELENGTH(x)	(((VECSEXP) (x))->vecsxp.truelength)
# define XLENGTH(x) LENGTH(x)
# define XTRUELENGTH(x) TRUELENGTH(x)
# define SETLENGTH(x,v)		((((VECSEXP) (x))->vecsxp.length)=(v))
# define SET_TRUELENGTH(x,v)	((((VECSEXP) (x))->vecsxp.truelength)=(v))
# define SET_SHORT_VEC_LENGTH SETLENGTH
# define SET_SHORT_VEC_TRUELENGTH SET_TRUELENGTH
# define IS_LONG_VEC(x) 0
# define IS_SCALAR(x, type) (TYPEOF(x) == (type) && LENGTH(x) == 1)
#endif

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define DATAPTR(x)	(((SEXPREC_ALIGN *) (x)) + 1)
#define CHAR(x)		((const char *) DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define STRING_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define VECTOR_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))

/* List Access Macros */
/* These also work for ... objects */
#define LISTVAL(x)	((x)->u.listsxp)
#define TAG(e)		((e)->u.listsxp.tagval)
#define CAR(e)		((e)->u.listsxp.carval)
#define CDR(e)		((e)->u.listsxp.cdrval)
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	((x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)

/* Calling do_ functions with explicit arguments */

#define ARGSHIFT(a)	argShift(&(a)) /* WARNING: do not use in function arguments! */

#define RETURN_EARG1(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); \
  return (func) (call, op, __a1, env); \
} while(0)

#define RETURN_EARG2(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); \
  return (func) (call, op, __a1, __a2, env); \
} while(0)

#define RETURN_EARG3(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); \
  return (func) (call, op, __a1, __a2, __a3, env); \
} while(0)

#define RETURN_EARG3_WITH_ARGS(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); \
  return (func) (call, op, __args, __a1, __a2, __a3, env); \
} while(0)

#define RETURN_EARG3_2ARGS(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); \
  SEXP __a3 = NULL; \
  return (func) (call, op, __a1, __a2, __a3, env); \
} while(0)

#define RETURN_EARG4(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); __args = CDR(__args); \
  SEXP __a4 = CAR(__args); \
  return (func) (call, op, __a1, __a2, __a3, __a4, env); \
} while(0)

#define RETURN_EARG4_3ARGS(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); \
  SEXP __a4 = NULL; \
  return (func) (call, op, __a1, __a2, __a3, __a4, env); \
} while(0)

#define RETURN_EARG5(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); __args = CDR(__args); \
  SEXP __a4 = CAR(__args); __args = CDR(__args); \
  SEXP __a5 = CAR(__args); \
  return (func) (call, op, __a1, __a2, __a3, __a4, __a5, env); \
} while(0)

#define RETURN_EARG6(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); __args = CDR(__args); \
  SEXP __a4 = CAR(__args); __args = CDR(__args); \
  SEXP __a5 = CAR(__args); __args = CDR(__args); \
  SEXP __a6 = CAR(__args); \
  return (func) (call, op, __a1, __a2, __a3, __a4, __a5, __a6, env); \
} while(0)

#define RETURN_EARG7(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); __args = CDR(__args); \
  SEXP __a4 = CAR(__args); __args = CDR(__args); \
  SEXP __a5 = CAR(__args); __args = CDR(__args); \
  SEXP __a6 = CAR(__args); __args = CDR(__args); \
  SEXP __a7 = CAR(__args); \
  return (func) (call, op, __a1, __a2, __a3, __a4, __a5, __a6, __a7, env); \
} while(0)

#define RETURN_EARG7_5ARGS(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); __args = CDR(__args); \
  SEXP __a4 = CAR(__args); __args = CDR(__args); \
  SEXP __a5 = CAR(__args); \
  SEXP __a6 = NULL; \
  SEXP __a7 = NULL; \
  return (func) (call, op, __a1, __a2, __a3, __a4, __a5, __a6, __a7, env); \
} while(0)

#define RETURN_EARG7_6ARGS(func, call, op, args, env) do { \
  SEXP __args = args; \
  SEXP __a1 = CAR(__args); __args = CDR(__args); \
  SEXP __a2 = CAR(__args); __args = CDR(__args); \
  SEXP __a3 = CAR(__args); __args = CDR(__args); \
  SEXP __a4 = CAR(__args); __args = CDR(__args); \
  SEXP __a5 = CAR(__args); __args = CDR(__args); \
  SEXP __a6 = CAR(__args); \
  SEXP __a7 = NULL; \
  return (func) (call, op, __a1, __a2, __a3, __a4, __a5, __a6, __a7, env); \
} while(0)

#define BUILD_1ARGS(args, arg1) ((args) == NULL ? CONS_NR(arg1, R_NilValue) : (args))
#define BUILD_2ARGS(args, arg1, arg2) ((args) == NULL ? CONS_NR(arg1, CONS_NR(arg2, R_NilValue)) : (args))
#define BUILD_3ARGS(args, arg1, arg2, arg3) ((args) == NULL ? CONS_NR(arg1, CONS_NR(arg2, CONS_NR(arg3, R_NilValue))) : (args))

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define RDEBUG(x)	((x)->sxpinfo.debug)
#define SET_RDEBUG(x,v)	(((x)->sxpinfo.debug)=(v))
#define RSTEP(x)	((x)->sxpinfo.spare)
#define SET_RSTEP(x,v)	(((x)->sxpinfo.spare)=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL_MASK	1
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */

/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp & HASHASH_MASK)
#define HASHVALUE(x)    TRUELENGTH(x)
#define SET_HASHASH(x,v) ((v) ? (((x)->sxpinfo.gp) |= HASHASH_MASK) : \
			  (((x)->sxpinfo.gp) &= (~HASHASH_MASK)))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, v)

#else /* not USE_RINTERNALS */

typedef struct SEXPREC *SEXP;

#define CHAR(x)		R_CHAR(x)
const char *(R_CHAR)(SEXP x);

/* Various tests with macro versions below */
Rboolean (Rf_isNull)(SEXP s);
Rboolean (Rf_isSymbol)(SEXP s);
Rboolean (Rf_isLogical)(SEXP s);
Rboolean (Rf_isReal)(SEXP s);
Rboolean (Rf_isComplex)(SEXP s);
Rboolean (Rf_isExpression)(SEXP s);
Rboolean (Rf_isEnvironment)(SEXP s);
Rboolean (Rf_isString)(SEXP s);
Rboolean (Rf_isObject)(SEXP s);

# define IS_SCALAR(x, type) (TYPEOF(x) == (type) && XLENGTH(x) == 1)

#endif /* USE_RINTERNALS */

#define NAMEDMAX 2
#define INCREMENT_NAMED(x) do {				\
	SEXP __x__ = (x);				\
	if (NAMED(__x__) != NAMEDMAX)			\
	    SET_NAMED(__x__, NAMED(__x__) + 1);		\
    } while (0)

#if defined(COMPUTE_REFCNT_VALUES)
# define SET_REFCNT(x,v) (REFCNT(x) = (v))
# if defined(EXTRA_REFCNT_FIELDS)
#  define SET_TRACKREFS(x,v) (TRACKREFS(x) = (v))
# else
#  define SET_TRACKREFS(x,v) ((x)->sxpinfo.spare = ! (v))
# endif
# define DECREMENT_REFCNT(x) do {					\
	SEXP drc__x__ = (x);						\
	if (REFCNT(drc__x__) > 0 && REFCNT(drc__x__) < REFCNTMAX)	\
	    SET_REFCNT(drc__x__, REFCNT(drc__x__) - 1);			\
    } while (0)
# define INCREMENT_REFCNT(x) do {			      \
	SEXP irc__x__ = (x);				      \
	if (REFCNT(irc__x__) < REFCNTMAX)		      \
	    SET_REFCNT(irc__x__, REFCNT(irc__x__) + 1);	      \
    } while (0)
#else
# define SET_REFCNT(x,v) do {} while(0)
# define SET_TRACKREFS(x,v) do {} while(0)
# define DECREMENT_REFCNT(x) do {} while(0)
# define INCREMENT_REFCNT(x) do {} while(0)
#endif

#define ENABLE_REFCNT(x) SET_TRACKREFS(x, TRUE)
#define DISABLE_REFCNT(x) SET_TRACKREFS(x, FALSE)

/* Macros for some common idioms. */
#ifdef SWITCH_TO_REFCNT
# define MAYBE_SHARED(x) (REFCNT(x) > 1)
# define NO_REFERENCES(x) (REFCNT(x) == 0)
# define MARK_NOT_MUTABLE(x) SET_REFCNT(x, REFCNTMAX)
#else
# define MAYBE_SHARED(x) (NAMED(x) > 1)
# define NO_REFERENCES(x) (NAMED(x) == 0)
# define MARK_NOT_MUTABLE(x) SET_NAMED(x, NAMEDMAX)
#endif
#define MAYBE_REFERENCED(x) (! NO_REFERENCES(x))

typedef SEXP R_bcstack_t;
#ifdef BC_INT_STACK
typedef union { void *p; int i; } IStackval;
#endif

#ifdef __MAIN__
# define INI_as(v) = v
# define extern0 attribute_hidden
#else
# define INI_as(v)
# define extern0 extern
#endif


#ifdef USE_RINTERNALS

/* Promargs stack */
#define USE_PROMARGS_STACK

#ifdef USE_PROMARGS_STACK
  /* required USE_RINTERNALS for SEXPREC */
#define R_PROMARGSSTACKINITSIZE 4096
LibExtern SEXPREC *R_PromargsStackBase, *R_PromargsStackEnd, *R_PromargsStackTop;

void switchPromargsStack(SEXPREC *, SEXPREC *, SEXPREC *);
void releasePromargs(SEXPREC *);

#define POINTER_IN_RANGE(start, x, end) ((uintptr_t) x - (uintptr_t)start <= (uintptr_t) end - (uintptr_t)start)
#define PROMISE_ARGS promiseArgsStack
#define RELEASE_PROMARGS(x) do { \
  if (x == R_NilValue) { \
  } else if (POINTER_IN_RANGE(R_PromargsStackBase, x, R_PromargsStackEnd)) { \
    R_PromargsStackTop = x; \
  } else { \
    releasePromargs(x); \
  } \
  x = NULL; \
} while(0)

#else /* not USE_PROMARGS_STACK */

#define PROMISE_ARGS promiseArgs
#define RELEASE_PROMARGS(x)

#endif

/* context management, for some reason this is only enabled with R_USE_SIGNALS */
#ifdef R_USE_SIGNALS

#ifdef Win32
# include <psignal.h>
#else
# include <signal.h>
# include <setjmp.h>
#endif

#ifdef HAVE_POSIX_SETJMP
# define SIGJMP_BUF sigjmp_buf
# define SIGSETJMP(x,s) sigsetjmp(x,s)
# define SIGLONGJMP(x,i) siglongjmp(x,i)
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,0)
# define LONGJMP(x,i) siglongjmp(x,i)
#else
# define SIGJMP_BUF jmp_buf
# define SIGSETJMP(x,s) setjmp(x)
# define SIGLONGJMP(x,i) longjmp(x,i)
# define JMP_BUF jmp_buf
# define SETJMP(x) setjmp(x)
# define LONGJMP(x,i) longjmp(x,i)
#endif

/* Stack entry for pending promises */
typedef struct RPRSTACK {
    SEXP promise;
    struct RPRSTACK *next;
} RPRSTACK;


/* Evaluation Context Structure */
typedef struct RCNTXT {
    struct RCNTXT *nextcontext;	/* The next context up the chain */
    int callflag;		/* The context "type" */
    JMP_BUF cjmpbuf;		/* C stack and register information */
    int cstacktop;		/* Top of the pointer protection stack */
    int evaldepth;	        /* evaluation depth at inception */
    SEXP promargs;		/* Promises supplied to closure */
    SEXP callfun;		/* The closure called */
    SEXP sysparent;		/* environment the closure was called from */
    SEXP call;			/* The call that effected this context*/
    SEXP cloenv;		/* The environment */
    SEXP conexit;		/* Interpreted "on.exit" code */
    void (*cend)(void *);	/* C "on.exit" thunk */
    void *cenddata;		/* data for C "on.exit" thunk */
    void *vmax;		        /* top of R_alloc stack */
    int intsusp;                /* interrupts are suspended */
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    struct RPRSTACK *prstack;   /* stack of pending promises */
    SEXP *nodestack;
#ifdef BC_INT_STACK
    IStackval *intstack;
#endif
#ifdef USE_PROMARGS_STACK
    SEXPREC *promargsstackbase;
    SEXPREC *promargsstacktop;
    SEXPREC *promargsstackend;    /* could be computed from base */
#endif
    SEXP *positionalPromargs;
    SEXP srcref;	        /* The source line in effect */
    int browserfinish;     /* should browser finish this context without stopping */
} RCNTXT, *context;

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum {
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT	  = 1,
    CTXT_BREAK	  = 2,
    CTXT_LOOP	  = 3,	/* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE	  = 8,
    CTXT_RETURN	  = 12,
    CTXT_BROWSER  = 16,
    CTXT_GENERIC  = 20,
    CTXT_RESTART  = 32,
    CTXT_BUILTIN  = 64  /* used in profiling */
};

/*
TOP   0 0 0 0 0 0  = 0
NEX   1 0 0 0 0 0  = 1
BRE   0 1 0 0 0 0  = 2
LOO   1 1 0 0 0 0  = 3
FUN   0 0 1 0 0 0  = 4
CCO   0 0 0 1 0 0  = 8
BRO   0 0 0 0 1 0  = 16
RET   0 0 1 1 0 0  = 12
GEN   0 0 1 0 1 0  = 20
RES   0 0 0 0 0 0 1 = 32
BUI   0 0 0 0 0 0 0 1 = 64
*/

#define IS_RESTART_BIT_SET(flags) ((flags) & CTXT_RESTART)
#define SET_RESTART_BIT_ON(flags) (flags |= CTXT_RESTART)
#define SET_RESTART_BIT_OFF(flags) (flags &= ~CTXT_RESTART)

#endif /* R_USE_SIGNALS */
#endif /* USE_RINTERNALS */

#ifdef CALLED_FROM_DEFN_H

LibExtern Rboolean R_interrupts_suspended INI_as(FALSE);
LibExtern int R_interrupts_pending INI_as(0);

/*	R_PPSSIZE  The pointer protection stack size  */
/*	R_NSIZE	   The number of cons cells	 */
/*	R_VSIZE	   The vector heap size in bytes */
/*  These values are defaults and can be overridden in config.h
    The maxima and minima are in startup.c */

#ifndef R_PPSSIZE
#define	R_PPSSIZE	50000L
#endif
#ifndef R_NSIZE
#define	R_NSIZE		350000L
#endif
#ifndef R_VSIZE
#define	R_VSIZE		6291456L
#endif

/* The Pointer Protection Stack */
LibExtern int	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
LibExtern int	R_PPStackTop;	    /* The top of the stack */
LibExtern SEXP*	R_PPStack;	    /* The pointer protection stack */

/* Evaluation Environment */
extern0 SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
extern0 SEXP	R_ReturnedValue;    /* Slot for return-ing values */
extern0 SEXP*	R_SymbolTable;	    /* The symbol table */
#ifdef R_USE_SIGNALS
extern0 RCNTXT R_Toplevel;	      /* Storage for the toplevel context */
extern0 RCNTXT* R_ToplevelContext;  /* The toplevel context */
LibExtern RCNTXT* R_GlobalContext;    /* The global context */
extern0 RCNTXT* R_SessionContext;   /* The session toplevel context */
#endif
extern Rboolean R_Visible;	    /* Value visibility flag */
LibExtern int	R_EvalDepth	INI_as(0);	/* Evaluation recursion depth */
extern0 int	R_BrowseLines	INI_as(0);	/* lines/per call in browser */

extern0 int	R_Expressions	INI_as(5000);	/* options(expressions) */
extern0 int	R_Expressions_keep INI_as(5000);	/* options(expressions) */
extern0 Rboolean R_KeepSource	INI_as(FALSE);	/* options(keep.source) */
extern0 Rboolean R_CBoundsCheck	INI_as(FALSE);	/* options(CBoundsCheck) */
extern0 int	R_WarnLength	INI_as(1000);	/* Error/warning max length */
extern0 int	R_nwarnings	INI_as(50);
extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
extern int	R_CStackDir	INI_as(1);	/* C stack direction */

#ifdef R_USE_SIGNALS
LibExtern struct RPRSTACK *R_PendingPromises INI_as(NULL); /* Pending promise stack */
#endif

/* Warnings/Errors */
extern0 int	R_CollectWarnings INI_as(0);	/* the number of warnings */
extern0 SEXP	R_Warnings;	    /* the warnings and their calls */
extern0 int	R_ShowErrorMessages INI_as(1);	/* show error messages? */
LibExtern SEXP	R_HandlerStack;	/* Condition handler stack */
LibExtern SEXP	R_RestartStack;	/* Stack of available restarts */
extern0 Rboolean R_warn_partial_match_args   INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_dollar INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_attr INI_as(FALSE);
extern0 Rboolean R_ShowWarnCalls INI_as(FALSE);
extern0 Rboolean R_ShowErrorCalls INI_as(FALSE);
extern0 int	R_NShowCalls INI_as(50);

#define R_BCNODESTACKSIZE 100000
LibExtern SEXP *R_BCNodeStackBase, *R_BCNodeStackTop, *R_BCNodeStackEnd;
#ifdef BC_INT_STACK
# define R_BCINTSTACKSIZE 10000
extern0 IStackval *R_BCIntStackBase, *R_BCIntStackTop, *R_BCIntStackEnd;
#endif

#endif /* CALLED_FROM_DEFN_H */

/* Complex assignment support */
/* temporary definition that will need to be refined to distinguish
   getter from setter calls */
#define IS_GETTER_CALL(call) (CADR(call) == R_TmpvalSymbol)

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the USE_RINTERNALS section.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int  (OBJECT)(SEXP x);
int  (MARK)(SEXP x);
int  (TYPEOF)(SEXP x);
int  (NAMED)(SEXP x);
void (SET_OBJECT)(SEXP x, int v);
void (SET_TYPEOF)(SEXP x, int v);
void (SET_NAMED)(SEXP x, int v);
void SET_ATTRIB(SEXP x, SEXP v);
void DUPLICATE_ATTRIB(SEXP to, SEXP from);

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x);
void (SET_S4_OBJECT)(SEXP x);
void (UNSET_S4_OBJECT)(SEXP x);

/* Vector Access Functions */
int  (LENGTH)(SEXP x);
int  (TRUELENGTH)(SEXP x);
void (SETLENGTH)(SEXP x, int v);
void (SET_TRUELENGTH)(SEXP x, int v);
R_xlen_t  (XLENGTH)(SEXP x);
R_xlen_t  (XTRUELENGTH)(SEXP x);
int  (IS_LONG_VEC)(SEXP x);
int  (LEVELS)(SEXP x);
int  (SETLEVELS)(SEXP x, int v);

int  *(LOGICAL)(SEXP x);
int  *(INTEGER)(SEXP x);
Rbyte *(RAW)(SEXP x);
double *(REAL)(SEXP x);
Rcomplex *(COMPLEX)(SEXP x);
SEXP (STRING_ELT)(SEXP x, R_xlen_t i);
SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i);
void SET_STRING_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP SET_VECTOR_ELT(SEXP x, R_xlen_t i, SEXP v);
SEXP *(STRING_PTR)(SEXP x);
SEXP *(VECTOR_PTR)(SEXP x);

/* List Access Functions */
/* These also work for ... objects */
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
SEXP (TAG)(SEXP e);
SEXP (CAR)(SEXP e);
SEXP (CDR)(SEXP e);
SEXP (CAAR)(SEXP e);
SEXP (CDAR)(SEXP e);
SEXP (CADR)(SEXP e);
SEXP (CDDR)(SEXP e);
SEXP (CADDR)(SEXP e);
SEXP (CADDDR)(SEXP e);
SEXP (CAD4R)(SEXP e);
int  (MISSING)(SEXP x);
void (SET_MISSING)(SEXP x, int v);
void SET_TAG(SEXP x, SEXP y);
SEXP SETCAR(SEXP x, SEXP y);
SEXP SETCDR(SEXP x, SEXP y);
SEXP SETCADR(SEXP x, SEXP y);
SEXP SETCADDR(SEXP x, SEXP y);
SEXP SETCADDDR(SEXP x, SEXP y);
SEXP SETCAD4R(SEXP e, SEXP y);

SEXP CONS_NR(SEXP a, SEXP b);

/* Closure Access Functions */
SEXP (FORMALS)(SEXP x);
SEXP (BODY)(SEXP x);
SEXP (CLOENV)(SEXP x);
int  (RDEBUG)(SEXP x);
int  (RSTEP)(SEXP x);
int  (RTRACE)(SEXP x);
void (SET_RDEBUG)(SEXP x, int v);
void (SET_RSTEP)(SEXP x, int v);
void (SET_RTRACE)(SEXP x, int v);
void SET_FORMALS(SEXP x, SEXP v);
void SET_BODY(SEXP x, SEXP v);
void SET_CLOENV(SEXP x, SEXP v);

/* Symbol Access Functions */
SEXP (PRINTNAME)(SEXP x);
SEXP (SYMVALUE)(SEXP x);
SEXP (INTERNAL)(SEXP x);
int  (DDVAL)(SEXP x);
void (SET_DDVAL)(SEXP x, int v);
void SET_PRINTNAME(SEXP x, SEXP v);
void SET_SYMVALUE(SEXP x, SEXP v);
void SET_INTERNAL(SEXP x, SEXP v);

/* Environment Access Functions */
SEXP (FRAME)(SEXP x);
SEXP (ENCLOS)(SEXP x);
SEXP (HASHTAB)(SEXP x);
int  (ENVFLAGS)(SEXP x);
void (SET_ENVFLAGS)(SEXP x, int v);
void SET_FRAME(SEXP x, SEXP v);
void SET_ENCLOS(SEXP x, SEXP v);
void SET_HASHTAB(SEXP x, SEXP v);

/* Promise Access Functions */
/* First five have macro versions in Defn.h */
SEXP (PRCODE)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
SEXP (PRVALUE_OR_CONST)(SEXP x);
int  (PRSEEN)(SEXP x);
void (SET_PRSEEN)(SEXP x, int v);
void SET_PRENV(SEXP x, SEXP v);
void SET_PRVALUE(SEXP x, SEXP v);
void SET_PRVALUE_IF_PROMISE(SEXP x, SEXP v);
void SET_PRCODE(SEXP x, SEXP v);
void SET_PRSEEN(SEXP x, int v);

/* Hashing Functions */
/* There are macro versions in Defn.h */
int  (HASHASH)(SEXP x);
int  (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);


/* External pointer access macros */
#define EXTPTR_PTR(x)	CAR(x)
#define EXTPTR_PROT(x)	CDR(x)
#define EXTPTR_TAG(x)	TAG(x)

/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)

/* Pointer Protection and Unprotection */
#define PROTECT(s)	Rf_protect(s)
#define UNPROTECT(n)	Rf_unprotect(n)
#define UNPROTECT_PTR(s)	Rf_unprotect_ptr(s)

/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef int PROTECT_INDEX;
#define PROTECT_WITH_INDEX(x,i) R_ProtectWithIndex(x,i)
#define REPROTECT(x,i) R_Reprotect(x,i)

/* Evaluation Environment */

LibExtern SEXP	R_BaseNamespace;    /* The (fake) namespace for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registered namespaces */
LibExtern SEXP	R_Srcref;           /* Current srcref, for debuggers */

#ifndef __MAIN__
    extern SEXP const R_GlobalEnv;	 /* The "global" environment */
    extern SEXP const R_BaseEnv;	/* The base environment; formerly R_NilValue */
    extern SEXP const R_EmptyEnv;	/* An empty environment at the root of the
                                           environment tree */
#else
    SEXPREC R_BaseEnvContent;
    SEXP const R_BaseEnv = &R_BaseEnvContent;
    SEXPREC R_EmptyEnvContent;
    SEXP const R_EmptyEnv = &R_EmptyEnvContent;
    SEXPREC R_GlobalEnvContent;
    SEXP const R_GlobalEnv = &R_GlobalEnvContent;
#endif

/* Special Values */
#ifndef __MAIN__
    extern SEXP const R_NilValue; /* The nil object */
    extern SEXP const R_UnboundValue; /* Unbound marker */
    extern SEXP const R_MissingArg; /* Missing argument marker */
#else
    SEXPREC R_NilValueContent;
    SEXP const R_NilValue = &R_NilValueContent;
    SEXPREC R_UnboundValueContent;
    SEXP const R_UnboundValue = &R_UnboundValueContent;
    SEXPREC R_MissingArgContent;
    SEXP const R_MissingArg = &R_MissingArgContent;
#endif

#ifdef __MAIN__
attribute_hidden
#else
extern
#endif
SEXP	R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */
LibExtern SEXP	R_Bracket2Symbol;   /* "[[" */
LibExtern SEXP	R_BracketSymbol;    /* "[" */
LibExtern SEXP	R_BraceSymbol;      /* "{" */
LibExtern SEXP	R_CSymbol;          /* "c" */
LibExtern SEXP	R_ClassSymbol;	    /* "class" */
LibExtern SEXP	R_DeviceSymbol;     /* ".Device" */
LibExtern SEXP	R_DimNamesSymbol;   /* "dimnames" */
LibExtern SEXP	R_DimSymbol;	    /* "dim" */
LibExtern SEXP	R_DollarSymbol;	    /* "$" */
LibExtern SEXP	R_DollarAssignSymbol;	    /* "$<-" */
LibExtern SEXP	R_DotsSymbol;	    /* "..." */
LibExtern SEXP	R_DropSymbol;	    /* "drop" */
LibExtern SEXP	R_LastvalueSymbol;  /* ".Last.value" */
LibExtern SEXP	R_LevelsSymbol;	    /* "levels" */
LibExtern SEXP	R_ModeSymbol;	    /* "mode" */
LibExtern SEXP	R_NameSymbol;	    /* "name" */
LibExtern SEXP	R_NamesSymbol;	    /* "names" */
LibExtern SEXP	R_NaRmSymbol;	    /* "na.rm" */
LibExtern SEXP  R_PackageSymbol;    /* "package" */
LibExtern SEXP  R_QuoteSymbol;	    /* "quote" */
LibExtern SEXP	R_RowNamesSymbol;   /* "row.names" */
LibExtern SEXP	R_SeedsSymbol;	    /* ".Random.seed" */
LibExtern SEXP	R_SourceSymbol;     /* "source" */
LibExtern SEXP	R_TspSymbol;	    /* "tsp" */

LibExtern SEXP  R_dot_defined;      /* ".defined" */
LibExtern SEXP  R_dot_Method;       /* ".Method" */
LibExtern SEXP  R_dot_target;       /* ".target" */

LibExtern SEXP  R_native_symbol;    /* "native symbol" */
LibExtern SEXP  R_registered_native_symbol;    /* "registered native symbol" */
LibExtern SEXP  R_PACKAGESymbol;    /* "PACKAGE" */
LibExtern SEXP  R_NAOKSymbol;       /* "NAOK" */
LibExtern SEXP  R_DUPSymbol;        /* "DUP" */
LibExtern SEXP  R_ENCODINGSymbol;   /* "ENCODING" */
LibExtern SEXP  R_CsingleSymbol;    /* "Csingle" */
LibExtern SEXP  R_OriginSymbol;     /* "origin" */
LibExtern SEXP	R_GenericSymbol;    /* "generic" */

LibExtern SEXP  R_S3MethodsTableSymbol;   /* ".__S3MethodsTable__." */
LibExtern SEXP  R_S3MethodsClassesSymbol; /* ".S3MethodsClasses" */
LibExtern SEXP  R_SortListSymbol;         /* "sort.list */
LibExtern SEXP  R_PreviousSymbol;         /* "previous" */
LibExtern SEXP  R_ContainsSymbol;         /* "contains */
LibExtern SEXP  R_SelectSuperClassesSymbol; /* ".selectSuperClasses" */
LibExtern SEXP  R_ClassEnvSymbol;         /* ".classEnv" */
LibExtern SEXP  R_InitMethodDispatchSymbol; /* "initMethodDispatch" */

/* CHARSXP shortcuts */
LibExtern SEXP R_NativeSymbolInfoCharSXP; /* "NativeSymbolInfo" */
LibExtern SEXP R_FactorCharSXP;           /* "factor" */
LibExtern SEXP R_OrderedCharSXP;          /* "ordered" */
LibExtern SEXP R_ConnectionCharSXP;       /* "connection" */
LibExtern SEXP R_RawConnectionCharSXP;    /* "rawConnection" */
LibExtern SEXP R_TextConnectionCharSXP;   /* "textConnection" */
LibExtern SEXP R_DLLInfoCharSXP;          /* "DLLInfo" */
LibExtern SEXP R_UserDefinedDatabaseCharSXP;	/* "UserDefinedDatabase" */
LibExtern SEXP R_POSIXltCharSXP;          /* "POSIXlt" */
LibExtern SEXP R_OpsCharSXP;              /* "Ops" */
LibExtern SEXP R_MathCharSXP;             /* "Math" */
LibExtern SEXP R_ComplexCharSXP;          /* "Complex" */
LibExtern SEXP R_SummaryCharSXP;          /* "Summary" */
LibExtern SEXP R_LengthCharSXP;           /* "length" */
LibExtern SEXP R_LengthAssignCharSXP;     /* "length<-" */
LibExtern SEXP R_NamesCharSXP;            /* "names" */
LibExtern SEXP R_NamesAssignCharSXP;      /* "names<-" */
LibExtern SEXP R_DimNamesCharSXP;         /* "dimnames" */
LibExtern SEXP R_DimNamesAssignCharSXP;   /* "dimnames<-" */
LibExtern SEXP R_DimCharSXP;              /* "dim" */
LibExtern SEXP R_DimAssignCharSXP;        /* "dim<-" */
LibExtern SEXP R_LevelsAssignCharSXP;     /* "levels<-" */
LibExtern SEXP R_AtAssignCharSXP;         /* "@<-" */
LibExtern SEXP R_CCharSXP;                /* "c" */
LibExtern SEXP R_UnlistCharSXP;           /* "unlist" */
LibExtern SEXP R_AsVectorCharSXP;         /* "as.vector" */
LibExtern SEXP R_IsNACharSXP;             /* "is.na" */
LibExtern SEXP R_AnyNACharSXP;            /* "anyNA" */
LibExtern SEXP R_IsNaNCharSXP;            /* "is.nan" */
LibExtern SEXP R_IsFiniteCharSXP;         /* "is.finite" */
LibExtern SEXP R_IsInfiniteCharSXP;       /* "is.infinite" */
LibExtern SEXP R_AsEnvironmentCharSXP;    /* "as.environment" */
LibExtern SEXP R_RepCharSXP;              /* "rep" */
LibExtern SEXP R_SeqCharSXP;              /* "seq" */
LibExtern SEXP R_IsUnsortedCharSXP;       /* "is.unsorted" */
LibExtern SEXP R_XtfrmCharSXP;            /* "xtfrm" */
LibExtern SEXP R_SubsetCharSXP;           /* "[" */
LibExtern SEXP R_Subset2CharSXP;          /* "[[" */
LibExtern SEXP R_SubassignCharSXP;        /* "[<-" */
LibExtern SEXP R_Subassign2CharSXP;       /* "[[<-" */
LibExtern SEXP R_DollarCharSXP;           /* "$" */
LibExtern SEXP R_DollarAssignCharSXP;     /* "$<-" */
LibExtern SEXP R_AsCharacterCharSXP;      /* "as.character" */
LibExtern SEXP R_AsIntegerCharSXP;        /* "as.integer" */
LibExtern SEXP R_AsDoubleCharSXP;         /* "as.double" */
LibExtern SEXP R_AsComplexCharSXP;        /* "as.complex" */
LibExtern SEXP R_AsLogicalCharSXP;        /* "as.logical" */
LibExtern SEXP R_AsRawCharSXP;            /* "as.raw" */
LibExtern SEXP R_IsNumericCharSXP;        /* "is.numeric" */
LibExtern SEXP R_IsMatrixCharSXP;         /* "is.matrix" */
LibExtern SEXP R_IsArrayCharSXP;          /* "is.array" */
LibExtern SEXP R_EmptyCharSXP;            /* "" */
LibExtern SEXP R_MatrixCharSXP;           /* "matrix" */
LibExtern SEXP R_ArrayCharSXP;            /* "array" */
LibExtern SEXP R_FunctionCharSXP;         /* "function" */
LibExtern SEXP R_NumericCharSXP;          /* "numeric" */
LibExtern SEXP R_NameCharSXP;             /* "name" */


/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
LibExtern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
LibExtern SEXP	R_BlankString;	    /* "" as a CHARSXP */

/* srcref related functions */
SEXP R_GetCurrentSrcref(int);
SEXP R_GetSrcFilename(SEXP);

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rf_asChar(SEXP);
SEXP Rf_coerceVector(SEXP, SEXPTYPE);
SEXP Rf_PairToVectorList(SEXP x);
SEXP Rf_VectorToPairList(SEXP x);
SEXP Rf_asCharacterFactor(SEXP x);
int Rf_asLogical(SEXP x);
int Rf_asInteger(SEXP x);
double Rf_asReal(SEXP x);
Rcomplex Rf_asComplex(SEXP x);


#ifndef R_ALLOCATOR_TYPE
#define R_ALLOCATOR_TYPE
typedef struct R_allocator R_allocator_t;
#endif

/* Other Internally Used Functions, excluding those which are inline-able*/

char * Rf_acopy_string(const char *);
SEXP Rf_alloc3DArray(SEXPTYPE, int, int, int);
SEXP Rf_allocArray(SEXPTYPE, SEXP);
SEXP Rf_allocMatrix(SEXPTYPE, int, int);
SEXP Rf_allocList(int);
SEXP Rf_allocS4Object(void);
SEXP Rf_allocSExp(SEXPTYPE);
void Rf_initializeOffHeapSEXP(SEXP, SEXPTYPE);
void Rf_initializeOffHeapEnvironment(SEXP newrho, SEXP namelist, SEXP valuelist, SEXP rho);
SEXP Rf_allocVector3(SEXPTYPE, R_xlen_t, R_allocator_t*);
R_xlen_t Rf_any_duplicated(SEXP x, Rboolean from_last);
R_xlen_t Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last);
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP,SEXP),
                       SEXP (*)(SEXP, int), SEXP);
SEXP Rf_classgets(SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
void Rf_copyMatrix(SEXP, SEXP, Rboolean);
void Rf_copyListMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
int Rf_countContexts(int, int);
SEXP Rf_CreateTag(SEXP);
void Rf_defineVar(SEXP, SEXP, SEXP);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_DropDims(SEXP);
SEXP Rf_duplicate(SEXP);
SEXP Rf_shallow_duplicate(SEXP);
SEXP Rf_lazy_duplicate(SEXP);
/* the next really should not be here and is also in Defn.h */
SEXP Rf_duplicated(SEXP, Rboolean);
Rboolean R_envHasNoSpecialSymbols(SEXP);
Rboolean Rf_equalS3Signature(const char *, const char *, const char *);
SEXP Rf_eval(SEXP, SEXP);
SEXP Rf_findFun(SEXP, SEXP);
SEXP Rf_findVar(SEXP, SEXP);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, Rboolean);
SEXP Rf_getAttrib(SEXP, SEXP);
SEXP Rf_getClassAttrib(SEXP);
SEXP Rf_getDDVALSymbol(int n);
SEXP Rf_getDimAttrib(SEXP);
SEXP Rf_getDimNamesAttrib(SEXP);
SEXP Rf_getGenericAttrib(SEXP);
SEXP Rf_getLevelsAttrib(SEXP);
SEXP Rf_getNamesAttrib(SEXP);
SEXP Rf_getTspAttrib(SEXP);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
void Rf_GetMatrixDimnames(SEXP, SEXP*, SEXP*, const char**, const char**);
SEXP Rf_GetOption(SEXP, SEXP); /* pre-2.13.0 compatibility */
SEXP Rf_GetOption1(SEXP);
int Rf_GetOptionDigits(void);
int Rf_GetOptionWidth(void);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP, SEXP, SEXP);
SEXP Rf_install(const char *);
SEXP Rf_installNativeCharSXP(SEXP, Rboolean);
SEXP Rf_installCharSXP(SEXP);
SEXP Rf_installCharSXPSignature(SEXP *, int, char);
SEXP Rf_installSignature(SEXP *, int, char);
SEXP Rf_installS3MethodSignature(const char *, const char *);
SEXP Rf_installS3MethodSignatureNativeCharSXP(SEXP, const char *);
Rboolean Rf_isFree(SEXP);
Rboolean Rf_isOrdered(SEXP);
Rboolean Rf_isUnordered(SEXP);
Rboolean Rf_isUnsorted(SEXP, Rboolean);
SEXP Rf_lengthgets(SEXP, R_len_t);
SEXP Rf_xlengthgets(SEXP, R_xlen_t);
SEXP R_lsInternal(SEXP, Rboolean);
SEXP Rf_match(SEXP, SEXP, int);
SEXP Rf_matchE(SEXP, SEXP, int, SEXP);
SEXP Rf_namesgets(SEXP, SEXP);

typedef enum {
    NT_NONE        = 0,
    NT_FROM_UTF8   = 1,
    NT_FROM_LATIN1 = 2,
} nttype_t;


nttype_t needsTranslationToNativeEncoding(SEXP);
SEXP Rf_mkChar(const char *);
SEXP Rf_mkCharLen(const char *, int);
Rboolean Rf_NonNullStringMatch(SEXP, SEXP);
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
SEXP Rf_nthcdr(SEXP, int);

Rboolean Rf_pmatch(SEXP, SEXP, Rboolean);
Rboolean Rf_psmatch(const char *, const char *, Rboolean);
void Rf_PrintValue(SEXP);
#ifndef INLINE_PROTECT
SEXP Rf_protect(SEXP);
#endif
SEXP Rf_setAttrib(SEXP, SEXP, SEXP);
void Rf_setSVector(SEXP*, int, SEXP);
void Rf_setVar(SEXP, SEXP, SEXP);
SEXPTYPE Rf_str2type(const char *);
int Rf_hashed_typename2type(const char *);
void Rf_initializeTypeTables();
void Rf_initializeS3DefaultTypes();
Rboolean Rf_StringBlank(SEXP);
SEXP Rf_substitute(SEXP,SEXP);
const char * Rf_translateChar(SEXP);
const char * Rf_translateChar0(SEXP);
const char * Rf_translateCharUTF8(SEXP);
const char * Rf_type2char(SEXPTYPE);
SEXP translateCharToCharSXP(SEXP);
SEXP Rf_type2str(SEXPTYPE);
SEXP Rf_type2str_noerr(SEXPTYPE);
SEXP Rf_type2ImmutableScalarString(SEXPTYPE);
#ifndef INLINE_PROTECT
void Rf_unprotect(int);
#endif
void Rf_unprotect_ptr(SEXP);

void R_signal_protect_error(void);
void R_signal_unprotect_error(void);
void R_signal_reprotect_error(PROTECT_INDEX i);

#ifndef INLINE_PROTECT
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
#endif
SEXP R_tryEval(SEXP, SEXP, int *);
SEXP R_tryEvalSilent(SEXP, SEXP, int *);
const char *R_curErrorBuf();

Rboolean Rf_isS4(SEXP);
SEXP Rf_asS4(SEXP, Rboolean, int);
SEXP Rf_S3Class(SEXP);
int Rf_isBasicClass(const char *);

Rboolean R_cycle_detected(SEXP s, SEXP child);

typedef enum {
    CE_NATIVE = 0,
    CE_UTF8   = 1,
    CE_LATIN1 = 2,
    CE_BYTES  = 3,
    CE_SYMBOL = 5,
    CE_ANY    =99
} cetype_t;

cetype_t Rf_getCharCE(SEXP);
SEXP Rf_mkCharCE(const char *, cetype_t);
SEXP Rf_mkCharLenCE(const char *, int, cetype_t);
const char *Rf_reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst);

				/* return(.) NOT reached : for -Wall */
#define error_return(msg)	{ Rf_error(msg);	   return R_NilValue; }
#define errorcall_return(cl,msg){ Rf_errorcall(cl, msg);   return R_NilValue; }

#ifdef __MAIN__
#undef extern
#undef LibExtern
#endif

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);
void *R_ExternalPtrAddr(SEXP s);
SEXP R_ExternalPtrTag(SEXP s);
SEXP R_ExternalPtrProtected(SEXP s);
void R_ClearExternalPtr(SEXP s);
void R_SetExternalPtrAddr(SEXP s, void *p);
void R_SetExternalPtrTag(SEXP s, SEXP tag);
void R_SetExternalPtrProtected(SEXP s, SEXP p);

/* Finalization interface */
typedef void (*R_CFinalizer_t)(SEXP);
void R_RegisterFinalizer(SEXP s, SEXP fun);
void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);
void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);
void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);
void R_RunPendingFinalizers(void);

/* Weak reference interface */
SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);
SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);
SEXP R_WeakRefKey(SEXP w);
SEXP R_WeakRefValue(SEXP w);
void R_RunWeakRefFinalizer(SEXP w);

SEXP R_PromiseExpr(SEXP);
SEXP R_ClosureExpr(SEXP);
void R_initialize_bcode(void);
SEXP R_bcEncode(SEXP bytes, SEXP constants);
SEXP R_bcDecode(SEXP code, SEXP constants);
#define PREXPR(e) R_PromiseExpr(e)
#define BODY_EXPR(e) R_ClosureExpr(e)

/* Protected evaluation */
Rboolean R_ToplevelExec(void (*fun)(void *), void *data);
SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
		       void (*cleanfun)(void *), void *cleandata);

/* Environment and Binding Features */
void R_RestoreHashCount(SEXP rho);
Rboolean R_IsPackageEnv(SEXP rho);
SEXP R_PackageEnvName(SEXP rho);
SEXP R_FindPackageEnv(SEXP info);
Rboolean R_IsNamespaceEnv(SEXP rho);
SEXP R_NamespaceEnvSpec(SEXP rho);
SEXP R_FindNamespace(SEXP info);
void R_LockEnvironment(SEXP env, Rboolean bindings);
Rboolean R_EnvironmentIsLocked(SEXP env);
void R_LockBinding(SEXP sym, SEXP env);
void R_unLockBinding(SEXP sym, SEXP env);
void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
Rboolean R_BindingIsActive(SEXP sym, SEXP env);
Rboolean R_HasFancyBindings(SEXP rho);


/* ../main/errors.c : */
/* needed for R_load/savehistory handling in front ends */
#if defined(__GNUC__) && __GNUC__ >= 3
void Rf_errorcall(SEXP, const char *, ...) __attribute__((noreturn));
#else
void Rf_errorcall(SEXP, const char *, ...);
#endif
void Rf_warningcall(SEXP, const char *, ...);
void Rf_warningcall_immediate(SEXP, const char *, ...);

/* Save/Load Interface */
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4

void R_XDREncodeDouble(double d, void *buf);
double R_XDRDecodeDouble(void *buf);
void R_XDREncodeInteger(int i, void *buf);
int R_XDRDecodeInteger(void *buf);

typedef void *R_pstream_data_t;

typedef enum {
    R_pstream_any_format,
    R_pstream_ascii_format,
    R_pstream_binary_format,
    R_pstream_xdr_format
} R_pstream_format_t;

typedef struct R_outpstream_st *R_outpstream_t;
struct R_outpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int version;
    void (*OutChar)(R_outpstream_t, int);
    void (*OutBytes)(R_outpstream_t, void *, int);
    SEXP (*OutPersistHookFunc)(SEXP, SEXP);
    SEXP OutPersistHookData;
};

typedef struct R_inpstream_st *R_inpstream_t;
struct R_inpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int (*InChar)(R_inpstream_t);
    void (*InBytes)(R_inpstream_t, void *, int);
    SEXP (*InPersistHookFunc)(SEXP, SEXP);
    SEXP InPersistHookData;
};

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		     R_pstream_format_t type,
		     int (*inchar)(R_inpstream_t),
		     void (*inbytes)(R_inpstream_t, void *, int),
		     SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata);

void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);

#ifdef NEED_CONNECTION_PSTREAMS
/* The connection interface is not available to packages.  To
   allow limited use of connection pointers this defines the opaque
   pointer type. */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

void R_Serialize(SEXP s, R_outpstream_t ops);
SEXP R_Unserialize(R_inpstream_t ips);

/* slot management (in attrib.c) */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value);
int R_has_slot(SEXP obj, SEXP name);

/* class definition, new objects (objects.c) */
SEXP R_do_MAKE_CLASS(const char *what);
SEXP R_getClassDef  (const char *what);
SEXP R_do_new_object(SEXP class_def);
/* supporting  a C-level version of  is(., .) : */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho);
int R_check_class_etc      (SEXP x, const char **valid);

/* preserve objects across GCs */
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);

/* Shutdown actions */
void R_dot_Last(void);		/* in main.c */
void R_RunExitFinalizers(void);	/* in memory.c */

/* Replacements for popen and system */
#ifdef HAVE_POPEN
FILE *R_popen(const char *, const char *);
#endif
int R_system(const char *);

/* R_compute_identical:  C version of identical() function
   The third arg to R_compute_identical() consists of bitmapped flags for non-default options:
   currently all default to TRUE, so the flag is set for FALSE values:
   1 = !NUM_EQ
   2 = !SINGLE_NA
   4 = !ATTR_AS_SET
   8 = !IGNORE_BYTECODE
*/
Rboolean R_compute_identical(SEXP, SEXP, int);

/* C version of R's  indx <- order(..., na.last, decreasing) :
   e.g.  arglist = Rf_lang2(x,y)  or  Rf_lang3(x,y,z) */
void R_orderVector(int *indx, int n, SEXP arglist, Rboolean nalast, Rboolean decreasing);

#ifndef R_NO_REMAP
#define accessPromargs		Rf_accessPromargs
#define acopy_string		Rf_acopy_string
#define alloc3DArray            Rf_alloc3DArray
#define allocArray		Rf_allocArray
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocS4Object		Rf_allocS4Object
#define allocSExp		Rf_allocSExp
#define allocVector		Rf_allocVector
#define allocVector3		Rf_allocVector3
#define any_duplicated		Rf_any_duplicated
#define any_duplicated3		Rf_any_duplicated3
#define applyClosure		Rf_applyClosure
#define argShift		Rf_argShift
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define asCharacterFactor	Rf_asCharacterFactor
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asReal			Rf_asReal
#define asS4			Rf_asS4
#define begincontext		Rf_begincontext
#define beginposcontext		Rf_beginposcontext
#define buildPositionalPromargs Rf_buildPositionalPromargs
#define classgets		Rf_classgets
#define coerceVector		Rf_coerceVector
#define conformable		Rf_conformable
#define cons			Rf_cons
#define copyListMatrix		Rf_copyListMatrix
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyVector		Rf_copyVector
#define countContexts		Rf_countContexts
#define CreateTag		Rf_CreateTag
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define DropDims                Rf_DropDims
#define duplicate		Rf_duplicate
#define duplicated		Rf_duplicated
#define elt			Rf_elt
#define endcontext		Rf_endcontext
#define errorcall		Rf_errorcall
#define equalS3Signature	Rf_equalS3Signature
#define eval			Rf_eval
#define findFun			Rf_findFun
#define findVar			Rf_findVar
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define getClassAttrib		Rf_getClassAttrib
#define getDDVALSymbol		Rf_getDDVALSymbol
#define getDimAttrib		Rf_getDimAttrib
#define getDimNamesAttrib	Rf_getDimNamesAttrib
#define getGenericAttrib	Rf_getGenericAttrib
#define getLevelsAttrib		Rf_getLevelsAttrib
#define getNamesAttrib		Rf_getNamesAttrib
#define getTspAttrib		Rf_getTspAttrib
#define getCharCE		Rf_getCharCE
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption1		Rf_GetOption1
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetOption		Rf_GetOption
#define GetRowNames		Rf_GetRowNames
#define gsetVar			Rf_gsetVar
#define hashed_typename2type	Rf_hashed_typename2type
#define hashCharSXP		Rf_hashCharSXP
#define inherits		Rf_inherits
#define initializeOffHeapSEXP	Rf_initializeOffHeapSEXP
#define initializeOffHeapEnvironment	Rf_initializeOffHeapEnvironment
#define initializeTypeTables	Rf_initializeTypeTables
#define initializeS3DefaultTypes	Rf_initializeS3DefaultTypes
#define inheritsCharSXP         Rf_inheritsCharSXP
#define install			Rf_install
#define installCharSXP		Rf_installCharSXP
#define installNativeCharSXP		Rf_installNativeCharSXP
#define installCharSXPSignature		Rf_installCharSXPSignature
#define installSignature	Rf_installSignature
#define installS3MethodSignature	Rf_installS3MethodSignature
#define installS3MethodSignatureNativeCharSXP	Rf_installS3MethodSignatureNativeCharSXP
#define isArray			Rf_isArray
#define isBasicClass            Rf_isBasicClass
#define isComplex		Rf_isComplex
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
#define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isSymbol		Rf_isSymbol
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isNumber		Rf_isNumber
#define isObject		Rf_isObject
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isPrimitive		Rf_isPrimitive
#define isReal			Rf_isReal
#define isS4			Rf_isS4
#define isString		Rf_isString
#define isTs			Rf_isTs
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
#define isUserBinop		Rf_isUserBinop
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorizable		Rf_isVectorizable
#define isVectorList		Rf_isVectorList
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lang5			Rf_lang5
#define lang6			Rf_lang6
#define lastElt			Rf_lastElt
#define lazy_duplicate		Rf_lazy_duplicate
#define lcons			Rf_lcons
#define length(x)		Rf_length(x)
#define lengthgets		Rf_lengthgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define list5			Rf_list5
#define listAppend		Rf_listAppend
#define match			Rf_match
#define matchE			Rf_matchE
#define mkChar			Rf_mkChar
#define mkCharCE		Rf_mkCharCE
#define mkCharLen		Rf_mkCharLen
#define mkCharLenCE		Rf_mkCharLenCE
#define mkNamed			Rf_mkNamed
#define mkString		Rf_mkString
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define needsTranslationToNativeEncoding		Rf_needsTranslationToNativeEncoding
#define Newhashpjw		Rf_Newhashpjw
#define NewhashpjwAppend	Rf_NewhashpjwAppend
#define nlevels			Rf_nlevels
#define NonNullStringMatch	Rf_NonNullStringMatch
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintValue		Rf_PrintValue
#define protect			Rf_protect
#define reEnc			Rf_reEnc
#define rownamesgets		Rf_rownamesgets
#define S3Class                 Rf_S3Class
#define ScalarComplex		Rf_ScalarComplex
#define ScalarInteger		Rf_ScalarInteger
#define ScalarLogical		Rf_ScalarLogical
#define ScalarReal		Rf_ScalarReal
#define ScalarString		Rf_ScalarString
#define ScalarRaw		Rf_ScalarRaw
#define setAttrib		Rf_setAttrib
#define setSVector		Rf_setSVector
#define setVar			Rf_setVar
#define shallow_duplicate	Rf_shallow_duplicate
#define str2type		Rf_str2type
#define StringBlank		Rf_StringBlank
#define substitute		Rf_substitute
#define translateChar		Rf_translateChar
#define translateChar0		Rf_translateChar0
#define translateCharUTF8      	Rf_translateCharUTF8
#define translateCharToCharSXP  Rf_translateCharToCharSXP
#define type2char		Rf_type2char
#define type2str		Rf_type2str
#define type2str_noerr		Rf_type2str_noerr
#define type2ImmutableScalarString		Rf_type2ImmutableScalarString
#define unprotect		Rf_unprotect
#define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#define warningcall		Rf_warningcall
#define warningcall_immediate	Rf_warningcall_immediate
#define xlength(x)		Rf_xlength(x)
#define xlengthgets		Rf_xlengthgets

#endif

#if defined(CALLED_FROM_DEFN_H) && !defined(__MAIN__) && (defined(COMPILING_R) || ( __GNUC__ && !defined(__INTEL_COMPILER) ))
#include "Rinlinedfuns.h"
#else
/* need remapped names here for use with R_NO_REMAP */

/*
   These are the inlinable functions that are provided in Rinlinedfuns.h
   It is *essential* that these do not appear in any other header file,
   with or without the Rf_ prefix.
*/

#if defined(R_USE_SIGNALS) && defined(USE_RINTERNALS)
SEXP	Rf_accessPromargs(RCNTXT* cptr);
void	Rf_beginposcontext(RCNTXT * cptr, int flags, SEXP syscall, SEXP env, SEXP sysp, SEXP promargs, SEXP callfun, SEXP* positionalPromargs);
void	Rf_begincontext(RCNTXT * cptr, int flags, SEXP syscall, SEXP env, SEXP sysp, SEXP promargs, SEXP callfun);
void	Rf_endcontext(RCNTXT * cptr);
#endif
SEXP     Rf_allocVector(SEXPTYPE, R_xlen_t);
SEXP     Rf_argShift(SEXP *);
SEXP     Rf_buildPositionalPromargs(int nargs, SEXP *last);
Rboolean Rf_conformable(SEXP, SEXP);
SEXP     Rf_elt(SEXP, int);
int      Rf_hashCharSXP(SEXP charSXP);
Rboolean Rf_inherits(SEXP, const char *);
Rboolean Rf_inheritsCharSXP(SEXP, SEXP);
Rboolean Rf_isArray(SEXP);
Rboolean Rf_isFactor(SEXP);
Rboolean Rf_isFrame(SEXP);
Rboolean Rf_isFunction(SEXP);
Rboolean Rf_isInteger(SEXP);
Rboolean Rf_isLanguage(SEXP);
Rboolean Rf_isList(SEXP);
Rboolean Rf_isMatrix(SEXP);
Rboolean Rf_isNewList(SEXP);
Rboolean Rf_isNumber(SEXP);
Rboolean Rf_isNumeric(SEXP);
Rboolean Rf_isPairList(SEXP);
Rboolean Rf_isPrimitive(SEXP);
Rboolean Rf_isTs(SEXP);
Rboolean Rf_isUserBinop(SEXP);
Rboolean Rf_isValidString(SEXP);
Rboolean Rf_isValidStringF(SEXP);
Rboolean Rf_isVector(SEXP);
Rboolean Rf_isVectorAtomic(SEXP);
Rboolean Rf_isVectorList(SEXP);
Rboolean Rf_isVectorizable(SEXP);
SEXP	 Rf_lang1(SEXP);
SEXP	 Rf_lang2(SEXP, SEXP);
SEXP	 Rf_lang3(SEXP, SEXP, SEXP);
SEXP	 Rf_lang4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lang6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_lastElt(SEXP);
SEXP	 Rf_lcons(SEXP, SEXP);
R_len_t  Rf_length(SEXP);
SEXP	 Rf_list1(SEXP);
SEXP	 Rf_list2(SEXP, SEXP);
SEXP	 Rf_list3(SEXP, SEXP, SEXP);
SEXP	 Rf_list4(SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_list5(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP	 Rf_listAppend(SEXP, SEXP);
SEXP	 Rf_mkNamed(SEXPTYPE, const char **);
SEXP	 Rf_mkString(const char *);
int	 Rf_nlevels(SEXP);
int      Rf_Newhashpjw(const char *);
int      Rf_NewhashpjwAppend(const char *, int);
int      Rf_NewhashpjwAppendChar(char, int);
SEXP	 Rf_ScalarComplex(Rcomplex);
SEXP	 Rf_ScalarInteger(int);
SEXP	 Rf_ScalarLogical(int);
SEXP	 Rf_ScalarRaw(Rbyte);
SEXP	 Rf_ScalarReal(double);
SEXP	 Rf_ScalarString(SEXP);
R_xlen_t  Rf_xlength(SEXP);
# ifdef INLINE_PROTECT
SEXP Rf_protect(SEXP);
void Rf_unprotect(int);
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
# endif
SEXP R_FixupRHS(SEXP x, SEXP y);
#endif

#ifdef USE_RINTERNALS

/* Test macros with function versions above */
#undef isNull
#define isNull(s)	(TYPEOF(s) == NILSXP)
#undef isSymbol
#define isSymbol(s)	(TYPEOF(s) == SYMSXP)
#undef isLogical
#define isLogical(s)	(TYPEOF(s) == LGLSXP)
#undef isReal
#define isReal(s)	(TYPEOF(s) == REALSXP)
#undef isComplex
#define isComplex(s)	(TYPEOF(s) == CPLXSXP)
#undef isExpression
#define isExpression(s) (TYPEOF(s) == EXPRSXP)
#undef isEnvironment
#define isEnvironment(s) (TYPEOF(s) == ENVSXP)
#undef isString
#define isString(s)	(TYPEOF(s) == STRSXP)
#undef isObject
#define isObject(s)	(OBJECT(s) != 0)

/* macro version of R_CheckStack */
#define R_CheckStack() do {						\
	void R_SignalCStackOverflow(intptr_t);				\
	int dummy;							\
	intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy); \
	if(R_CStackLimit != -1 && usage > ((intptr_t) R_CStackLimit))	\
	    R_SignalCStackOverflow(usage);					\
    } while (FALSE)
#endif

#ifdef __cplusplus
}
#endif

#endif /* R_INTERNALS_H_ */
