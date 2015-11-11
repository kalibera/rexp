/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2015	    The R Core Team.
 *  Copyright (C) 2003--2015	    The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000


#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif
#include <Defn.h>		/*-> Arith.h -> math.h */
#ifdef __OpenBSD__
# undef __LIBM_PRIVATE
#endif

#include <Internal.h>

#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("non-numeric argument to mathematical function")

#include <Rmath.h>

#include <R_ext/Itermacros.h>

#include "arithmetic.h"

#include <errno.h>

#ifdef HAVE_MATHERR

/* Override the SVID matherr function:
   the main difference here is not to print warnings.
 */
#ifndef __cplusplus
int matherr(struct exception *exc)
{
    switch (exc->type) {
    case DOMAIN:
    case SING:
	errno = EDOM;
	break;
    case OVERFLOW:
	errno = ERANGE;
	break;
    case UNDERFLOW:
	exc->retval = 0.0;
	break;
	/*
	   There are cases TLOSS and PLOSS which are ignored here.
	   According to the Solaris man page, there are for
	   trigonometric algorithms and not needed for good ones.
	 */
    }
    return 1;
}
#endif
#endif

typedef union
{
    double value;
    unsigned int word[2];
} ieee_double;

/* gcc had problems with static const on AIX and Solaris
   Solaris was for gcc 3.1 and 3.2 under -O2 32-bit on 64-bit kernel */
#ifdef _AIX
#define CONST
#elif defined(sparc) && defined (__GNUC__) && __GNUC__ == 3
#define CONST
#else
#define CONST const
#endif

#ifdef WORDS_BIGENDIAN
static CONST int hw = 0;
static CONST int lw = 1;
#else  /* !WORDS_BIGENDIAN */
static CONST int hw = 1;
static CONST int lw = 0;
#endif /* WORDS_BIGENDIAN */


static double R_ValueOfNA(void)
{
    /* The gcc shipping with Fedora 9 gets this wrong without
     * the volatile declaration. Thanks to Marc Schwartz. */
    volatile ieee_double x;
    x.word[hw] = 0x7ff00000;
    x.word[lw] = 1954;
    return x.value;
}

int R_IsNA(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

int R_IsNaN(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] != 1954);
    }
    return 0;
}

/* ISNAN uses isnan, which is undefined by C++ headers
   This workaround is called only when ISNAN() is used
   in a user code in a file with __cplusplus defined */

int R_isnancpp(double x)
{
   return (isnan(x)!=0);
}


/* Mainly for use in packages */
int R_finite(double x)
{
#ifdef HAVE_WORKING_ISFINITE
    return isfinite(x);
#else
    return (!isnan(x) & (x != R_PosInf) & (x != R_NegInf));
#endif
}


/* Arithmetic Initialization */

void attribute_hidden InitArithmetic()
{
    R_NaInt = INT_MIN;
    R_NaReal = R_ValueOfNA();
// we assume C99, so
#ifndef OLD
    R_NaN = NAN;
    R_PosInf = INFINITY;
    R_NegInf = -INFINITY;
#else
    R_NaN = 0.0/R_Zero_Hack;
    R_PosInf = 1.0/R_Zero_Hack;
    R_NegInf = -1.0/R_Zero_Hack;
#endif
}

/* Keep these two in step */
/* FIXME: consider using
    tmp = (LDOUBLE)x1 - floor(q) * (LDOUBLE)x2;
 */
static double myfmod(double x1, double x2)
{
    if (x2 == 0.0) return R_NaN;
    double q = x1 / x2, tmp = x1 - floor(q) * x2;
    if(R_FINITE(q) && (fabs(q) > 1/R_AccuracyInfo.eps))
	warning(_("probable complete loss of accuracy in modulus"));
    q = floor(tmp/x2);
    return tmp - q * x2;
}

static double myfloor(double x1, double x2)
{
    double q = x1 / x2, tmp;

    if (x2 == 0.0) return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp/x2);
}

double R_pow(double x, double y) /* = x ^ y */
{
    /* squaring is the most common of the specially handled cases so
       check for it first. */
    if(y == 2.0)
	return x * x;
    if(x == 1. || y == 0.)
	return(1.);
    if(x == 0.) {
	if(y > 0.) return(0.);
	else if(y < 0) return(R_PosInf);
	else return(y); /* NA or NaN, we assert */
    }
    if (R_FINITE(x) && R_FINITE(y)) {
	/* There was a special case for y == 0.5 here, but
	   gcc 4.3.0 -g -O2 mis-compiled it.  Showed up with
	   100^0.5 as 3.162278, example(pbirthday) failed. */
#ifdef USE_POWL_IN_R_POW
    return powl(x, y);
#else
    return pow(x, y);
#endif
    }
    if (ISNAN(x) || ISNAN(y))
	return(x + y);
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return (y < 0.)? 0. : R_PosInf;
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return (y < 0.) ? 0. : (myfmod(y, 2.) != 0 ? x  : -x);
	}
    }
    if(!R_FINITE(y)) {
	if(x >= 0) {
	    if(y > 0)		/* y == +Inf */
		return (x >= 1) ? R_PosInf : 0.;
	    else		/* y == -Inf */
		return (x < 1) ? R_PosInf : 0.;
	}
    }
    return R_NaN; // all other cases: (-Inf)^{+-Inf, non-int}; (neg)^{+-Inf}
}

double R_pow_di(double x, int n)
{
    double xn = 1.0;

    if (ISNAN(x)) return x;
    if (n == NA_INTEGER) return NA_REAL;

    if (n != 0) {
	if (!R_FINITE(x)) return R_POW(x, (double)n);

	Rboolean is_neg = (n < 0);
	if(is_neg) n = -n;
	for(;;) {
	    if(n & 01) xn *= x;
	    if(n >>= 1) x *= x; else break;
	}
	if(is_neg) xn = 1. / xn;
    }
    return xn;
}


/* General Base Logarithms */

SEXP R_unary(SEXP, SEXP, SEXP);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
static SEXP logical_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP real_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP real_binary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_binary(ARITHOP_TYPE, SEXP, SEXP, SEXP);

#if 0
static int naflag;
static SEXP lcall;
#endif

/* Integer arithmetic support */

/* The tests using integer comparisons are a bit faster than the tests
   using doubles, but they depend on a two's complement representation
   (but that is almost universal).  The tests that compare results to
   double's depend on being able to accurately represent all int's as
   double's.  Since int's are almost universally 32 bit that should be
   OK. */

#ifndef INT_32_BITS
/* configure checks whether int is 32 bits.  If not this code will
   need to be rewritten.  Since 32 bit ints are pretty much universal,
   we can worry about writing alternate code when the need arises.
   To be safe, we signal a compiler error if int is not 32 bits. */
# error code requires that int have 32 bits
#endif

#define INTEGER_OVERFLOW_WARNING _("NAs produced by integer overflow")

#define CHECK_INTEGER_OVERFLOW(call, ans, naflag) do {		\
	if (naflag) {						\
	    PROTECT(ans);					\
	    warningcall(call, INTEGER_OVERFLOW_WARNING);	\
	    UNPROTECT(1);					\
	}							\
    } while(0)

#define R_INT_MAX  INT_MAX
#define R_INT_MIN -INT_MAX
// .. relying on fact that NA_INTEGER is outside of these

static R_INLINE int R_integer_plus(int x, int y, Rboolean *pnaflag)
{
    if (x == NA_INTEGER || y == NA_INTEGER)
	return NA_INTEGER;

    if (((y > 0) && (x > (R_INT_MAX - y))) ||
	((y < 0) && (x < (R_INT_MIN - y)))) {
	if (pnaflag != NULL)
	    *pnaflag = TRUE;
	return NA_INTEGER;
    }
    return x + y;
}

static R_INLINE int R_integer_minus(int x, int y, Rboolean *pnaflag)
{
    if (x == NA_INTEGER || y == NA_INTEGER)
	return NA_INTEGER;

    if (((y < 0) && (x > (R_INT_MAX + y))) ||
	((y > 0) && (x < (R_INT_MIN + y)))) {
	if (pnaflag != NULL)
	    *pnaflag = TRUE;
	return NA_INTEGER;
    }
    return x - y;
}

#define GOODIPROD(x, y, z) ((double) (x) * (double) (y) == (z))
static R_INLINE int R_integer_times(int x, int y, Rboolean *pnaflag)
{
    if (x == NA_INTEGER || y == NA_INTEGER)
	return NA_INTEGER;
    else {
	int z = x * y;  // UBSAN will warn if this overflows (happens in bda)
	if (GOODIPROD(x, y, z) && z != NA_INTEGER)
	    return z;
	else {
	    if (pnaflag != NULL)
		*pnaflag = TRUE;
	    return NA_INTEGER;
	}
    }
}

static R_INLINE double R_integer_divide(int x, int y)
{
    if (x == NA_INTEGER || y == NA_INTEGER)
	return NA_REAL;
    else
	return (double) x / (double) y;
}

static R_INLINE SEXP ScalarValue1(SEXP x)
{
    if (NO_REFERENCES(x))
	return x;
    else
	return allocVector(TYPEOF(x), 1);
}

static R_INLINE SEXP ScalarValue2(SEXP x, SEXP y)
{
    if (NO_REFERENCES(x))
	return x;
    else if (NO_REFERENCES(y))
	return y;
    else
	return allocVector(TYPEOF(x), 1);
}

/* Unary and Binary Operators */

SEXP attribute_hidden do_arith(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, arg1, arg2;
    int argc;

    if (args == R_NilValue)
	argc = 0;
    else if (CDR(args) == R_NilValue)
	argc = 1;
    else if (CDDR(args) == R_NilValue)
	argc = 2;
    else
	argc = length(args);
    arg1 = CAR(args);
    arg2 = CADR(args);

    if (ATTRIB(arg1) != R_NilValue || ATTRIB(arg2) != R_NilValue) {
	if (DispatchGroup("Ops", call, op, args, env, &ans))
	    return ans;
    }
    else if (argc == 2) {
	/* Handle some scaler operations immediately */
	if (IS_SCALAR(arg1, REALSXP)) {
	    if (IS_SCALAR(arg2, REALSXP)) {
		double x1 = REAL(arg1)[0];
		double x2 = REAL(arg2)[0];
		ans = ScalarValue2(arg1, arg2);
		switch (PRIMVAL(op)) {
		case PLUSOP: REAL(ans)[0] = x1 + x2; return ans;
		case MINUSOP: REAL(ans)[0] = x1 - x2; return ans;
		case TIMESOP: REAL(ans)[0] = x1 * x2; return ans;
		case DIVOP: REAL(ans)[0] = x1 / x2; return ans;
		}
	    }
	    else if (IS_SCALAR(arg2, INTSXP)) {
		double x1 = REAL(arg1)[0];
		double x2 = INTEGER(arg2)[0] != NA_INTEGER ?
		    (double) INTEGER(arg2)[0] : NA_REAL;
		ans = ScalarValue1(arg1);
		switch (PRIMVAL(op)) {
		case PLUSOP: REAL(ans)[0] = x1 + x2; return ans;
		case MINUSOP: REAL(ans)[0] = x1 - x2; return ans;
		case TIMESOP: REAL(ans)[0] = x1 * x2; return ans;
		case DIVOP: REAL(ans)[0] = x1 / x2; return ans;
		}
	    }
	}
	else if (IS_SCALAR(arg1, INTSXP)) {
	    if (IS_SCALAR(arg2, REALSXP)) {
		double x1 = INTEGER(arg1)[0] != NA_INTEGER ?
		    (double) INTEGER(arg1)[0] : NA_REAL;
		double x2 = REAL(arg2)[0];
		ans = ScalarValue1(arg2);
		switch (PRIMVAL(op)) {
		case PLUSOP: REAL(ans)[0] = x1 + x2; return ans;
		case MINUSOP: REAL(ans)[0] = x1 - x2; return ans;
		case TIMESOP: REAL(ans)[0] = x1 * x2; return ans;
		case DIVOP: REAL(ans)[0] = x1 / x2; return ans;
		}
	    }
	    else if (IS_SCALAR(arg2, INTSXP)) {
		Rboolean naflag = FALSE;
		int x1 = INTEGER(arg1)[0];
		int x2 = INTEGER(arg2)[0];
		switch (PRIMVAL(op)) {
		case PLUSOP:
		    ans = ScalarValue2(arg1, arg2);
		    INTEGER(ans)[0] = R_integer_plus(x1, x2, &naflag);
		    CHECK_INTEGER_OVERFLOW(call, ans, naflag);
		    return ans;
		case MINUSOP:
		    ans = ScalarValue2(arg1, arg2);
		    INTEGER(ans)[0] = R_integer_minus(x1, x2, &naflag);
		    CHECK_INTEGER_OVERFLOW(call, ans, naflag);
		    return ans;
		case TIMESOP:
		    ans = ScalarValue2(arg1, arg2);
		    INTEGER(ans)[0] = R_integer_times(x1, x2, &naflag);
		    CHECK_INTEGER_OVERFLOW(call, ans, naflag);
		    return ans;
		case DIVOP:
		    ans = ScalarReal(R_integer_divide(x1, x2));
		    return ans;
		}
	    }
	}
    }
    else if (argc == 1) {
	if (IS_SCALAR(arg1, REALSXP)) {
	    switch(PRIMVAL(op)) {
	    case PLUSOP: return(arg1);
	    case MINUSOP:
		ans = ScalarValue1(arg1);
		REAL(ans)[0] = -REAL(arg1)[0];
		return ans;
	    }
	}
	else if (IS_SCALAR(arg1, INTSXP)) {
	    switch(PRIMVAL(op)) {
	    case PLUSOP: return(arg1);
	    case MINUSOP:
		ans = ScalarValue1(arg1);
		INTEGER(ans)[0] = INTEGER(arg1)[0] == NA_INTEGER ?
		    NA_INTEGER : -INTEGER(arg1)[0];
		return ans;
	    }
	}
    }

    if (argc == 2)
	return R_binary(call, op, arg1, arg2);
    else if (argc == 1)
	return R_unary(call, op, arg1);
    else
	errorcall(call,_("operator needs one or two arguments"));
    return ans;			/* never used; to keep -Wall happy */
}

#define COERCE_IF_NEEDED(v, tp, vpi) do { \
    if (TYPEOF(v) != (tp)) { \
	int __vo__ = OBJECT(v); \
	REPROTECT(v = coerceVector(v, (tp)), vpi); \
	if (__vo__) SET_OBJECT(v, 1); \
    } \
} while (0)

#define FIXUP_NULL_AND_CHECK_TYPES(v, vpi) do { \
    switch (TYPEOF(v)) { \
    case NILSXP: REPROTECT(v = allocVector(REALSXP,0), vpi); break; \
    case CPLXSXP: case REALSXP: case INTSXP: case LGLSXP: break; \
    default: errorcall(lcall, _("non-numeric argument to binary operator")); \
    } \
} while (0)

SEXP attribute_hidden R_binary(SEXP call, SEXP op, SEXP x, SEXP y)
{
    SEXP klass, dims, tsp, xnames, ynames, val;
    R_xlen_t nx, ny, mismatch = 0;
    int xarray, yarray, xts, yts, xS4 = 0, yS4 = 0;
    int xattr, yattr;
    SEXP lcall = call;
    PROTECT_INDEX xpi, ypi;
    ARITHOP_TYPE oper = (ARITHOP_TYPE) PRIMVAL(op);
    int nprotect = 2; /* x and y */


    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    FIXUP_NULL_AND_CHECK_TYPES(x, xpi);
    FIXUP_NULL_AND_CHECK_TYPES(y, ypi);

    nx = XLENGTH(x);
    if (ATTRIB(x) != R_NilValue) {
	xattr = TRUE;
	xarray = isArray(x);
	xts = isTs(x);
	xS4 = isS4(x);
    }
    else xarray = xts = xattr = FALSE;
    ny = XLENGTH(y);
    if (ATTRIB(y) != R_NilValue) {
	yattr = TRUE;
	yarray = isArray(y);
	yts = isTs(y);
	yS4 = isS4(y);
    }
    else yarray = yts = yattr = FALSE;

    /* If either x or y is a matrix with length 1 and the other is a
       vector, we want to coerce the matrix to be a vector.
       Do we want to?  We don't do it!  BDR 2004-03-06
    */

    /* FIXME: Danger Will Robinson.
     * -----  We might be trashing arguments here.
     */
    if (xarray != yarray) {
	if (xarray && nx==1 && ny!=1) {
	    REPROTECT(x = duplicate(x), xpi);
	    setAttrib(x, R_DimSymbol, R_NilValue);
	}
	if (yarray && ny==1 && nx!=1) {
	    REPROTECT(y = duplicate(y), ypi);
	    setAttrib(y, R_DimSymbol, R_NilValue);
	}
    }

    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(lcall, _("non-conformable arrays"));
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else {			/* (yarray) */
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	}
	nprotect++;
	if (xattr) {
	    PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	    nprotect++;
	}
	else xnames = R_NilValue;
	if (yattr) {
	    PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
	    nprotect++;
	}
	else ynames = R_NilValue;
    }
    else {
	dims = R_NilValue;
	if (xattr) {
	    PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	    nprotect++;
	}
	else xnames = R_NilValue;
	if (yattr) {
	    PROTECT(ynames = getAttrib(y, R_NamesSymbol));
	    nprotect++;
	}
	else ynames = R_NilValue;
    }
    if (nx == ny || nx == 1 || ny == 1) mismatch = 0;
    else if (nx > 0 && ny > 0) {
	if (nx > ny) mismatch = nx % ny;
	else mismatch = ny % nx;
    }

    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(lcall, _("non-conformable time-series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (nx < ny)
		ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else {			/* (yts) */
	    if (ny < nx)
		ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
	nprotect += 2;
    }
    else klass = tsp = NULL; /* -Wall */

    if (mismatch)
	warningcall(lcall,
		    _("longer object length is not a multiple of shorter object length"));

    /* need to preserve object here, as *_binary copies class attributes */
    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
	COERCE_IF_NEEDED(x, CPLXSXP, xpi);
	COERCE_IF_NEEDED(y, CPLXSXP, ypi);
	val = complex_binary(oper, x, y);
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	/* real_binary can handle REALSXP or INTSXP operand, but not LGLSXP. */
	/* Can get a LGLSXP. In base-Ex.R on 24 Oct '06, got 8 of these. */
	if (TYPEOF(x) != INTSXP) COERCE_IF_NEEDED(x, REALSXP, xpi);
	if (TYPEOF(y) != INTSXP) COERCE_IF_NEEDED(y, REALSXP, ypi);
	val = real_binary(oper, x, y);
    }
    else val = integer_binary(oper, x, y, lcall);

    /* quick return if there are no attributes */
    if (! xattr && ! yattr) {
	UNPROTECT(nprotect);
	return val;
    }

    PROTECT(val);
    nprotect++;

    /* Don't set the dims if one argument is an array of size 0 and the
       other isn't of size zero, cos they're wrong */
    /* Not if the other argument is a scalar (PR#1979) */
    if (dims != R_NilValue) {
	if (!((xarray && (nx == 0) && (ny > 1)) ||
	      (yarray && (ny == 0) && (nx > 1)))){
	    setAttrib(val, R_DimSymbol, dims);
	    if (xnames != R_NilValue)
		setAttrib(val, R_DimNamesSymbol, xnames);
	    else if (ynames != R_NilValue)
		setAttrib(val, R_DimNamesSymbol, ynames);
	}
    }
    else {
	if (XLENGTH(val) == xlength(xnames))
	    setAttrib(val, R_NamesSymbol, xnames);
	else if (XLENGTH(val) == xlength(ynames))
	    setAttrib(val, R_NamesSymbol, ynames);
    }

    if (xts || yts) {		/* must set *after* dims! */
	setAttrib(val, R_TspSymbol, tsp);
	setAttrib(val, R_ClassSymbol, klass);
    }

    if(xS4 || yS4) {   /* Only set the bit:  no method defined! */
	val = asS4(val, TRUE, TRUE);
    }
    UNPROTECT(nprotect);
    return val;
}

SEXP attribute_hidden R_unary(SEXP call, SEXP op, SEXP s1)
{
    ARITHOP_TYPE operation = (ARITHOP_TYPE) PRIMVAL(op);
    switch (TYPEOF(s1)) {
    case LGLSXP:
	return logical_unary(operation, s1, call);
    case INTSXP:
	return integer_unary(operation, s1, call);
    case REALSXP:
	return real_unary(operation, s1, call);
    case CPLXSXP:
	return complex_unary(operation, s1, call);
    default:
	errorcall(call, _("invalid argument to unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP logical_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    R_xlen_t n = XLENGTH(s1);
    SEXP ans = PROTECT(allocVector(INTSXP, n));
    SEXP names = PROTECT(getAttrib(s1, R_NamesSymbol));
    SEXP dim = PROTECT(getAttrib(s1, R_DimSymbol));
    SEXP dimnames = PROTECT(getAttrib(s1, R_DimNamesSymbol));
    if(names != R_NilValue) setAttrib(ans, R_NamesSymbol, names);
    if(dim != R_NilValue) setAttrib(ans, R_DimSymbol, dim);
    if(dimnames != R_NilValue) setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(3);

    switch (code) {
    case PLUSOP:
	for (R_xlen_t  i = 0; i < n; i++) INTEGER(ans)[i] = LOGICAL(s1)[i];
	break;
    case MINUSOP:
	for (R_xlen_t  i = 0; i < n; i++) {
	    int x = LOGICAL(s1)[i];
	    INTEGER(ans)[i] = (x == NA_INTEGER) ?
		NA_INTEGER : ((x == 0.0) ? 0 : -x);
	}
	break;
    default:
	errorcall(call, _("invalid unary operator"));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP integer_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    R_xlen_t i, n;
    int x;
    SEXP ans;

    switch (code) {
    case PLUSOP:
	return s1;
    case MINUSOP:
	ans = NO_REFERENCES(s1) ? s1 : duplicate(s1);
	n = XLENGTH(s1);
	for (i = 0; i < n; i++) {
	    x = INTEGER(s1)[i];
	    INTEGER(ans)[i] = (x == NA_INTEGER) ?
		NA_INTEGER : ((x == 0.0) ? 0 : -x);
	}
	return ans;
    default:
	errorcall(call, _("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP real_unary(ARITHOP_TYPE code, SEXP s1, SEXP lcall)
{
    R_xlen_t i, n;
    SEXP ans;

    switch (code) {
    case PLUSOP: return s1;
    case MINUSOP:
	ans = NO_REFERENCES(s1) ? s1 : duplicate(s1);
	n = XLENGTH(s1);
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = -REAL(s1)[i];
	return ans;
    default:
	errorcall(lcall, _("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP integer_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2, SEXP lcall)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    int x1, x2;
    SEXP ans;
    Rboolean naflag = FALSE;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) n = 0; else n = (n1 > n2) ? n1 : n2;

    if (code == DIVOP || code == POWOP)
	ans = allocVector(REALSXP, n);
    else
	ans = R_allocOrReuseVector(s1, s2, INTSXP, n);
    if (n == 0) return(ans);
    PROTECT(ans);

    switch (code) {
    case PLUSOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		x1 = INTEGER(s1)[i1];
		x2 = INTEGER(s2)[i2];
		INTEGER(ans)[i] = R_integer_plus(x1, x2, &naflag);
	    });
	if (naflag)
	    warningcall(lcall, INTEGER_OVERFLOW_WARNING);
	break;
    case MINUSOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		x1 = INTEGER(s1)[i1];
		x2 = INTEGER(s2)[i2];
		INTEGER(ans)[i] = R_integer_minus(x1, x2, &naflag);
	    });
	if (naflag)
	    warningcall(lcall, INTEGER_OVERFLOW_WARNING);
	break;
    case TIMESOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		x1 = INTEGER(s1)[i1];
		x2 = INTEGER(s2)[i2];
		INTEGER(ans)[i] = R_integer_times(x1, x2, &naflag);
	    });
	if (naflag)
	    warningcall(lcall, INTEGER_OVERFLOW_WARNING);
	break;
    case DIVOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		x1 = INTEGER(s1)[i1];
		x2 = INTEGER(s2)[i2];
		REAL(ans)[i] = R_integer_divide(x1, x2);
	    });
	break;
    case POWOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		if((x1 = INTEGER(s1)[i1]) == 1 || (x2 = INTEGER(s2)[i2]) == 0)
		    REAL(ans)[i] = 1.;
		else if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		    REAL(ans)[i] = NA_REAL;
		else
		    REAL(ans)[i] = R_POW((double) x1, (double) x2);
	    });
	break;
    case MODOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		x1 = INTEGER(s1)[i1];
		x2 = INTEGER(s2)[i2];
		if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
		    INTEGER(ans)[i] = NA_INTEGER;
		else {
		    INTEGER(ans)[i] = /* till 0.63.2:	x1 % x2 */
			(x1 >= 0 && x2 > 0) ? x1 % x2 :
			(int)myfmod((double)x1,(double)x2);
		}
	    });
	break;
    case IDIVOP:
	MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2, {
		x1 = INTEGER(s1)[i1];
		x2 = INTEGER(s2)[i2];
		/* This had x %/% 0 == 0 prior to 2.14.1, but
		   it seems conventionally to be undefined */
		if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
		    INTEGER(ans)[i] = NA_INTEGER;
		else
		    INTEGER(ans)[i] = (int) floor((double)x1 / (double)x2);
	    });
	break;
    }
    UNPROTECT(1);

    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue)
	return ans;

    /* Copy attributes from longer argument. */

    if (ans != s2 && n == n2 && ATTRIB(s2) != R_NilValue)
	copyMostAttrib(s2, ans);
    if (ans != s1 && n == n1 && ATTRIB(s1) != R_NilValue)
	copyMostAttrib(s1, ans); /* Done 2nd so s1's attrs overwrite s2's */

    return ans;
}

#define R_INTEGER(robj, i) (double) (INTEGER(robj)[i] == NA_INTEGER ? NA_REAL : INTEGER(robj)[i])

static SEXP real_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    SEXP ans;

    /* Note: "s1" and "s2" are protected above. */
    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);

    /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) return(allocVector(REALSXP, 0));

    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = R_allocOrReuseVector(s1, s2, REALSXP, n));

    switch (code) {
    case PLUSOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    double *dx = REAL(s1);
	    double *dy = REAL(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] + tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp + dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] + dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] + dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = R_INTEGER(s1, i1) + REAL(s2)[i2];);
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = REAL(s1)[i1] + R_INTEGER(s2, i2););
	break;
    case MINUSOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    double *dx = REAL(s1);
	    double *dy = REAL(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] - tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp - dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] - dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] - dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = R_INTEGER(s1, i1) - REAL(s2)[i2];);
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = REAL(s1)[i1] - R_INTEGER(s2, i2););
	break;
    case TIMESOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    double *dx = REAL(s1);
	    double *dy = REAL(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] * tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = REAL(s1)[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp * dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] * dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] * dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = R_INTEGER(s1, i1) * REAL(s2)[i2];);
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = REAL(s1)[i1] * R_INTEGER(s2, i2););
	break;
    case DIVOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    double *dx = REAL(s1);
	    double *dy = REAL(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] / tmp;);
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = tmp / dy[i];);
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = dx[i] / dy[i];);
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = dx[i1] / dy[i2];);
	}
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = R_INTEGER(s1, i1) / REAL(s2)[i2];);
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = REAL(s1)[i1] / R_INTEGER(s2, i2););
	break;
    case POWOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	    double *da = REAL(ans);
	    double *dx = REAL(s1);
	    double *dy = REAL(s2);
	    if (n2 == 1) {
		double tmp = dy[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = R_POW(dx[i], tmp););
	    }
	    else if (n1 == 1) {
		double tmp = dx[0];
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = R_POW(tmp, dy[i]););
	    }
	    else if (n1 == n2)
		R_ITERATE_CHECK(NINTERRUPT, n, i, da[i] = R_POW(dx[i], dy[i]););
	    else
		MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
				  da[i] = R_POW(dx[i1], dy[i2]););
	}
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = R_POW( R_INTEGER(s1, i1),
						    REAL(s2)[i2]););
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = R_POW(REAL(s1)[i1],
						   R_INTEGER(s2, i2)););
	break;
    case MODOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP)
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = myfmod(REAL(s1)[i1],
						    REAL(s2)[i2]););
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = myfmod( R_INTEGER(s1, i1),
						     REAL(s2)[i2]););
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = myfmod(REAL(s1)[i1],
						    R_INTEGER(s2, i2)););
	break;
    case IDIVOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP)
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = myfloor(REAL(s1)[i1],
						     REAL(s2)[i2]););
	else if(TYPEOF(s1) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = myfloor(R_INTEGER(s1, i1),
						     REAL(s2)[i2]););
	else if(TYPEOF(s2) == INTSXP )
	    MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
			      REAL(ans)[i] = myfloor(REAL(s1)[i1],
						     R_INTEGER(s2,i2)););
	break;
    }
    UNPROTECT(1);

    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue)
	return ans;

    /* Copy attributes from longer argument. */

    if (ans != s2 && n == n2 && ATTRIB(s2) != R_NilValue)
	copyMostAttrib(s2, ans);
    if (ans != s1 && n == n1 && ATTRIB(s1) != R_NilValue)
	copyMostAttrib(s1, ans); /* Done 2nd so s1's attrs overwrite s2's */

    return ans;
}


/* Mathematical Functions of One Argument */

static SEXP math1(SEXP sa, double(*f)(double), SEXP lcall)
{
    SEXP sy;
    double *y, *a;
    R_xlen_t i, n;
    int naflag;

    if (!isNumeric(sa))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    n = XLENGTH(sa);
    /* coercion can lose the object bit */
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sy = NO_REFERENCES(sa) ? sa : allocVector(REALSXP, n));
    a = REAL(sa);
    y = REAL(sy);
    naflag = 0;
    for (i = 0; i < n; i++) {
	if (ISNAN(a[i]))
	    y[i] = a[i];
	else {
	    y[i] = f(a[i]);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    /* These are primitives, so need to use the call */
    if(naflag) warningcall(lcall, R_MSG_NA);

    if (sa != sy && ATTRIB(sa) != R_NilValue)
	SHALLOW_DUPLICATE_ATTRIB(sy, sa);
    UNPROTECT(2);
    return sy;
}


SEXP attribute_hidden do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    if (isComplex(CAR(args)))
	return complex_math1(call, op, args, env);

#define MATH1(x) math1(CAR(args), x, call);
    switch (PRIMVAL(op)) {
    case 1: return MATH1(floor);
    case 2: return MATH1(ceil);
    case 3: return MATH1(sqrt);
    case 4: return MATH1(sign);
	/* case 5: return MATH1(trunc); separate from 2.6.0 */

    case 10: return MATH1(exp);
    case 11: return MATH1(expm1);
    case 12: return MATH1(log1p);
    case 20: return MATH1(cos);
    case 21: return MATH1(sin);
    case 22: return MATH1(tan);
    case 23: return MATH1(acos);
    case 24: return MATH1(asin);
    case 25: return MATH1(atan);

    case 30: return MATH1(cosh);
    case 31: return MATH1(sinh);
    case 32: return MATH1(tanh);
    case 33: return MATH1(acosh);
    case 34: return MATH1(asinh);
    case 35: return MATH1(atanh);

    case 40: return MATH1(lgammafn);
    case 41: return MATH1(gammafn);

    case 42: return MATH1(digamma);
    case 43: return MATH1(trigamma);
	/* case 44: return MATH1(tetragamma);
	   case 45: return MATH1(pentagamma);
	   removed in 2.0.0

	   case 46: return MATH1(Rf_gamma_cody); removed in 2.8.0
	*/
    case 47: return MATH1(cospi);
    case 48: return MATH1(sinpi);
#ifndef HAVE_TANPI
    case 49: return MATH1(tanpi);
#else
    case 49: return MATH1(Rtanpi);
#endif

    default:
	errorcall(call, _("unimplemented real function of 1 argument"));
    }
    return s; /* never used; to keep -Wall happy */
}

/* methods are allowed to have more than one arg */
SEXP attribute_hidden do_trunc(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;
    checkArity(op, args); /* but is -1 in names.c */
    check1arg(args, call, "x");
    if (isComplex(CAR(args)))
	errorcall(call, _("unimplemented complex function"));
    return math1(CAR(args), trunc, call);
}

/*
   Note that this is slightly different from the do_math1 set,
   both for integer/logical inputs and what it dispatches to for complex ones.
*/

SEXP attribute_hidden do_abs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, s = R_NilValue /* -Wall */;

    checkArity(op, args);
    check1arg(args, call, "x");
    x = CAR(args);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    if (isInteger(x) || isLogical(x)) {
	/* integer or logical ==> return integer,
	   factor was covered by Math.factor. */
	R_xlen_t i, n = XLENGTH(x);
	s = (NO_REFERENCES(x) && TYPEOF(x) == INTSXP) ?
	    x : allocVector(INTSXP, n);
	PROTECT(s);
	/* Note: relying on INTEGER(.) === LOGICAL(.) : */
	for(i = 0 ; i < n ; i++) {
	    int xi = INTEGER(x)[i];
	    INTEGER(s)[i] = (xi == NA_INTEGER) ? xi : abs(xi);
	}
    } else if (TYPEOF(x) == REALSXP) {
	R_xlen_t i, n = XLENGTH(x);
	PROTECT(s = NO_REFERENCES(x) ? x : allocVector(REALSXP, n));
	for(i = 0 ; i < n ; i++)
	    REAL(s)[i] = fabs(REAL(x)[i]);
    } else if (isComplex(x)) {
	SET_TAG(args, R_NilValue); /* cmathfuns want "z"; we might have "x" PR#16047 */
	return do_cmathfuns(call, op, args, env);
    } else
	errorcall(call, R_MSG_NONNUM_MATH);

    if (x != s && ATTRIB(x) != R_NilValue)
	SHALLOW_DUPLICATE_ATTRIB(s, x);
    UNPROTECT(1);
    return s;
}

/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

/* math2_1 and math2_2 and related can be removed  once the byte
  compiler knows how to optimize to .External rather than
  .Internal */

#define if_NA_Math2_set(y,a,b)				\
	if      (ISNA (a) || ISNA (b)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)) y = R_NaN;

static SEXP math2(SEXP sa, SEXP sb, double (*f)(double, double),
		  SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    /* for 0-length a we want the attributes of a, not those of b
       as no recycling will occur */
#define SETUP_Math2					\
    na = XLENGTH(sa);					\
    nb = XLENGTH(sb);					\
    if ((na == 0) || (nb == 0))	{			\
	PROTECT(sy = allocVector(REALSXP, 0));		\
	if (na == 0) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
	UNPROTECT(1);					\
	return(sy);					\
    }							\
    n = (na < nb) ? nb : na;				\
    PROTECT(sa = coerceVector(sa, REALSXP));		\
    PROTECT(sb = coerceVector(sb, REALSXP));		\
    PROTECT(sy = allocVector(REALSXP, n));		\
    a = REAL(sa);					\
    b = REAL(sb);					\
    y = REAL(sy);					\
    naflag = 0

    SETUP_Math2;

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math2					\
    if(naflag) warning(R_MSG_NA);			\
    if (n == na)  SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    UNPROTECT(3)

    FINISH_Math2;

    return sy;
} /* math2() */

static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI,
		    double (*f)(double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int m_opt;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    m_opt = asInteger(sI);

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, m_opt);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math2;
    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2,
		    double (*f)(double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int i_1, i_2;
    int naflag;
    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    i_1 = asInteger(sI1);
    i_2 = asInteger(sI2);

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math2;
    return sy;
} /* math2_2() */

/* This is only used directly by .Internal for Bessel functions,
   so managing R_alloc stack is only prudence */
static SEXP math2B(SEXP sa, SEXP sb, double (*f)(double, double, double *),
		   SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int naflag;
    double amax, *work;
    size_t nw;

#define besselJY_max_nu 1e7

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    /* for 0-length a we want the attributes of a, not those of b
       as no recycling will occur */
    SETUP_Math2;

    /* allocate work array for BesselJ, BesselY large enough for all
       arguments */
    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (amax < av)
	    amax = av;
    }
    if (amax > besselJY_max_nu)
	amax = besselJY_max_nu; // and warning will happen in ../nmath/bessel_[jy].c
    const void *vmax = vmaxget();
    nw = 1 + (size_t)floor(amax);
    work = (double *) R_alloc(nw, sizeof(double));

    MOD_ITERATE2(n, na, nb, i, ia, ib, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    vmaxset(vmax);
    FINISH_Math2;

    return sy;
} /* math2B() */

SEXP attribute_hidden
dc_math2(SEXP call, SEXP op, SEXP env, SEXP sa, SEXP sb, SEXP sI, SEXP sJ)
{
    if (isComplex(sa) ||
	(PRIMVAL(op) == 0 && isComplex(sb)))
	return complex_math2(call, op, env, sa, sb);

    switch (PRIMVAL(op)) {

    case  0: return math2(sa, sb, atan2, call);
    case 10001: return math2(sa, sb, fround, call);// round(),  ../nmath/fround.c
    case 10004: return math2(sa, sb, fprec, call); // signif(), ../nmath/fprec.c

    case  2: return math2(sa, sb, lbeta, call);
    case  3: return math2(sa, sb, beta, call);
    case  4: return math2(sa, sb, lchoose, call);
    case  5: return math2(sa, sb, choose, call);

    case  6: return math2_1(sa, sb, sI, dchisq, call);
    case  7: return math2_2(sa, sb, sI, sJ, pchisq, call);
    case  8: return math2_2(sa, sb, sI, sJ, qchisq, call);

    case  9: return math2_1(sa, sb, sI, dexp, call);
    case 10: return math2_2(sa, sb, sI, sJ, pexp, call);
    case 11: return math2_2(sa, sb, sI, sJ, qexp, call);

    case 12: return math2_1(sa, sb, sI, dgeom, call);
    case 13: return math2_2(sa, sb, sI, sJ, pgeom, call);
    case 14: return math2_2(sa, sb, sI, sJ, qgeom, call);

    case 15: return math2_1(sa, sb, sI, dpois, call);
    case 16: return math2_2(sa, sb, sI, sJ, ppois, call);
    case 17: return math2_2(sa, sb, sI, sJ, qpois, call);

    case 18: return math2_1(sa, sb, sI, dt, call);
    case 19: return math2_2(sa, sb, sI, sJ, pt, call);
    case 20: return math2_2(sa, sb, sI, sJ, qt, call);

    case 21: return math2_1(sa, sb, sI, dsignrank, call);
    case 22: return math2_2(sa, sb, sI, sJ, psignrank, call);
    case 23: return math2_2(sa, sb, sI, sJ, qsignrank, call);

    case 24: return math2B(sa, sb, bessel_j_ex, call);
    case 25: return math2B(sa, sb, bessel_y_ex, call);
    case 26: return math2(sa, sb, psigamma, call);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 2);
    }
    return op;			/* never used; to keep -Wall happy */
}


/* The S4 Math2 group, round and signif */
/* This is a primitive SPECIALSXP with internal argument matching */
SEXP attribute_hidden do_Math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2;
    int n, nprotect = 2;
    static SEXP do_Math2_formals = NULL;

    if (length(args) >= 2 &&
	isSymbol(CADR(args)) && R_isMissing(CADR(args), env)) {
	double digits = 0;
	if(PRIMVAL(op) == 10004) digits = 6.0; // for signif()
	PROTECT(args = list2(CAR(args), ScalarReal(digits))); nprotect++;
    }

    PROTECT(args = evalListKeepMissing(args, env));
    PROTECT(call2 = lang2(CAR(call), R_NilValue));
    SETCDR(call2, args);

    n = length(args);
    if (n != 1 && n != 2)
	error(ngettext("%d argument passed to '%s' which requires 1 or 2 arguments",
		       "%d arguments passed to '%s'which requires 1 or 2 arguments", n),
	      n, PRIMNAME(op));

    if (! DispatchGroup("Math", call2, op, args, env, &res)) {
	if(n == 1) {
	    double digits = 0.0;
	    if(PRIMVAL(op) == 10004) digits = 6.0;
	    SETCDR(args, CONS(ScalarReal(digits), R_NilValue));
	} else {
	    /* If named, do argument matching by name */
	    if (TAG(args) != R_NilValue || TAG(CDR(args)) != R_NilValue) {
		if (do_Math2_formals == NULL)
		    do_Math2_formals = allocFormalsList2(install("x"),
							 install("digits"));
		PROTECT(args = matchArgs(do_Math2_formals, args, call));
		nprotect++;
	    }
	    if (length(CADR(args)) == 0)
		errorcall(call, _("invalid second argument of length 0"));
	}
	res = dc_math2(call, op, env, CAR(args), CADR(args), R_NilValue, R_NilValue);
    }
    UNPROTECT(nprotect);
    return res;
}

/* log{2,10} are builtins */
SEXP attribute_hidden do_log1arg(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res, call2, args2, tmp = R_NilValue /* -Wall */;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchGroup("Math", call, op, args, env, &res)) return res;

    SEXP sLog = install("log");
    if(PRIMVAL(op) == 10) tmp = ScalarReal(10.0);
    if(PRIMVAL(op) == 2)  tmp = ScalarReal(2.0);

    PROTECT(call2 = lang3(sLog, CAR(args), tmp));
    PROTECT(args2 = lang2(CAR(args), tmp));
    if (! DispatchGroup("Math", call2, op, args2, env, &res)) {
	if (isComplex(CAR(args)))
	    res = complex_math2(call2, op, env, CAR(args), tmp);
	else
	    res = math2(CAR(args), tmp, logbase, call);
    }
    UNPROTECT(2);
    return res;
}

#ifdef M_E
# define DFLT_LOG_BASE M_E
#else
# define DFLT_LOG_BASE exp(1.)
#endif

/* do_log is a primitive SPECIALSXP with internal argument
   matching. do_log_builtin is the BUILTIN version that expects
   evaluated arguments to be passed as 'args', expect that these may
   contain missing arguments.  */
SEXP attribute_hidden do_log(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = evalListKeepMissing(args, env);
    return  do_log_builtin(call, op, args, env);
}

SEXP attribute_hidden do_log_builtin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    PROTECT(args);
    int n = length(args);
    SEXP res;

    if (n == 1 && TAG(args) == R_NilValue) {
	/* log(x) is handled here */
	SEXP x = CAR(args);
	if (x != R_MissingArg && ! OBJECT(x)) {
	    if (isComplex(x))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(x, R_log, call);
	    UNPROTECT(1);
	    return res;
	}
    }
    else if (n == 2 &&
	     TAG(args) == R_NilValue &&
	     (TAG(CDR(args)) == R_NilValue || TAG(CDR(args)) == R_BaseSymbol)) {
	/* log(x, y) or log(x, base = y) are handled here */
	SEXP x = CAR(args);
	SEXP y = CADR(args);
	if (x != R_MissingArg && y != R_MissingArg &&
	    ! OBJECT(x) && ! OBJECT(y)) {
	    if (isComplex(x) || isComplex(y))
		res = complex_math2(call, op, env, x, y);
	    else
		res = math2(x, y, logbase, call);
	    UNPROTECT(1);
	    return res;
	}
    }

    static SEXP do_log_formals = NULL;
    static SEXP R_x_Symbol = NULL;
    if (do_log_formals == NULL) {
	R_x_Symbol = install("x");
	do_log_formals = allocFormalsList2(R_x_Symbol, R_BaseSymbol);
    }

    if (n == 1) {
	if (CAR(args) == R_MissingArg ||
	    (TAG(args) != R_NilValue && TAG(args) != R_x_Symbol))
	    error(_("argument \"%s\" is missing, with no default"), "x");

	if (! DispatchGroup("Math", call, op, args, env, &res)) {
	    if (isComplex(CAR(args)))
		res = complex_math1(call, op, args, env);
	    else
		res = math1(CAR(args), R_log, call);
	}
	UNPROTECT(1);
	return res;
    }
    else {
	/* match argument names if supplied */
	/* will signal an error unless there are one or two arguments */
	/* after the match, length(args) will be 2 */
	PROTECT(args = matchArgs(do_log_formals, args, call));

	if(CAR(args) == R_MissingArg)
	    error(_("argument \"%s\" is missing, with no default"), "x");
	if (CADR(args) == R_MissingArg)
	    SETCADR(args, ScalarReal(DFLT_LOG_BASE));

	if (! DispatchGroup("Math", call, op, args, env, &res)) {
	    if (length(CADR(args)) == 0)
		errorcall(call, _("invalid argument 'base' of length 0"));
	    if (isComplex(CAR(args)) || isComplex(CADR(args)))
		res = complex_math2(call, op, env, CAR(args), CADR(args));
	    else
		res = math2(CAR(args), CADR(args), logbase, call);
	}
	UNPROTECT(2);
	return res;
    }
}


/* Mathematical Functions of Three (Real) Arguments */

/* math3_1 and math3_2 and related can be removed once the byte
  compiler knows how to optimize to .External rather than
  .Internal */


#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define SETUP_Math3						\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))	\
	errorcall(lcall, R_MSG_NONNUM_MATH);			\
								\
    na = XLENGTH(sa);						\
    nb = XLENGTH(sb);						\
    nc = XLENGTH(sc);						\
    if ((na == 0) || (nb == 0) || (nc == 0))			\
	return(allocVector(REALSXP, 0));			\
    n = na;							\
    if (n < nb) n = nb;						\
    if (n < nc) n = nc;						\
    PROTECT(sa = coerceVector(sa, REALSXP));			\
    PROTECT(sb = coerceVector(sb, REALSXP));			\
    PROTECT(sc = coerceVector(sc, REALSXP));			\
    PROTECT(sy = allocVector(REALSXP, n));			\
    a = REAL(sa);						\
    b = REAL(sb);						\
    c = REAL(sc);						\
    y = REAL(sy);						\
    naflag = 0

#define FINISH_Math3					\
    if(naflag) warning(R_MSG_NA);			\
							\
    if (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    UNPROTECT(4)

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI,
		    double (*f)(double, double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);

    MOD_ITERATE3(n, na, nb, nc, i, ia, ib, ic, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;
    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1,i_2;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    MOD_ITERATE3 (n, na, nb, nc, i, ia, ib, ic, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;
    return sy;
} /* math3_2 */

/* This is only used directly by .Internal for Bessel functions,
   so managing R_alloc stack is only prudence */
static SEXP math3B(SEXP sa, SEXP sb, SEXP sc,
		   double (*f)(double, double, double, double *), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int naflag;
    double amax, *work;
    size_t nw;

    SETUP_Math3;

    /* allocate work array for BesselI, BesselK large enough for all
       arguments */
    amax = 0.0;
    for (i = 0; i < nb; i++) {
	double av = b[i] < 0 ? -b[i] : b[i];
	if (av > amax) amax = av;
    }
    const void *vmax = vmaxget();
    nw = 1 + (size_t)floor(amax);
    work = (double *) R_alloc(nw, sizeof(double));

    MOD_ITERATE3 (n, na, nb, nc, i, ia, ib, ic, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, work);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

    FINISH_Math3;
    vmaxset(vmax);

    return sy;
} /* math3B */

SEXP attribute_hidden
dc_math3(SEXP call, SEXP op, SEXP env, SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ)
{
    switch (PRIMVAL(op)) {

    case  1:  return math3_1(sa, sb, sc, sI, dbeta, call);
    case  2:  return math3_2(sa, sb, sc, sI, sJ, pbeta, call);
    case  3:  return math3_2(sa, sb, sc, sI, sJ, qbeta, call);

    case  4:  return math3_1(sa, sb, sc, sI, dbinom, call);
    case  5:  return math3_2(sa, sb, sc, sI, sJ, pbinom, call);
    case  6:  return math3_2(sa, sb, sc, sI, sJ, qbinom, call);

    case  7:  return math3_1(sa, sb, sc, sI, dcauchy, call);
    case  8:  return math3_2(sa, sb, sc, sI, sJ, pcauchy, call);
    case  9:  return math3_2(sa, sb, sc, sI, sJ, qcauchy, call);

    case 10:  return math3_1(sa, sb, sc, sI, df, call);
    case 11:  return math3_2(sa, sb, sc, sI, sJ, pf, call);
    case 12:  return math3_2(sa, sb, sc, sI, sJ, qf, call);

    case 13:  return math3_1(sa, sb, sc, sI, dgamma, call);
    case 14:  return math3_2(sa, sb, sc, sI, sJ, pgamma, call);
    case 15:  return math3_2(sa, sb, sc, sI, sJ, qgamma, call);

    case 16:  return math3_1(sa, sb, sc, sI, dlnorm, call);
    case 17:  return math3_2(sa, sb, sc, sI, sJ, plnorm, call);
    case 18:  return math3_2(sa, sb, sc, sI, sJ, qlnorm, call);

    case 19:  return math3_1(sa, sb, sc, sI, dlogis, call);
    case 20:  return math3_2(sa, sb, sc, sI, sJ, plogis, call);
    case 21:  return math3_2(sa, sb, sc, sI, sJ, qlogis, call);

    case 22:  return math3_1(sa, sb, sc, sI, dnbinom, call);
    case 23:  return math3_2(sa, sb, sc, sI, sJ, pnbinom, call);
    case 24:  return math3_2(sa, sb, sc, sI, sJ, qnbinom, call);

    case 25:  return math3_1(sa, sb, sc, sI, dnorm, call);
    case 26:  return math3_2(sa, sb, sc, sI, sJ, pnorm, call);
    case 27:  return math3_2(sa, sb, sc, sI, sJ, qnorm, call);

    case 28:  return math3_1(sa, sb, sc, sI, dunif, call);
    case 29:  return math3_2(sa, sb, sc, sI, sJ, punif, call);
    case 30:  return math3_2(sa, sb, sc, sI, sJ, qunif, call);

    case 31:  return math3_1(sa, sb, sc, sI, dweibull, call);
    case 32:  return math3_2(sa, sb, sc, sI, sJ, pweibull, call);
    case 33:  return math3_2(sa, sb, sc, sI, sJ, qweibull, call);

    case 34:  return math3_1(sa, sb, sc, sI, dnchisq, call);
    case 35:  return math3_2(sa, sb, sc, sI, sJ, pnchisq, call);
    case 36:  return math3_2(sa, sb, sc, sI, sJ, qnchisq, call);

    case 37:  return math3_1(sa, sb, sc, sI, dnt, call);
    case 38:  return math3_2(sa, sb, sc, sI, sJ, pnt, call);
    case 39:  return math3_2(sa, sb, sc, sI, sJ, qnt, call);

    case 40:  return math3_1(sa, sb, sc, sI, dwilcox, call);
    case 41:  return math3_2(sa, sb, sc, sI, sJ, pwilcox, call);
    case 42:  return math3_2(sa, sb, sc, sI, sJ, qwilcox, call);

    case 43:  return math3B(sa, sb, sc, bessel_i_ex, call);
    case 44:  return math3B(sa, sb, sc, bessel_k_ex, call);

    case 45:  return math3_1(sa, sb, sc, sI, dnbinom_mu, call);
    case 46:  return math3_2(sa, sb, sc, sI, sJ, pnbinom_mu, call);
    case 47:  return math3_2(sa, sb, sc, sI, sJ, qnbinom_mu, call);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 3);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

/* This can be removed completely once the byte compiler knows how to
  optimize to .External rather than .Internal */

#define if_NA_Math4_set(y,a,b,c,d)				\
	if      (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)) y = NA_REAL;\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)) y = R_NaN;


static SEXP math4(SEXP sa, SEXP sb, SEXP sc, SEXP sd,
		  double (*f)(double, double, double, double), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int naflag;

#define SETUP_Math4							\
    if(!isNumeric(sa)|| !isNumeric(sb)|| !isNumeric(sc)|| !isNumeric(sd))\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0))		\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math4;

    MOD_ITERATE4 (n, na, nb, nc, nd, i, ia, ib, ic, id, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math4					\
    if(naflag) warning(R_MSG_NA);			\
							\
    if (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) SHALLOW_DUPLICATE_ATTRIB(sy, sd);	\
    UNPROTECT(5)

    FINISH_Math4;

    return sy;
} /* math4() */

static SEXP math4_1(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, double (*f)(double, double, double, double, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);

    MOD_ITERATE4 (n, na, nb, nc, nd, i, ia, ib, ic, id, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math4;
    return sy;
} /* math4_1() */

static SEXP math4_2(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1, i_2;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    MOD_ITERATE4 (n, na, nb, nc, nd, i, ia, ib, ic, id, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });
    FINISH_Math4;
    return sy;
} /* math4_2() */

SEXP attribute_hidden dc_math4(SEXP call, SEXP op, SEXP env,
  SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ)
{
    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- math4() at all! : */
    case -99: return math4(sa, sb, sc, sd,
                (double (*)(double, double, double, double))NULL, call);

    case  1: return math4_1(sa, sb, sc, sd, sI, dhyper, call);
    case  2: return math4_2(sa, sb, sc, sd, sI, sJ, phyper, call);
    case  3: return math4_2(sa, sb, sc, sd, sI, sJ, qhyper, call);

    case  4: return math4_1(sa, sb, sc, sd, sI, dnbeta, call);
    case  5: return math4_2(sa, sb, sc, sd, sI, sJ, pnbeta, call);
    case  6: return math4_2(sa, sb, sc, sd, sI, sJ, qnbeta, call);
    case  7: return math4_1(sa, sb, sc, sd, sI, dnf, call);
    case  8: return math4_2(sa, sb, sc, sd, sI, sJ, pnf, call);
    case  9: return math4_2(sa, sb, sc, sd, sI, sJ, qnf, call);
#ifdef UNIMP
    case 10: return math4_1(sa, sb, sc, sd, sI, dtukey, call);
#endif
    case 11: return math4_2(sa, sb, sc, sd, sI, sJ, ptukey, call);
    case 12: return math4_2(sa, sb, sc, sd, sI, sJ, qtukey, call);
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 4);
    }
    return op;			/* never used; to keep -Wall happy */
}


#ifdef WHEN_MATH5_IS_THERE/* as in ./arithmetic.h */

/* Mathematical Functions of Five (Real) Arguments */

#define if_NA_Math5_set(y,a,b,c,d,e)					\
	if     (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)|| ISNA (e))	\
		y = NA_REAL;						\
	else if(ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)|| ISNAN(e))	\
		y = R_NaN;

static SEXP math5(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se, double (*f)())
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne;
    double ai, bi, ci, di, ei, *a, *b, *c, *d, *e, *y;

#define SETUP_Math5							\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||		\
	!isNumeric(sd) || !isNumeric(se))				\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    ne = XLENGTH(se);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0))	\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */		\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(se = coerceVector(se, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    e = REAL(se);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math5;

    MOD_ITERATE5 (n, na, nb, nc, nd, ne,
		  i, ia, ib, ic, id, ie, {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if_NA_Math5_set(y[i], ai,bi,ci,di,ei)
	else {
	    y[i] = f(ai, bi, ci, di, ei);
	    if (ISNAN(y[i])) naflag = 1;
	}
    });

#define FINISH_Math5					\
    if(naflag) warning(R_MSG_NA);			\
							\
    if (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) SHALLOW_DUPLICATE_ATTRIB(sy, sd);	\
    else if (n == ne) SHALLOW_DUPLICATE_ATTRIB(sy, se);	\
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

SEXP attribute_hidden do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- use math5() at all! : */
    case -99: return Math5(args, dhyper);
#ifdef UNIMP
    case  2: return Math5(args, p...);
    case  3: return Math5(args, q...);
#endif
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 5);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */

/* This is used for experimenting with parallelized nmath functions -- LT */
CCODE R_get_arith_function(int which)
{
    // if this functionality is needed, we can implement wrappers to
    // emulate the old interface
    error("R_get_arith_function is now defunct"); return NULL;
/*
    switch (which) {
    case 1: return do_math1;
    case 2: return do_math2;
    case 3: return do_math3;
    case 4: return do_math4;
    case 11: return complex_math1;
    case 12: return complex_math2;
    default: error("bad arith function index"); return NULL;
    }
*/
}
