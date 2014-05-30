/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2012  The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* this header is always to be included from others.
   It is only called if COMPILING_R is defined (in util.c) or
   from GNU C systems.

   There are different conventions for inlining across compilation units.
   See http://www.greenend.org.uk/rjk/2003/03/inline.html
 */
#ifndef R_INLINES_H_
#define R_INLINES_H_

/* Probably not able to use C99 semantics in gcc < 4.3.0 */
#if __GNUC__ == 4 && __GNUC_MINOR__ >= 3 && defined(__GNUC_STDC_INLINE__) && !defined(C99_INLINE_SEMANTICS)
#define C99_INLINE_SEMANTICS 1
#endif

/* Apple's gcc build >5400 (since Xcode 3.0) doesn't support GNU inline in C99 mode */
#if __APPLE_CC__ > 5400 && !defined(C99_INLINE_SEMANTICS) && __STDC_VERSION__ >= 199901L
#define C99_INLINE_SEMANTICS 1
#endif

#ifdef COMPILING_R
/* defined only in inlined.c: this emits standalone code there */
# define INLINE_FUN
#else
/* This section is normally only used for versions of gcc which do not
   support C99 semantics.  __GNUC_STDC_INLINE__ is defined if
   GCC is following C99 inline semantics by default: we
   switch R's usage to the older GNU semantics via attributes.
   Do this even for __GNUC_GNUC_INLINE__ to shut up warnings in 4.2.x.
   __GNUC_STDC_INLINE__ and __GNUC_GNU_INLINE__ were added in gcc 4.2.0.
*/
# if defined(__GNUC_STDC_INLINE__) || defined(__GNUC_GNU_INLINE__)
#  define INLINE_FUN extern __attribute__((gnu_inline)) inline
# else
#  define INLINE_FUN extern R_INLINE
# endif
#endif /* ifdef COMPILING_R */

#if C99_INLINE_SEMANTICS
# undef INLINE_FUN
# ifdef COMPILING_R
/* force exported copy */
#  define INLINE_FUN extern inline
# else
/* either inline or link to extern version at compiler's choice */
#  define INLINE_FUN inline
# endif /* ifdef COMPILING_R */
#endif /* C99_INLINE_SEMANTICS */


#include <string.h> /* for strlen, strcmp */

/* define inline-able functions */

#ifdef INLINE_PROTECT
extern int R_PPStackSize;
extern int R_PPStackTop;
extern SEXP* R_PPStack;

INLINE_FUN SEXP protect(SEXP s)
{
    if (R_PPStackTop < R_PPStackSize)
	R_PPStack[R_PPStackTop++] = s;
    else R_signal_protect_error();
    return s;
}

INLINE_FUN void unprotect(int l)
{
#ifdef PROTECT_PARANOID
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else R_signal_unprotect_error();
#else
    R_PPStackTop -= l;
#endif
}

INLINE_FUN void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    protect(s);
    *pi = R_PPStackTop - 1;
}

INLINE_FUN void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    if (i >= R_PPStackTop || i < 0)
	R_signal_reprotect_error(i);
    R_PPStack[i] = s;
}
#endif /* INLINE_PROTECT */

/* from dstruct.c */

/*  length - length of objects  */

int Rf_envlength(SEXP rho);

/* TODO: a  Length(.) {say} which is  length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.c
*/
INLINE_FUN R_len_t length(SEXP s)
{
    int i;
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return LENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    case ENVSXP:
	return Rf_envlength(s);
    default:
	return 1;
    }
}

INLINE_FUN R_xlen_t xlength(SEXP s)
{
    int i;
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return XLENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    case ENVSXP:
	return Rf_envlength(s);
    default:
	return 1;
    }
}

/* regular allocVector() as a special case of allocVector3() with no custom allocator */
INLINE_FUN SEXP allocVector(SEXPTYPE type, R_xlen_t length)
{
    return allocVector3(type, length, NULL);
}

/* from list.c */
/* Return a dotted pair with the given CAR and CDR. */
/* The (R) TAG slot on the cell is set to NULL. */


/* Get the i-th element of a list */
INLINE_FUN SEXP elt(SEXP list, int i)
{
    int j;
    SEXP result = list;

    if ((i < 0) || (i > length(list)))
	return R_NilValue;
    else
	for (j = 0; j < i; j++)
	    result = CDR(result);

    return CAR(result);
}


/* Return the last element of a list */
INLINE_FUN SEXP lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue) {
	result = list;
	list = CDR(list);
    }
    return result;
}


/* Shorthands for creating small lists */

INLINE_FUN SEXP list1(SEXP s)
{
    return CONS(s, R_NilValue);
}


INLINE_FUN SEXP list2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = CONS(s, list1(t));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP list3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = CONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = CONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = CONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}


/* Destructive list append : See also ``append'' */

INLINE_FUN SEXP listAppend(SEXP s, SEXP t)
{
    SEXP r;
    if (s == R_NilValue)
	return t;
    r = s;
    while (CDR(r) != R_NilValue)
	r = CDR(r);
    SETCDR(r, t);
    return s;
}


/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/* Return a (language) dotted pair with the given car and cdr */

INLINE_FUN SEXP lcons(SEXP car, SEXP cdr)
{
    SEXP e = cons(car, cdr);
    SET_TYPEOF(e, LANGSXP);
    return e;
}

INLINE_FUN SEXP lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

INLINE_FUN SEXP lang2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = LCONS(s, list1(t));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = LCONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = LCONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = LCONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = LCONS(s, list5(t, u, v, w, x));
    UNPROTECT(1);
    return s;
}

/* from util.c */

/* Check to see if the arrays "x" and "y" have the identical extents */

INLINE_FUN Rboolean conformable(SEXP x, SEXP y)
{
    int i, n;
    PROTECT(x = getAttrib(x, R_DimSymbol));
    y = getAttrib(y, R_DimSymbol);
    UNPROTECT(1);
    if ((n = length(x)) != length(y))
	return FALSE;
    for (i = 0; i < n; i++)
	if (INTEGER(x)[i] != INTEGER(y)[i])
	    return FALSE;
    return TRUE;
}

/* NOTE: R's inherits() is based on inherits3() in ../main/objects.c
 * Here, use char / CHAR() instead of the slower more general translateChar()
 */
INLINE_FUN Rboolean inherits(SEXP s, const char *name)
{
    SEXP klass;
    int i, nclass;
    if (OBJECT(s)) {
	klass = getAttrib(s, R_ClassSymbol);
	nclass = length(klass);
	for (i = 0; i < nclass; i++) {
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), name)) {
		return TRUE;
            }
	}
    }
    return FALSE;
}

INLINE_FUN Rboolean inheritsCharSXP(SEXP s, SEXP nameCharSXP)
{
    SEXP klass;
    int i, nclass;
    const char *name = CHAR(nameCharSXP);

    if (OBJECT(s)) {
	klass = getAttrib(s, R_ClassSymbol);
	nclass = length(klass);
	/* fast comparison using equals */
	for (i = 0; i < nclass; i++) {
	    SEXP sCharSXP = STRING_ELT(klass, i);
	    if (sCharSXP == nameCharSXP) {
	        return TRUE;
	    }
        }
        /* FIXME: could we rely on CHARSXP caching/interning instead and avoid this slow checking? */
        for (i = 0; i < nclass; i++) {
            SEXP sCharSXP = STRING_ELT(klass, i);
	    if (!strcmp(CHAR(sCharSXP), name)) {
		return TRUE;
            }
	}
    }
    return FALSE;
}




INLINE_FUN Rboolean isValidString(SEXP x)
{
    return TYPEOF(x) == STRSXP && LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP;
}

/* non-empty ("") valid string :*/
INLINE_FUN Rboolean isValidStringF(SEXP x)
{
    return isValidString(x) && CHAR(STRING_ELT(x, 0))[0];
}

INLINE_FUN Rboolean isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP) {
	const char *str = CHAR(PRINTNAME(s));
	if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str)-1] == '%')
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isFunction(SEXP s)
{
    return (TYPEOF(s) == CLOSXP ||
	    TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean isPrimitive(SEXP s)
{
    return (TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean isList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LISTSXP);
}


INLINE_FUN Rboolean isNewList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == VECSXP);
}

INLINE_FUN Rboolean isPairList(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
    case LISTSXP:
    case LANGSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean isVectorList(SEXP s)
{
    switch (TYPEOF(s)) {
    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean isVectorAtomic(SEXP s)
{
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	return TRUE;
    default: /* including NULL */
	return FALSE;
    }
}

INLINE_FUN Rboolean isVector(SEXP s)/* === isVectorList() or isVectorAtomic() */
{
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean isFrame(SEXP s)
{
    SEXP klass;
    int i;
    if (OBJECT(s)) {
	klass = getAttrib(s, R_ClassSymbol);
	for (i = 0; i < length(klass); i++)
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), "data.frame")) return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isLanguage(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LANGSXP);
}

INLINE_FUN Rboolean isMatrix(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a non-integer dim,
	   although this might be possible by misuse of ATTRIB. */
	if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isArray(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a 0-length dim,
	 nor a non-integer dim */
	if (TYPEOF(t) == INTSXP && LENGTH(t) > 0)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isTs(SEXP s)
{
    return (isVector(s) && getAttrib(s, R_TspSymbol) != R_NilValue);
}


INLINE_FUN Rboolean isInteger(SEXP s)
{
    return (TYPEOF(s) == INTSXP && !inheritsCharSXP(s, R_FactorCharSXP));
}

INLINE_FUN Rboolean isFactor(SEXP s)
{
    return (TYPEOF(s) == INTSXP  && inheritsCharSXP(s, R_FactorCharSXP));
}

INLINE_FUN int nlevels(SEXP f)
{
    if (!isFactor(f))
	return 0;
    return LENGTH(getAttrib(f, R_LevelsSymbol));
}

/* Is an object of numeric type. */
/* FIXME:  the LGLSXP case should be excluded here
 * (really? in many places we affirm they are treated like INTs)*/

INLINE_FUN Rboolean isNumeric(SEXP s)
{
    switch(TYPEOF(s)) {
    case INTSXP:
	if (inheritsCharSXP(s, R_FactorCharSXP)) return FALSE;
    case LGLSXP:
    case REALSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

/** Is an object "Numeric" or  complex */
INLINE_FUN Rboolean isNumber(SEXP s)
{
    switch(TYPEOF(s)) {
    case INTSXP:
	if (inheritsCharSXP(s, R_FactorCharSXP)) return FALSE;
    case LGLSXP:
    case REALSXP:
    case CPLXSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

/* As from R 2.4.0 we check that the value is allowed. */
INLINE_FUN SEXP ScalarLogical(int x)
{
    extern SEXP R_LogicalNAValue, R_TrueValue, R_FalseValue;
    if (x == NA_LOGICAL) return R_LogicalNAValue;
    else if (x != 0) return R_TrueValue;
    else return R_FalseValue;
}

INLINE_FUN SEXP ScalarInteger(int x)
{
    SEXP ans = allocVector(INTSXP, (R_xlen_t)1);
    INTEGER(ans)[0] = x;
    return ans;
}

INLINE_FUN SEXP ScalarReal(double x)
{
    SEXP ans = allocVector(REALSXP, (R_xlen_t)1);
    REAL(ans)[0] = x;
    return ans;
}


INLINE_FUN SEXP ScalarComplex(Rcomplex x)
{
    SEXP ans = allocVector(CPLXSXP, (R_xlen_t)1);
    COMPLEX(ans)[0] = x;
    return ans;
}

INLINE_FUN SEXP ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = allocVector(STRSXP, (R_xlen_t)1);
    SET_STRING_ELT(ans, (R_xlen_t)0, x);
    UNPROTECT(1);
    return ans;
}

INLINE_FUN SEXP ScalarRaw(Rbyte x)
{
    SEXP ans = allocVector(RAWSXP, (R_xlen_t)1);
    RAW(ans)[0] = x;
    return ans;
}

/* Check to see if a list can be made into a vector. */
/* it must have every element being a vector of length 1. */
/* BUT it does not exclude 0! */

INLINE_FUN Rboolean isVectorizable(SEXP s)
{
    if (s == R_NilValue) return TRUE;
    else if (isNewList(s)) {
	R_xlen_t i, n;

	n = XLENGTH(s);
	for (i = 0 ; i < n; i++)
	    if (!isVector(VECTOR_ELT(s, i)) || XLENGTH(VECTOR_ELT(s, i)) > 1)
		return FALSE;
	return TRUE;
    }
    else if (isList(s)) {
	for ( ; s != R_NilValue; s = CDR(s))
	    if (!isVector(CAR(s)) || LENGTH(CAR(s)) > 1) return FALSE;
	return TRUE;
    }
    else return FALSE;
}


/**
 * Create a named vector of type TYP
 *
 * @example const char *nms[] = {"xi", "yi", "zi", ""};
 *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
 *
 * @param TYP a vector SEXP type (e.g. REALSXP)
 * @param names names of list elements with null string appended
 *
 * @return (pointer to a) named vector of type TYP
 */
INLINE_FUN SEXP mkNamed(SEXPTYPE TYP, const char **names)
{
    SEXP ans, nms;
    R_xlen_t i, n;

    for (n = 0; strlen(names[n]) > 0; n++) {}
    ans = PROTECT(allocVector(TYP, n));
    nms = PROTECT(allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
	SET_STRING_ELT(nms, i, mkChar(names[i]));
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}

/* from gram.y */

/* short cut for  ScalarString(mkChar(s)) : */
INLINE_FUN SEXP mkString(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, (R_xlen_t)1));
    SET_STRING_ELT(t, (R_xlen_t)0, mkChar(s));
    UNPROTECT(1);
    return t;
}

/* duplicate RHS value of complex assignment if necessary to prevent cycles */
INLINE_FUN SEXP R_FixupRHS(SEXP x, SEXP y)
{
    if( y != R_NilValue && MAYBE_REFERENCED(y) ) {
	if (R_cycle_detected(x, y)) {
#ifdef WARNING_ON_CYCLE_DETECT
	    warning("cycle detected");
	    R_cycle_detected(x, y);
#endif
	    y = duplicate(y);
	}
	else if (NAMED(y) < 2) SET_NAMED(y, 2);
    }
    return y;
}
#endif /* R_INLINES_H_ */

/* FIXME: with GCC only, this could be changed into a macro using a compound statement */
INLINE_FUN SEXP argShift(SEXP *argsp) {
    SEXP res = CAR(*argsp);
    *argsp = CDR(*argsp);
    return res;
}

/* Builds promargs cells from what is on the stack (last would be typically
 * a pointer for the last argument on the node stack).  This is for
 * positional calls only. */

INLINE_FUN SEXP buildPositionalPromargs(int nargs, SEXP *last) {
    if (nargs == 0) {
        return R_NilValue;
    }
    SEXP pargs = CONS_NR(*last, R_NilValue);
    SEXP *src = last;
    int i;
    for(i = nargs - 1; i > 0; i--) {
        src = src - 1;
        
        pargs = CONS_NR(*src, pargs); /* FIXME: the ast interpreter uses CONS but the bc interpreter uses CONS_NR */
    }
    return pargs;    
}


/*----------------------------------------------------------------------

  String Hashing

  This is taken from the second edition of the "Dragon Book" by
  Aho, Ullman and Sethi.

*/

/* was extern: used in this file and names.c (for the symbol table).

   This hash function seems to work well enough for symbol tables,
   and hash tables get saved as part of environments so changing it
   is a major decision.

   Has to be kept in sync with installCharSXPSignature.
 */
INLINE_FUN int Newhashpjw(const char *s)
{
    char *p;
    unsigned h = 0, g;
    for (p = (char *) s; *p; p++) {
	h = (h << 4) + (*p);
	if ((g = h & 0xf0000000) != 0) {
	    h = h ^ (g >> 24);
	    h = h ^ g;
	}
    }
    return h;
}

INLINE_FUN int hashCharSXP(SEXP charSXP) {
    int hashcode;

    if (HASHASH(charSXP)) {
        hashcode = HASHVALUE(charSXP);
    } else {
        hashcode = Newhashpjw(CHAR(charSXP));
        SET_HASHVALUE(charSXP, hashcode);
        SET_HASHASH(charSXP, 1);
    }
    return hashcode;
}

#if defined(R_USE_SIGNALS) && defined(CALLED_FROM_DEFN_H)

/* builds promargs if necessary */
INLINE_FUN SEXP accessPromargs(RCNTXT* cptr) {
    if (cptr->promargs != NULL) {
        return cptr->promargs;
    }
    int nargs = length(CDR(cptr->call));
    SEXP pargs = buildPositionalPromargs(nargs, cptr->positionalPromargs + nargs - 1);
    cptr->promargs = pargs;
    return pargs;
}


/* begincontext and endcontext are used in dataentry.c and modules */
INLINE_FUN void beginposcontext(RCNTXT * cptr, int flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun, SEXP* positionalPromargs)
{
    cptr->cstacktop = R_PPStackTop;
    cptr->evaldepth = R_EvalDepth;
    cptr->callflag = flags;
    cptr->call = syscall;
    cptr->cloenv = env;
    cptr->sysparent = sysp;
    cptr->conexit = R_NilValue;
    cptr->cend = NULL;
    cptr->promargs = promargs;
    cptr->callfun = callfun;
    cptr->vmax = vmaxget();
    cptr->intsusp = R_interrupts_suspended;
    cptr->handlerstack = R_HandlerStack;
    cptr->restartstack = R_RestartStack;
    cptr->prstack = R_PendingPromises;
    cptr->nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
    cptr->intstack = R_BCIntStackTop;
#endif
#ifdef USE_PROMARGS_STACK
    cptr->promargsstackbase = R_PromargsStackBase;
    cptr->promargsstacktop = R_PromargsStackTop;
    cptr->promargsstackend = R_PromargsStackEnd;
#endif
    cptr->positionalPromargs = positionalPromargs;
    cptr->srcref = R_Srcref;    
    cptr->browserfinish = R_GlobalContext->browserfinish;
    cptr->nextcontext = R_GlobalContext;

    R_GlobalContext = cptr;
}

INLINE_FUN void begincontext(RCNTXT * cptr, int flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun) {

    beginposcontext(cptr, flags, syscall, env, sysp, promargs, callfun, NULL);		  
}

/* endcontext - end an execution context */

INLINE_FUN void endcontext(RCNTXT * cptr)
{
    R_HandlerStack = cptr->handlerstack;
    R_RestartStack = cptr->restartstack;
#ifdef USE_PROMARGS_STACK    
    switchPromargsStack(cptr->promargsstackbase, cptr->promargsstacktop, cptr->promargsstackend);    
#endif    
    if (cptr->cloenv != R_NilValue && cptr->conexit != R_NilValue ) {
	SEXP s = cptr->conexit;
	Rboolean savevis = R_Visible;
	cptr->conexit = R_NilValue; /* prevent recursion */
	PROTECT(s);
	eval(s, cptr->cloenv);
	UNPROTECT(1);
	R_Visible = savevis;
    }
    R_GlobalContext = cptr->nextcontext;
}


#endif /* R_USE_SIGNALS and USE_RINTERNALS */
