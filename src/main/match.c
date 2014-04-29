/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2013   The R Core Team.
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
 *  http://www.r-project.org/Licenses/


 *  Matching and Partial Matching for Strings
 *
 *  In theory all string matching code should be placed in this file
 *  At present there are still a couple of rogue matchers about.
 *
 *
 *  psmatch(char *, char *, int);
 *
 *  This code will perform partial matching for list tags.  When
 *  exact is 1, and exact match is required (typically after ...)
 *  otherwise partial matching is performed.
 *
 *  Examples:
 *
 *	psmatch("aaa", "aaa", 0) -> 1
 *	psmatch("aaa", "aa", 0) -> 1
 *	psmatch("aa", "aaa", 0) -> 0
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <Internal.h>

/* used in subscript.c and subassign.c */
Rboolean NonNullStringMatch(SEXP s, SEXP t)
{
    /* "" or NA string matches nothing */
    if (s == NA_STRING || t == NA_STRING) return FALSE;
    if (CHAR(s)[0] && CHAR(t)[0] && Seql(s, t))
	return TRUE;
    else
	return FALSE;
}

/* currently unused outside this file */
Rboolean psmatch(const char *f, const char *t, Rboolean exact)
{
    if (exact)
	return (Rboolean)!strcmp(f, t);
    /* else */
    while (*t) {
	if (*t != *f)   return FALSE;
	t++;
	f++;
    }
    return TRUE;
}


/* Matching formals and arguments */

/* Are these are always native charset? */
Rboolean pmatch(SEXP formal, SEXP tag, Rboolean exact)
{
    const char *f, *t;
    const void *vmax = vmaxget();
    switch (TYPEOF(formal)) {
    case SYMSXP:
	f = CHAR(PRINTNAME(formal));
	break;
    case CHARSXP:
	f = CHAR(formal);
	break;
    case STRSXP:
	f = translateChar(STRING_ELT(formal, 0));
	break;
    default:
	goto fail;
    }
    switch(TYPEOF(tag)) {
    case SYMSXP:
	t = CHAR(PRINTNAME(tag));
	break;
    case CHARSXP:
	t = CHAR(tag);
	break;
    case STRSXP:
	t = translateChar(STRING_ELT(tag, 0));
	break;
    default:
	goto fail;
    }
    Rboolean res = psmatch(f, t, exact);
    vmaxset(vmax);
    return res;
 fail:
    error(_("invalid partial string match"));
    return FALSE;/* for -Wall */
}


/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a C string. */

static SEXP matchPar_int(const char *tag, SEXP *list, Rboolean exact)
{
    if (*list == R_NilValue)
	return R_MissingArg;
    else if (TAG(*list) != R_NilValue &&
	     psmatch(tag, CHAR(PRINTNAME(TAG(*list))), exact)) {
	SEXP s = *list;
	*list = CDR(*list);
	return CAR(s);
    }
    else {
	SEXP last = *list;
	SEXP next = CDR(*list);
	while (next != R_NilValue) {
	    if (TAG(next) != R_NilValue &&
		psmatch(tag, CHAR(PRINTNAME(TAG(next))), exact)) {
		SETCDR(last, CDR(next));
		return CAR(next);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return R_MissingArg;
    }
}

/* unused outside this file */
SEXP attribute_hidden matchPar(const char *tag, SEXP * list)
{
    return matchPar_int(tag, list, FALSE);
}



/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArg(SEXP tag, SEXP * list)
{
    return matchPar(CHAR(PRINTNAME(tag)), list);
}


/* Destructively Extract A Named List Element. */
/* Returns the first exactly matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArgExact(SEXP tag, SEXP * list)
{
      return matchPar_int(CHAR(PRINTNAME(tag)), list, TRUE);
}


/* Match the supplied arguments with the formals and */
/* return the matched arguments in actuals. */

#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)


/* We need to leave 'supplied' unchanged in case we call UseMethod */
/* MULTIPLE_MATCHES was added by RI in Jan 2005 but never activated:
   code in R-2-8-branch */

SEXP attribute_hidden matchArgs(SEXP formals, SEXP supplied, SEXP call)
{
    int i, seendots, arg_i = 0;
    SEXP f, a, b, dots, actuals;

    actuals = R_NilValue;
    for (f = formals ; f != R_NilValue ; f = CDR(f), arg_i++) {
	/* CONS_NR is used since argument lists created here are only
	   used internally and so should not increment reference
	   counts */
	actuals = CONS_NR(R_MissingArg, actuals);
	SET_MISSING(actuals, 1);
    }
    /* We use fargused instead of ARGUSED/SET_ARGUSED on elements of
       formals to avoid modification of the formals SEXPs.  A gc can
       cause matchArgs to be called from finalizer code, resulting in
       another matchArgs call with the same formals.  In R-2.10.x, this
       corrupted the ARGUSED data of the formals and resulted in an
       incorrect "formal argument 'foo' matched by multiple actual
       arguments" error.
     */
    int fargused[arg_i ? arg_i : 1]; // avoid undefined behaviour
    memset(fargused, 0, sizeof(fargused));

    for(b = supplied; b != R_NilValue; b = CDR(b)) SET_ARGUSED(b, 0);

    PROTECT(actuals);

    /* First pass: exact matches by tag */
    /* Grab matched arguments and check */
    /* for multiple exact matches. */

    f = formals;
    a = actuals;
    arg_i = 0;
    while (f != R_NilValue) {
	if (TAG(f) != R_DotsSymbol) {
	    i = 1;
	    for (b = supplied; b != R_NilValue; b = CDR(b)) {
		if (TAG(b) != R_NilValue && pmatch(TAG(f), TAG(b), 1)) {
		    if (fargused[arg_i] == 2)
			error(_("formal argument \"%s\" matched by multiple actual arguments"),
			      CHAR(PRINTNAME(TAG(f))));
		    if (ARGUSED(b) == 2)
			error(_("argument %d matches multiple formal arguments"), i);
		    SETCAR(a, CAR(b));
		    if(CAR(b) != R_MissingArg) SET_MISSING(a, 0);
		    SET_ARGUSED(b, 2);
		    fargused[arg_i] = 2;
		}
		i++;
	    }
	}
	f = CDR(f);
	a = CDR(a);
        arg_i++;
    }

    /* Second pass: partial matches based on tags */
    /* An exact match is required after first ... */
    /* The location of the first ... is saved in "dots" */

    dots = R_NilValue;
    seendots = 0;
    f = formals;
    a = actuals;
    arg_i = 0;
    while (f != R_NilValue) {
	if (fargused[arg_i] == 0) {
	    if (TAG(f) == R_DotsSymbol && !seendots) {
		/* Record where ... value goes */
		dots = a;
		seendots = 1;
	    } else {
		i = 1;
		for (b = supplied; b != R_NilValue; b = CDR(b)) {
		    if (ARGUSED(b) != 2 && TAG(b) != R_NilValue &&
			pmatch(TAG(f), TAG(b), seendots)) {
			if (ARGUSED(b))
			    error(_("argument %d matches multiple formal arguments"), i);
			if (fargused[arg_i] == 1)
			    error(_("formal argument \"%s\" matched by multiple actual arguments"),
				  CHAR(PRINTNAME(TAG(f))));
			if (R_warn_partial_match_args) {
			    warningcall(call,
					_("partial argument match of '%s' to '%s'"),
					CHAR(PRINTNAME(TAG(b))),
					CHAR(PRINTNAME(TAG(f))) );
			}
			SETCAR(a, CAR(b));
			if (CAR(b) != R_MissingArg) SET_MISSING(a, 0);
			SET_ARGUSED(b, 1);
			fargused[arg_i] = 1;
		    }
		    i++;
		}
	    }
	}
	f = CDR(f);
	a = CDR(a);
        arg_i++;
    }

    /* Third pass: matches based on order */
    /* All args specified in tag=value form */
    /* have now been matched.  If we find ... */
    /* we gobble up all the remaining args. */
    /* Otherwise we bind untagged values in */
    /* order to any unmatched formals. */

    f = formals;
    a = actuals;
    b = supplied;
    seendots = 0;

    while (f != R_NilValue && b != R_NilValue && !seendots) {
	if (TAG(f) == R_DotsSymbol) {
	    /* Skip ... matching until all tags done */
	    seendots = 1;
	    f = CDR(f);
	    a = CDR(a);
	} else if (CAR(a) != R_MissingArg) {
	    /* Already matched by tag */
	    /* skip to next formal */
	    f = CDR(f);
	    a = CDR(a);
	} else if (ARGUSED(b) || TAG(b) != R_NilValue) {
	    /* This value used or tagged , skip to next value */
	    /* The second test above is needed because we */
	    /* shouldn't consider tagged values for positional */
	    /* matches. */
	    /* The formal being considered remains the same */
	    b = CDR(b);
	} else {
	    /* We have a positional match */
	    SETCAR(a, CAR(b));
	    if(CAR(b) != R_MissingArg) SET_MISSING(a, 0);
	    SET_ARGUSED(b, 1);
	    b = CDR(b);
	    f = CDR(f);
	    a = CDR(a);
	}
    }

    if (dots != R_NilValue) {
	/* Gobble up all unused actuals */
	SET_MISSING(dots, 0);
	i = 0;
	for(a = supplied; a != R_NilValue ; a = CDR(a)) if(!ARGUSED(a)) i++;

	if (i) {
	    a = allocList(i);
	    SET_TYPEOF(a, DOTSXP);
	    f = a;
	    for(b = supplied; b != R_NilValue; b = CDR(b))
		if(!ARGUSED(b)) {
		    SETCAR(f, CAR(b));
		    SET_TAG(f, TAG(b));
		    f = CDR(f);
		}
	    SETCAR(dots, a);
	}
    } else {
	/* Check that all arguments are used */
	SEXP unused = R_NilValue, last = R_NilValue;
	for (b = supplied; b != R_NilValue; b = CDR(b))
	    if (!ARGUSED(b)) {
		if(last == R_NilValue) {
		    PROTECT(unused = CONS(CAR(b), R_NilValue));
		    SET_TAG(unused, TAG(b));
		    last = unused;
		} else {
		    SETCDR(last, CONS(CAR(b), R_NilValue));
		    last = CDR(last);
		    SET_TAG(last, TAG(b));
		}
	    }

	if(last != R_NilValue) {
            /* show bad arguments in call without evaluating them */
            SEXP unusedForError = R_NilValue, last = R_NilValue;

            for(b = unused ; b != R_NilValue ; b = CDR(b)) {
                SEXP tagB = TAG(b), carB = CAR(b) ;
                if (TYPEOF(carB) == PROMSXP) carB = PREXPR(carB) ;
                if (last == R_NilValue) {
                    PROTECT(last = CONS(carB, R_NilValue));
                    SET_TAG(last, tagB);
                    unusedForError = last;
                } else {
                    SETCDR(last, CONS(carB, R_NilValue));
                    last = CDR(last);
                    SET_TAG(last, tagB);
                }
            }
	    errorcall(call /* R_GlobalContext->call */,
		      ngettext("unused argument %s",
			       "unused arguments %s",
			       (unsigned long) length(unusedForError)),
		      CHAR(STRING_ELT(deparse1line(unusedForError, 0), 0)) + 4);
                      /* '+ 4' is to remove 'list' from 'list(badTag1,...)' */
	}
    }
    UNPROTECT(1);
    return(actuals);
}

/* 
   A specialized version of matchArgs, which can be used when the supplied
   arguments do not contain any names.
  
   In addition to matchArgs, this specialized version also sets the tags of
   the resulting actuals and creates the resulting environment.  Also, it
   provides promises for default values if applicable.  And it enables
   reference counting.
   
   The supplied arguments are re-used and modified (including links, tags,
   possibly values).  This function is expected to be called with the
   supplied arguments that have disabled reference counting.
   
   Returns the new environment.
   Returns the actuals in output parameter outActuals.
   Supplied do not have to be protected when calling this function.
*/
   
SEXP attribute_hidden matchUnnamedArgsCreateEnv(SEXP formals, SEXP supplied, SEXP call, SEXP rho, SEXP* outActuals)
{
    SEXP f, s;    
    SEXP actuals = PROTECT(supplied);
    SEXP newrho = PROTECT(NewEnvironmentNR(rho));
    SEXP prevS = R_NilValue;
    
    for (f = formals, s = supplied ; f != R_NilValue ; f = CDR(f), prevS = s, s = CDR(s)) {
        
        if (TAG(f) == R_DotsSymbol) {
            /* pack all arguments into ... */
            
            SEXP dots = CONS_NR(R_MissingArg, R_NilValue);
            SET_TAG(dots, R_DotsSymbol);
            if (prevS == R_NilValue) {
                UNPROTECT(1); /* old actuals */
                PROTECT(actuals = dots);
            } else {
                SETCDR(prevS, dots);
                ENABLE_REFCNT(prevS); /* dots are part of a protected list */
            }
            if (s != R_NilValue) {
                SET_TYPEOF(s, DOTSXP);
                SETCAR(dots, s);
                s = R_NilValue;
            }
            prevS = dots;
            f = CDR(f);
            /* falls through into s == R_NilValue case */
        }
            
        if (s == R_NilValue) {
            /* fewer supplied arguments than formals */
            SEXP ds;
            for(; f != R_NilValue ; f = CDR(f), prevS = ds) { 
                ds = CONS_NR(R_MissingArg, R_NilValue);
                SET_TAG(ds, TAG(f));
                if (prevS == R_NilValue) {
                    UNPROTECT(1); /* old actuals */
                    PROTECT(actuals = ds);
                } else {
                    SETCDR(prevS, ds);
                    ENABLE_REFCNT(prevS); /* ds is part of a protected list */
                }
                SEXP fdefault = CAR(f);
                if (fdefault != R_MissingArg) {
                    SET_MISSING(ds, 2);
                    SETCAR(ds, mkPROMISEorConst(fdefault, newrho));
                } else {
                    SET_MISSING(ds, 1);
                }
            }
            break;
        }
        
        /* normal case, the next supplied arg is available */
        
        SET_TAG(s, TAG(f));
        if (CAR(s) == R_MissingArg) {
            SEXP fdefault = CAR(f);
            if (fdefault != R_MissingArg) {
                SET_MISSING(s, 2);
                SETCAR(s, mkPROMISEorConst(fdefault, newrho));
            } else {
                SET_MISSING(s, 1);
            }
        }
        if (prevS != R_NilValue) {
            ENABLE_REFCNT(prevS);
        }
    }

    if (s != R_NilValue) {
        /* some arguments are not used */
        SEXP unusedForError = PROTECT(s);
        SETCDR(prevS, R_NilValue); /* make sure they're not in the new environment */
            
        /* show bad arguments in call without evaluating them */
        for (; s != R_NilValue; s = CDR(s)) {
            SEXP carS = CAR(s);
            if (TYPEOF(carS) == PROMSXP) {
                SETCAR(s, PREXPR(carS));
            }
        }
        errorcall(call /* R_GlobalContext->call */,
	   ngettext("unused argument %s",
	     "unused arguments %s",
	     (unsigned long) length(unusedForError)),
	     CHAR(STRING_ELT(deparse1line(unusedForError, 0), 0)) + 4);
                  /* '+ 4' is to remove 'list' from 'list(badTag1,...)' */
        UNPROTECT(1);
    }
        
    if (prevS != R_NilValue) {
        ENABLE_REFCNT(prevS);
    }
    
    SET_FRAME(newrho, actuals);
    ENABLE_REFCNT(newrho);
    UNPROTECT(2); /* newrho, actuals */
    
    *outActuals = actuals;
    return(newrho);
}

/* 
   A specialized version of matchArgs for arguments (promargs) provided as a
   C array (typically on the byte-code interpreter node-stack).
   
   Specialized for positional calls: no names, no caller dots, no
   compile-time missing arguments.
  
   In addition to matchArgs, this specialized version also sets the tags of
   the resulting actuals and creates the resulting environment.  Also, it
   provides promises for default values if applicable.  And it enables
   reference counting.
   
   Returns the new environment.
   Returns the actuals in output parameter outActuals (not protected).
   Supplied will not be protected by this function (it should be a C array safe from the GC).
*/
   
SEXP attribute_hidden matchPositionalArgsCreateEnv(SEXP formals, SEXP *supplied, int nsupplied, SEXP call, SEXP rho, SEXP* outActuals)
{
    SEXP *s;
    SEXP f, a;    
    SEXP actuals = R_NilValue;
    
    SEXP newrho = PROTECT(NewEnvironmentNR(rho));    
    
    SEXP *endSupplied = supplied + nsupplied;
    for (f = formals, s = supplied, a = actuals ; f != R_NilValue ; f = CDR(f), s++) {
    
        if (TAG(f) == R_DotsSymbol) {
            /* pack all remaining arguments into ... */
            
            SEXP *rs = endSupplied - 1;
            SEXP dotsContent = R_NilValue;
            for(; rs >= s; rs--) {
                dotsContent = CONS(*rs, dotsContent); /* FIXME: enabling refcnt? */
            }
            if (dotsContent != R_NilValue) {
                SET_TYPEOF(dotsContent, DOTSXP);
            }
            SEXP dots = CONS_NR(dotsContent, R_NilValue);
            SET_TAG(dots, R_DotsSymbol);

            if (a == R_NilValue) {
                PROTECT(actuals = dots);
            } else {
                SETCDR(a, dots);
                ENABLE_REFCNT(a); /* dots are part of a protected list */
            }
            a = dots;
            f = CDR(f);
            s = endSupplied;
            /* falls through into noMoreSupplied branch below */
        }
            
        if (s == endSupplied) {
            /* possibly fewer supplied arguments than formals */
            SEXP ds;
            for(; f != R_NilValue ; f = CDR(f), a = ds) { 
                ds = CONS_NR(R_MissingArg, R_NilValue);
                SET_TAG(ds, TAG(f));
                if (a == R_NilValue) {
                    PROTECT(actuals = ds);
                } else {
                    SETCDR(a, ds);
                    ENABLE_REFCNT(a); /* ds is part of a protected list */
                }
                SEXP fdefault = CAR(f);
                if (fdefault != R_MissingArg) {
                    SET_MISSING(ds, 2);
                    SETCAR(ds, mkPROMISEorConst(fdefault, newrho));
                } else {
                    SET_MISSING(ds, 1);
                }
            }
            break;
        }
        
        /* normal case, the next supplied arg is available */
        
        SEXP arg = CONS_NR(*s, R_NilValue);
        SET_TAG(arg, TAG(f));

        if (a == R_NilValue) {
            PROTECT(actuals = arg);
        } else {
            SETCDR(a, arg);
            ENABLE_REFCNT(a);
        }
        a = arg;
    }

    if (s < endSupplied) {
        /* some arguments are not used */

        SEXP *rs = endSupplied - 1;
        SEXP unusedForError = R_NilValue;
        for(; rs >= s; rs--) {
            SEXP rsValue = *rs;
            if (TYPEOF(rsValue) == PROMSXP) {
                rsValue = PREXPR(rsValue);
            }
            unusedForError = CONS(rsValue, unusedForError);
        }
        PROTECT(unusedForError); /* needed? */
        errorcall(call /* R_GlobalContext->call */,
	   ngettext("unused argument %s",
	     "unused arguments %s",
	     (unsigned long) length(unusedForError)),
	     CHAR(STRING_ELT(deparse1line(unusedForError, 0), 0)) + 4);
                  /* '+ 4' is to remove 'list' from 'list(badTag1,...)' */
        UNPROTECT(1);
    }
        
    if (a != R_NilValue) {
        ENABLE_REFCNT(a);
    }
    
    SET_FRAME(newrho, actuals);
    ENABLE_REFCNT(newrho);
    UNPROTECT(1);  /* newrho */
    if (actuals != R_NilValue) {
        UNPROTECT(1); /* actuals */
    }    
    *outActuals = actuals;
    
    return(newrho);
}
