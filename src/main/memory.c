/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2017  The R Core Team.
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/*
 *	This code implements a non-moving generational collector
 *      with two or three generations.
 *
 *	Memory allocated by R_alloc is maintained in a stack.  Code
 *	that R_allocs memory must use vmaxget and vmaxset to obtain
 *	and reset the stack pointer.
 */

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdarg.h>

#include <R_ext/RS.h> /* for S4 allocation */
#include <R_ext/Print.h>

/* Declarations for Valgrind.

   These are controlled by the
     --with-valgrind-instrumentation=
   option to configure, which sets VALGRIND_LEVEL to the
   supplied value (default 0) and defines NVALGRIND if
   the value is 0.

   level 0 is no additional instrumentation
   level 1 marks uninitialized numeric, logical, integer, raw,
	   complex vectors and R_alloc memory
   level 2 marks the data section of vector nodes as inaccessible
	   when they are freed.

   level 3 was withdrawn in R 3.2.0.

   It may be necessary to define NVALGRIND for a non-gcc
   compiler on a supported architecture if it has different
   syntax for inline assembly language from gcc.

   For Win32, Valgrind is useful only if running under Wine.
*/
#ifdef Win32
# ifndef USE_VALGRIND_FOR_WINE
# define NVALGRIND 1
#endif
#endif


#ifndef VALGRIND_LEVEL
# define VALGRIND_LEVEL 0
#endif

#ifndef NVALGRIND
# ifdef HAVE_VALGRIND_MEMCHECK_H
#  include "valgrind/memcheck.h"
# else
// internal version of headers
#  include "vg/memcheck.h"
# endif
#endif


#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <R_ext/GraphicsEngine.h> /* GEDevDesc, GEgetDevice */
#include <R_ext/Rdynload.h>
#include <R_ext/Rallocators.h> /* for R_allocator_t structure */
#include <Rmath.h> // R_pow_di
#include <Print.h> // R_print

#if defined(Win32)
extern void *Rm_malloc(size_t n);
extern void *Rm_calloc(size_t n_elements, size_t element_size);
extern void Rm_free(void * p);
extern void *Rm_realloc(void * p, size_t n);
#define calloc Rm_calloc
#define malloc Rm_malloc
#define realloc Rm_realloc
#define free Rm_free
#endif

/* malloc uses size_t.  We are assuming here that size_t is at least
   as large as unsigned long.  Changed from int at 1.6.0 to (i) allow
   2-4Gb objects on 32-bit system and (ii) objects limited only by
   length on a 64-bit system.
*/

static int gc_reporting = 0;
static int gc_count = 0;

/* These are used in profiling to separate out time in GC */
int R_gc_running() { return R_in_gc; }

#ifdef TESTING_WRITE_BARRIER
# define PROTECTCHECK
#endif

#ifdef PROTECTCHECK
// TODO!!
/* This is used to help detect unprotected SEXP values.  It is most
   useful if the strict barrier is enabled as well. The strategy is:

       All GCs are full GCs

       New nodes are marked as NEWSXP

       After a GC all free nodes that are not of type NEWSXP are
       marked as type FREESXP

       Most calls to accessor functions check their SEXP inputs and
       SEXP outputs with CHK() to see if a reachable node is a
       FREESXP and signal an error if a FREESXP is found.

   Combined with GC torture this can help locate where an unprotected
   SEXP is being used.

   This approach will miss cases where an unprotected node has been
   re-allocated.  For these cases it is possible to set
   gc_inhibit_release to TRUE.  FREESXP nodes will not be reallocated,
   or large ones released, until gc_inhibit_release is set to FALSE
   again.  This will of course result in memory growth and should be
   used with care and typically in combination with OS mechanisms to
   limit process memory usage.  LT */

/* Before a node is marked as a FREESXP by the collector the previous
   type is recorded.  For now using the LEVELS field seems
   reasonable.  */

static R_INLINE SEXP CHK(SEXP x)
{
    /* **** NULL check because of R_CurrentExpr */
    if (x != NULL && TYPEOF(x) == FREESXP)
	error("unprotected object (%p) encountered (was %s)",
	      x, sexptype2char(OLDTYPE(x)));
    return x;
}
#else
#define CHK(x) x
#endif

/* The following three variables definitions are used to record the
   address and type of the first bad type seen during a collection,
   and for FREESXP nodes they record the old type as well. */
static SEXPTYPE bad_sexp_type_seen = 0;
static SEXP bad_sexp_type_sexp = NULL;
#ifdef PROTECTCHECK
static SEXPTYPE bad_sexp_type_old_type = 0;
#endif
static int bad_sexp_type_line = 0;

static R_INLINE void register_bad_sexp_type(SEXP s, int line)
{
    if (bad_sexp_type_seen == 0) {
	bad_sexp_type_seen = TYPEOF(s);
	bad_sexp_type_sexp = s;
	bad_sexp_type_line = line;
#ifdef PROTECTCHECK
	if (TYPEOF(s) == FREESXP)
	    bad_sexp_type_old_type = OLDTYPE(s);
#endif
    }
}


/* also called from typename() in inspect.c */
attribute_hidden
const char *sexptype2char(SEXPTYPE type) {
    switch (type) {
    case NILSXP:	return "NILSXP";
    case SYMSXP:	return "SYMSXP";
    case LISTSXP:	return "LISTSXP";
    case CLOSXP:	return "CLOSXP";
    case ENVSXP:	return "ENVSXP";
    case PROMSXP:	return "PROMSXP";
    case LANGSXP:	return "LANGSXP";
    case SPECIALSXP:	return "SPECIALSXP";
    case BUILTINSXP:	return "BUILTINSXP";
    case CHARSXP:	return "CHARSXP";
    case LGLSXP:	return "LGLSXP";
    case INTSXP:	return "INTSXP";
    case REALSXP:	return "REALSXP";
    case CPLXSXP:	return "CPLXSXP";
    case STRSXP:	return "STRSXP";
    case DOTSXP:	return "DOTSXP";
    case ANYSXP:	return "ANYSXP";
    case VECSXP:	return "VECSXP";
    case EXPRSXP:	return "EXPRSXP";
    case BCODESXP:	return "BCODESXP";
    case EXTPTRSXP:	return "EXTPTRSXP";
    case WEAKREFSXP:	return "WEAKREFSXP";
    case S4SXP:		return "S4SXP";
    case RAWSXP:	return "RAWSXP";
    case NEWSXP:	return "NEWSXP"; /* should never happen */
    case FREESXP:	return "FREESXP";
    default:		return "<unknown>";
    }
}

static void R_gc_internal(R_size_t size_needed);
static void R_gc_full(R_size_t size_needed);
static void mem_err_heap(R_size_t size);
static void mem_err_malloc(R_size_t size);

static int gc_pending = 0;

/* Maximal Heap Limits.  These variables contain upper limits on the
   heap sizes.  They could be made adjustable from the R level,
   perhaps by a handler for a recoverable error.

   Access to these values is provided with reader and writer
   functions; the writer function insures that the maximal values are
   never set below the current ones. */
static R_size_t R_MaxVSize = R_SIZE_T_MAX;
static R_size_t R_MaxNSize = R_SIZE_T_MAX;
static int vsfac = 1; /* current units for vsize: changes at initialization */

R_size_t attribute_hidden R_GetMaxVSize(void)
{
    if (R_MaxVSize == R_SIZE_T_MAX) return R_SIZE_T_MAX;
    return R_MaxVSize*vsfac;
}

void attribute_hidden R_SetMaxVSize(R_size_t size)
{
    if (size == R_SIZE_T_MAX) return;
    if (size / vsfac >= R_VSize) R_MaxVSize = (size+1)/vsfac;
}

R_size_t attribute_hidden R_GetMaxNSize(void)
{
    return R_MaxNSize;
}

void attribute_hidden R_SetMaxNSize(R_size_t size)
{
    if (size >= R_NSize) R_MaxNSize = size;
}

void attribute_hidden R_SetPPSize(R_size_t size)
{
    R_PPStackSize = (int) size;
}

/* Miscellaneous Globals. */

static SEXP R_VStack = NULL;		/* R_alloc stack pointer */
static SEXP R_PreciousList = NULL;      /* List of Persistent Objects */

static R_size_t R_N_maxused=0;
static R_size_t R_V_maxused=0;

#include "new_gc.c"

/* compute size in VEC units so result will fit in LENGTH field for FREESXPs */
static R_INLINE R_size_t getVecSizeInVEC(SEXP s)
{
    if (IS_GROWABLE(s))
	SETLENGTH(s, XTRUELENGTH(s));

    R_size_t size;
    switch (TYPEOF(s)) {	/* get size in bytes */
    case CHARSXP:
	size = XLENGTH(s) + 1;
	break;
    case RAWSXP:
	size = XLENGTH(s);
	break;
    case LGLSXP:
    case INTSXP:
	size = XLENGTH(s) * sizeof(int);
	break;
    case REALSXP:
	size = XLENGTH(s) * sizeof(double);
	break;
    case CPLXSXP:
	size = XLENGTH(s) * sizeof(Rcomplex);
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	size = XLENGTH(s) * sizeof(SEXP);
	break;
    default:
	register_bad_sexp_type(s, __LINE__);
	size = 0;
    }
    return BYTE2VEC(size);
}

static void custom_node_free(void *ptr);

/* Finalization and Weak References */

/* The design of this mechanism is very close to the one described in
   "Stretching the storage manager: weak pointers and stable names in
   Haskell" by Peyton Jones, Marlow, and Elliott (at
   www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz). --LT */

static SEXP R_weak_refs = NULL;

#define READY_TO_FINALIZE_MASK 1

#define SET_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp |= READY_TO_FINALIZE_MASK)
#define CLEAR_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp &= ~READY_TO_FINALIZE_MASK)
#define IS_READY_TO_FINALIZE(s) ((s)->sxpinfo.gp & READY_TO_FINALIZE_MASK)

#define FINALIZE_ON_EXIT_MASK 2

#define SET_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp |= FINALIZE_ON_EXIT_MASK)
#define CLEAR_FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp &= ~FINALIZE_ON_EXIT_MASK)
#define FINALIZE_ON_EXIT(s) ((s)->sxpinfo.gp & FINALIZE_ON_EXIT_MASK)

#define WEAKREF_SIZE 4
#define WEAKREF_KEY(w) VECTOR_ELT(w, 0)
#define SET_WEAKREF_KEY(w, k) SET_VECTOR_ELT(w, 0, k)
#define WEAKREF_VALUE(w) VECTOR_ELT(w, 1)
#define SET_WEAKREF_VALUE(w, v) SET_VECTOR_ELT(w, 1, v)
#define WEAKREF_FINALIZER(w) VECTOR_ELT(w, 2)
#define SET_WEAKREF_FINALIZER(w, f) SET_VECTOR_ELT(w, 2, f)
#define WEAKREF_NEXT(w) VECTOR_ELT(w, 3)
#define SET_WEAKREF_NEXT(w, n) SET_VECTOR_ELT(w, 3, n)

static SEXP MakeCFinalizer(R_CFinalizer_t cfun);

static SEXP NewWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    SEXP w;

    switch (TYPEOF(key)) {
    case NILSXP:
    case ENVSXP:
    case EXTPTRSXP:
    case BCODESXP:
	break;
    default: error(_("can only weakly reference/finalize reference objects"));
    }

    PROTECT(key);
    PROTECT(val = MAYBE_REFERENCED(val) ? duplicate(val) : val);
    PROTECT(fin);
    w = allocVector(VECSXP, WEAKREF_SIZE);
    SET_TYPEOF(w, WEAKREFSXP);
    if (key != R_NilValue) {
	/* If the key is R_NilValue we don't register the weak reference.
	   This is used in loading saved images. */
	SET_WEAKREF_KEY(w, key);
	SET_WEAKREF_VALUE(w, val);
	SET_WEAKREF_FINALIZER(w, fin);
	SET_WEAKREF_NEXT(w, R_weak_refs);
	CLEAR_READY_TO_FINALIZE(w);
	if (onexit)
	    SET_FINALIZE_ON_EXIT(w);
	else
	    CLEAR_FINALIZE_ON_EXIT(w);
	R_weak_refs = w;
    }
    UNPROTECT(3);
    return w;
}

SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit)
{
    switch (TYPEOF(fin)) {
    case NILSXP:
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
	break;
    default: error(_("finalizer must be a function or NULL"));
    }
    return NewWeakRef(key, val, fin, onexit);
}

SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit)
{
    SEXP w;
    PROTECT(key);
    PROTECT(val);
    w = NewWeakRef(key, val, MakeCFinalizer(fin), onexit);
    UNPROTECT(2);
    return w;
}

static Rboolean R_finalizers_pending = FALSE;
static void CheckFinalizers(void)
{
    SEXP s;
    R_finalizers_pending = FALSE;
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
	if (! NODE_IS_MARKED(WEAKREF_KEY(s)) && ! IS_READY_TO_FINALIZE(s))
	    SET_READY_TO_FINALIZE(s);
	if (IS_READY_TO_FINALIZE(s))
	    R_finalizers_pending = TRUE;
    }
}

/* C finalizers are stored in a CHARSXP.  It would be nice if we could
   use EXTPTRSXP's but these only hold a void *, and function pointers
   are not guaranteed to be compatible with a void *.  There should be
   a cleaner way of doing this, but this will do for now. --LT */
/* Changed to RAWSXP in 2.8.0 */
static Rboolean isCFinalizer(SEXP fun)
{
    return TYPEOF(fun) == RAWSXP;
    /*return TYPEOF(fun) == EXTPTRSXP;*/
}

static SEXP MakeCFinalizer(R_CFinalizer_t cfun)
{
    SEXP s = allocVector(RAWSXP, sizeof(R_CFinalizer_t));
    *((R_CFinalizer_t *) RAW(s)) = cfun;
    return s;
    /*return R_MakeExternalPtr((void *) cfun, R_NilValue, R_NilValue);*/
}

static R_CFinalizer_t GetCFinalizer(SEXP fun)
{
    return *((R_CFinalizer_t *) RAW(fun));
    /*return (R_CFinalizer_t) R_ExternalPtrAddr(fun);*/
}

SEXP R_WeakRefKey(SEXP w)
{
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    return WEAKREF_KEY(w);
}

SEXP R_WeakRefValue(SEXP w)
{
    SEXP v;
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    v = WEAKREF_VALUE(w);
    if (v != R_NilValue && NAMED(v) <= 1)
	SET_NAMED(v, 2);
    return v;
}

void R_RunWeakRefFinalizer(SEXP w)
{
    SEXP key, fun, e;
    if (TYPEOF(w) != WEAKREFSXP)
	error(_("not a weak reference"));
    key = WEAKREF_KEY(w);
    fun = WEAKREF_FINALIZER(w);
    SET_WEAKREF_KEY(w, R_NilValue);
    SET_WEAKREF_VALUE(w, R_NilValue);
    SET_WEAKREF_FINALIZER(w, R_NilValue);
    if (! IS_READY_TO_FINALIZE(w))
	SET_READY_TO_FINALIZE(w); /* insures removal from list on next gc */
    PROTECT(key);
    PROTECT(fun);
    if (isCFinalizer(fun)) {
	/* Must be a C finalizer. */
	R_CFinalizer_t cfun = GetCFinalizer(fun);
	cfun(key);
    }
    else if (fun != R_NilValue) {
	/* An R finalizer. */
	PROTECT(e = LCONS(fun, LCONS(key, R_NilValue)));
	eval(e, R_GlobalEnv);
	UNPROTECT(1);
    }
    UNPROTECT(2);
}

static Rboolean RunFinalizers(void)
{
    /* Prevent this function from running again when already in
       progress. Jumps can only occur inside the top level context
       where they will be caught, so the flag is guaranteed to be
       reset at the end. */
    static Rboolean running = FALSE;
    if (running) return FALSE;
    running = TRUE;

    volatile SEXP s, last;
    volatile Rboolean finalizer_run = FALSE;

    for (s = R_weak_refs, last = R_NilValue; s != R_NilValue;) {
	SEXP next = WEAKREF_NEXT(s);
	if (IS_READY_TO_FINALIZE(s)) {
	    RCNTXT thiscontext;
	    RCNTXT * volatile saveToplevelContext;
	    volatile int savestack;
	    volatile SEXP topExp;

	    finalizer_run = TRUE;

	    /* A top level context is established for the finalizer to
	       insure that any errors that might occur do not spill
	       into the call that triggered the collection. */
	    begincontext(&thiscontext, CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv,
			 R_BaseEnv, R_NilValue, R_NilValue);
	    saveToplevelContext = R_ToplevelContext;
	    PROTECT(topExp = R_CurrentExpr);
	    savestack = R_PPStackTop;
	    /* The value of 'next' is protected to make it safe
	       for this routine to be called recursively from a
	       gc triggered by a finalizer. */
	    PROTECT(next);
	    if (! SETJMP(thiscontext.cjmpbuf)) {
		R_GlobalContext = R_ToplevelContext = &thiscontext;

		/* The entry in the weak reference list is removed
		   before running the finalizer.  This insures that a
		   finalizer is run only once, even if running it
		   raises an error. */
		if (last == R_NilValue)
		    R_weak_refs = next;
		else
		    SET_WEAKREF_NEXT(last, next);
		R_RunWeakRefFinalizer(s);
	    }
	    endcontext(&thiscontext);
	    UNPROTECT(1); /* next */
	    R_ToplevelContext = saveToplevelContext;
	    R_PPStackTop = savestack;
	    R_CurrentExpr = topExp;
	    UNPROTECT(1);
	}
	else last = s;
	s = next;
    }
    running = FALSE;
    R_finalizers_pending = FALSE;
    return finalizer_run;
}

void R_RunExitFinalizers(void)
{
    SEXP s;

    R_checkConstants(TRUE);

    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s))
	if (FINALIZE_ON_EXIT(s))
	    SET_READY_TO_FINALIZE(s);
    RunFinalizers();
}

void R_RunPendingFinalizers(void)
{
    if (R_finalizers_pending)
	RunFinalizers();
}

void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit)
{
    R_MakeWeakRef(s, R_NilValue, fun, onexit);
}

void R_RegisterFinalizer(SEXP s, SEXP fun)
{
    R_RegisterFinalizerEx(s, fun, FALSE);
}

void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit)
{
    R_MakeWeakRefC(s, R_NilValue, fun, onexit);
}

void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun)
{
    R_RegisterCFinalizerEx(s, fun, FALSE);
}

/* R interface function */

SEXP attribute_hidden do_regFinaliz(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int onexit;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != ENVSXP && TYPEOF(CAR(args)) != EXTPTRSXP)
	error(_("first argument must be environment or external pointer"));
    if (TYPEOF(CADR(args)) != CLOSXP)
	error(_("second argument must be a function"));

    onexit = asLogical(CADDR(args));
    if(onexit == NA_LOGICAL)
	error(_("third argument must be 'TRUE' or 'FALSE'"));

    R_RegisterFinalizerEx(CAR(args), CADR(args), onexit);
    return R_NilValue;
}


static void traceHeap()
{

    /* forward all roots */
    FORWARD_NODE(R_NilValue);	           /* Builtin constants */
    FORWARD_NODE(NA_STRING);
    FORWARD_NODE(R_BlankString);
    FORWARD_NODE(R_BlankScalarString);
    FORWARD_NODE(R_CurrentExpression);
    FORWARD_NODE(R_UnboundValue);
    FORWARD_NODE(R_RestartToken);
    FORWARD_NODE(R_MissingArg);
    FORWARD_NODE(R_InBCInterpreter);

    FORWARD_NODE(R_GlobalEnv);	           /* Global environment */
    FORWARD_NODE(R_BaseEnv);
    FORWARD_NODE(R_EmptyEnv);
    FORWARD_NODE(R_Warnings);	           /* Warnings, if any */
    FORWARD_NODE(R_ReturnedValue);

    FORWARD_NODE(R_HandlerStack);          /* Condition handler stack */
    FORWARD_NODE(R_RestartStack);          /* Available restarts stack */

    FORWARD_NODE(R_BCbody);                /* Current byte code object */
    FORWARD_NODE(R_Srcref);                /* Current source reference */

    FORWARD_NODE(R_TrueValue);
    FORWARD_NODE(R_FalseValue);
    FORWARD_NODE(R_LogicalNAValue);

    FORWARD_NODE(R_print.na_string);
    FORWARD_NODE(R_print.na_string_noquote);

    int i;
    RCNTXT* ctxt;
    SEXP s;

    if (R_SymbolTable != NULL)             /* in case of GC during startup */
	for (i = 0; i < HSIZE; i++)        /* Symbol table */
	    FORWARD_NODE(R_SymbolTable[i]);

    if (R_CurrentExpr != NULL)	           /* Current expression */
	FORWARD_NODE(R_CurrentExpr);

    for (i = 0; i < R_MaxDevices; i++) {   /* Device display lists */
	pGEDevDesc gdd = GEgetDevice(i);
	if (gdd) {
	    FORWARD_NODE(gdd->displayList);
	    FORWARD_NODE(gdd->savedSnapshot);
	    if (gdd->dev)
		FORWARD_NODE(gdd->dev->eventEnv);
	}
    }

    for (ctxt = R_GlobalContext ; ctxt != NULL ; ctxt = ctxt->nextcontext) {
	FORWARD_NODE(ctxt->conexit);       /* on.exit expressions */
	FORWARD_NODE(ctxt->promargs);	   /* promises supplied to closure */
	FORWARD_NODE(ctxt->callfun);       /* the closure called */
	FORWARD_NODE(ctxt->sysparent);     /* calling environment */
	FORWARD_NODE(ctxt->call);          /* the call */
	FORWARD_NODE(ctxt->cloenv);        /* the closure environment */
	FORWARD_NODE(ctxt->bcbody);        /* the current byte code object */
	FORWARD_NODE(ctxt->handlerstack);  /* the condition handler stack */
	FORWARD_NODE(ctxt->restartstack);  /* the available restarts stack */
	FORWARD_NODE(ctxt->srcref);	   /* the current source reference */
	FORWARD_NODE(ctxt->returnValue);   /* For on.exit calls */
    }

    FORWARD_NODE(R_PreciousList);

#ifndef CONSERVATIVE_STACK_SCAN
    for (i = 0; i < R_PPStackTop; i++)	   /* Protected pointers */
	FORWARD_NODE(R_PPStack[i]);
#endif

    FORWARD_NODE(R_VStack);		   /* R_alloc stack */

    for (R_bcstack_t *sp = R_BCNodeStackBase; sp < R_BCNodeStackTop; sp++) {
#ifdef TYPED_STACK
	if (sp->tag == RAWMEM_TAG)
	    sp += sp->u.ival;
	else if (sp->tag == 0 || IS_PARTIAL_SXP_TAG(sp->tag))
	    FORWARD_NODE(sp->u.sxpval);
#else
	FORWARD_NODE(*sp);
#endif
    }
    FORWARD_NODE(R_CachedScalarReal);
    FORWARD_NODE(R_CachedScalarInteger);

    /* main processing loop */
    PROCESS_NODES();

    /* identify weakly reachable nodes */
    {
	Rboolean recheck_weak_refs;
	do {
	    recheck_weak_refs = FALSE;
	    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
		if (NODE_IS_MARKED(WEAKREF_KEY(s))) {
		    if (! NODE_IS_MARKED(WEAKREF_VALUE(s))) {
			recheck_weak_refs = TRUE;
			FORWARD_NODE(WEAKREF_VALUE(s));
		    }
		    if (! NODE_IS_MARKED(WEAKREF_FINALIZER(s))) {
			recheck_weak_refs = TRUE;
			FORWARD_NODE(WEAKREF_FINALIZER(s));
		    }
		}
	    }
	    PROCESS_NODES();
	} while (recheck_weak_refs);
    }

    /* mark nodes ready for finalizing */
    CheckFinalizers();

    /* process the weak reference chain */
    for (s = R_weak_refs; s != R_NilValue; s = WEAKREF_NEXT(s)) {
	FORWARD_NODE(s);
	FORWARD_NODE(WEAKREF_KEY(s));
	FORWARD_NODE(WEAKREF_VALUE(s));
	FORWARD_NODE(WEAKREF_FINALIZER(s));
    }
    PROCESS_NODES();

    /* process CHARSXP cache */
    if (R_StringHash != NULL) /* in case of GC during initialization */
    {
	SEXP t;
	int nc = 0;
	for (i = 0; i < LENGTH(R_StringHash); i++) {
	    s = VECTOR_ELT(R_StringHash, i);
	    t = R_NilValue;
	    while (s != R_NilValue) {
		if (! NODE_IS_MARKED(CXHEAD(s))) { /* remove unused CHARSXP and cons cell */
		    if (t == R_NilValue) /* head of list */
			VECTOR_ELT(R_StringHash, i) = CXTAIL(s);
		    else
			CXTAIL(t) = CXTAIL(s);
		    s = CXTAIL(s);
		    continue;
		}
		FORWARD_NODE(s);
		FORWARD_NODE(CXHEAD(s));
		t = s;
		s = CXTAIL(s);
	    }
	    if(VECTOR_ELT(R_StringHash, i) != R_NilValue) nc++;
	}
	SET_TRUELENGTH(R_StringHash, nc); /* SET_HASHPRI, really */
    }
    FORWARD_NODE(R_StringHash);
    PROCESS_NODES();
}

#ifdef COMPUTE_REFCNT_VALUES
#define FIX_REFCNT(x, old, new) do {					\
	if (TRACKREFS(x)) {						\
	    SEXP __old__ = (old);					\
	    SEXP __new__ = (new);					\
	    if (__old__ != __new__) {					\
		if (__old__) DECREMENT_REFCNT(__old__);			\
		if (__new__) INCREMENT_REFCNT(__new__);			\
	    }								\
	}								\
    } while (0)
#else
#define FIX_REFCNT(x, old, new) do {} while (0)
#endif


SEXP attribute_hidden do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = ScalarLogical(gc_reporting);
    checkArity(op, args);
    i = asLogical(CAR(args));
    if (i != NA_LOGICAL)
	gc_reporting = i;
    return old;
}

/* reports memory use to profiler in eval.c */

void attribute_hidden get_current_mem(size_t *smallvsize,
				      size_t *largevsize,
				      size_t *nodes)
{
    *smallvsize = smallVallocSize();
    *largevsize = largeVallocSize();
    *nodes = nodesNumber() * sizeof(SEXPREC);
    return;
}

SEXP attribute_hidden do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    int ogc, reset_max;

    checkArity(op, args);
    ogc = gc_reporting;
    gc_reporting = asLogical(CAR(args));
    reset_max = asLogical(CADR(args));
    fullCollection = TRUE;
    R_gc();
#ifndef IMMEDIATE_FINALIZERS
    R_RunPendingFinalizers();
#endif
    gc_reporting = ogc;
    size_t nodesnum = nodesNumber();
    size_t nodetrigger = HEAP.page_limit * nodesPerPage();
    size_t heapsize = vheapUsed();
    /*- now return the [used , gc trigger size] for cells and heap */
    PROTECT(value = allocVector(REALSXP, 14));
    REAL(value)[0] = nodesnum;
    REAL(value)[1] = heapsize;
    REAL(value)[4] = nodetrigger;
    REAL(value)[5] = HEAP.heapLimit;
    /* next four are in 0.1Mb, rounded up */
    REAL(value)[2] = 0.1*ceil(10. * (nodesnum)/Mega * sizeof(SEXPREC));
    REAL(value)[3] = 0.1*ceil(10. * heapsize/Mega * vsfac);
    REAL(value)[6] = 0.1*ceil(10. * nodetrigger/Mega * sizeof(SEXPREC));
    REAL(value)[7] = 0.1*ceil(10. * HEAP.heapLimit/Mega * vsfac);
    REAL(value)[8] = (R_MaxNSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxNSize/Mega * sizeof(SEXPREC)) : NA_REAL;
    REAL(value)[9] = (R_MaxVSize < R_SIZE_T_MAX) ?
	0.1*ceil(10. * R_MaxVSize/Mega * vsfac) : NA_REAL;
    if (reset_max){
	    R_N_maxused = nodesnum;
	    R_V_maxused = heapsize;
    }
    REAL(value)[10] = R_N_maxused;
    REAL(value)[11] = R_V_maxused;
    REAL(value)[12] = 0.1*ceil(10. * R_N_maxused/Mega*sizeof(SEXPREC));
    REAL(value)[13] = 0.1*ceil(10. * R_V_maxused/Mega*vsfac);
    UNPROTECT(1);
    return value;
}


static void NORET mem_err_heap(R_size_t size)
{
    errorcall(R_NilValue, _("vector memory exhausted (limit reached?)"));
}


static void NORET mem_err_cons(void)
{
    errorcall(R_NilValue, _("cons memory exhausted (limit reached?)"));
}

static void NORET mem_err_malloc(R_size_t size)
{
    errorcall(R_NilValue, _("memory exhausted (limit reached?)"));
}

/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

#define PP_REDZONE_SIZE 1000L
static int R_StandardPPStackSize, R_RealPPStackSize;

void attribute_hidden InitMemory()
{
    new_gc_initHeap();
    int i;
    int gen;

    gc_reporting = R_Verbose;
    R_StandardPPStackSize = R_PPStackSize;
    R_RealPPStackSize = R_PPStackSize + PP_REDZONE_SIZE;
    if (!(R_PPStack = (SEXP *) malloc(R_RealPPStackSize * sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for pointer stack");
    R_PPStackTop = 0;
#if VALGRIND_LEVEL > 1
    VALGRIND_MAKE_MEM_NOACCESS(R_PPStack+R_PPStackSize, PP_REDZONE_SIZE);
#endif
    vsfac = sizeof(VECREC);
    R_VSize = (R_VSize + 1)/vsfac;
    if (R_MaxVSize < R_SIZE_T_MAX) R_MaxVSize = (R_MaxVSize + 1)/vsfac;

    /* R_NilValue */
    /* THIS MUST BE THE FIRST CONS CELL ALLOCATED */
    /* OR ARMAGEDDON HAPPENS. */
    /* Field assignments for R_NilValue must not go through write barrier
       since the write barrier prevents assignments to R_NilValue's fields.
       because of checks for nil */
    GET_FREE_NODE(R_NilValue);
    INIT_REFCNT(R_NilValue);
    SET_REFCNT(R_NilValue, REFCNTMAX);
    SET_TYPEOF(R_NilValue, NILSXP);
    CAR(R_NilValue) = R_NilValue;
    CDR(R_NilValue) = R_NilValue;
    TAG(R_NilValue) = R_NilValue;
    ATTRIB(R_NilValue) = R_NilValue;
    MARK_NOT_MUTABLE(R_NilValue);

    R_BCNodeStackBase =
	(R_bcstack_t *) malloc(R_BCNODESTACKSIZE * sizeof(R_bcstack_t));
    if (R_BCNodeStackBase == NULL)
	R_Suicide("couldn't allocate node stack");
#ifdef BC_INT_STACK
    R_BCIntStackBase =
      (IStackval *) malloc(R_BCINTSTACKSIZE * sizeof(IStackval));
    if (R_BCIntStackBase == NULL)
	R_Suicide("couldn't allocate integer stack");
#endif
    R_BCNodeStackTop = R_BCNodeStackBase;
    R_BCNodeStackEnd = R_BCNodeStackBase + R_BCNODESTACKSIZE;
#ifdef BC_INT_STACK
    R_BCIntStackTop = R_BCIntStackBase;
    R_BCIntStackEnd = R_BCIntStackBase + R_BCINTSTACKSIZE;
#endif

    R_weak_refs = R_NilValue;

    R_HandlerStack = R_RestartStack = R_NilValue;

    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;

    /*  The current source line */
    R_Srcref = R_NilValue;

    /* R_TrueValue and R_FalseValue */
    R_TrueValue = mkTrue();
    MARK_NOT_MUTABLE(R_TrueValue);
    R_FalseValue = mkFalse();
    MARK_NOT_MUTABLE(R_FalseValue);
    R_LogicalNAValue = allocVector(LGLSXP, 1);
    LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
    MARK_NOT_MUTABLE(R_LogicalNAValue);
}

/* Since memory allocated from the heap is non-moving, R_alloc just
   allocates off the heap as RAWSXP/REALSXP and maintains the stack of
   allocations through the ATTRIB pointer.  The stack pointer R_VStack
   is traced by the collector. */
void *vmaxget(void)
{
    return (void *) R_VStack;
}

void vmaxset(const void *ovmax)
{
    R_VStack = (SEXP) ovmax;
}

char *R_alloc(size_t nelem, int eltsize)
{
    R_size_t size = nelem * eltsize;
    /* doubles are a precaution against integer overflow on 32-bit */
    double dsize = (double) nelem * eltsize;
    if (dsize > 0) {
	SEXP s;
#ifdef LONG_VECTOR_SUPPORT
	/* 64-bit platform: previous version used REALSXPs */
	if(dsize > R_XLEN_T_MAX)  /* currently 4096 TB */
	    error(_("cannot allocate memory block of size %0.f Tb"),
		  dsize/R_pow_di(1024.0, 4));
	s = allocVector(RAWSXP, size + 1);
#else
	if(dsize > R_LEN_T_MAX) /* must be in the Gb range */
	    error(_("cannot allocate memory block of size %0.1f Gb"),
		  dsize/R_pow_di(1024.0, 3));
	s = allocVector(RAWSXP, size + 1);
#endif
	ATTRIB(s) = R_VStack;
	R_VStack = s;
	return (char *) DATAPTR(s);
    }
    /* One programmer has relied on this, but it is undocumented! */
    else return NULL;
}

#ifdef HAVE_STDALIGN_H
# include <stdalign.h>
#endif

#include <stdint.h>

long double *R_allocLD(size_t nelem)
{
#if __alignof_is_defined
    // This is C11: picky compilers may warn.
    size_t ld_align = alignof(long double);
#elif __GNUC__
    // This is C99, but do not rely on it.
    size_t ld_align = offsetof(struct { char __a; long double __b; }, __b);
#else
    size_t ld_align = 0x0F; // value of x86_64, known others are 4 or 8
#endif
    if (ld_align > 8) {
	uintptr_t tmp = (uintptr_t) R_alloc(nelem + 1, sizeof(long double));
	tmp = (tmp + ld_align - 1) & ~ld_align;
	return (long double *) tmp;
    } else {
	return (long double *) R_alloc(nelem, sizeof(long double));
    }
}


/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
    R_size_t size  = nelem * eltsize;
    char *p = R_alloc(nelem, eltsize);

    if(p) memset(p, 0, size);
    return p;
}


char *S_realloc(char *p, long new, long old, int size)
{
    size_t nold;
    char *q;
    /* shrinking is a no-op */
    if(new <= old) return p; // so nnew > 0 below
    q = R_alloc((size_t)new, size);
    nold = (size_t)old * size;
    memcpy(q, p, nold);
    memset(q + nold, 0, (size_t)new*size - nold);
    return q;
}

/* "allocSExp" allocate a SEXPREC */
/* call gc if necessary */

SEXP allocSExp(SEXPTYPE t)
{
    SEXP s;
    ALLOC_SEXP(s, t);
    INIT_REFCNT(s);
    SET_TYPEOF(s, t);
    CAR(s) = R_NilValue;
    CDR(s) = R_NilValue;
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

static SEXP allocSExpNonCons(SEXPTYPE t)
{
    SEXP s;
    ALLOC_SEXP(s, t);
    INIT_REFCNT(s);
    SET_TYPEOF(s, t);
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

/* cons is defined directly to avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP cons(SEXP car, SEXP cdr)
{
    SEXP s;
    ALLOC_CONS(s, car, cdr);

    INIT_REFCNT(s);
    SET_TYPEOF(s, LISTSXP);
    CAR(s) = CHK(car); if (car) INCREMENT_REFCNT(car);
    CDR(s) = CHK(cdr); if (cdr) INCREMENT_REFCNT(cdr);
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

SEXP CONS_NR(SEXP car, SEXP cdr)
{
    SEXP s;
    ALLOC_CONS(s, car, cdr);

    INIT_REFCNT(s);
    DISABLE_REFCNT(s);
    SET_TYPEOF(s, LISTSXP);
    CAR(s) = CHK(car);
    CDR(s) = CHK(cdr);
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    return s;
}

/*----------------------------------------------------------------------

  NewEnvironment

  Create an environment by extending "rho" with a frame obtained by
  pairing the variable names given by the tags on "namelist" with
  the values given by the elements of "valuelist".

  NewEnvironment is defined directly to avoid the need to protect its
  arguments unless a GC will actually occur.  This definition allows
  the namelist argument to be shorter than the valuelist; in this
  case the remaining values must be named already.  (This is useful
  in cases where the entire valuelist is already named--namelist can
  then be R_NilValue.)

  The valuelist is destructively modified and used as the
  environment's frame.
*/
SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    SEXP v, n, newrho;

    ALLOC_ENV(newrho, namelist, valuelist, rho);

    INIT_REFCNT(newrho);
    SET_TYPEOF(newrho, ENVSXP);
    FRAME(newrho) = valuelist;
    ENCLOS(newrho) = CHK(rho);
    HASHTAB(newrho) = R_NilValue;
    ATTRIB(newrho) = R_NilValue;

    v = CHK(valuelist);
    n = CHK(namelist);
    while (v != R_NilValue && n != R_NilValue) {
	SET_TAG(v, TAG(n));
	v = CDR(v);
	n = CDR(n);
    }
    return (newrho);
}

/* mkPROMISE is defined directly do avoid the need to protect its arguments
   unless a GC will actually occur. */
SEXP attribute_hidden mkPROMISE(SEXP expr, SEXP rho)
{
    SEXP s;
    ALLOC_PROM(s, expr, rho);

    /* precaution to ensure code does not get modified via
       substitute() and the like */
    if (NAMED(expr) < 2) SET_NAMED(expr, 2);

    INIT_REFCNT(s);
    SET_TYPEOF(s, PROMSXP);
    PRCODE(s) = CHK(expr);
    PRENV(s) = CHK(rho);
    PRVALUE(s) = R_UnboundValue;
    PRSEEN(s) = 0;
    ATTRIB(s) = R_NilValue;
    return s;
}

SEXP R_mkEVPROMISE(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, R_NilValue);
    SET_PRVALUE(prom, val);
    return prom;
}

SEXP R_mkEVPROMISE_NR(SEXP expr, SEXP val)
{
    SEXP prom = mkPROMISE(expr, R_NilValue);
    DISABLE_REFCNT(prom);
    SET_PRVALUE(prom, val);
    return prom;
}

/* support for custom allocators that allow vectors to be allocated
   using non-standard means such as COW mmap() */

static void *custom_node_alloc(R_allocator_t *allocator, size_t size) {
    if (!allocator || !allocator->mem_alloc) return NULL;
    void *ptr = allocator->mem_alloc(allocator, size + sizeof(R_allocator_t));
    if (ptr) {
	R_allocator_t *ca = (R_allocator_t*) ptr;
	*ca = *allocator;
	return (void*) (ca + 1);
    }
    return NULL;
}

static void custom_node_free(void *ptr) {
    if (ptr) {
	R_allocator_t *allocator = ((R_allocator_t*) ptr) - 1;
	allocator->mem_free(allocator, (void*)allocator);
    }
}

/* All vector objects must be a multiple of sizeof(SEXPREC_ALIGN)
   bytes so that alignment is preserved for all objects */

/* Allocate a vector object (and also list-like objects).
   This ensures only validity of list-like (LISTSXP, VECSXP, EXPRSXP),
   STRSXP and CHARSXP types;  e.g., atomic types remain un-initialized
   and must be initialized upstream, e.g., in do_makevector().
*/
#define intCHARSXP 73

SEXP allocVector3(SEXPTYPE type, R_xlen_t length, R_allocator_t *allocator)
{
  return new_gc_allocVector3(type, length, allocator);
}

/* For future hiding of allocVector(CHARSXP) */
SEXP attribute_hidden allocCharsxp(R_len_t len)
{
    return allocVector(intCHARSXP, len);
}

SEXP allocList(int n)
{
    int i;
    SEXP result;
    result = R_NilValue;
    for (i = 0; i < n; i++)
	result = CONS(R_NilValue, result);
    return result;
}

SEXP allocS4Object(void)
{
   SEXP s;
   s = allocSExpNonCons(S4SXP);
   SET_S4_OBJECT(s);
   return s;
}

static SEXP allocFormalsList(int nargs, ...)
{
    SEXP res = R_NilValue;
    SEXP n;
    int i;
    va_list(syms);
    va_start(syms, nargs);

    for(i = 0; i < nargs; i++) {
	res = CONS(R_NilValue, res);
    }
    R_PreserveObject(res);

    n = res;
    for(i = 0; i < nargs; i++) {
	SET_TAG(n, (SEXP) va_arg(syms, SEXP));
	MARK_NOT_MUTABLE(n);
	n = CDR(n);
    }
    va_end(syms);

    return res;
}


SEXP allocFormalsList2(SEXP sym1, SEXP sym2)
{
    return allocFormalsList(2, sym1, sym2);
}

SEXP allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3)
{
    return allocFormalsList(3, sym1, sym2, sym3);
}

SEXP allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4)
{
    return allocFormalsList(4, sym1, sym2, sym3, sym4);
}

SEXP allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5)
{
    return allocFormalsList(5, sym1, sym2, sym3, sym4, sym5);
}

SEXP allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4,
		       SEXP sym5, SEXP sym6)
{
    return allocFormalsList(6, sym1, sym2, sym3, sym4, sym5, sym6);
}

/* "gc" a mark-sweep or in-place generational garbage collector */

void R_gc(void)
{
    R_gc_internal(0);
}

static void R_gc_full(R_size_t size_needed)
{
    fullCollection = TRUE;
    R_gc_internal(size_needed);
}

static double gctimes[5], gcstarttimes[5];
static Rboolean gctime_enabled = FALSE;

/* this is primitive */
SEXP attribute_hidden do_gctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (args == R_NilValue)
	gctime_enabled = TRUE;
    else {
	check1arg(args, call, "on");
	gctime_enabled = asLogical(CAR(args));
    }
    ans = allocVector(REALSXP, 5);
    REAL(ans)[0] = gctimes[0];
    REAL(ans)[1] = gctimes[1];
    REAL(ans)[2] = gctimes[2];
    REAL(ans)[3] = gctimes[3];
    REAL(ans)[4] = gctimes[4];
    return ans;
}

static void gc_start_timing(void)
{
    if (gctime_enabled)
	R_getProcTime(gcstarttimes);
}

static void gc_end_timing(void)
{
    if (gctime_enabled) {
	double times[5], delta;
	R_getProcTime(times);

	/* add delta to compensate for timer resolution */
#if 0
	/* this seems to over-compensate too */
	delta = R_getClockIncrement();
#else
	delta = 0;
#endif

	gctimes[0] += times[0] - gcstarttimes[0] + delta;
	gctimes[1] += times[1] - gcstarttimes[1] + delta;
	gctimes[2] += times[2] - gcstarttimes[2];
	gctimes[3] += times[3] - gcstarttimes[3];
	gctimes[4] += times[4] - gcstarttimes[4];
    }
}

#define R_MAX(a,b) (a) < (b) ? (b) : (a)

static void R_gc_internal(R_size_t size_needed)
{
    if (!R_GCEnabled) {
      if (HEAP.num_pages == HEAP.page_limit) {
        HEAP.page_limit++;
        R_NSize = HEAP.page_limit * nodesPerPage();
      }
      if (size_needed > HEAP.heapLimit - HEAP.size) {
        HEAP.heapLimit += size_needed - (HEAP.heapLimit - HEAP.size);
        R_VSize = HEAP.heapLimit;
      }
      gc_pending = TRUE;
      return;
    }
    gc_pending = FALSE;

    double ncells, vcells, vfrac, nfrac;
    SEXPTYPE first_bad_sexp_type = 0;
#ifdef PROTECTCHECK
    SEXPTYPE first_bad_sexp_type_old_type = 0;
#endif
    SEXP first_bad_sexp_type_sexp = NULL;
    int first_bad_sexp_type_line = 0;

#ifdef IMMEDIATE_FINALIZERS
    Rboolean first = TRUE;
 again:
#endif

    gc_count++;

    R_N_maxused = R_MAX(R_N_maxused, nodesNumber());
    R_V_maxused = R_MAX(R_V_maxused, vheapUsed());

    BEGIN_SUSPEND_INTERRUPTS {
	R_in_gc = TRUE;
	gc_start_timing();
	doGc(NUM_BUCKETS);
	gc_end_timing();
	R_in_gc = FALSE;
    } END_SUSPEND_INTERRUPTS;

    if (bad_sexp_type_seen != 0 && first_bad_sexp_type == 0) {
	first_bad_sexp_type = bad_sexp_type_seen;
#ifdef PROTECTCHECK
	first_bad_sexp_type_old_type = bad_sexp_type_old_type;
#endif
	first_bad_sexp_type_sexp = bad_sexp_type_sexp;
	first_bad_sexp_type_line = bad_sexp_type_line;
    }

    if (gc_reporting) {
	ncells = nodesNumber();
	nfrac = (100.0 * ncells) / R_NSize;
	/* We try to make this consistent with the results returned by gc */
	ncells = 0.1*ceil(10*ncells * sizeof(SEXPREC)/Mega);
	REprintf("\n%.1f Mbytes of cons cells used (%d%%)\n",
		 ncells, (int) (nfrac + 0.5));
	vcells = vheapUsed();
	vfrac = (100.0 * vcells) / R_VSize;
	vcells = 0.1*ceil(10*vcells * vsfac/Mega);
	REprintf("%.1f Mbytes of vectors used (%d%%)\n",
		 vcells, (int) (vfrac + 0.5));
    }

#ifdef IMMEDIATE_FINALIZERS
    if (first) {
	first = FALSE;
	/* Run any eligible finalizers.  The return result of
	   RunFinalizers is TRUE if any finalizers are actually run.
	   There is a small chance that running finalizers here may
	   chew up enough memory to make another immediate collection
	   necessary.  If so, we jump back to the beginning and run
	   the collection, but on this second pass we do not run
	   finalizers. */
	if (RunFinalizers() &&
	    (NO_FREE_NODES() || size_needed > VHEAP_FREE()))
	    goto again;
    }
#endif

    if (first_bad_sexp_type != 0) {
#ifdef PROTECTCHECK
	if (first_bad_sexp_type == FREESXP)
	    error("GC encountered a node (%p) with type FREESXP (was %s)"
		  " at memory.c:%d",
		  first_bad_sexp_type_sexp,
		  sexptype2char(first_bad_sexp_type_old_type),
		  first_bad_sexp_type_line);
	else
	    error("GC encountered a node (%p) with an unknown SEXP type: %s"
		  " at memory.c:%d",
		  first_bad_sexp_type_sexp,
		  sexptype2char(first_bad_sexp_type),
		  first_bad_sexp_type_line);
#else
	error("GC encountered a node (%p) with an unknown SEXP type: %s"
	      " at memory.c:%d",
	      first_bad_sexp_type_sexp,
	      sexptype2char(first_bad_sexp_type),
	      first_bad_sexp_type_line);
#endif
    }

    /* sanity check on logical scalar values */
    if (R_TrueValue != NULL && LOGICAL(R_TrueValue)[0] != TRUE) {
	LOGICAL(R_TrueValue)[0] = TRUE;
	error("internal TRUE value has been modified");
    }
    if (R_FalseValue != NULL && LOGICAL(R_FalseValue)[0] != FALSE) {
	LOGICAL(R_FalseValue)[0] = FALSE;
	error("internal FALSE value has been modified");
    }
    if (R_LogicalNAValue != NULL &&
	LOGICAL(R_LogicalNAValue)[0] != NA_LOGICAL) {
	LOGICAL(R_LogicalNAValue)[0] = NA_LOGICAL;
	error("internal logical NA value has been modified");
    }
    /* compiler constants are checked in RunGenCollect */
}


SEXP attribute_hidden do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nms;
    int i, tmp;

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 24));
    PROTECT(nms = allocVector(STRSXP, 24));
    for (i = 0; i < 24; i++) {
	INTEGER(ans)[i] = 0;
	SET_STRING_ELT(nms, i, type2str(i > LGLSXP? i+2 : i));
    }
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {
      int gen;

      /* run a full GC to make sure that all stuff in use is in Old space */
      fullCollection = TRUE;
      R_gc();
      // TODO
    } END_SUSPEND_INTERRUPTS;
    UNPROTECT(2);
    return ans;
}

/* "protect" push a single argument onto R_PPStack */

/* In handling a stack overflow we have to be careful not to use
   PROTECT. error("protect(): stack overflow") would call deparse1,
   which uses PROTECT and segfaults.*/

/* However, the traceback creation in the normal error handler also
   does a PROTECT, as does the jumping code, at least if there are
   cleanup expressions to handle on the way out.  So for the moment
   we'll allocate a slightly larger PP stack and only enable the added
   red zone during handling of a stack overflow error.  LT */

static void reset_pp_stack(void *data)
{
    int *poldpps = data;
    R_PPStackSize =  *poldpps;
}

void NORET R_signal_protect_error(void)
{
    RCNTXT cntxt;
    int oldpps = R_PPStackSize;

    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &reset_pp_stack;
    cntxt.cenddata = &oldpps;

    if (R_PPStackSize < R_RealPPStackSize)
	R_PPStackSize = R_RealPPStackSize;
    errorcall(R_NilValue, _("protect(): protection stack overflow"));

    endcontext(&cntxt); /* not reached */
}

void NORET R_signal_unprotect_error(void)
{
    error(ngettext("unprotect(): only %d protected item",
		   "unprotect(): only %d protected items", R_PPStackTop),
	  R_PPStackTop);
}

#ifndef INLINE_PROTECT
SEXP protect(SEXP s)
{
#ifndef CONSERVATIVE_STACK_SCAN
    if (R_PPStackTop >= R_PPStackSize)
	R_signal_protect_error();
    R_PPStack[R_PPStackTop++] = CHK(s);
#endif
    return s;
}


/* "unprotect" pop argument list from top of R_PPStack */

void unprotect(int l)
{
#ifndef CONSERVATIVE_STACK_SCAN
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else R_signal_unprotect_error();
#endif
}
#endif

/* "unprotect_ptr" remove pointer from somewhere in R_PPStack */

void unprotect_ptr(SEXP s)
{
#ifndef CONSERVATIVE_STACK_SCAN
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
	if (i == 0)
	    error(_("unprotect_ptr: pointer not found"));
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it, if any */

    while (++i < R_PPStackTop) R_PPStack[i - 1] = R_PPStack[i];

    R_PPStackTop--;
#endif
}

/* Debugging function:  is s protected? */

int Rf_isProtected(SEXP s)
{
#ifndef CONSERVATIVE_STACK_SCAN
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    do {
	if (i == 0)
	    return(i);
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    return(i);
#else
    return TRUE;
#endif
}


#ifndef INLINE_PROTECT
void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
#ifndef CONSERVATIVE_STACK_SCAN
    protect(s);
    *pi = R_PPStackTop - 1;
#endif
}
#endif

void NORET R_signal_reprotect_error(PROTECT_INDEX i)
{
    error(ngettext("R_Reprotect: only %d protected item, can't reprotect index %d",
		   "R_Reprotect: only %d protected items, can't reprotect index %d",
		   R_PPStackTop),
	  R_PPStackTop, i);
}

#ifndef INLINE_PROTECT
void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
#ifndef CONSERVATIVE_STACK_SCAN
    if (i >= R_PPStackTop || i < 0)
	R_signal_reprotect_error(i);
    R_PPStack[i] = s;
#endif
}
#endif

#ifdef UNUSED
/* remove all objects from the protection stack from index i upwards
   and return them in a vector. The order in the vector is from new
   to old. */
SEXP R_CollectFromIndex(PROTECT_INDEX i)
{
    SEXP res;
    int top = R_PPStackTop, j = 0;
    if (i > top) i = top;
    res = protect(allocVector(VECSXP, top - i));
    while (i < top)
	SET_VECTOR_ELT(res, j++, R_PPStack[--top]);
    R_PPStackTop = top; /* this includes the protect we used above */
    return res;
}
#endif

/* "initStack" initialize environment stack */
attribute_hidden
void initStack(void)
{
    R_PPStackTop = 0;
}


/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifndef HAVE_WORKING_CALLOC
    if(nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if(!p) /* problem here is that we don't have a format for size_t. */
	error(_("'Calloc' could not allocate memory (%.0f of %u bytes)"),
	      (double) nelem, elsize);
    return(p);
}

void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    /* Protect against broken realloc */
    if(ptr) p = realloc(ptr, size); else p = malloc(size);
    if(!p)
	error(_("'Realloc' could not re-allocate memory (%.0f bytes)"),
	      (double) size);
    return(p);
}

void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning("attempt to free NULL pointer by Free"); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_ReleaseObject. */

void R_PreserveObject(SEXP object)
{
    R_PreciousList = CONS(object, R_PreciousList);
}

static SEXP RecursiveRelease(SEXP object, SEXP list)
{
    if (!isNull(list)) {
	if (object == CAR(list))
	    return CDR(list);
	else
	    CDR(list) = RecursiveRelease(object, CDR(list));
    }
    return list;
}

void R_ReleaseObject(SEXP object)
{
    R_PreciousList =  RecursiveRelease(object, R_PreciousList);
}


/* External Pointer Objects */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    SEXP s = allocSExp(EXTPTRSXP);
    EXTPTR_PTR(s) = p;
    EXTPTR_PROT(s) = CHK(prot);
    EXTPTR_TAG(s) = CHK(tag);
    return s;
}

void *R_ExternalPtrAddr(SEXP s)
{
    return EXTPTR_PTR(CHK(s));
}

SEXP R_ExternalPtrTag(SEXP s)
{
    return CHK(EXTPTR_TAG(CHK(s)));
}

SEXP R_ExternalPtrProtected(SEXP s)
{
    return CHK(EXTPTR_PROT(CHK(s)));
}

void R_ClearExternalPtr(SEXP s)
{
    EXTPTR_PTR(s) = NULL;
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    EXTPTR_PTR(s) = p;
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    FIX_REFCNT(s, EXTPTR_TAG(s), tag);
    EXTPTR_TAG(s) = tag;
    WRITE_BARRIER(s, tag);
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    FIX_REFCNT(s, EXTPTR_PROT(s), p);
    EXTPTR_PROT(s) = p;
    WRITE_BARRIER(s, p);
}

/*
   Added to API in R 3.4.0.
   Work around casting issues: works where it is needed.
 */
typedef union {void *p; DL_FUNC fn;} fn_ptr;

SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot)
{
    fn_ptr tmp;
    SEXP s = allocSExp(EXTPTRSXP);
    tmp.fn = p;
    EXTPTR_PTR(s) = tmp.p;
    EXTPTR_PROT(s) = CHK(prot);
    EXTPTR_TAG(s) = CHK(tag);
    return s;
}

DL_FUNC R_ExternalPtrAddrFn(SEXP s)
{
    fn_ptr tmp;
    tmp.p =  EXTPTR_PTR(CHK(s));
    return tmp.fn;
}

SEXP attribute_hidden do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    // TODO
    return ScalarInteger(0);
}

SEXP attribute_hidden do_gctorture2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    // TODO
    return ScalarInteger(0);
}



/* The following functions are replacements for the accessor macros.
   They are used by code that does not have direct access to the
   internal representation of objects.  The replacement functions
   implement the write barrier. */

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x) { return CHK(ATTRIB(CHK(x))); }
int (OBJECT)(SEXP x) { return OBJECT(CHK(x)); }
int (TYPEOF)(SEXP x) { return TYPEOF(CHK(x)); }
int (NAMED)(SEXP x) { return NAMED(CHK(x)); }
int (RTRACE)(SEXP x) { return RTRACE(CHK(x)); }
int (LEVELS)(SEXP x) { return LEVELS(CHK(x)); }
int (REFCNT)(SEXP x) { return REFCNT(x); }

void (SET_ATTRIB)(SEXP x, SEXP v) {
    if(TYPEOF(v) != LISTSXP && TYPEOF(v) != NILSXP)
	error("value of 'SET_ATTRIB' must be a pairlist or NULL, not a '%s'",
	      type2char(TYPEOF(x)));
    FIX_REFCNT(x, ATTRIB(x), v);
    ATTRIB(x) = v;
    ATTRIB_WRITE_BARRIER(x, v);
}
void (SET_OBJECT)(SEXP x, int v) { SET_OBJECT(CHK(x), v); }
void (SET_TYPEOF)(SEXP x, int v) { SET_TYPEOF(CHK(x), v); }
void (SET_NAMED)(SEXP x, int v) { SET_NAMED(CHK(x), v); }
void (SET_RTRACE)(SEXP x, int v) { SET_RTRACE(CHK(x), v); }
int (SETLEVELS)(SEXP x, int v) { return SETLEVELS(CHK(x), v); }
void DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    IS_S4_OBJECT(from) ?  SET_S4_OBJECT(to) : UNSET_S4_OBJECT(to);
}
void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from) {
    SET_ATTRIB(CHK(to), shallow_duplicate(CHK(ATTRIB(CHK(from)))));
    SET_OBJECT(CHK(to), OBJECT(from));
    IS_S4_OBJECT(from) ?  SET_S4_OBJECT(to) : UNSET_S4_OBJECT(to);
}

/* S4 object testing */
int (IS_S4_OBJECT)(SEXP x){ return IS_S4_OBJECT(CHK(x)); }
void (SET_S4_OBJECT)(SEXP x){ SET_S4_OBJECT(CHK(x)); }
void (UNSET_S4_OBJECT)(SEXP x){ UNSET_S4_OBJECT(CHK(x)); }

/* JIT optimization support */
int (NOJIT)(SEXP x) { return NOJIT(CHK(x)); }
int (MAYBEJIT)(SEXP x) { return MAYBEJIT(CHK(x)); }
void (SET_NOJIT)(SEXP x) { SET_NOJIT(CHK(x)); }
void (SET_MAYBEJIT)(SEXP x) { SET_MAYBEJIT(CHK(x)); }
void (UNSET_MAYBEJIT)(SEXP x) { UNSET_MAYBEJIT(CHK(x)); }

/* Growable vector support */
int (IS_GROWABLE)(SEXP x) { return IS_GROWABLE(CHK(x)); }
void (SET_GROWABLE_BIT)(SEXP x) { SET_GROWABLE_BIT(CHK(x)); }

static int nvec[32] = {
    0,1,1,1,1,1,1,1,  // does NILSXP really count?
    1,0,0,1,1,0,0,0,
    0,1,1,0,0,1,1,0,
    0,1,1,1,1,1,1,1
};

static R_INLINE SEXP CHK2(SEXP x)
{
    x = CHK(x);
    if(nvec[TYPEOF(x)])
	error("LENGTH or similar applied to %s object", type2char(TYPEOF(x)));
    return x;
}

/* Vector Accessors */
int (LENGTH)(SEXP x) { return LENGTH(CHK2(x)); }
int (TRUELENGTH)(SEXP x) { return TRUELENGTH(CHK2(x)); }
void (SETLENGTH)(SEXP x, int v) { SETLENGTH(CHK2(x), v); }
void (SET_TRUELENGTH)(SEXP x, int v) { SET_TRUELENGTH(CHK2(x), v); }
R_xlen_t (XLENGTH)(SEXP x) { return XLENGTH(CHK2(x)); }
R_xlen_t (XTRUELENGTH)(SEXP x) { return XTRUELENGTH(CHK2(x)); }
int  (IS_LONG_VEC)(SEXP x) { return IS_LONG_VEC(CHK2(x)); }

const char *(R_CHAR)(SEXP x) {
    if(TYPEOF(x) != CHARSXP) // Han-Tak proposes to prepend  'x && '
	error("%s() can only be applied to a '%s', not a '%s'",
	      "CHAR", "CHARSXP", type2char(TYPEOF(x)));
    return (const char *)CHAR(x);
}

SEXP (STRING_ELT)(SEXP x, R_xlen_t i) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "STRING_ELT", "character vector", type2char(TYPEOF(x)));
    return CHK(STRING_ELT(x, i));
}

SEXP (VECTOR_ELT)(SEXP x, R_xlen_t i) {
    /* We need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "VECTOR_ELT", "list", type2char(TYPEOF(x)));
    return CHK(VECTOR_ELT(x, i));
}

int *(LOGICAL)(SEXP x) {
    if(TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "LOGICAL",  "logical", type2char(TYPEOF(x)));
  return LOGICAL(x);
}

/* Maybe this should exclude logicals, but it is widely used */
int *(INTEGER)(SEXP x) {
    if(TYPEOF(x) != INTSXP && TYPEOF(x) != LGLSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "INTEGER", "integer", type2char(TYPEOF(x)));
    return INTEGER(x);
}

Rbyte *(RAW)(SEXP x) {
    if(TYPEOF(x) != RAWSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "RAW", "raw", type2char(TYPEOF(x)));
    return RAW(x);
}

double *(REAL)(SEXP x) {
    if(TYPEOF(x) != REALSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "REAL", "numeric", type2char(TYPEOF(x)));
    return REAL(x);
}

Rcomplex *(COMPLEX)(SEXP x) {
    if(TYPEOF(x) != CPLXSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "COMPLEX", "complex", type2char(TYPEOF(x)));
    return COMPLEX(x);
}

SEXP *(STRING_PTR)(SEXP x) { return STRING_PTR(CHK(x)); }

SEXP * NORET (VECTOR_PTR)(SEXP x)
{
  error(_("not safe to return vector pointer"));
}

void (SET_STRING_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    if(TYPEOF(x) != STRSXP)
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_STRING_ELT", "character vector", type2char(TYPEOF(x)));
    if(TYPEOF(v) != CHARSXP)
       error("Value of SET_STRING_ELT() must be a 'CHARSXP' not a '%s'",
	     type2char(TYPEOF(v)));
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lu/%lu in SET_STRING_ELT"),
	      i, XLENGTH(x));
    FIX_REFCNT(x, STRING_ELT(x, i), v);
    STRING_ELT(x, i) = v;
    WRITE_BARRIER(x, v);
}

SEXP (SET_VECTOR_ELT)(SEXP x, R_xlen_t i, SEXP v) {
    /*  we need to allow vector-like types here */
    if(TYPEOF(x) != VECSXP &&
       TYPEOF(x) != EXPRSXP &&
       TYPEOF(x) != WEAKREFSXP) {
	error("%s() can only be applied to a '%s', not a '%s'",
	      "SET_VECTOR_ELT", "list", type2char(TYPEOF(x)));
    }
    if (i < 0 || i >= XLENGTH(x))
	error(_("attempt to set index %lu/%lu in SET_VECTOR_ELT"),
	      i, XLENGTH(x));
    FIX_REFCNT(x, VECTOR_ELT(x, i), v);
    SEXP res = VECTOR_ELT(x, i) = v;
    WRITE_BARRIER(x, v);
    return res;
}


/* List Accessors */
SEXP (TAG)(SEXP e) { return CHK(TAG(CHK(e))); }
SEXP (CAR)(SEXP e) { return CHK(CAR(CHK(e))); }
SEXP (CDR)(SEXP e) { return CHK(CDR(CHK(e))); }
SEXP (CAAR)(SEXP e) { return CHK(CAAR(CHK(e))); }
SEXP (CDAR)(SEXP e) { return CHK(CDAR(CHK(e))); }
SEXP (CADR)(SEXP e) { return CHK(CADR(CHK(e))); }
SEXP (CDDR)(SEXP e) { return CHK(CDDR(CHK(e))); }
SEXP (CDDDR)(SEXP e) { return CHK(CDDDR(CHK(e))); }
SEXP (CADDR)(SEXP e) { return CHK(CADDR(CHK(e))); }
SEXP (CADDDR)(SEXP e) { return CHK(CADDDR(CHK(e))); }
SEXP (CAD4R)(SEXP e) { return CHK(CAD4R(CHK(e))); }
int (MISSING)(SEXP x) { return MISSING(CHK(x)); }

void (SET_TAG)(SEXP x, SEXP v) { FIX_REFCNT(x, TAG(x), v); TAG(x) = v; WRITE_BARRIER(x, v); }

SEXP (SETCAR)(SEXP x, SEXP y)
{
    if (x == NULL || x == R_NilValue)
	error(_("bad value"));
    FIX_REFCNT(x, CAR(x), y);
    CAR(x) = y;
    WRITE_BARRIER(x, y);
    return y;
}

SEXP (SETCDR)(SEXP x, SEXP y)
{
    if (x == NULL || x == R_NilValue)
	error(_("bad value"));
    FIX_REFCNT(x, CDR(x), y);
    CDR(x) = y;
    WRITE_BARRIER(x, y);
    return y;
}

SEXP (SETCADR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (x == NULL || x == R_NilValue ||
	CDR(x) == NULL || CDR(x) == R_NilValue)
	error(_("bad value"));
    cell = CDR(x);
    FIX_REFCNT(cell, CAR(cell), y);
    CAR(cell) = y;
    WRITE_BARRIER(cell, y);
    return y;
}

SEXP (SETCADDR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (x == NULL || x == R_NilValue ||
	CDR(x) == NULL || CDR(x) == R_NilValue ||
	CDDR(x) == NULL || CDDR(x) == R_NilValue)
	error(_("bad value"));
    cell = CDDR(x);
    FIX_REFCNT(cell, CAR(cell), y);
    CAR(cell) = y;
    WRITE_BARRIER(cell, y);
    return y;
}

SEXP (SETCADDDR)(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHK(x) == NULL || x == R_NilValue ||
	CHK(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHK(CDDR(x)) == NULL || CDDR(x) == R_NilValue ||
	CHK(CDDDR(x)) == NULL || CDDDR(x) == R_NilValue)
	error(_("bad value"));
    cell = CDDDR(x);
    FIX_REFCNT(cell, CAR(cell), y);
    CAR(cell) = y;
    WRITE_BARRIER(cell, y);
    return y;
}

#define CD4R(x) CDR(CDR(CDR(CDR(x))))

SEXP (SETCAD4R)(SEXP x, SEXP y)
{
    SEXP cell;
    if (CHK(x) == NULL || x == R_NilValue ||
	CHK(CDR(x)) == NULL || CDR(x) == R_NilValue ||
	CHK(CDDR(x)) == NULL || CDDR(x) == R_NilValue ||
	CHK(CDDDR(x)) == NULL || CDDDR(x) == R_NilValue ||
	CHK(CD4R(x)) == NULL || CD4R(x) == R_NilValue)
	error(_("bad value"));
    cell = CD4R(x);
    FIX_REFCNT(cell, CAR(cell), y);
    CAR(cell) = y;
    WRITE_BARRIER(cell, y);
    return y;
}

void (SET_MISSING)(SEXP x, int v) { SET_MISSING(CHK(x), v); }

/* Closure Accessors */
SEXP (FORMALS)(SEXP x) { return CHK(FORMALS(CHK(x))); }
SEXP (BODY)(SEXP x) { return CHK(BODY(CHK(x))); }
SEXP (CLOENV)(SEXP x) { return CHK(CLOENV(CHK(x))); }
int (RDEBUG)(SEXP x) { return RDEBUG(CHK(x)); }
int (RSTEP)(SEXP x) { return RSTEP(CHK(x)); }

void (SET_FORMALS)(SEXP x, SEXP v) { FIX_REFCNT(x, FORMALS(x), v); FORMALS(x) = v; WRITE_BARRIER(x, v); }
void (SET_BODY)(SEXP x, SEXP v) { FIX_REFCNT(x, BODY(x), v); BODY(x) = v; WRITE_BARRIER(x, v); }
void (SET_CLOENV)(SEXP x, SEXP v) { FIX_REFCNT(x, CLOENV(x), v); CLOENV(x) = v; WRITE_BARRIER(x, v); }
void (SET_RDEBUG)(SEXP x, int v) { SET_RDEBUG(CHK(x), v); }
void (SET_RSTEP)(SEXP x, int v) { SET_RSTEP(CHK(x), v); }

/* These are only needed with the write barrier on */
#ifdef TESTING_WRITE_BARRIER
/* Primitive Accessors */
/* not hidden since needed in some base packages */
int (PRIMOFFSET)(SEXP x) { return PRIMOFFSET(x); }
attribute_hidden
void (SET_PRIMOFFSET)(SEXP x, int v) { SET_PRIMOFFSET(x, v); }
#endif

/* Symbol Accessors */
SEXP (PRINTNAME)(SEXP x) { return CHK(PRINTNAME(CHK(x))); }
SEXP (SYMVALUE)(SEXP x) { return CHK(SYMVALUE(CHK(x))); }
SEXP (INTERNAL)(SEXP x) { return CHK(INTERNAL(CHK(x))); }
int (DDVAL)(SEXP x) { return DDVAL(CHK(x)); }

void (SET_PRINTNAME)(SEXP x, SEXP v) { FIX_REFCNT(x, PRINTNAME(x), v); PRINTNAME(x) = v; WRITE_BARRIER(x, v); }
void (SET_SYMVALUE)(SEXP x, SEXP v) { FIX_REFCNT(x, SYMVALUE(x), v); SYMVALUE(x) = v; WRITE_BARRIER(x, v); }
void (SET_INTERNAL)(SEXP x, SEXP v) { FIX_REFCNT(x, INTERNAL(x), v); INTERNAL(x) = v; WRITE_BARRIER(x, v); }
void (SET_DDVAL)(SEXP x, int v) { SET_DDVAL(CHK(x), v); }

/* Environment Accessors */
SEXP (FRAME)(SEXP x) { return CHK(FRAME(CHK(x))); }
SEXP (ENCLOS)(SEXP x) { return CHK(ENCLOS(CHK(x))); }
SEXP (HASHTAB)(SEXP x) { return CHK(HASHTAB(CHK(x))); }
int (ENVFLAGS)(SEXP x) { return ENVFLAGS(CHK(x)); }

void (SET_FRAME)(SEXP x, SEXP v) { FIX_REFCNT(x, FRAME(x), v); FRAME(x) = v; WRITE_BARRIER(x, v); }
void (SET_ENCLOS)(SEXP x, SEXP v) { FIX_REFCNT(x, ENCLOS(x), v); ENCLOS(x) = v; WRITE_BARRIER(x, v); }
void (SET_HASHTAB)(SEXP x, SEXP v) { FIX_REFCNT(x, HASHTAB(x), v); HASHTAB(x) = v; WRITE_BARRIER(x, v); }
void (SET_ENVFLAGS)(SEXP x, int v) { SET_ENVFLAGS(x, v); }

/* Promise Accessors */
SEXP (PRCODE)(SEXP x) { return CHK(PRCODE(CHK(x))); }
SEXP (PRENV)(SEXP x) { return CHK(PRENV(CHK(x))); }
SEXP (PRVALUE)(SEXP x) { return CHK(PRVALUE(CHK(x))); }
int (PRSEEN)(SEXP x) { return PRSEEN(CHK(x)); }

void (SET_PRENV)(SEXP x, SEXP v){ FIX_REFCNT(x, PRENV(x), v); PRENV(x) = v; WRITE_BARRIER(x, v); }
void (SET_PRVALUE)(SEXP x, SEXP v) { FIX_REFCNT(x, PRVALUE(x), v); PRVALUE(x) = v; WRITE_BARRIER(x, v); }
void (SET_PRCODE)(SEXP x, SEXP v) { FIX_REFCNT(x, PRCODE(x), v); PRCODE(x) = v; WRITE_BARRIER(x, v); }
void (SET_PRSEEN)(SEXP x, int v) { SET_PRSEEN(CHK(x), v); }

/* Hashing Accessors */
#ifdef TESTING_WRITE_BARRIER
attribute_hidden
int (HASHASH)(SEXP x) { return HASHASH(CHK(x)); }
attribute_hidden
int (HASHVALUE)(SEXP x) { return HASHVALUE(CHK(x)); }

attribute_hidden
void (SET_HASHASH)(SEXP x, int v) { SET_HASHASH(CHK(x), v); }
attribute_hidden
void (SET_HASHVALUE)(SEXP x, int v) { SET_HASHVALUE(CHK(x), v); }
#endif

attribute_hidden
SEXP (SET_CXTAIL)(SEXP x, SEXP v) {
#ifdef USE_TYPE_CHECKING
    if(TYPEOF(v) != CHARSXP && TYPEOF(v) != NILSXP)
	error("value of 'SET_CXTAIL' must be a char or NULL, not a '%s'",
	      type2char(TYPEOF(v)));
#endif
    ATTRIB(x) = v;
    /*WRITE_BARRIER(x, v); *//* not needed since not properly traced */
    return x;
}

/* Test functions */
Rboolean Rf_isNull(SEXP s) { return isNull(s); }
Rboolean Rf_isSymbol(SEXP s) { return isSymbol(s); }
Rboolean Rf_isLogical(SEXP s) { return isLogical(s); }
Rboolean Rf_isReal(SEXP s) { return isReal(s); }
Rboolean Rf_isComplex(SEXP s) { return isComplex(s); }
Rboolean Rf_isExpression(SEXP s) { return isExpression(s); }
Rboolean Rf_isEnvironment(SEXP s) { return isEnvironment(s); }
Rboolean Rf_isString(SEXP s) { return isString(s); }
Rboolean Rf_isObject(SEXP s) { return isObject(s); }

/* Bindings accessors */
Rboolean attribute_hidden
(IS_ACTIVE_BINDING)(SEXP b) {return IS_ACTIVE_BINDING(b);}
Rboolean attribute_hidden
(BINDING_IS_LOCKED)(SEXP b) {return BINDING_IS_LOCKED(b);}
void attribute_hidden
(SET_ACTIVE_BINDING_BIT)(SEXP b) {SET_ACTIVE_BINDING_BIT(b);}
void attribute_hidden (LOCK_BINDING)(SEXP b) {LOCK_BINDING(b);}
void attribute_hidden (UNLOCK_BINDING)(SEXP b) {UNLOCK_BINDING(b);}

attribute_hidden
void (SET_BASE_SYM_CACHED)(SEXP b) { SET_BASE_SYM_CACHED(b); }
attribute_hidden
void (UNSET_BASE_SYM_CACHED)(SEXP b) { UNSET_BASE_SYM_CACHED(b); }
attribute_hidden
Rboolean (BASE_SYM_CACHED)(SEXP b) { return BASE_SYM_CACHED(b); }

attribute_hidden
void (SET_SPECIAL_SYMBOL)(SEXP b) { SET_SPECIAL_SYMBOL(b); }
attribute_hidden
void (UNSET_SPECIAL_SYMBOL)(SEXP b) { UNSET_SPECIAL_SYMBOL(b); }
attribute_hidden
Rboolean (IS_SPECIAL_SYMBOL)(SEXP b) { return IS_SPECIAL_SYMBOL(b); }
attribute_hidden
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b) { SET_NO_SPECIAL_SYMBOLS(b); }
attribute_hidden
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b) { UNSET_NO_SPECIAL_SYMBOLS(b); }
attribute_hidden
Rboolean (NO_SPECIAL_SYMBOLS)(SEXP b) { return NO_SPECIAL_SYMBOLS(b); }

/* R_FunTab accessors, only needed when write barrier is on */
/* Not hidden to allow experimentaiton without rebuilding R - LT */
/* attribute_hidden */
int (PRIMVAL)(SEXP x) { return PRIMVAL(x); }
/* attribute_hidden */
CCODE (PRIMFUN)(SEXP x) { return PRIMFUN(x); }
/* attribute_hidden */
void (SET_PRIMFUN)(SEXP x, CCODE f) { PRIMFUN(x) = f; }

/* for use when testing the write barrier */
int  attribute_hidden (IS_BYTES)(SEXP x) { return IS_BYTES(x); }
int  attribute_hidden (IS_LATIN1)(SEXP x) { return IS_LATIN1(x); }
int  attribute_hidden (IS_ASCII)(SEXP x) { return IS_ASCII(x); }
int  attribute_hidden (IS_UTF8)(SEXP x) { return IS_UTF8(x); }
void attribute_hidden (SET_BYTES)(SEXP x) { SET_BYTES(x); }
void attribute_hidden (SET_LATIN1)(SEXP x) { SET_LATIN1(x); }
void attribute_hidden (SET_UTF8)(SEXP x) { SET_UTF8(x); }
void attribute_hidden (SET_ASCII)(SEXP x) { SET_ASCII(x); }
int  (ENC_KNOWN)(SEXP x) { return ENC_KNOWN(x); }
void attribute_hidden (SET_CACHED)(SEXP x) { SET_CACHED(x); }
int  (IS_CACHED)(SEXP x) { return IS_CACHED(x); }

/*******************************************/
/* Non-sampling memory use profiler
   reports all large vector heap
   allocations and all calls to GetNewPage */
/*******************************************/

#ifndef R_MEMORY_PROFILING

SEXP NORET do_Rprofmem(SEXP args)
{
    error(_("memory profiling is not available on this system"));
}

#else
static int R_IsMemReporting;  /* Rboolean more appropriate? */
static FILE *R_MemReportingOutfile;
static R_size_t R_MemReportingThreshold;

static void R_OutputStackTrace(FILE *file)
{
    RCNTXT *cptr;

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    fprintf(file, "\"%s\" ",
		    TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
}

static void R_ReportAllocation(R_size_t size)
{
    if (R_IsMemReporting) {
	if(size > R_MemReportingThreshold) {
	    fprintf(R_MemReportingOutfile, "%lu :", (unsigned long) size);
	    R_OutputStackTrace(R_MemReportingOutfile);
	    fprintf(R_MemReportingOutfile, "\n");
	}
    }
    return;
}

static void R_ReportNewPage(void)
{
    if (R_IsMemReporting) {
	fprintf(R_MemReportingOutfile, "new page:");
	R_OutputStackTrace(R_MemReportingOutfile);
	fprintf(R_MemReportingOutfile, "\n");
    }
    return;
}

static void R_EndMemReporting()
{
    if(R_MemReportingOutfile != NULL) {
	/* does not fclose always flush? */
	fflush(R_MemReportingOutfile);
	fclose(R_MemReportingOutfile);
	R_MemReportingOutfile=NULL;
    }
    R_IsMemReporting = 0;
    return;
}

static void R_InitMemReporting(SEXP filename, int append,
			       R_size_t threshold)
{
    if(R_MemReportingOutfile != NULL) R_EndMemReporting();
    R_MemReportingOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_MemReportingOutfile == NULL)
	error(_("Rprofmem: cannot open output file '%s'"), filename);
    R_MemReportingThreshold = threshold;
    R_IsMemReporting = 1;
    return;
}

SEXP do_Rprofmem(SEXP args)
{
    SEXP filename;
    R_size_t threshold;
    int append_mode;

    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	error(_("invalid '%s' argument"), "filename");
    append_mode = asLogical(CADR(args));
    filename = STRING_ELT(CAR(args), 0);
    threshold = (R_size_t) REAL(CADDR(args))[0];
    if (strlen(CHAR(filename)))
	R_InitMemReporting(filename, append_mode, threshold);
    else
	R_EndMemReporting();
    return R_NilValue;
}

#endif /* R_MEMORY_PROFILING */

/* RBufferUtils, moved from deparse.c */

#include "RBufferUtils.h"

void *R_AllocStringBuffer(size_t blen, R_StringBuffer *buf)
{
    size_t blen1, bsize = buf->defaultSize;

    /* for backwards compatibility, this used to free the buffer */
    if(blen == (size_t)-1)
	error("R_AllocStringBuffer( (size_t)-1 ) is no longer allowed");

    if(blen * sizeof(char) < buf->bufsize) return buf->data;
    blen1 = blen = (blen + 1) * sizeof(char);
    blen = (blen / bsize) * bsize;
    if(blen < blen1) blen += bsize;

    if(buf->data == NULL) {
	buf->data = (char *) malloc(blen);
	if(buf->data)
	    buf->data[0] = '\0';
    } else
	buf->data = (char *) realloc(buf->data, blen);
    buf->bufsize = blen;
    if(!buf->data) {
	buf->bufsize = 0;
	/* don't translate internal error message */
	error("could not allocate memory (%u Mb) in C function 'R_AllocStringBuffer'",
	      (unsigned int) blen/1024/1024);
    }
    return buf->data;
}

void
R_FreeStringBuffer(R_StringBuffer *buf)
{
    if (buf->data != NULL) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

void attribute_hidden
R_FreeStringBufferL(R_StringBuffer *buf)
{
    if (buf->bufsize > buf->defaultSize) {
	free(buf->data);
	buf->bufsize = 0;
	buf->data = NULL;
    }
}

/* ======== This needs direct access to gp field for efficiency ======== */

/* this has NA_STRING = NA_STRING */
attribute_hidden
int Seql(SEXP a, SEXP b)
{
    /* The only case where pointer comparisons do not suffice is where
      we have two strings in different encodings (which must be
      non-ASCII strings). Note that one of the strings could be marked
      as unknown. */
    if (a == b) return 1;
    /* Leave this to compiler to optimize */
    if (IS_CACHED(a) && IS_CACHED(b) && ENC_KNOWN(a) == ENC_KNOWN(b))
	return 0;
    else {
	SEXP vmax = R_VStack;
	int result = !strcmp(translateCharUTF8(a), translateCharUTF8(b));
	R_VStack = vmax; /* discard any memory used by translateCharUTF8 */
	return result;
    }
}


#ifdef LONG_VECTOR_SUPPORT
R_len_t NORET R_BadLongVector(SEXP x, const char *file, int line)
{
    error(_("long vectors not supported yet: %s:%d"), file, line);
}
#endif
