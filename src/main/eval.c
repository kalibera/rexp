/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2013	The R Core Team.
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
 */


#undef HASHING

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <R_ext/Print.h>


#define ARGUSED(x) LEVELS(x)

static SEXP bcEval(SEXP, SEXP, Rboolean);
static int findOp(void *addr);

/* BC_PROILFING needs to be defined here and in registration.c */
/*#define BC_PROFILING*/
#ifdef BC_PROFILING
static Rboolean bc_profiling = FALSE;
#endif

static int R_Profiling = 0;

#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.c with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled. [BDR: we have since
   also added contexts for the BUILTIN calls to foreign code.]

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>		/* for CreateEvent, SetEvent */
# include <process.h>		/* for _beginthread, _endthread */
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
# include <signal.h>
#endif /* not Win32 */

static FILE *R_ProfileOutfile = NULL;
static int R_Mem_Profiling=0;
extern void get_current_mem(size_t *,size_t *,size_t *); /* in memory.c */
extern unsigned long get_duplicate_counter(void);  /* in duplicate.c */
extern void reset_duplicate_counter(void);         /* in duplicate.c */
static int R_GC_Profiling = 0;                     /* indicates GC profiling */
static int R_Line_Profiling = 0;                   /* indicates line profiling, and also counts the filenames seen (+1) */
static char **R_Srcfiles;			   /* an array of pointers into the filename buffer */
static size_t R_Srcfile_bufcount;                  /* how big is the array above? */
static SEXP R_Srcfiles_buffer = NULL;              /* a big RAWSXP to use as a buffer for filenames and pointers to them */
static int R_Profiling_Error;		   /* record errors here */

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;
#endif /* Win32 */

/* Careful here!  These functions are called asynchronously, maybe in the middle of GC,
   so don't do any allocations */

/* This does a linear search through the previously recorded filenames.  If
   this one is new, we try to add it.  FIXME:  if there are eventually
   too many files for an efficient linear search, do hashing. */

static int getFilenum(const char* filename) {
    int fnum;

    for (fnum = 0; fnum < R_Line_Profiling-1
		   && strcmp(filename, R_Srcfiles[fnum]); fnum++);

    if (fnum == R_Line_Profiling-1) {
	size_t len = strlen(filename);
	if (fnum >= R_Srcfile_bufcount) { /* too many files */
	    R_Profiling_Error = 1;
	    return 0;
	}
	if (R_Srcfiles[fnum] - (char*)RAW(R_Srcfiles_buffer) + len + 1 > length(R_Srcfiles_buffer)) {
	      /* out of space in the buffer */
	    R_Profiling_Error = 2;
	    return 0;
	}
	strcpy(R_Srcfiles[fnum], filename);
	R_Srcfiles[fnum+1] = R_Srcfiles[fnum] + len + 1;
	*(R_Srcfiles[fnum+1]) = '\0';
	R_Line_Profiling++;
    }

    return fnum + 1;
}

/* These, together with sprintf/strcat, are not safe -- we should be
   using snprintf and such and computing needed sizes, but these
   settings are better than what we had. LT */

#define PROFBUFSIZ 10500
#define PROFITEMMAX  500
#define PROFLINEMAX (PROFBUFSIZ - PROFITEMMAX)

/* It would also be better to flush the buffer when it gets full,
   even if the line isn't complete. But this isn't possible if we rely
   on writing all line profiling files first.  With these sizes
   hitting the limit is fairly unlikely, but if we do then the output
   file is wrong. Maybe writing an overflow marker of some sort would
   be better.  LT */

static void lineprof(char* buf, SEXP srcref)
{
    size_t len;
    if (srcref && !isNull(srcref) && (len = strlen(buf)) < PROFLINEMAX) {
	int fnum, line = asInteger(srcref);
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	const char *filename;

	if (!srcfile || TYPEOF(srcfile) != ENVSXP) return;
	srcfile = findVar(install("filename"), srcfile);
	if (TYPEOF(srcfile) != STRSXP || !length(srcfile)) return;
	filename = CHAR(STRING_ELT(srcfile, 0));

	if ((fnum = getFilenum(filename)))
	    snprintf(buf+len, PROFBUFSIZ - len, "%d#%d ", fnum, line);
    }
}

/* FIXME: This should be done wih a proper configure test, also making
   sure that the pthreads library is linked in. LT */
#ifndef Win32
#if (defined(__APPLE__) || defined(_REENTRANT) || defined(HAVE_OPENMP)) && \
     ! defined(HAVE_PTHREAD)
# define HAVE_PTHREAD
#endif
#ifdef HAVE_PTHREAD
# include <pthread.h>
static pthread_t R_profiled_thread;
# endif
#endif

static void doprof(int sig)  /* sig is ignored in Windows */
{
    RCNTXT *cptr;
    char buf[PROFBUFSIZ];
    size_t bigv, smallv, nodes;
    size_t len;
    int prevnum = R_Line_Profiling;

    buf[0] = '\0';

#ifdef Win32
    SuspendThread(MainThread);
#elif defined(HAVE_PTHREAD)
    if (! pthread_equal(pthread_self(), R_profiled_thread)) {
	pthread_kill(R_profiled_thread, sig);
	return;
    }
#endif /* Win32 */

    if (R_Mem_Profiling){
	    get_current_mem(&smallv, &bigv, &nodes);
	    if((len = strlen(buf)) < PROFLINEMAX)
		snprintf(buf+len, PROFBUFSIZ - len,
			 ":%lu:%lu:%lu:%lu:", 
			 (unsigned long) smallv, (unsigned long) bigv,
			 (unsigned long) nodes, get_duplicate_counter());
	    reset_duplicate_counter();
    }

    if (R_GC_Profiling && R_gc_running())
	strcat(buf, "\"<GC>\" ");

    if (R_Line_Profiling)
	lineprof(buf, R_Srcref);

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < PROFLINEMAX) {
		strcat(buf, "\"");
		strcat(buf, TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
			"<Anonymous>");
		strcat(buf, "\" ");
		if (R_Line_Profiling)
		    lineprof(buf, cptr->srcref);
	    }
	}
    }

    /* I believe it would be slightly safer to place this _after_ the
       next two bits, along with the signal() call. LT */
#ifdef Win32
    ResumeThread(MainThread);
#endif /* Win32 */

    for (int i = prevnum; i < R_Line_Profiling; i++)
	fprintf(R_ProfileOutfile, "#File %d: %s\n", i, R_Srcfiles[i-1]);

    if(*buf)
	fprintf(R_ProfileOutfile, "%s\n", buf);

#ifndef Win32
    signal(SIGPROF, doprof);
#endif /* not Win32 */

}

#ifdef Win32
/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof(0);
    }
}
#else /* not Win32 */
static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif /* not Win32 */


static void R_EndProfiling(void)
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
#else /* not Win32 */
    struct itimerval itv;

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, doprof_null);

#endif /* not Win32 */
    if(R_ProfileOutfile) fclose(R_ProfileOutfile);
    R_ProfileOutfile = NULL;
    R_Profiling = 0;
    if (R_Srcfiles_buffer) {
	R_ReleaseObject(R_Srcfiles_buffer);
	R_Srcfiles_buffer = NULL;
    }
    if (R_Profiling_Error)
	warning(_("source files skipped by Rprof; please increase '%s'"),
		R_Profiling_Error == 1 ? "numfiles" : "bufsize");
}

static void R_InitProfiling(SEXP filename, int append, double dinterval,
			    int mem_profiling, int gc_profiling,
			    int line_profiling, int numfiles, int bufsize)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval;

    interval = (int)(1e6 * dinterval + 0.5);
    if(R_ProfileOutfile != NULL) R_EndProfiling();
    R_ProfileOutfile = RC_fopen(filename, append ? "a" : "w", TRUE);
    if (R_ProfileOutfile == NULL)
	error(_("Rprof: cannot open profile file '%s'"),
	      translateChar(filename));
    if(mem_profiling)
	fprintf(R_ProfileOutfile, "memory profiling: ");
    if(gc_profiling)
	fprintf(R_ProfileOutfile, "GC profiling: ");
    if(line_profiling)
	fprintf(R_ProfileOutfile, "line profiling: ");
    fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

    R_Mem_Profiling=mem_profiling;
    if (mem_profiling)
	reset_duplicate_counter();

    R_Profiling_Error = 0;
    R_Line_Profiling = line_profiling;
    R_GC_Profiling = gc_profiling;
    if (line_profiling) {
	/* Allocate a big RAW vector to use as a buffer.  The first len1 bytes are an array of pointers
	   to strings; the actual strings are stored in the second len2 bytes. */
	R_Srcfile_bufcount = numfiles;
	size_t len1 = R_Srcfile_bufcount*sizeof(char *), len2 = bufsize;
	R_PreserveObject( R_Srcfiles_buffer = Rf_allocVector(RAWSXP, len1 + len2) );
 //	memset(RAW(R_Srcfiles_buffer), 0, len1+len2);
	R_Srcfiles = (char **) RAW(R_Srcfiles_buffer);
	R_Srcfiles[0] = (char *)RAW(R_Srcfiles_buffer) + len1;
	*(R_Srcfiles[0]) = '\0';
    }

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if(!(ProfileEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
       (_beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide("unable to create profiling thread");
    Sleep(wait/2); /* suspend this thread to ensure that the other one starts */
#else /* not Win32 */
#ifdef HAVE_PTHREAD
    R_profiled_thread = pthread_self();
#endif

    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	R_Suicide("setting profile timer failed");
#endif /* not Win32 */
    R_Profiling = 1;
}

SEXP do_Rprof(SEXP args)
{
    SEXP filename;
    int append_mode, mem_profiling, gc_profiling, line_profiling;
    double dinterval;
    int numfiles, bufsize;

#ifdef BC_PROFILING
    if (bc_profiling) {
	warning("cannot use R profiling while byte code profiling");
	return R_NilValue;
    }
#endif
    if (!isString(filename = CAR(args)) || (LENGTH(filename)) != 1)
	error(_("invalid '%s' argument"), "filename");
					      args = CDR(args);
    append_mode = asLogical(CAR(args));       args = CDR(args);
    dinterval = asReal(CAR(args));            args = CDR(args);
    mem_profiling = asLogical(CAR(args));     args = CDR(args);
    gc_profiling = asLogical(CAR(args));      args = CDR(args);
    line_profiling = asLogical(CAR(args));    args = CDR(args);
    numfiles = asInteger(CAR(args));	      args = CDR(args);
    if (numfiles < 0)
	error(_("invalid '%s' argument"), "numfiles");
    bufsize = asInteger(CAR(args));
    if (bufsize < 0)
	error(_("invalid '%s' argument"), "bufsize");

    filename = STRING_ELT(filename, 0);
    if (LENGTH(filename))
	R_InitProfiling(filename, append_mode, dinterval, mem_profiling,
			gc_profiling, line_profiling, numfiles, bufsize);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else /* not R_PROFILING */
SEXP do_Rprof(SEXP args)
{
    error(_("R profiling is not available on this system"));
    return R_NilValue;		/* -Wall */
}
#endif /* not R_PROFILING */

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */

void attribute_hidden check_stack_balance(SEXP op, int save)
{
    if(save == R_PPStackTop) return;
    REprintf("Warning: stack imbalance in '%s', %d then %d\n",
	     PRIMNAME(op), save, R_PPStackTop);
}


static SEXP forcePromise(SEXP e)
{
    if (PRVALUE(e) == R_UnboundValue) {
	RPRSTACK prstack;
	SEXP val;
	if(PRSEEN(e)) {
	    if (PRSEEN(e) == 1)
		errorcall(R_GlobalContext->call,
			  _("promise already under evaluation: recursive default argument reference or earlier problems?"));
	    else warningcall(R_GlobalContext->call,
			     _("restarting interrupted promise evaluation"));
	}
	/* Mark the promise as under evaluation and push it on a stack
	   that can be used to unmark pending promises if a jump out
	   of the evaluation occurs. */
	SET_PRSEEN(e, 1);
	prstack.promise = e;
	prstack.next = R_PendingPromises;
	R_PendingPromises = &prstack;

	val = eval(PRCODE(e), PRENV(e));

	/* Pop the stack, unmark the promise and set its value field.
	   Also set the environment to R_NilValue to allow GC to
	   reclaim the promise environment; this is also useful for
	   fancy games with delayedAssign() */
	R_PendingPromises = prstack.next;
	SET_PRSEEN(e, 0);
	SET_PRVALUE(e, val);
        SET_NAMED (val, 2);
	SET_PRENV(e, R_NilValue);
    }
    return PRVALUE(e);
}

/* Return value of "e" evaluated in "rho". */

/* some places, e.g. deparse2buff, call this with a promise and rho = NULL */
SEXP eval(SEXP e, SEXP rho)
{
    SEXP op, tmp;
    static int evalcount = 0;

    R_Visible = TRUE;

    /* this is needed even for self-evaluating objects or something like
       'while (TRUE) NULL' will not be interruptable */
    if (++evalcount > 1000) { /* was 100 before 2.8.0 */
	R_CheckUserInterrupt();
#ifndef IMMEDIATE_FINALIZERS
	/* finalizers are run here since this should only be called at
	   points where running arbitrary code should be safe */
	R_RunPendingFinalizers();
#endif
	evalcount = 0 ;
    }

    /* handle self-evluating objects with minimal overhead */
    switch (TYPEOF(e)) {
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
    case S4SXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
    case EXPRSXP:
	/* Make sure constants in expressions are NAMED before being
	   used as values.  Setting NAMED to 2 makes sure weird calls
	   to replacement functions won't modify constants in
	   expressions.  */
	if (NAMED(e) <= 1) SET_NAMED(e, 2);
	return e;
    default: break;
    }

    if (!rho)
	error("'rho' cannot be C NULL: detected in C-level eval");
    if (!isEnvironment(rho))
	error("'rho' must be an environment not %s: detected in C-level eval",
	      type2char(TYPEOF(rho)));

    /* Save the current srcref context. */

    SEXP srcrefsave = R_Srcref;

    /* The use of depthsave below is necessary because of the
       possibility of non-local returns from evaluation.  Without this
       an "expression too complex error" is quite likely. */

    int depthsave = R_EvalDepth++;

    /* We need to explicit set a NULL call here to circumvent attempts
       to deparse the call in the error-handler */
    if (R_EvalDepth > R_Expressions) {
	R_Expressions = R_Expressions_keep + 500;
	errorcall(R_NilValue,
		  _("evaluation nested too deeply: infinite recursion / options(expressions=)?"));
    }
    R_CheckStack();

    tmp = R_NilValue;		/* -Wall */
#ifdef Win32
    /* This is an inlined version of Rwin_fpreset (src/gnuwin/extra.c)
       and resets the precision, rounding and exception modes of a ix86
       fpu.
     */
    __asm__ ( "fninit" );
#endif

    switch (TYPEOF(e)) {
    case BCODESXP:
	tmp = bcEval(e, rho, TRUE);
	    break;
    case SYMSXP:
	if (e == R_DotsSymbol)
	    error(_("'...' used in an incorrect context"));
	if( DDVAL(e) )
		tmp = ddfindVar(e,rho);
	else
		tmp = findVar(e, rho);
	if (tmp == R_UnboundValue)
	    error(_("object '%s' not found"), EncodeChar(PRINTNAME(e)));
	/* if ..d is missing then ddfindVar will signal */
	else if (tmp == R_MissingArg && !DDVAL(e) ) {
	    const char *n = CHAR(PRINTNAME(e));
	    if(*n) error(_("argument \"%s\" is missing, with no default"),
			 CHAR(PRINTNAME(e)));
	    else error(_("argument is missing, with no default"));
	}
	else if (TYPEOF(tmp) == PROMSXP) {
	    if (PRVALUE(tmp) == R_UnboundValue) {
		/* not sure the PROTECT is needed here but keep it to
		   be on the safe side. */
		PROTECT(tmp);
		tmp = forcePromise(tmp);
		UNPROTECT(1);
	    }
	    else tmp = PRVALUE(tmp);
	    SET_NAMED(tmp, 2);
	}
	else if (!isNull(tmp) && NAMED(tmp) == 0)
	    SET_NAMED(tmp, 1);
	break;
    case PROMSXP:
	if (PRVALUE(e) == R_UnboundValue)
	    /* We could just unconditionally use the return value from
	       forcePromise; the test avoids the function call if the
	       promise is already evaluated. */
	    forcePromise(e);
	tmp = PRVALUE(e);
	/* This does _not_ change the value of NAMED on the value tmp,
	   in contrast to the handling of promises bound to symbols in
	   the SYMSXP case above.  The reason is that one (typically
	   the only) place promises appear in source code is as
	   wrappers for the RHS value in replacement function calls for
	   complex assignment expression created in applydefine().  If
	   the RHS value is freshly created it will have NAMED = 0 and
	   we want it to stay that way or a BUILTIN or SPECIAL
	   replacement function might have to duplicate the value
	   before inserting it to avoid creating cycles.  (Closure
	   replacement functions will get the value via the SYMSXP case
	   from evaluating their 'value' argument so the value will
	   end up getting duplicated if NAMED = 2.) LT */
	break;
    case LANGSXP:
	if (TYPEOF(CAR(e)) == SYMSXP)
	    /* This will throw an error if the function is not found */
	    PROTECT(op = findFun(CAR(e), rho));
	else
	    PROTECT(op = eval(CAR(e), rho));

	if(RTRACE(op) && R_current_trace_state()) {
	    Rprintf("trace: ");
	    PrintValue(e);
	}
	if (TYPEOF(op) == SPECIALSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    PROTECT(CDR(e));
	    R_Visible = flag != 1;
	    tmp = PRIMFUN(op) (e, op, CDR(e), rho);
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		if(strcmp(nm, "for")
		   && strcmp(nm, "repeat") && strcmp(nm, "while")
		   && strcmp(nm, "[[<-") && strcmp(nm, "on.exit"))
		    printf("vis: special %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == BUILTINSXP) {
	    int save = R_PPStackTop, flag = PRIMPRINT(op);
	    const void *vmax = vmaxget();
	    RCNTXT cntxt;
	    PROTECT(tmp = evalList(CDR(e), rho, e, 0));
	    if (flag < 2) R_Visible = flag != 1;
	    /* We used to insert a context only if profiling,
	       but helps for tracebacks on .C etc. */
	    if (R_Profiling || (PPINFO(op).kind == PP_FOREIGN)) {
		SEXP oldref = R_Srcref;
		begincontext(&cntxt, CTXT_BUILTIN, e,
			     R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
		R_Srcref = NULL;
		tmp = PRIMFUN(op) (e, op, tmp, rho);
		R_Srcref = oldref;
		endcontext(&cntxt);
	    } else {
		tmp = PRIMFUN(op) (e, op, tmp, rho);
	    }
#ifdef CHECK_VISIBILITY
	    if(flag < 2 && R_Visible == flag) {
		char *nm = PRIMNAME(op);
		printf("vis: builtin %s\n", nm);
	    }
#endif
	    if (flag < 2) R_Visible = flag != 1;
	    UNPROTECT(1);
	    check_stack_balance(op, save);
	    vmaxset(vmax);
	}
	else if (TYPEOF(op) == CLOSXP) {
	    SEXP pargs;
	    PROTECT(pargs = PROMISE_ARGS(CDR(e), rho));
	    tmp = applyClosure(e, op, pargs, rho, R_BaseEnv);
	    UNPROTECT(1);
	    RELEASE_PROMARGS(pargs);
	}
	else
	    error(_("attempt to apply non-function"));
	UNPROTECT(1);
	break;
    case DOTSXP:
	error(_("'...' used in an incorrect context"));
    default:
	UNIMPLEMENTED_TYPE("eval", e);
    }
    R_EvalDepth = depthsave;
    R_Srcref = srcrefsave;
    return (tmp);
}

attribute_hidden
void SrcrefPrompt(const char * prefix, SEXP srcref)
{
    /* If we have a valid srcref, use it */
    if (srcref && srcref != R_NilValue) {
	if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
	SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
	if (TYPEOF(srcfile) == ENVSXP) {
	    SEXP filename = findVar(install("filename"), srcfile);
	    if (isString(filename) && length(filename)) {
		Rprintf(_("%s at %s#%d: "), prefix, CHAR(STRING_ELT(filename, 0)),
					    asInteger(srcref));
		return;
	    }
	}
    }
    /* default: */
    Rprintf("%s: ", prefix);
}

/* Apply SEXP op of type CLOSXP to actuals */

static void loadCompilerNamespace(void)
{
    SEXP fun, arg, expr;

    PROTECT(fun = install("getNamespace"));
    PROTECT(arg = mkString("compiler"));
    PROTECT(expr = lang2(fun, arg));
    eval(expr, R_GlobalEnv);
    UNPROTECT(3);
}

static int R_disable_bytecode = 0;

void attribute_hidden R_init_jit_enabled(void)
{
    if (R_jit_enabled <= 0) {
	char *enable = getenv("R_ENABLE_JIT");
	if (enable != NULL) {
	    int val = atoi(enable);
	    if (val > 0)
		loadCompilerNamespace();
	    R_jit_enabled = val;
	}
    }

    if (R_compile_pkgs <= 0) {
	char *compile = getenv("R_COMPILE_PKGS");
	if (compile != NULL) {
	    int val = atoi(compile);
	    if (val > 0)
		R_compile_pkgs = TRUE;
	    else
		R_compile_pkgs = FALSE;
	}
    }

    if (R_disable_bytecode <= 0) {
	char *disable = getenv("R_DISABLE_BYTECODE");
	if (disable != NULL) {
	    int val = atoi(disable);
	    if (val > 0)
		R_disable_bytecode = TRUE;
	    else
		R_disable_bytecode = FALSE;
	}
    }
}

SEXP attribute_hidden R_cmpfun(SEXP fun)
{
    SEXP packsym, funsym, call, fcall, val;

    packsym = install("compiler");
/*    funsym = install("tryCmpfun");  CTK - make the errors visible to simplify debugging of the compiler */
    funsym = install("cmpfun");

    PROTECT(fcall = lang3(R_TripleColonSymbol, packsym, funsym));
    PROTECT(call = lang2(fcall, fun));
    val = eval(call, R_GlobalEnv);
    UNPROTECT(2);
    return val;
}

static SEXP R_compileExpr(SEXP expr, SEXP rho)
{
    SEXP packsym, funsym, quotesym;
    SEXP qexpr, call, fcall, val;

    packsym = install("compiler");
    funsym = install("compile");
    quotesym = install("quote");

    PROTECT(fcall = lang3(R_DoubleColonSymbol, packsym, funsym));
    PROTECT(qexpr = lang2(quotesym, expr));
    PROTECT(call = lang3(fcall, qexpr, rho));
    val = eval(call, R_GlobalEnv);
    UNPROTECT(3);
    return val;
}

static SEXP R_compileAndExecute(SEXP call, SEXP rho)
{
    int old_enabled = R_jit_enabled;
    SEXP code, val;

    R_jit_enabled = 0;
    PROTECT(call);
    PROTECT(rho);
    PROTECT(code = R_compileExpr(call, rho));
    R_jit_enabled = old_enabled;

    val = bcEval(code, rho, TRUE);
    UNPROTECT(3);
    return val;
}

SEXP attribute_hidden do_enablejit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_jit_enabled, new;
    checkArity(op, args);
    new = asInteger(CAR(args));
    if (new > 0)
	loadCompilerNamespace();
    R_jit_enabled = new;
    return ScalarInteger(old);
}

SEXP attribute_hidden do_compilepkgs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_compile_pkgs, new;
    checkArity(op, args);
    new = asLogical(CAR(args));
    if (new != NA_LOGICAL && new)
	loadCompilerNamespace();
    R_compile_pkgs = new;
    return ScalarLogical(old);
}

/* forward declaration */
static SEXP bytecodeExpr(SEXP);

/* this function gets the srcref attribute from a statement block,
   and confirms it's in the expected format */

static R_INLINE SEXP getBlockSrcrefs(SEXP call)
{
    SEXP srcrefs = getAttrib(call, R_SrcrefSymbol);
    if (TYPEOF(srcrefs) == VECSXP) return srcrefs;
    return R_NilValue;
}

/* this function extracts one srcref, and confirms the format */
/* It assumes srcrefs has already been validated to be a VECSXP or NULL */

static R_INLINE SEXP getSrcref(SEXP srcrefs, int ind)
{
    SEXP result;
    if (!isNull(srcrefs)
	&& length(srcrefs) > ind
	&& !isNull(result = VECTOR_ELT(srcrefs, ind))
	&& TYPEOF(result) == INTSXP
	&& length(result) >= 6)
	return result;
    else
	return R_NilValue;
}

SEXP applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedenv)
{
    SEXP formals, actuals, savedrho;
    volatile SEXP body, newrho;
    SEXP f, a, tmp;
    RCNTXT cntxt;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    /* protection against rho = NULL */
    // these are deliberately not translated
    if (!rho)
	errorcall(call,
		  "'rho' cannot be C NULL: detected in C-level applyClosure");
    if (!isEnvironment(rho))
	errorcall(call, "'rho' must be an environment not %s: detected in C-level applyClosure",
		  type2char(TYPEOF(rho)));

    formals = FORMALS(op);
    body = BODY(op);
    savedrho = CLOENV(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    /*  Set up a context with the call in it so error has access to it */

    begincontext(&cntxt, CTXT_RETURN, call, savedrho, rho, arglist, op);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  Ideally this environment sould be
	hashed.  */

	    /* calculate the length of arglist and check if any names are specified at all */
    int nargs = 0;
    int seenNames = 0;
    for(a = arglist; a != R_NilValue; a = CDR(a), nargs++) {
        if (!seenNames && TAG(a) != R_NilValue) {
            seenNames = 1;
        }
    }
    
    if (!seenNames) {
    
        /* we need to copy the arguments because the original is saved as promargs into context and 
           is needed for object dispatch, recall, browser, etc */
        SEXP arglistCopy = allocList(nargs);
        SEXP s,t;
        for (t = arglistCopy, s = arglist; t != R_NilValue; s = CDR(s), t = CDR(t)) {
            SETCAR(t, CAR(s)); 
            SET_TAG(t, TAG(s));
        }
        
        newrho = matchUnnamedArgsCreateEnv(formals, arglistCopy, call, savedrho, &actuals);
        PROTECT(newrho);
        PROTECT(actuals);

    } else {
        PROTECT(actuals = matchArgs(formals, arglist, call));
        PROTECT(newrho = NewEnvironment(formals, actuals, savedrho));
  
        /* Turn on reference counting for the binding cells so local
           assignments arguments increment REFCNT values */
        for (a = actuals; a != R_NilValue; a = CDR(a))
  	  ENABLE_REFCNT(a);

        /*  Use the default code for unbound formals.  FIXME: It looks like
	    this code should preceed the building of the environment so that
	    this will also go into the hash table.  */

        /* This piece of code is destructively modifying the actuals list,
           which is now also the list of bindings in the frame of newrho.
           This is one place where internal structure of environment
           bindings leaks out of envir.c.  It should be rewritten
           eventually so as not to break encapsulation of the internal
           environment layout.  We can live with it for now since it only
           happens immediately after the environment creation.  LT */

        f = formals;
        a = actuals;
        while (f != R_NilValue) {
            if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
	        SETCAR(a, mkPROMISEorConst(CAR(f), newrho));
	        SET_MISSING(a, 2);
            }
            f = CDR(f);
            a = CDR(a);
        }
    } 

    if (R_envHasNoSpecialSymbols(newrho))
	SET_NO_SPECIAL_SYMBOLS(newrho);

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedenv != R_NilValue) {
	for (tmp = FRAME(suppliedenv); tmp != R_NilValue; tmp = CDR(tmp)) {
	    defineVarIfNotPresent(TAG(tmp), CAR(tmp), newrho);
	}
    }

    /* Patch the context with the new environment */    

    cntxt.cstacktop += 2; /* the new context should have protection stack including newrho, actuals */	
    cntxt.cloenv = newrho;    	
    if( cntxt.nextcontext->callflag == CTXT_GENERIC ) {
        cntxt.sysparent = cntxt.nextcontext->sysparent;
    }

    /* Get the srcref record from the closure object */

    R_Srcref = getAttrib(op, R_SrcrefSymbol);

    /* Debugging */

    SET_RDEBUG(newrho, RDEBUG(op) || RSTEP(op)
		     || (RDEBUG(rho) && R_BrowserLastCommand == 's')) ;
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    if (RDEBUG(newrho)) {
	int old_bl = R_BrowseLines,
	    blines = asInteger(GetOption1(install("deparse.max.lines")));
	SEXP savesrcref;
	cntxt.browserfinish = 0; /* Don't want to inherit the "f" */
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	PrintValueRec(call, rho);
	R_BrowseLines = old_bl;

	/* Is the body a bare symbol (PR#6804) */
	if (!isSymbol(body) & !isVectorAtomic(body)){
		/* Find out if the body is function with only one statement. */
		if (isSymbol(CAR(body)))
			tmp = findFun(CAR(body), rho); /* FIXME: the value of tmp is never used */
		else
			tmp = eval(CAR(body), rho); /* FIXME: the value of tmp is never used */
	}
	savesrcref = R_Srcref;
	PROTECT(R_Srcref = getSrcref(getBlockSrcrefs(body), 0));
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(body);
	do_browser(call, op, R_NilValue, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  It isn't completely clear that this is the right place to do
	this, but maybe (if the matchArgs above reverses the
	arguments) it might just be perfect.

	This will not currently work as the entry points in envir.c
	are static.
    */

#ifdef  HASHING
    {
	SEXP R_NewHashTable(int);
	SEXP R_HashFrame(SEXP);
	HASHTAB(newrho) = R_NewHashTable(nargs);
	newrho = R_HashFrame(newrho);
    }
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }

    endcontext(&cntxt);

    if (RDEBUG(op)) {
	Rprintf("exiting from: ");
	PrintValueRec(call, rho);
    }
    UNPROTECT(3);
    return (tmp);
}

/* 
  This is a specialized version for calls that have no names, no dots, no (compile-time) missings.
  Arguments are given as an array (SEXP*); most of the elements will be promises, but some may be
  constants. 
  
  It does not support suppliedenv (usemethod).
 */
SEXP applyPositionalClosure(SEXP call, SEXP op, SEXP *args, int nargs, SEXP rho)
{
    SEXP formals, actuals, savedrho;
    volatile SEXP body, newrho;
    SEXP f, a, tmp;
    RCNTXT cntxt;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals and also added to the environment of the new function */
    /* arglist = the tagged list of arguments */

    /* protection against rho = NULL */
    // these are deliberately not translated
    if (!rho)
	errorcall(call,
		  "'rho' cannot be C NULL: detected in C-level applyClosure");
    if (!isEnvironment(rho))
	errorcall(call, "'rho' must be an environment not %s: detected in C-level applyClosure",
		  type2char(TYPEOF(rho)));

    formals = FORMALS(op);
    body = BODY(op);
    savedrho = CLOENV(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    /*  Set up a context with the call in it so error has access to it */
    /* FIXME: can we avoid this overhead? */

    beginposcontext(&cntxt, CTXT_RETURN, call, savedrho, rho, NULL, op, args);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  Ideally this environment sould be
	hashed.  */

    newrho = matchPositionalArgsCreateEnv(formals, args, nargs, call, savedrho, &actuals);
    PROTECT(newrho);
    PROTECT(actuals);

    if (NO_SPECIAL_SYMBOLS(formals)) {  
        /* FIXME: is this needed at all with the compiler, which disallows overriding of some(?) special symbols */
                
        /* the call has no dots and no names, so the variable names in newrho are just the formals */
        SET_NO_SPECIAL_SYMBOLS(newrho);
    }

    /* Patch the context with the new environment */    

    cntxt.cstacktop += 2; /* the new context should have protection stack including newrho, actuals */	
    cntxt.cloenv = newrho;    	
    if( cntxt.nextcontext->callflag == CTXT_GENERIC ) {
        cntxt.sysparent = cntxt.nextcontext->sysparent;
    }        

    /* Get the srcref record from the closure object */

    R_Srcref = getAttrib(op, R_SrcrefSymbol);

    /* Debugging */

    SET_RDEBUG(newrho, RDEBUG(op) || RSTEP(op)
		     || (RDEBUG(rho) && R_BrowserLastCommand == 's')) ;
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    if (RDEBUG(newrho)) {
	int old_bl = R_BrowseLines,
	    blines = asInteger(GetOption1(install("deparse.max.lines")));
	SEXP savesrcref;
	cntxt.browserfinish = 0; /* Don't want to inherit the "f" */
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	if(blines != NA_INTEGER && blines > 0)
	    R_BrowseLines = blines;
	PrintValueRec(call, rho);
	R_BrowseLines = old_bl;

	/* Is the body a bare symbol (PR#6804) */
	if (!isSymbol(body) & !isVectorAtomic(body)){
		/* Find out if the body is function with only one statement. */
		if (isSymbol(CAR(body)))
			tmp = findFun(CAR(body), rho); /* FIXME: the value of tmp is never used */
		else
			tmp = eval(CAR(body), rho); /* FIXME: the value of tmp is never used */
	}
	savesrcref = R_Srcref;
	PROTECT(R_Srcref = getSrcref(getBlockSrcrefs(body), 0));
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(body);
	do_browser(call, op, R_NilValue, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  It isn't completely clear that this is the right place to do
	this, but maybe (if the matchArgs above reverses the
	arguments) it might just be perfect.

	This will not currently work as the entry points in envir.c
	are static.
    */

#ifdef  HASHING
    {
	SEXP R_NewHashTable(int);
	SEXP R_HashFrame(SEXP);
	HASHTAB(newrho) = R_NewHashTable(nargs);
	newrho = R_HashFrame(newrho);
    }
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }

    endcontext(&cntxt);

    if (RDEBUG(op)) {
	Rprintf("exiting from: ");
	PrintValueRec(call, rho);
    }
    UNPROTECT(3);
    
    return (tmp);
}


/* **** FIXME: This code is factored out of applyClosure.  If we keep
   **** it we should change applyClosure to run through this routine
   **** to avoid code drift. */
static SEXP R_execClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho,
			  SEXP newrho)
{
    volatile SEXP body;
    SEXP tmp;
    RCNTXT cntxt;

    body = BODY(op);

    if (R_jit_enabled > 0 && TYPEOF(body) != BCODESXP) {
	int old_enabled = R_jit_enabled;
	SEXP newop;
	R_jit_enabled = 0;
	newop = R_cmpfun(op);
	body = BODY(newop);
	SET_BODY(op, body);
	R_jit_enabled = old_enabled;
    }

    begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist, op);

    /* The default return value is NULL.  FIXME: Is this really needed
       or do we always get a sensible value returned?  */

    tmp = R_NilValue;

    /* Debugging */

    SET_RDEBUG(newrho, RDEBUG(op) || RSTEP(op)
		     || (RDEBUG(rho) && R_BrowserLastCommand == 's')) ;
    if( RSTEP(op) ) SET_RSTEP(op, 0);
    if (RDEBUG(op)) {
	SEXP savesrcref;
	/* switch to interpreted version when debugging compiled code */
	if (TYPEOF(body) == BCODESXP)
	    body = bytecodeExpr(body);
	Rprintf("debugging in: ");
	PrintValueRec(call,rho);
	/* Find out if the body is function with only one statement. */
	if (isSymbol(CAR(body)))
	    tmp = findFun(CAR(body), rho);
	else
	    tmp = eval(CAR(body), rho);
	savesrcref = R_Srcref;
	PROTECT(R_Srcref = getSrcref(getBlockSrcrefs(body), 0));
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(body);
	do_browser(call, op, R_NilValue, newrho);
	R_Srcref = savesrcref;
	UNPROTECT(1);
    }

    /*  It isn't completely clear that this is the right place to do
	this, but maybe (if the matchArgs above reverses the
	arguments) it might just be perfect.  */

#ifdef  HASHING
#define HASHTABLEGROWTHRATE  1.2
    {
	SEXP R_NewHashTable(int, double);
	SEXP R_HashFrame(SEXP);
	int nargs = length(arglist);
	HASHTAB(newrho) = R_NewHashTable(nargs, HASHTABLEGROWTHRATE);
	newrho = R_HashFrame(newrho);
    }
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_RestartToken) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }

    endcontext(&cntxt);

    if (RDEBUG(op)) {
	Rprintf("exiting from: ");
	PrintValueRec(call, rho);
    }
    UNPROTECT(1);
    return (tmp);
}

/* **** FIXME: Temporary code to execute S4 methods in a way that
   **** preserves lexical scope. */

/* called from methods_list_dispatch.c */
SEXP R_execMethod(SEXP op, SEXP rho)
{
    SEXP call, arglist, callerenv, newrho, next, val;
    RCNTXT *cptr;

    /* create a new environment frame enclosed by the lexical
       environment of the method */
    PROTECT(newrho = Rf_NewEnvironment(R_NilValue, R_NilValue, CLOENV(op)));

    /* copy the bindings for the formal environment from the top frame
       of the internal environment of the generic call to the new
       frame.  need to make sure missingness information is preserved
       and the environments for any default expression promises are
       set to the new environment.  should move this to envir.c where
       it can be done more efficiently. */
    for (next = FORMALS(op); next != R_NilValue; next = CDR(next)) {
	SEXP symbol =  TAG(next);
	R_varloc_t loc;
	int missing;
	loc = R_findVarLocInFrame(rho,symbol);
	if(loc == NULL)
	    error(_("could not find symbol \"%s\" in environment of the generic function"),
		  CHAR(PRINTNAME(symbol)));
	missing = R_GetVarLocMISSING(loc);
	val = R_GetVarLocValue(loc);
	if (missing) {
	    switch(TYPEOF(val)) {
                case NILSXP:
                case LISTSXP:
                case LGLSXP:
                case INTSXP:
                case REALSXP:
                case STRSXP:
                case CPLXSXP:
                case RAWSXP:
                case S4SXP:
                case SPECIALSXP:
                case BUILTINSXP:
                case ENVSXP:
                case CLOSXP:
                case VECSXP:
                case EXTPTRSXP:
                case WEAKREFSXP:
                case EXPRSXP:
                    {
                        /* turn into a promise for the case that the method overrides defaults */

                        if (symbol == R_DotsSymbol) {
                            break;
                        }
                        SEXP deflt;
                        for(deflt = CAR(op); deflt != R_NilValue; deflt = CDR(deflt)) { /* FIXME: why is the loop needed? */
                            if(TAG(deflt) == symbol) break;
                        }
                        if(deflt == R_NilValue)
                            error(_("symbol \"%s\" not in environment of method"), CHAR(PRINTNAME(symbol)));
                        val = mkPROMISEorConst(CAR(deflt), newrho);
                        break;
                    }

	        case PROMSXP:
	            {
	                if (PRENV(val) == rho) {
                            SEXP deflt;
                            SET_PRENV(val, newrho);
                            /* find the symbol in the method, copy its expression
                            * to the promise */
                            for(deflt = CAR(op); deflt != R_NilValue; deflt = CDR(deflt)) { /* FIXME: why is the loop needed? */
		                if(TAG(deflt) == symbol) break;
                            }
                            if(deflt == R_NilValue)
                                error(_("symbol \"%s\" not in environment of method"), CHAR(PRINTNAME(symbol)));
                            SET_PRCODE(val, CAR(deflt));
                        }
                        break;
                    }
	    }
            SET_FRAME(newrho, CONS(val, FRAME(newrho)));
	    SET_TAG(FRAME(newrho), symbol);
	    SET_MISSING(FRAME(newrho), missing);
	} else { /* val is not missing, just add it to the new environment */
	    SET_FRAME(newrho, CONS(val, FRAME(newrho)));
	    SET_TAG(FRAME(newrho), symbol);
	}
    }

    /* copy the bindings of the spacial dispatch variables in the top
       frame of the generic call to the new frame */
    defineVar(R_dot_defined, findVarInFrame(rho, R_dot_defined), newrho);
    defineVar(R_dot_Method, findVarInFrame(rho, R_dot_Method), newrho);
    defineVar(R_dot_target, findVarInFrame(rho, R_dot_target), newrho);

    /* copy the bindings for .Generic and .Methods.  We know (I think)
       that they are in the second frame, so we could use that. */
    defineVar(R_dot_Generic, findVar(R_dot_Generic, rho), newrho);
    defineVar(R_dot_Methods, findVar(R_dot_Methods, rho), newrho);

    /* Find the calling context.  Should be R_GlobalContext unless
       profiling has inserted a CTXT_BUILTIN frame. */
    cptr = R_GlobalContext;
    if (cptr->callflag & CTXT_BUILTIN)
	cptr = cptr->nextcontext;

    /* The calling environment should either be the environment of the
       generic, rho, or the environment of the caller of the generic,
       the current sysparent. */
    callerenv = cptr->sysparent; /* or rho? */

    /* get the rest of the stuff we need from the current context,
       execute the method, and return the result */
    call = cptr->call;
    arglist = accessPromargs(cptr);
    val = R_execClosure(call, op, arglist, callerenv, newrho);
    UNPROTECT(1);
    return val;
}

static SEXP EnsureLocal(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if ((vl = findVarInFrame3(rho, symbol, TRUE)) != R_UnboundValue) {
	vl = eval(symbol, rho);	/* for promises */
	if(MAYBE_SHARED(vl)) {
	    PROTECT(vl = shallow_duplicate(vl));
	    defineVar(symbol, vl, rho);
	    UNPROTECT(1);
	    SET_NAMED(vl, 1);
	}
	return vl;
    }

    vl = eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));

    PROTECT(vl = shallow_duplicate(vl));
    defineVar(symbol, vl, rho);
    UNPROTECT(1);
    SET_NAMED(vl, 1);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

static SEXP R_valueSym = NULL; /* initialized in R_initAsignSymbols below */

static SEXP replaceCall(SEXP fun, SEXP val, SEXP args, SEXP rhs)
{
    SEXP tmp, ptmp;
    PROTECT(fun);
    PROTECT(args);
    PROTECT(rhs);
    PROTECT(val);
    ptmp = tmp = allocList(length(args)+3);
    UNPROTECT(4);
    SETCAR(ptmp, fun); ptmp = CDR(ptmp);
    SETCAR(ptmp, val); ptmp = CDR(ptmp);
    while(args != R_NilValue) {
	SETCAR(ptmp, CAR(args));
	SET_TAG(ptmp, TAG(args));
	ptmp = CDR(ptmp);
	args = CDR(args);
    }
    SETCAR(ptmp, rhs);
    SET_TAG(ptmp, R_valueSym);
    SET_TYPEOF(tmp, LANGSXP);
    return tmp;
}


static SEXP assignCall(SEXP op, SEXP symbol, SEXP fun,
		       SEXP val, SEXP args, SEXP rhs)
{
    PROTECT(op);
    PROTECT(symbol);
    val = replaceCall(fun, val, args, rhs);
    UNPROTECT(2);
    return lang3(op, symbol, val);
}


static R_INLINE Rboolean asLogicalNoNA(SEXP s, SEXP call)
{
    Rboolean cond = NA_LOGICAL;

    if (length(s) > 1)
	warningcall(call,
		    _("the condition has length > 1 and only the first element will be used"));
    if (length(s) > 0) {
	/* inline common cases for efficiency */
	switch(TYPEOF(s)) {
	case LGLSXP:
	    cond = LOGICAL(s)[0];
	    break;
	case INTSXP:
	    cond = INTEGER(s)[0]; /* relies on NA_INTEGER == NA_LOGICAL */
	    break;
	default:
	    cond = asLogical(s);
	}
    }

    if (cond == NA_LOGICAL) {
	char *msg = length(s) ? (isLogical(s) ?
				 _("missing value where TRUE/FALSE needed") :
				 _("argument is not interpretable as logical")) :
	    _("argument is of length zero");
	errorcall(call, msg);
    }
    return cond;
}


#define BodyHasBraces(body) \
    ((isLanguage(body) && CAR(body) == R_BraceSymbol) ? 1 : 0)

/* Allocate space for the loop variable value the first time through
   (when v == R_NilValue) and when the value has been assigned to
   another variable (NAMED(v) > 1). This should be safe and avoid
   allocation in many cases. */
#define ALLOC_LOOP_VAR(v, val_type, vpi) do { \
	if (v == R_NilValue || MAYBE_SHARED(v)) { \
	    REPROTECT(v = allocVector(val_type, 1), vpi); \
	    SET_NAMED(v, 1); \
	} \
    } while(0)

SEXP attribute_hidden do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP Cond, Stmt=R_NilValue;
    int vis=0;

    PROTECT(Cond = eval(CAR(args), rho));
    if (asLogicalNoNA(Cond, call))
	Stmt = CAR(CDR(args));
    else {
	if (length(args) > 2)
	   Stmt = CAR(CDR(CDR(args)));
	else
	   vis = 1;
    }
    if( !vis && RDEBUG(rho) && !BodyHasBraces(Stmt) && !R_GlobalContext->browserfinish) {
	SrcrefPrompt("debug", R_Srcref);
	PrintValue(Stmt);
	do_browser(call, op, R_NilValue, rho);
    }
    UNPROTECT(1);
    if( vis ) {
	R_Visible = FALSE; /* case of no 'else' so return invisible NULL */
	return Stmt;
    }
    return (eval(Stmt, rho));
}

static R_INLINE SEXP GET_BINDING_CELL(SEXP symbol, SEXP rho)
{
    if (rho == R_BaseEnv || rho == R_BaseNamespace)
	return R_NilValue;
    else {
	SEXP loc = (SEXP) R_findVarLocInFrame(rho, symbol);
	return (loc != NULL) ? loc : R_NilValue;
    }
}

static R_INLINE Rboolean SET_BINDING_VALUE(SEXP loc, SEXP value) {
    /* This depends on the current implementation of bindings */
    if (loc != R_NilValue &&
	! BINDING_IS_LOCKED(loc) && ! IS_ACTIVE_BINDING(loc)) {
	if (CAR(loc) != value) {
	    SETCAR(loc, value);
	    if (MISSING(loc))
		SET_MISSING(loc, 0);
	}
	return TRUE;
    }
    else
	return FALSE;
}

SEXP attribute_hidden do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* Need to declare volatile variables whose values are relied on
       after for_next or for_break longjmps and might change between
       the setjmp and longjmp calls. Theoretically this does not
       include n and bgn, but gcc -O2 -Wclobbered warns about these so
       to be safe we declare them volatile as well. */
    volatile int i = 0, n, bgn;
    volatile SEXP v, val, cell;
    int dbg, val_type;
    SEXP sym, body;
    RCNTXT cntxt;
    PROTECT_INDEX vpi;

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !isSymbol(sym) ) errorcall(call, _("non-symbol loop variable"));

    if (R_jit_enabled > 2 && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }

    PROTECT(args);
    PROTECT(rho);
    PROTECT(val = eval(val, rho));
    defineVar(sym, R_NilValue, rho);
    PROTECT(cell = GET_BINDING_CELL(sym, rho));

    /* deal with the case where we are iterating over a factor
       we need to coerce to character - then iterate */

    if ( inheritsCharSXP(val, R_FactorCharSXP) ) {
	SEXP tmp = asCharacterFactor(val);
	UNPROTECT(1); /* val from above */
	PROTECT(val = tmp);
    }

    if (isList(val) || isNull(val))
	n = length(val);
    else
	n = LENGTH(val);

    val_type = TYPEOF(val);

    dbg = RDEBUG(rho);
    bgn = BodyHasBraces(body);

    /* bump up NAMED count of sequence to avoid modification by loop code */
    INCREMENT_NAMED(val);
    INCREMENT_REFCNT(val);

    PROTECT_WITH_INDEX(v = R_NilValue, &vpi);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    switch (SETJMP(cntxt.cjmpbuf)) {
    case CTXT_BREAK: goto for_break;
    case CTXT_NEXT: goto for_next;
    }
    for (i = 0; i < n; i++) {

	switch (val_type) {

	case EXPRSXP:
	case VECSXP:
	    /* make sure loop variable is not modified via other vars */
	    SET_NAMED(VECTOR_ELT(val, i), 2);
	    /* defineVar is used here and below rather than setVar in
	       case the loop code removes the variable. */
	    defineVar(sym, VECTOR_ELT(val, i), rho);
	    break;

	case LISTSXP:
	    /* make sure loop variable is not modified via other vars */
	    SET_NAMED(CAR(val), 2);
	    defineVar(sym, CAR(val), rho);
	    val = CDR(val);
	    break;

	default:

	    switch (val_type) {
	    case LGLSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		LOGICAL(v)[0] = LOGICAL(val)[i];
		break;
	    case INTSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		INTEGER(v)[0] = INTEGER(val)[i];
		break;
	    case REALSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		REAL(v)[0] = REAL(val)[i];
		break;
	    case CPLXSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		COMPLEX(v)[0] = COMPLEX(val)[i];
		break;
	    case STRSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		SET_STRING_ELT(v, 0, STRING_ELT(val, i));
		break;
	    case RAWSXP:
		ALLOC_LOOP_VAR(v, val_type, vpi);
		RAW(v)[0] = RAW(val)[i];
		break;
	    default:
		errorcall(call, _("invalid for() loop sequence"));
	    }
	    if (CAR(cell) == R_UnboundValue || ! SET_BINDING_VALUE(cell, v))
		defineVar(sym, v, rho);
	}
	if (!bgn && RDEBUG(rho) && !R_GlobalContext->browserfinish) {
	    SrcrefPrompt("debug", R_Srcref);
	    PrintValue(body);
	    do_browser(call, op, R_NilValue, rho);
	}
	eval(body, rho);

    for_next:
	; /* needed for strict ISO C compliance, according to gcc 2.95.2 */
    }
 for_break:
    endcontext(&cntxt);
    DECREMENT_REFCNT(val);
    UNPROTECT(5);
    SET_RDEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile int bgn;
    volatile SEXP body;
    RCNTXT cntxt;

    checkArity(op, args);

    if (R_jit_enabled > 2 && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }


    dbg = RDEBUG(rho);
    body = CADR(args);
    bgn = BodyHasBraces(body);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	while (asLogicalNoNA(eval(CAR(args), rho), call)) {
	    if (RDEBUG(rho) && !bgn && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		PrintValue(body);
		do_browser(call, op, R_NilValue, rho);
	    }
	    eval(body, rho);
	    if (RDEBUG(rho) && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		Rprintf("(while) ");
		PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
	}
    }
    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile SEXP body;
    RCNTXT cntxt;

    checkArity(op, args);

    if (R_jit_enabled > 2 && ! R_PendingPromises) {
	R_compileAndExecute(call, rho);
	return R_NilValue;
    }

    dbg = RDEBUG(rho);
    body = CAR(args);

    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK) {
	for (;;) {
	    eval(body, rho);
	}
    }
    endcontext(&cntxt);
    SET_RDEBUG(rho, dbg);
    return R_NilValue;
}


SEXP attribute_hidden do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    findcontext(PRIMVAL(op), rho, R_NilValue);
    return R_NilValue;
}


SEXP attribute_hidden do_paren(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args);
}

SEXP attribute_hidden do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;
    if (args != R_NilValue) {
	SEXP srcrefs = getBlockSrcrefs(call);
	int i = 1;
	while (args != R_NilValue) {
	    PROTECT(R_Srcref = getSrcref(srcrefs, i++));
	    if (RDEBUG(rho) && !R_GlobalContext->browserfinish) {
		SrcrefPrompt("debug", R_Srcref);
		PrintValue(CAR(args));
		do_browser(call, op, R_NilValue, rho);
	    }
	    s = eval(CAR(args), rho);
	    UNPROTECT(1);
	    args = CDR(args);
	}
	R_Srcref = R_NilValue;
    }
    return s;
}


SEXP attribute_hidden do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP v;

    if (args == R_NilValue) /* zero arguments provided */
	v = R_NilValue;
    else if (CDR(args) == R_NilValue) /* one argument */
	v = eval(CAR(args), rho);
    else {
	v = R_NilValue; /* to avoid compiler warnings */
	errorcall(call, _("multi-argument returns are not permitted"));
    }

    findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, v);

    return R_NilValue; /*NOTREACHED*/
}

/* Declared with a variable number of args in names.c */
SEXP attribute_hidden do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval, srcref;

    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	SET_NAMED(op, 2);
    }
    if (length(args) < 2) WrongArgCount("function");
    CheckFormals(CAR(args));
    rval = mkCLOSXP(CAR(args), CADR(args), rho);
    srcref = CADDR(args);
    if (!isNull(srcref)) setAttrib(rval, R_SrcrefSymbol, srcref);
    return rval;
}


/*
 *  Assignments for complex LVAL specifications. This is the stuff that
 *  nightmares are made of ...	Note that "evalseq" preprocesses the LHS
 *  of an assignment.  Given an expression, it builds a list of partial
 *  values for the exression.  For example, the assignment x$a[3] <- 10
 *  with LHS x$a[3] yields the (improper) list:
 *
 *	 (eval(x$a[3])	eval(x$a)  eval(x)  .  x)
 *
 *  (Note the terminating symbol).  The partial evaluations are carried
 *  out efficiently using previously computed components.
 */

/*
  For complex superassignment  x[y==z]<<-w
  we want x required to be nonlocal, y,z, and w permitted to be local or
  nonlocal.
*/

static SEXP evalseq(SEXP expr, SEXP rho, int forcelocal,  R_varloc_t tmploc)
{
    SEXP val, nval, nexpr;
    if (isNull(expr))
	error(_("invalid (NULL) left side of assignment"));
    if (isSymbol(expr)) {
	PROTECT(expr);
	if(forcelocal) {
	    nval = EnsureLocal(expr, rho);
	}
	else {/* now we are down to the target symbol */
	  nval = eval(expr, ENCLOS(rho));
	}
	if (MAYBE_SHARED(nval))
	    nval = shallow_duplicate(nval);
	UNPROTECT(1);
	return CONS_NR(nval, expr);
    }
    else if (isLanguage(expr)) {
	PROTECT(expr);
	PROTECT(val = evalseq(CADR(expr), rho, forcelocal, tmploc));
	R_SetVarLocValue(tmploc, CAR(val));
	PROTECT(nexpr = LCONS(R_GetVarLocSymbol(tmploc), CDDR(expr)));
	PROTECT(nexpr = LCONS(CAR(expr), nexpr));
	nval = eval(nexpr, rho);
	/* duplicate nval if it might be shared _or_ if the container,
	   CAR(val), has become possibly shared by going through a
	   closure.  This is taken to indicate that the corresponding
	   replacement function might be a closure and will need to
	   see an unmodified LHS value. This heuristic fails if the
	   accessor function called here is not a closure but the
	   replacement function is. */
	if (MAYBE_REFERENCED(nval) &&
	    (MAYBE_SHARED(nval) || MAYBE_SHARED(CAR(val))))
	    nval = shallow_duplicate(nval);
	UNPROTECT(4);
	return CONS_NR(nval, val);
    }
    else error(_("target of assignment expands to non-language object"));
    return R_NilValue;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

static const char * const asym[] = {":=", "<-", "<<-", "="};
#define NUM_ASYM (sizeof(asym) / sizeof(char *))
static SEXP asymSymbol[NUM_ASYM];

static SEXP R_ReplaceFunsTable = NULL;
static SEXP R_SubsetSym = NULL;
static SEXP R_SubassignSym = NULL;
static SEXP R_Subset2Sym = NULL;
static SEXP R_Subassign2Sym = NULL;
static SEXP R_DollarGetsSymbol = NULL;

void attribute_hidden R_initAsignSymbols(void)
{
    for (int i = 0; i < NUM_ASYM; i++)
	asymSymbol[i] = install(asym[i]);

    R_ReplaceFunsTable = R_NewHashedEnv(R_EmptyEnv, ScalarInteger(1099));
    R_PreserveObject(R_ReplaceFunsTable);

    R_SubsetSym = install("[");
    R_SubassignSym = install("[<-");
    R_Subset2Sym = install("[[");
    R_Subassign2Sym = install("[[<-");
    R_DollarGetsSymbol = install("$<-");
    R_valueSym = install("value");
}

static R_INLINE SEXP lookupAssignFcnSymbol(SEXP fun)
{
    return findVarInFrame(R_ReplaceFunsTable, fun);
}

static void enterAssignFcnSymbol(SEXP fun, SEXP val)
{
    defineVar(fun, val, R_ReplaceFunsTable);
}

static void tmp_cleanup(void *data)
{
    unbindVar(R_TmpvalSymbol, (SEXP) data);
}

/* This macro stores the current assignment target in the saved
   binding location. It duplicates if necessary to make sure
   replacement functions are always called with a target with NAMED ==
   1. The SET_CAR is intended to protect against possible GC in
   R_SetVarLocValue; this might occur it the binding is an active
   binding. */
#define SET_TEMPVARLOC_FROM_CAR(loc, lhs) do { \
	SEXP __lhs__ = (lhs); \
	SEXP __v__ = CAR(__lhs__); \
	if (MAYBE_SHARED(__v__)) { \
	    __v__ = shallow_duplicate(__v__); \
	    SET_NAMED(__v__, 1); \
	    SETCAR(__lhs__, __v__); \
	} \
	R_SetVarLocValue(loc, __v__); \
    } while(0)

/* This macro makes sure the RHS NAMED value is 0 or 2. This is
   necessary to make sure the RHS value returned by the assignment
   expression is correct when the RHS value is part of the LHS
   object. */
#define FIXUP_RHS_NAMED(r) do { \
	SEXP __rhs__ = (r); \
	if (NAMED(__rhs__) && NAMED(__rhs__) <= 1) \
	    SET_NAMED(__rhs__, 2); \
    } while (0)

#define ASSIGNBUFSIZ 32
static SEXP installAssignFcnSymbol(SEXP fun)
{
    char buf[ASSIGNBUFSIZ];

    /* install the symbol */
    if(strlen(CHAR(PRINTNAME(fun))) + 3 > ASSIGNBUFSIZ)
	error(_("overlong name in '%s'"), EncodeChar(PRINTNAME(fun)));
    sprintf(buf, "%s<-", CHAR(PRINTNAME(fun)));
    SEXP val = install(buf);

    enterAssignFcnSymbol(fun, val);
    return val;
}

static R_INLINE SEXP getAssignFcnSymbol(SEXP fun)
{
    /* handle [<-, [[<-, and $<- efficiently */
    if (fun == R_SubsetSym)
	return R_SubassignSym;
    else if (fun == R_Subset2Sym)
	return R_Subassign2Sym;
    else if (fun == R_DollarSymbol)
	return R_DollarGetsSymbol;

    /* look up in the replacement functions table */
    SEXP val = lookupAssignFcnSymbol(fun);
    if (val != R_UnboundValue)
	return val;

    /* instal symbol, entern in table,  and return */
    return installAssignFcnSymbol(fun);
}

static R_INLINE SEXP mkRHSPROMISE(SEXP expr, SEXP rhs)
{
    return R_mkEVPROMISE_NR(expr, rhs);
}

static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr, lhs, rhs, saverhs, tmp, afun, rhsprom;
    R_varloc_t tmploc;
    RCNTXT cntxt;
    int nprot;

    expr = CAR(args);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

    PROTECT(saverhs = rhs = eval(CADR(args), rho));
    INCREMENT_REFCNT(saverhs);

    /*  FIXME: We need to ensure that this works for hashed
	environments.  This code only works for unhashed ones.  the
	syntax error here is a deliberate marker so I don't forget that
	this needs to be done.  The code used in "missing" will help
	here.  */

    /*  FIXME: This strategy will not work when we are working in the
	data frame defined by the system hash table.  The structure there
	is different.  Should we special case here?  */

    /*  We need a temporary variable to hold the intermediate values
	in the computation.  For efficiency reasons we record the
	location where this variable is stored.  We need to protect
	the location in case the biding is removed from its
	environment by user code or an assignment within the
	assignment arguments */

    /*  There are two issues with the approach here:

	    A complex assignment within a complex assignment, like
	    f(x, y[] <- 1) <- 3, can cause the value temporary
	    variable for the outer assignment to be overwritten and
	    then removed by the inner one.  This could be addressed by
	    using multiple temporaries or using a promise for this
	    variable as is done for the RHS.  Printing of the
	    replacement function call in error messages might then need
	    to be adjusted.

	    With assignments of the form f(g(x, z), y) <- w the value
	    of 'z' will be computed twice, once for a call to g(x, z)
	    and once for the call to the replacement function g<-.  It
	    might be possible to address this by using promises.
	    Using more temporaries would not work as it would mess up
	    replacement functions that use substitute and/or
	    nonstandard evaluation (and there are packages that do
	    that -- igraph is one).

	    LT */

    FIXUP_RHS_NAMED(rhs);

    if (rho == R_BaseNamespace)
	errorcall(call, _("cannot do complex assignments in base namespace"));
    if (rho == R_BaseEnv)
	errorcall(call, _("cannot do complex assignments in base environment"));
    defineVar(R_TmpvalSymbol, R_NilValue, rho);
    PROTECT((SEXP) (tmploc = R_findVarLocInFrame(rho, R_TmpvalSymbol)));
    DISABLE_REFCNT((SEXP) tmploc);
    DECREMENT_REFCNT(CDR((SEXP) tmploc));

    /* Now set up a context to remove it when we are done, even in the
     * case of an error.  This all helps error() provide a better call.
     */
    begincontext(&cntxt, CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &tmp_cleanup;
    cntxt.cenddata = rho;

    /*  Do a partial evaluation down through the LHS. */
    lhs = evalseq(CADR(expr), rho,
		  PRIMVAL(op)==1 || PRIMVAL(op)==3, tmploc);

    PROTECT(lhs);
    PROTECT(rhsprom = mkRHSPROMISE(CADR(args), rhs));

    while (isLanguage(CADR(expr))) {
	nprot = 1; /* the PROTECT of rhs below from this iteration */
	if (TYPEOF(CAR(expr)) == SYMSXP)
	    tmp = getAssignFcnSymbol(CAR(expr));
	else {
	    /* check for and handle assignments of the form
	       foo::bar(x) <- y or foo:::bar(x) <- y */
	    tmp = R_NilValue; /* avoid uninitialized variable warnings */
	    if (TYPEOF(CAR(expr)) == LANGSXP &&
		(CAR(CAR(expr)) == R_DoubleColonSymbol ||
		 CAR(CAR(expr)) == R_TripleColonSymbol) &&
		length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
		tmp = getAssignFcnSymbol(CADDR(CAR(expr)));
		PROTECT(tmp = lang3(CAAR(expr), CADR(CAR(expr)), tmp));
		nprot++;
	    }
	    else
		error(_("invalid function in complex assignment"));
	}
	SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
	PROTECT(rhs = replaceCall(tmp, R_TmpvalSymbol, CDDR(expr), rhsprom));
	rhs = eval(rhs, rho);
	SET_PRVALUE(rhsprom, rhs);
	SET_PRCODE(rhsprom, rhs); /* not good but is what we have been doing */
	UNPROTECT(nprot);
	lhs = CDR(lhs);
	expr = CADR(expr);
    }
    nprot = 5; /* the commont case */
    if (TYPEOF(CAR(expr)) == SYMSXP)
	afun = getAssignFcnSymbol(CAR(expr));
    else {
	/* check for and handle assignments of the form
	   foo::bar(x) <- y or foo:::bar(x) <- y */
	afun = R_NilValue; /* avoid uninitialized variable warnings */
	if (TYPEOF(CAR(expr)) == LANGSXP &&
	    (CAR(CAR(expr)) == R_DoubleColonSymbol ||
	     CAR(CAR(expr)) == R_TripleColonSymbol) &&
	    length(CAR(expr)) == 3 && TYPEOF(CADDR(CAR(expr))) == SYMSXP) {
	    afun = getAssignFcnSymbol(CADDR(CAR(expr)));
	    PROTECT(afun = lang3(CAAR(expr), CADR(CAR(expr)), afun));
	    nprot++;
	}
	else
	    error(_("invalid function in complex assignment"));
    }
    SET_TEMPVARLOC_FROM_CAR(tmploc, lhs);
    PROTECT(expr = assignCall(asymSymbol[PRIMVAL(op)], CDR(lhs),
			      afun, R_TmpvalSymbol, CDDR(expr), rhsprom));
    expr = eval(expr, rho);
    UNPROTECT(nprot);
    endcontext(&cntxt); /* which does not run the remove */
    unbindVar(R_TmpvalSymbol, rho);
#ifdef OLD_RHS_NAMED
    /* we do not duplicate the value, so to be conservative mark the
       value as NAMED = 2 */
    SET_NAMED(saverhs, 2);
#else
    INCREMENT_NAMED(saverhs);
#endif
    DECREMENT_REFCNT(saverhs);
    return saverhs;
}

/* Defunct in 1.5.0
SEXP attribute_hidden do_alias(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op,args);
    Rprintf(".Alias is deprecated; there is no replacement \n");
    SET_NAMED(CAR(args), 0);
    return CAR(args);
}
*/

/*  Assignment in its various forms  */

SEXP attribute_hidden do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP lhs, rhs;

    if (args == R_NilValue ||
	CDR(args) == R_NilValue ||
	CDDR(args) != R_NilValue)
	WrongArgCount(asym[PRIMVAL(op)]);

    lhs = CAR(args);

    switch (TYPEOF(lhs)) {
    case STRSXP:
	lhs = installTrChar(STRING_ELT(lhs, 0));
	/* fall through */
    case SYMSXP:
	rhs = eval(CADR(args), rho);
	INCREMENT_NAMED(rhs);
	if (PRIMVAL(op) == 2)                       /* <<- */
	    setVar(lhs, rhs, ENCLOS(rho));
	else                                        /* <-, = */
	    defineVar(lhs, rhs, rho);
	R_Visible = FALSE;
	return rhs;
    case LANGSXP:
	R_Visible = FALSE;
	return applydefine(call, op, args, rho);
    default:
	errorcall(call, _("invalid (do_set) left-hand side to assignment"));
    }

    return R_NilValue;/*NOTREACHED*/
}


/* Evaluate each expression in "el" in the environment "rho".  This is
   a naturally recursive algorithm, but we use the iterative form below
   because it is does not cause growth of the pointer protection stack,
   and because it is a little more efficient.
*/

#define COPY_TAG(to, from) do { \
  SEXP __tag__ = TAG(from); \
  if (__tag__ != R_NilValue) SET_TAG(to, __tag__); \
} while (0)

/* Used in eval and applyMethod (object.c) for builtin primitives,
   do_internal (names.c) for builtin .Internals
   and in evalArgs.

   'n' is the number of arguments already evaluated and hence not
   passed to evalArgs and hence to here.
 */
SEXP attribute_hidden evalList(SEXP el, SEXP rho, SEXP call, int n)
{
    SEXP head, tail, ev, h;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {
	n++;

	if (CAR(el) == R_DotsSymbol) {
	    /* If we have a ... symbol, we look to see what it is bound to.
	     * If its binding is Null (i.e. zero length)
	     *	we just ignore it and return the cdr with all its expressions evaluated;
	     * if it is bound to a ... list of promises,
	     *	we force all the promises and then splice
	     *	the list of resulting values into the return value.
	     * Anything else bound to a ... symbol is an error
	     */
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    ev = CONS_NR(eval(CAR(h), rho), R_NilValue);
		    if (head==R_NilValue)
			PROTECT(head = ev);
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	} else if (CAR(el) == R_MissingArg) {
	    /* It was an empty element: most likely get here from evalArgs
	       which may have been called on part of the args. */
	    errorcall(call, _("argument %d is empty"), n);
#ifdef CHECK_IS_MISSING_IN_evalList
	    /* Radford Newl drops this R_isMissing check in pqR in
	       03-zap-isMissing (but it seems to creep in again later
	       with helper thread stuff?)  as it takes quite a bit of
	       time (essentially the equivalent of evaluating the
	       symbol, but maybe not as efficiently as eval) and only
	       serves to change the error message, not always for the
	       better. Also, the byte code interpreter does not do
	       this, so dropping this makes compiled and interreted
	       cod emore consistent. */
	} else if (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)) {
	    /* It was missing */
	    errorcall(call, _("'%s' is missing"), EncodeChar(PRINTNAME(CAR(el))));
#endif
	} else {
	    ev = CONS_NR(eval(CAR(el), rho), R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;

} /* evalList() */


/* A slight variation of evaluating each expression in "el" in "rho". */

/* used in evalArgs, arithmetic.c, seq.c */
SEXP attribute_hidden evalListKeepMissing(SEXP el, SEXP rho)
{
    SEXP head, tail, ev, h;

    head = R_NilValue;
    tail = R_NilValue; /* to prevent uninitialized variable warnings */

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
			ev = CONS_NR(R_MissingArg, R_NilValue);
		    else
			ev = CONS_NR(eval(CAR(h), rho), R_NilValue);
		    if (head==R_NilValue)
			PROTECT(head = ev);
		    else
			SETCDR(tail, ev);
		    COPY_TAG(ev, h);
		    tail = ev;
		    h = CDR(h);
		}
	    }
	    else if(h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	}
	else {
	    if (CAR(el) == R_MissingArg ||
		 (isSymbol(CAR(el)) && R_isMissing(CAR(el), rho)))
		ev = CONS_NR(R_MissingArg, R_NilValue);
	    else
		ev = CONS_NR(eval(CAR(el), rho), R_NilValue);
	    if (head==R_NilValue)
		PROTECT(head = ev);
	    else
		SETCDR(tail, ev);
	    COPY_TAG(ev, el);
	    tail = ev;
	}
	el = CDR(el);
    }

    if (head!=R_NilValue)
	UNPROTECT(1);

    return head;
}

#ifdef USE_PROMARGS_STACK

R_INLINE SEXP allocatePromargsCell(SEXP tag, SEXP value, SEXP next) {
    SEXPREC *top = R_PromargsStackTop;
    
    if (top == R_PromargsStackEnd) {
        /* double the stack */
        expandPromargsStack();
        top = R_PromargsStackTop;
    }
    
    R_PromargsStackTop = top + 1;
    SEXP s = top;
    
    CAR(s) = value; /* ? refcnt */
    CDR(s) = next;
    TAG(s) = tag; /* ? refcnt */
    s->sxpinfo = R_NilValue->sxpinfo; /* FIXME: avoid dereference? */
    TYPEOF(s) = LISTSXP;
    ATTRIB(s) = R_NilValue;
    s->gengc_next_node = NULL; /* unused! */
    s->gengc_prev_node = NULL; /* unused! */
    MARK(s) = 1; /* the GC should leave this alone */
    
    /* FIXME: what to do about REFCNT? */
    
    return s;
}

R_INLINE SEXP allocatePromargsCellNoTag(SEXP value) {
    return allocatePromargsCell(R_NilValue, value, R_NilValue);
}

R_INLINE SEXP allocatePromargsTuple(SEXP firstValue, SEXP secondValue) {
    SEXP first = allocatePromargsCell(R_NilValue, firstValue, R_NilValue);
    CDR(first) = allocatePromargsCell(R_NilValue, secondValue, R_NilValue);
    return first;
}

R_INLINE SEXP allocatePromargsTriple(SEXP firstValue, SEXP secondValue, SEXP thirdValue) {
    SEXP first = allocatePromargsCell(R_NilValue, firstValue, R_NilValue);
    SEXP second = allocatePromargsCell(R_NilValue, secondValue, R_NilValue);
    CDR(first) = second;
    CDR(second) = allocatePromargsCell(R_NilValue, thirdValue, R_NilValue);
    
    return first;
}

R_INLINE SEXP allocatePromargsTripleLastValue(SEXP firstValue, SEXP secondValue, SEXP thirdValue) {
    SEXP first = allocatePromargsCell(R_NilValue, firstValue, R_NilValue);
    SEXP second = allocatePromargsCell(R_NilValue, secondValue, R_NilValue);
    CDR(first) = second;
    CDR(second) = allocatePromargsCell(R_valueSym, thirdValue, R_NilValue);
    
    return first;
}

R_INLINE SEXP allocatePromargsQuadrupleLastValue(SEXP firstValue, SEXP secondValue, SEXP thirdValue, SEXP fourthValue) {
    SEXP first = allocatePromargsCell(R_NilValue, firstValue, R_NilValue);
    SEXP second = allocatePromargsCell(R_NilValue, secondValue, R_NilValue);
    CDR(first) = second;
    SEXP third = allocatePromargsCell(R_NilValue, thirdValue, R_NilValue);
    CDR(second) = third;
    CDR(third) = allocatePromargsCell(R_valueSym, fourthValue, R_NilValue);
    
    return first;
}


#endif /* USE_PROMARGS_STACK */

R_INLINE SEXP allocateCallargTuple(SEXP firstValue, SEXP secondValue) {
    SEXP second = CONS_NR(secondValue, R_NilValue);
    return CONS_NR(firstValue, second);
}

R_INLINE SEXP allocateCallargTriple(SEXP firstValue, SEXP secondValue, SEXP thirdValue) {
    SEXP third = CONS_NR(thirdValue, R_NilValue);
    SEXP second = CONS_NR(secondValue, third);
    return CONS_NR(firstValue, second);
}

R_INLINE SEXP allocateCallargTripleLastValue(SEXP firstValue, SEXP secondValue, SEXP thirdValue) {
    SEXP third = CONS_NR(thirdValue, R_NilValue);
    SET_TAG(third, R_valueSym);
    SEXP second = CONS_NR(secondValue, third);
    return CONS_NR(firstValue, second);
}

R_INLINE SEXP allocateCallargQuadrupleLastValue(SEXP firstValue, SEXP secondValue, SEXP thirdValue, SEXP fourthValue) {
    SEXP fourth = CONS_NR(fourthValue, R_NilValue);
    SET_TAG(fourth, R_valueSym);
    SEXP third = CONS_NR(thirdValue, fourth);
    SEXP second = CONS_NR(secondValue, third);
    return CONS_NR(firstValue, second);
}

/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP attribute_hidden promiseArgs(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while(el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* Is this double promise mechanism really needed? */

	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    SETCDR(tail, CONS(mkPROMISEorConst(CAR(h), rho), R_NilValue));
		    tail = CDR(tail);
		    COPY_TAG(tail, h);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	else {
	    SETCDR(tail, CONS(mkPROMISEorConst(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    COPY_TAG(tail, el);
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}

#ifdef USE_PROMARGS_STACK

SEXP attribute_hidden promiseArgsStack(SEXP el, SEXP rho)
{
    SEXP ans, h, arg, tail;

    ans = R_NilValue;
    tail = R_NilValue;

    for(; el != R_NilValue; el = CDR(el)) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* Is this double promise mechanism really needed? */

	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    arg = allocatePromargsCell(TAG(h), mkPROMISEorConst(CAR(h), rho), R_NilValue); /* avoid barrier */
		    if (tail == R_NilValue) {
		        ans = arg;
		    } else {
		        CDR(tail) = arg;
		    }
		    tail = arg;
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
            continue;
	}

	if (CAR(el) == R_MissingArg) {
            arg = allocatePromargsCell(TAG(el), R_MissingArg, R_NilValue); /* avoid barrier */
        } else {
            arg = allocatePromargsCell(TAG(el), mkPROMISEorConst(CAR(el), rho), R_NilValue); /* avoid barrier */
        }
        if (tail == R_NilValue) {
            ans = arg;
        } else {
            CDR(tail) = arg;
        }
        tail = arg;
    }
    return ans;
}

#endif /* USE_PROMARGS_STACK */


/* Check that each formal is a symbol */

/* used in coerce.c */
void attribute_hidden CheckFormals(SEXP ls)
{
    if (isList(ls)) {
        int sawSpecialSymbol = 0;
	for (; ls != R_NilValue; ls = CDR(ls)) {
	    SEXP name = TAG(ls);
	    if (TYPEOF(name) != SYMSXP)
		goto err;
            if (IS_SPECIAL_SYMBOL(name)) {
                sawSpecialSymbol = 1;
            }
        }
        if (!sawSpecialSymbol) {
            SET_NO_SPECIAL_SYMBOLS(ls);
        }
	return;
    }
 err:
    error(_("invalid formal argument list for \"function\""));
}


static SEXP VectorToPairListNamed(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named;
    const void *vmax = vmaxget();

    PROTECT(x);
    PROTECT(xnames = getNamesAttrib(x)); /* isn't this protected via x? */
    named = (xnames != R_NilValue);
    if(named)
	for (i = 0; i < length(x); i++)
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') len++;

    if(len) {
	PROTECT(xnew = allocList(len));
	xptr = xnew;
	for (i = 0; i < length(x); i++) {
	    if (CHAR(STRING_ELT(xnames, i))[0] != '\0') {
		SETCAR(xptr, VECTOR_ELT(x, i));
		SET_TAG(xptr, installTrChar(STRING_ELT(xnames, i)));
		xptr = CDR(xptr);
	    }
	}
	UNPROTECT(1);
    } else xnew = allocList(0);
    UNPROTECT(2);
    vmaxset(vmax);
    return xnew;
}

#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)

/* "eval": Evaluate the first argument
   in the environment specified by the second argument. */

SEXP attribute_hidden do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl, x, xptr;
    volatile SEXP expr, env, tmp;

    int frame;
    RCNTXT cntxt;

    checkArity(op, args);
    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    SEXPTYPE tEncl = TYPEOF(encl);
    if (isNull(encl)) {
	/* This is supposed to be defunct, but has been kept here
	   (and documented as such) */
	encl = R_BaseEnv;
    } else if ( !isEnvironment(encl) &&
		!isEnvironment((encl = simple_as_environment(encl))) ) {
	error(_("invalid '%s' argument of type '%s'"),
	      "enclos", type2char(tEncl));
    }
    if(IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
	env = R_getS4DataSlot(env, ANYSXP); /* usually an ENVSXP */
    switch(TYPEOF(env)) {
    case NILSXP:
	env = encl;     /* so eval(expr, NULL, encl) works */
	/* falls through */
    case ENVSXP:
	PROTECT(env);	/* so we can unprotect 2 at the end */
	break;
    case LISTSXP:
	/* This usage requires all the pairlist to be named */
	env = NewEnvironment(R_NilValue, duplicate(CADR(args)), encl);
	PROTECT(env);
	break;
    case VECSXP:
	/* PR#14035 */
	x = VectorToPairListNamed(CADR(args));
	for (xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr))
	    SET_NAMED(CAR(xptr) , 2);
	env = NewEnvironment(R_NilValue, x, encl);
	PROTECT(env);
	break;
    case INTSXP:
    case REALSXP:
	if (length(env) != 1)
	    error(_("numeric 'envir' arg not of length one"));
	frame = asInteger(env);
	if (frame == NA_INTEGER)
	    error(_("invalid '%s' argument of type '%s'"),
		  "envir", type2char(TYPEOF(env)));
	PROTECT(env = R_sysframe(frame, R_GlobalContext));
	break;
    default:
	error(_("invalid '%s' argument of type '%s'"),
	      "envir", type2char(TYPEOF(env)));
    }

    /* isLanguage include NILSXP, and that does not need to be
       evaluated
    if (isLanguage(expr) || isSymbol(expr) || isByteCode(expr)) { */
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP || isByteCode(expr)) {
	PROTECT(expr);
	begincontext(&cntxt, CTXT_RETURN, call, env, rho, args, op);
	if (!SETJMP(cntxt.cjmpbuf))
	    expr = eval(expr, env);
	else {
	    expr = R_ReturnedValue;
	    if (expr == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		error(_("restarts not supported in 'eval'"));
	    }
	}
	endcontext(&cntxt);
	UNPROTECT(1);
    }
    else if (TYPEOF(expr) == EXPRSXP) {
	int i, n;
	SEXP srcrefs = getBlockSrcrefs(expr);
	PROTECT(expr);
	n = LENGTH(expr);
	tmp = R_NilValue;
	begincontext(&cntxt, CTXT_RETURN, call, env, rho, args, op);
	if (!SETJMP(cntxt.cjmpbuf))
	    for(i = 0 ; i < n ; i++) {
		R_Srcref = getSrcref(srcrefs, i);
		tmp = eval(VECTOR_ELT(expr, i), env);
	    }
	else {
	    tmp = R_ReturnedValue;
	    if (tmp == R_RestartToken) {
		cntxt.callflag = CTXT_RETURN;  /* turn restart off */
		error(_("restarts not supported in 'eval'"));
	    }
	}
	endcontext(&cntxt);
	UNPROTECT(1);
	expr = tmp;
    }
    else if( TYPEOF(expr) == PROMSXP ) {
	expr = eval(expr, rho);
    } /* else expr is returned unchanged */
    UNPROTECT(1);
    return expr;
}

/* This is a special .Internal */
SEXP attribute_hidden do_withVisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, nm, ret;

    checkArity(op, args);
    x = CAR(args);
    x = eval(x, rho);
    PROTECT(x);
    PROTECT(ret = allocVector(VECSXP, 2));
    PROTECT(nm = allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("value"));
    SET_STRING_ELT(nm, 1, mkChar("visible"));
    SET_VECTOR_ELT(ret, 0, x);
    SET_VECTOR_ELT(ret, 1, ScalarLogical(R_Visible));
    setAttrib(ret, R_NamesSymbol, nm);
    UNPROTECT(3);
    return ret;
}

/* This is a special .Internal */
SEXP attribute_hidden do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    SEXP s, ans ;
    cptr = R_GlobalContext;
    /* get the args supplied */
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == rho)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr != NULL) {
	args = accessPromargs(cptr);
    }
    /* get the env recall was called from */
    s = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == s)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error(_("'Recall' called from outside a closure"));

    /* If the function has been recorded in the context, use it
       otherwise search for it by name or evaluate the expression
       originally used to get it.
    */
    if (cptr->callfun != R_NilValue)
	PROTECT(s = cptr->callfun);
    else if( TYPEOF(CAR(cptr->call)) == SYMSXP)
	PROTECT(s = findFun(CAR(cptr->call), cptr->sysparent));
    else
	PROTECT(s = eval(CAR(cptr->call), cptr->sysparent));
    if (TYPEOF(s) != CLOSXP)
	error(_("'Recall' called from outside a closure"));
    ans = applyClosure(cptr->call, s, args, cptr->sysparent, R_BaseEnv);
    UNPROTECT(1);
    return ans;
}


static SEXP evalArgs(SEXP el, SEXP rho, int dropmissing, SEXP call, int n)
{
    if(dropmissing) return evalList(el, rho, call, n);
    else return evalListKeepMissing(el, rho);
}


/* A version of DispatchOrEval that checks for possible S4 methods for
 * any argument, not just the first.  Used in the code for `[` in
 * do_subset.  Differs in that all arguments are evaluated
 * immediately, rather than after the call to R_possible_dispatch.
 */
attribute_hidden
int DispatchAnyOrEval(SEXP call, SEXP op, SEXP genericNativeCharSXP, SEXP args,
		      SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
    if(R_has_methods(op)) {
	SEXP argValue, el,  value;
	/* Rboolean hasS4 = FALSE; */
	int nprotect = 0, dispatch;
	if(!argsevald) {
	    PROTECT(argValue = evalArgs(args, rho, dropmissing, call, 0));
	    nprotect++;
	    argsevald = TRUE;
	}
	else argValue = args;
	for(el = argValue; el != R_NilValue; el = CDR(el)) {
	    if(IS_S4_OBJECT(CAR(el))) {
		value = R_possible_dispatch(call, op, argValue, rho, TRUE);
		if(value) {
		    *ans = value;
		    UNPROTECT(nprotect);
		    return 1;
		}
		else break;
	    }
	}
	 /* else, use the regular DispatchOrEval, but now with evaluated args */
	dispatch = DispatchOrEval(call, op, genericNativeCharSXP, argValue, rho, ans, dropmissing, argsevald);
	UNPROTECT(nprotect);
	return dispatch;
    }
    return DispatchOrEval(call, op, genericNativeCharSXP, args, rho, ans, dropmissing, argsevald);
}


/* DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * (unevaluated) arguments it comes in with and returns them so that
 * the generic built-in C code can continue.

 * To call this an ugly hack would be to insult all existing ugly hacks
 * at large in the world.
 */
attribute_hidden
int DispatchOrEval(SEXP call, SEXP op, SEXP genericNativeCharSXP, SEXP args,
		   SEXP rho, SEXP *ans, int dropmissing, int argsevald)
{
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    SEXP x = R_NilValue;
    int dots = FALSE, nprotect = 0;;

    if( argsevald )
	{PROTECT(x = CAR(args)); nprotect++;}
    else {
	/* Find the object to dispatch on, dropping any leading
	   ... arguments with missing or empty values.  If there are no
	   arguments, R_NilValue is used. */
	for (; args != R_NilValue; args = CDR(args)) {
	    if (CAR(args) == R_DotsSymbol) {
		SEXP h = findVar(R_DotsSymbol, rho);
		if (TYPEOF(h) == DOTSXP) {
#ifdef DODO
		    /**** any self-evaluating value should be OK; this
			  is used in byte compiled code. LT */
		    /* just a consistency check */
		    if (TYPEOF(CAR(h)) != PROMSXP)
			error(_("value in '...' is not a promise"));
#endif
		    dots = TRUE;
		    x = eval(CAR(h), rho);
		    break;
		}
		else if (h != R_NilValue && h != R_MissingArg)
		    error(_("'...' used in an incorrect context"));
	    }
	    else {
		dots = FALSE;
		x = eval(CAR(args), rho);
		break;
	    }
	}
	PROTECT(x); nprotect++;
    }
	/* try to dispatch on the object */
    if( isObject(x) ) {
	char *pt;
	/* Try for formal method. */
	if(IS_S4_OBJECT(x) && R_has_methods(op)) {
	    SEXP value, argValue;
	    SEXP pargs = NULL;
	    /* create a promise to pass down to applyClosure  */
	    if(!argsevald) {
		argValue = pargs = PROMISE_ARGS(args, rho);
		SET_PRVALUE_IF_PROMISE(CAR(argValue), x);
	    } else argValue = args;
	    PROTECT(argValue); nprotect++;
	    /* This means S4 dispatch */
	    value = R_possible_dispatch(call, op, argValue, rho, TRUE);
	    if(value) {
		*ans = value;
		UNPROTECT(nprotect);
		if (pargs != NULL) {
		    RELEASE_PROMARGS(pargs);
		}
		return 1;
	    }
	    else {
		/* go on, with the evaluated args.  Not guaranteed to have
		   the same semantics as if the arguments were not
		   evaluated, in special cases (e.g., arg values that are
		   LANGSXP).
		   The use of the promiseArgs is supposed to prevent
		   multiple evaluation after the call to possible_dispatch.
		*/
		if (dots)
		    PROTECT(argValue = evalArgs(argValue, rho, dropmissing,
						call, 0));
		else {
		    PROTECT(argValue = CONS_NR(x, evalArgs(CDR(argValue), rho,
							dropmissing, call, 1)));
		    SET_TAG(argValue, CreateTag(TAG(args)));
		}
		nprotect++;
		args = argValue;
		argsevald = 1;
	    }
	    if (pargs != NULL) {
	        RELEASE_PROMARGS(pargs);
	    }
	}
	if (TYPEOF(CAR(call)) == SYMSXP)
	    pt = Rf_strrchr(CHAR(PRINTNAME(CAR(call))), '.');
	else
	    pt = NULL;

	if (pt == NULL || strcmp(pt,".default")) {
	    RCNTXT cntxt;
	    SEXP pargs, rho1;
	    PROTECT(pargs = PROMISE_ARGS(args, rho)); nprotect++;
	    /* The context set up here is needed because of the way
	       usemethod() is written.  DispatchGroup() repeats some
	       internal usemethod() code and avoids the need for a
	       context; perhaps the usemethod() code should be
	       refactored so the contexts around the usemethod() calls
	       in this file can be removed.

	       Using rho for current and calling environment can be
	       confusing for things like sys.parent() calls captured
	       in promises (Gabor G had an example of this).  Also,
	       since the context is established without a SETJMP using
	       an R-accessible environment allows a segfault to be
	       triggered (by something very obscure, but still).
	       Hence here and in the other usemethod() uses below a
	       new environment rho1 is created and used.  LT */
	    PROTECT(rho1 = NewEnvironment(R_NilValue, R_NilValue, rho)); nprotect++;
	    SET_PRVALUE_IF_PROMISE(CAR(pargs), x);
	    begincontext(&cntxt, CTXT_RETURN, call, rho1, rho, pargs, op);
	    if(usemethod(genericNativeCharSXP, x, call, pargs, rho1, rho, R_BaseEnv, ans))
	    {
		endcontext(&cntxt);
		UNPROTECT(nprotect);
		RELEASE_PROMARGS(pargs);
		return 1;
	    }
	    endcontext(&cntxt);
	    DECREMENT_REFCNT(x);
	    RELEASE_PROMARGS(pargs);
	}
    }
    if(!argsevald) {
	if (dots)
	    /* The first call argument was ... and may contain more than the
	       object, so it needs to be evaluated here.  The object should be
	       in a promise, so evaluating it again should be no problem. */
	    *ans = evalArgs(args, rho, dropmissing, call, 0);
	else {
	    PROTECT(*ans = CONS_NR(x, evalArgs(CDR(args), rho, dropmissing, call, 1))); /* FIXME: use promargs stack? */
	    SET_TAG(*ans, CreateTag(TAG(args)));
	    UNPROTECT(1);
	}
    }
    else *ans = args;
    UNPROTECT(nprotect);
    return 0;
}


/* gr needs to be protected on return from this function */
static void findmethod(SEXP Class, SEXP groupNativeCharSXP, SEXP genericNativeCharSXP,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       SEXP rho)
{
    int len, whichclass;
    const void *vmax = vmaxget();

    len = length(Class);

    /* Need to interleave looking for group and generic methods
       e.g. if class(x) is c("foo", "bar)" then x > 3 should invoke
       "Ops.foo" rather than ">.bar"
    */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	const char *ss = translateChar(STRING_ELT(Class, whichclass));
	*meth = installS3MethodSignatureNativeCharSXP(genericNativeCharSXP, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = mkString("");
	    break;
	}
	*meth = installS3MethodSignatureNativeCharSXP(groupNativeCharSXP, ss);
	*sxp = R_LookupMethod(*meth, rho, rho, R_BaseEnv);
	if (isFunction(*sxp)) {
	    *gr = ScalarString(groupNativeCharSXP);
	    break;
	}
    }
    vmaxset(vmax);
    *which = whichclass;
}

attribute_hidden
int DispatchGroup(SEXP groupNativeCharSXP, SEXP call, SEXP op, SEXP args, SEXP rho,
		  SEXP *ans)
{
    int i, j, nargs, lwhich, rwhich, set;
    SEXP lclass, s, t, m, lmeth, lsxp, lgr, newrho;
    SEXP rclass, rmeth, rgr, rsxp, value;
    char *pt;
    SEXP genericNativeCharSXP;
    Rboolean useS4 = TRUE, isOps = FALSE;

    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (args != R_NilValue && ! isObject(CAR(args)) &&
	(CDR(args) == R_NilValue || ! isObject(CADR(args))))
	return 0;

    isOps = groupNativeCharSXP == R_OpsCharSXP;

    /* try for formal method */
    if(length(args) == 1 && !IS_S4_OBJECT(CAR(args))) useS4 = FALSE;
    if(length(args) == 2 &&
       !IS_S4_OBJECT(CAR(args)) && !IS_S4_OBJECT(CADR(args))) useS4 = FALSE;
    if(useS4) {
	/* Remove argument names to ensure positional matching */
	if(isOps)
	    for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);
	if(R_has_methods(op) &&
	   (value = R_possible_dispatch(call, op, args, rho, FALSE))) {
	       *ans = value;
	       return 1;
	}
	/* else go on to look for S3 methods */
    }

    /* check whether we are processing the default method */
    if ( isSymbol(CAR(call)) ) {
        SEXP callCharSXP = PRINTNAME(CAR(call));
	const char *cstr = strchr(CHAR(callCharSXP), '.');
	if (cstr && !strcmp(cstr + 1, "default")) {
	    return 0;
	}
    }

    if(isOps)
	nargs = length(args);
    else
	nargs = 1;

    if( nargs == 1 && !isObject(CAR(args)) )
	return 0;

    if(!isObject(CAR(args)) && !isObject(CADR(args)))
	return 0;

    genericNativeCharSXP = PRIMCHARSXP(op);

    lclass = IS_S4_OBJECT(CAR(args)) ? R_data_class2(CAR(args))
      : getClassAttrib(CAR(args));

    if( nargs == 2 )
	rclass = IS_S4_OBJECT(CADR(args)) ? R_data_class2(CADR(args))
      : getClassAttrib(CADR(args));
    else
	rclass = R_NilValue;

    lsxp = R_NilValue; lgr = R_NilValue; lmeth = R_NilValue;
    rsxp = R_NilValue; rgr = R_NilValue; rmeth = R_NilValue;

    findmethod(lclass, groupNativeCharSXP, genericNativeCharSXP, &lsxp, &lgr, &lmeth, &lwhich,
	       rho);
    PROTECT(lgr);
    const void *vmax = vmaxget();
    if(isFunction(lsxp) && IS_S4_OBJECT(CAR(args)) && lwhich > 0
       && isBasicClass(translateChar(STRING_ELT(lclass, lwhich)))) {
	/* This and the similar test below implement the strategy
	 for S3 methods selected for S4 objects.  See ?Methods */
	value = CAR(args);
	if(NAMED(value)) SET_NAMED(value, 2);
	value = R_getS4DataSlot(value, S4SXP); /* the .S3Class obj. or NULL*/
	if(value != R_NilValue) /* use the S3Part as the inherited object */
	    SETCAR(args, value);
    }

    if( nargs == 2 )
	findmethod(rclass, groupNativeCharSXP, genericNativeCharSXP, &rsxp, &rgr, &rmeth,
		   &rwhich, rho);
    else
	rwhich = 0;

    if(isFunction(rsxp) && IS_S4_OBJECT(CADR(args)) && rwhich > 0
       && isBasicClass(translateChar(STRING_ELT(rclass, rwhich)))) {
	value = CADR(args);
	if(NAMED(value)) SET_NAMED(value, 2);
	value = R_getS4DataSlot(value, S4SXP);
	if(value != R_NilValue) SETCADR(args, value);
    }
    vmaxset(vmax);

    PROTECT(rgr);

    if( !isFunction(lsxp) && !isFunction(rsxp) ) {
	UNPROTECT(2);
	return 0; /* no generic or group method so use default*/
    }

    if( lsxp != rsxp ) {
	if ( isFunction(lsxp) && isFunction(rsxp) ) {
	    /* special-case some methods involving difftime */
	    const char *lname = CHAR(PRINTNAME(lmeth)),
		*rname = CHAR(PRINTNAME(rmeth));
	    if( streql(rname, "Ops.difftime") &&
		(streql(lname, "+.POSIXt") || streql(lname, "-.POSIXt") ||
		 streql(lname, "+.Date") || streql(lname, "-.Date")) )
		rsxp = R_NilValue;
	    else if (streql(lname, "Ops.difftime") &&
		     (streql(rname, "+.POSIXt") || streql(rname, "+.Date")) )
		lsxp = R_NilValue;
	    else {
		warning(_("Incompatible methods (\"%s\", \"%s\") for \"%s\""),
			lname, rname, CHAR(genericNativeCharSXP));
		UNPROTECT(2);
		return 0;
	    }
	}
	/* if the right hand side is the one */
	if( !isFunction(lsxp) ) { /* copy over the righthand stuff */
	    lsxp = rsxp;
	    lmeth = rmeth;
	    lgr = rgr;
	    lclass = rclass;
	    lwhich = rwhich;
	}
    }

    /* we either have a group method or a class method */

    PROTECT(newrho = allocSExp(ENVSXP));
    PROTECT(m = allocVector(STRSXP,nargs));
    vmax = vmaxget();
    s = args;
    for (i = 0 ; i < nargs ; i++) {
	t = IS_S4_OBJECT(CAR(s)) ? R_data_class2(CAR(s))
	  : getClassAttrib(CAR(s));
	set = 0;
	if (isString(t)) {
	    for (j = 0 ; j < length(t) ; j++) {
                if (equalCharSXPsWhenTranslated(STRING_ELT(t, j), STRING_ELT(lclass, lwhich))) {
		    SET_STRING_ELT(m, i, PRINTNAME(lmeth));
		    set = 1;
		    break;
		}
	    }
	}
	if( !set )
	    SET_STRING_ELT(m, i, R_BlankString);
	s = CDR(s);
    }
    vmaxset(vmax);

    defineVarAssertNotPresent(R_dot_Method, m, newrho);
    UNPROTECT(1);
    PROTECT(t = ScalarString(genericNativeCharSXP));
    defineVarAssertNotPresent(R_dot_Generic, t, newrho);
    UNPROTECT(1);
    defineVarAssertNotPresent(R_dot_Group, lgr, newrho);
    set = length(lclass) - lwhich;
    PROTECT(t = allocVector(STRSXP, set));
    for(j = 0 ; j < set ; j++ )
	SET_STRING_ELT(t, j, duplicate(STRING_ELT(lclass, lwhich++)));
    defineVarAssertNotPresent(R_dot_Class, t, newrho);
    UNPROTECT(1);
    defineVarAssertNotPresent(R_dot_GenericCallEnv, rho, newrho);
    defineVarAssertNotPresent(R_dot_GenericDefEnv, R_BaseEnv, newrho);

    PROTECT(t = LCONS(lmeth, CDR(call)));

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */

    PROTECT(s = PROMISE_ARGS(CDR(call), rho));
    if (length(s) != length(args))
	error(_("dispatch error in group dispatch"));
    for (m = s ; m != R_NilValue ; m = CDR(m), args = CDR(args) ) {
	SET_PRVALUE_IF_PROMISE(CAR(m), CAR(args));
	/* ensure positional matching for operators */
	if(isOps) SET_TAG(m, R_NilValue);
    }

    *ans = applyClosure(t, lsxp, s, rho, newrho);
    UNPROTECT(5);
    RELEASE_PROMARGS(s);
    return 1;
}

/* start of bytecode section */
static int R_bcVersion = 7;
static int R_bcMinVersion = 6;

static SEXP R_AddSym = NULL;
static SEXP R_SubSym = NULL;
static SEXP R_MulSym = NULL;
static SEXP R_DivSym = NULL;
static SEXP R_ExptSym = NULL;
static SEXP R_SqrtSym = NULL;
static SEXP R_ExpSym = NULL;
static SEXP R_EqSym = NULL;
static SEXP R_NeSym = NULL;
static SEXP R_LtSym = NULL;
static SEXP R_LeSym = NULL;
static SEXP R_GeSym = NULL;
static SEXP R_GtSym = NULL;
static SEXP R_AndSym = NULL;
static SEXP R_OrSym = NULL;
static SEXP R_NotSym = NULL;
static SEXP R_CSym = NULL;

#if defined(__GNUC__) && ! defined(BC_PROFILING) && (! defined(NO_THREADED_CODE))
# define THREADED_CODE
#endif

attribute_hidden
void R_initialize_bcode(void)
{
  R_AddSym = install("+");
  R_SubSym = install("-");
  R_MulSym = install("*");
  R_DivSym = install("/");
  R_ExptSym = install("^");
  R_SqrtSym = install("sqrt");
  R_ExpSym = install("exp");
  R_EqSym = install("==");
  R_NeSym = install("!=");
  R_LtSym = install("<");
  R_LeSym = install("<=");
  R_GeSym = install(">=");
  R_GtSym = install(">");
  R_AndSym = install("&");
  R_OrSym = install("|");
  R_NotSym = install("!");
  R_CSym = install("c");

#ifdef THREADED_CODE
  bcEval(NULL, NULL, FALSE);
#endif
}

enum {
  BCMISMATCH_OP,
  RETURN_OP,
  GOTO_OP,
  BRIFNOT_OP,
  POP_OP,
  DUP_OP,
  PRINTVALUE_OP,
  STARTLOOPCNTXT_OP,
  ENDLOOPCNTXT_OP,
  DOLOOPNEXT_OP,
  DOLOOPBREAK_OP,
  STARTFOR_OP,
  STEPFOR_OP,
  ENDFOR_OP,
  SETLOOPVAL_OP,
  INVISIBLE_OP,
  LDCONST_OP,
  LDNULL_OP,
  LDTRUE_OP,
  LDFALSE_OP,
  GETVAR_OP,
  DDVAL_OP,
  SETVAR_OP,
  GETFUN_OP,
  GETGLOBFUN_OP,
  GETSYMFUN_OP,
  GETBUILTIN_OP,
  GETINTLBUILTIN_OP,
  CHECKFUN_OP,
  MAKEPROM_OP,
  DOMISSING_OP,
  SETTAG_OP,
  DODOTS_OP,
  PUSHARG_OP,
  PUSHCONSTARG_OP,
  PUSHNULLARG_OP,
  PUSHTRUEARG_OP,
  PUSHFALSEARG_OP,
  CALL_OP,
  CALLBUILTIN_OP,
  CALLSPECIAL_OP,
  MAKECLOSURE_OP,
  UMINUS_OP,
  UPLUS_OP,
  ADD_OP,
  SUB_OP,
  MUL_OP,
  DIV_OP,
  EXPT_OP,
  SQRT_OP,
  EXP_OP,
  EQ_OP,
  NE_OP,
  LT_OP,
  LE_OP,
  GE_OP,
  GT_OP,
  AND_OP,
  OR_OP,
  NOT_OP,
  DOTSERR_OP,
  STARTASSIGN_OP,
  ENDASSIGN_OP,
  STARTSUBSET_OP,
  DFLTSUBSET_OP,
  STARTSUBASSIGN_OP,
  DFLTSUBASSIGN_OP,
  STARTC_OP,
  DFLTC_OP,
  STARTSUBSET2_OP,
  DFLTSUBSET2_OP,
  STARTSUBASSIGN2_OP,
  DFLTSUBASSIGN2_OP,
  DOLLAR_OP,
  DOLLARGETS_OP,
  ISNULL_OP,
  ISLOGICAL_OP,
  ISINTEGER_OP,
  ISDOUBLE_OP,
  ISCOMPLEX_OP,
  ISCHARACTER_OP,
  ISSYMBOL_OP,
  ISOBJECT_OP,
  ISNUMERIC_OP,
  VECSUBSET_OP,
  MATSUBSET_OP,
  SETVECSUBSET_OP,
  SETMATSUBSET_OP,
  AND1ST_OP,
  AND2ND_OP,
  OR1ST_OP,
  OR2ND_OP,
  GETVAR_MISSOK_OP,
  DDVAL_MISSOK_OP,
  VISIBLE_OP,
  SETVAR2_OP,
  STARTASSIGN2_OP,
  ENDASSIGN2_OP,
  SETTER_CALL_OP,
  GETTER_CALL_OP,
  SWAP_OP,
  DUP2ND_OP,
  SWITCH_OP,
  RETURNJMP_OP,
  MAKEGETVARPROM_OP,
  PUSHNULLEARG_OP,
  PUSHTRUEEARG_OP,
  PUSHFALSEEARG_OP,
  PUSHCONSTEARG_OP,
  CALLBUILTINEARG0_OP,
  CALLBUILTINEARG1_OP,
  CALLBUILTINEARG2_OP,
  CALLBUILTINEARG3_OP,
  CALLBUILTINEARG4_OP,
  CALLBUILTINEARG5_OP,
  CALLBUILTINEARG6_OP,
  CALLBUILTINEARG7_OP,
  GETBUILTINEARG0_OP,
  GETBUILTINEARG1_OP,
  GETBUILTINEARG2_OP,
  GETBUILTINEARG3_OP,
  GETBUILTINEARG4_OP,
  GETBUILTINEARG5_OP,
  GETBUILTINEARG6_OP,
  GETBUILTINEARG7_OP,
  GETINTLBUILTINEARG0_OP,
  GETINTLBUILTINEARG1_OP,
  GETINTLBUILTINEARG2_OP,
  GETINTLBUILTINEARG3_OP,
  GETINTLBUILTINEARG4_OP,
  GETINTLBUILTINEARG5_OP,
  GETINTLBUILTINEARG6_OP,
  GETINTLBUILTINEARG7_OP,  
  PUSHEARG_OP,
  MAKEPROMEARG_OP,
  MAKEGETVARPROMEARG_OP,
  CALLEARG_OP,
  GETFUNEARG_OP,
  CHECKFUNEARG_OP,
  STARTVECSUBSET_OP,
  STARTMATSUBSET_OP,
  STARTSETVECSUBSET_OP,
  STARTSETMATSUBSET_OP,
  OPCOUNT
};


SEXP R_unary(SEXP, SEXP, SEXP);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
SEXP do_math1(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2_dflt(SEXP, SEXP, SEXP, SEXP);

#define GETSTACK_PTR(s) (*(s))
#define GETSTACK(i) GETSTACK_PTR(R_BCNodeStackTop + (i))

#define SETSTACK_PTR(s, v) do { \
    SEXP __v__ = (v); \
    *(s) = __v__; \
} while (0)

#define SETSTACK(i, v) SETSTACK_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_REAL_PTR(s, v) SETSTACK_PTR(s, ScalarReal(v))

#define SETSTACK_REAL(i, v) SETSTACK_REAL_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_INTEGER_PTR(s, v) SETSTACK_PTR(s, ScalarInteger(v))

#define SETSTACK_INTEGER(i, v) SETSTACK_INTEGER_PTR(R_BCNodeStackTop + (i), v)

#define SETSTACK_LOGICAL_PTR(s, v) do { \
    int __ssl_v__ = (v); \
    if (__ssl_v__ == NA_LOGICAL) \
	SETSTACK_PTR(s, ScalarLogical(NA_LOGICAL)); \
    else \
	SETSTACK_PTR(s, __ssl_v__ ? R_TrueValue : R_FalseValue); \
} while(0)

#define SETSTACK_LOGICAL(i, v) SETSTACK_LOGICAL_PTR(R_BCNodeStackTop + (i), v)

typedef union { double dval; int ival; } scalar_value_t;

/* bcStackScalar() checks whether the object in the specified stack
   location is a simple real, integer, or logical scalar (i.e. length
   one and no attributes.  If so, the type is returned as the function
   value and the value is returned in the structure pointed to by the
   second argument; if not, then zero is returned as the function
   value. */
static R_INLINE int bcStackScalar(R_bcstack_t *s, scalar_value_t *v)
{
    SEXP x = *s;
    if (ATTRIB(x) == R_NilValue) {
	switch(TYPEOF(x)) {
	case REALSXP:
	    if (LENGTH(x) == 1) {
		v->dval = REAL(x)[0];
		return REALSXP;
	    }
	    else return 0;
	case INTSXP:
	    if (LENGTH(x) == 1) {
		v->ival = INTEGER(x)[0];
		return INTSXP;
	    }
	    else return 0;
	case LGLSXP:
	    if (LENGTH(x) == 1) {
		v->ival = LOGICAL(x)[0];
		return LGLSXP;
	    }
	    else return 0;
	default: return 0;
	}
    }
    else return 0;
}

/*
If enabled, SEXP operands will be directly in the generated byte-code for a
function, rather than accessed indirectly through the constant pool.

Currently this is also done when the constant pool index is not needed for
anything but the lookup.  However, we could possibly store both the index
and the resolved SEXP in the generated bytecode.

RESOLVE_CONST_OP only works with THREADED_CODE
*/

/* 
With current bytecode and (at least) 32 bit machines, this check is not
going to fail.
*/
#define SEXPFAILIFZERO(x) ((x) ? (x) : (error(_("constant SEXP operands do not fit into unsigned bitmask")),0))
#define CONSTOP(i) ((i == 0) ? 0U : SEXPFAILIFZERO(1U << ((unsigned)i-1U)))

#define LABELFAILIFZERO(x) ((x) ? (x) : (error(_("label operands do not fit into unsigned bitmask")),0))
#define LABELOP(i) ((i == 0) ? 0U : LABELFAILIFZERO(1U << ((unsigned)i-1U)))

#define RESOLVE_CONST_OP

#ifdef RESOLVE_CONST_OP

#ifndef THREADED_CODE
#error "RESOLVE_CONST_OP is only supported with THREADED_CODE, but THREADED_CODE is not defined."
#endif

/* #define GETCONSTOP() ( printf("GETCONSTOP: "), PrintValue((*pc).sexp), printf("\n"), (*pc++).sexp ) */

#define GETCONSTOP() ((*pc++).sexp)
#define SETCONSTMAP(lhs, m) lhs = m

#else /* not RESOLVE_CONST_OP */

#define GETCONSTOP() (VECTOR_ELT(constants, GETOP()))
#define SETCONSTMAP(lhs, m)

#endif /* RESOLVE_CONST_OP */

/*
If enabled, label operands will be translated to absolute addresses of instructions.

DIRECT_LABELS only work with THREADED_CODE

Currently, labels for the switch statement are always indirect
*/

#define GETINTOP() ((*pc++).i)

#define DIRECT_LABELS

#ifdef DIRECT_LABELS

#ifndef THREADED_CODE
#error "DIRECT_LABELS is only supported with THREADED_CODE, but THREADED_CODE is not defined."
#endif

#ifndef THREADED_CODE
/* Note that independent activation of these two optimizations could be enabled. */
#error "DIRECT_LABELS is only supported with RESOLVE_CONST_OP, but RESOLVE_CONST_OP is not defined."
#endif

#define GETLABELOP() ((*pc++).label)
#define SETLABELMAP(lhs, m) lhs = m
#define LABEL_TYPE BCODE*
#define PC_FOR_LABEL(l) l

#else /* not DIRECT_LABELS */

#define GETLABELOP GETINTOP
#define SETLABELMAP(lhs, m)
#define LABEL_TYPE int
#define PC_FOR_LABEL(l) (codebase + l)

#endif

#define DO_FAST_RELOP2(op,a,b) do { \
    SKIP_OP(); \
    SETSTACK_LOGICAL(-2, ((a) op (b)) ? TRUE : FALSE);	\
    R_BCNodeStackTop--; \
    NEXT(); \
} while (0)

# define FastRelop2(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
    int typex = bcStackScalar(R_BCNodeStackTop - 2, &vx); \
    int typey = bcStackScalar(R_BCNodeStackTop - 1, &vy); \
    if (typex == REALSXP && ! ISNAN(vx.dval)) { \
	if (typey == REALSXP && ! ISNAN(vy.dval)) \
	    DO_FAST_RELOP2(op, vx.dval, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_RELOP2(op, vx.dval, vy.ival); \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP && ! ISNAN(vy.dval)) \
	    DO_FAST_RELOP2(op, vx.ival, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
	    DO_FAST_RELOP2(op, vx.ival, vy.ival); \
	} \
    } \
    Relop2(opval, opsym); \
} while (0)

static R_INLINE SEXP getPrimitive(SEXP symbol, SEXPTYPE type)
{
    SEXP value = SYMVALUE(symbol);
    if (TYPEOF(value) == PROMSXP) {
	value = forcePromise(value);
	SET_NAMED(value, 2);
    }
    if (TYPEOF(value) != type) {
	/* probably means a package redefined the base function so
	   try to get the real thing from the internal table of
	   primitives */
	value = R_Primitive(CHAR(PRINTNAME(symbol)));
	if (TYPEOF(value) != type)
	    /* if that doesn't work we signal an error */
	    error(_("\"%s\" is not a %s function"),
		  CHAR(PRINTNAME(symbol)),
		  type == BUILTINSXP ? "BUILTIN" : "SPECIAL");
    }
    return value;
}

static SEXP cmp_relop(SEXP call, int opval, SEXP opsym, SEXP x, SEXP y,
		      SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x) || isObject(y)) {
	SEXP args, ans;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	PROTECT(args);
	if (DispatchGroup(R_OpsCharSXP, call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return do_relop_dflt(call, op, x, y);
}

static SEXP cmp_arith1(SEXP call, SEXP opsym, SEXP x, SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (isObject(x)) {
	SEXP args, ans;
	args = CONS_NR(x, R_NilValue);
	PROTECT(args);
	if (DispatchGroup(R_OpsCharSXP, call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return R_unary(call, op, x);
}

static SEXP cmp_arith2(SEXP call, int opval, SEXP opsym, SEXP x, SEXP y,
		       SEXP rho)
{
    SEXP op = getPrimitive(opsym, BUILTINSXP);
    if (TYPEOF(op) == PROMSXP) {
	op = forcePromise(op);
	SET_NAMED(op, 2);
    }
    if (isObject(x) || isObject(y)) {
	SEXP args, ans;
	args = CONS_NR(x, CONS_NR(y, R_NilValue));
	PROTECT(args);
	if (DispatchGroup(R_OpsCharSXP, call, op, args, rho, &ans)) {
	    UNPROTECT(1);
	    return ans;
	}
	UNPROTECT(1);
    }
    return R_binary(call, op, x, y);
}

#define Builtin1(do_fun,which,rho) do { \
  SEXP call = GETCONSTOP(); \
  SEXP args = CREATE_CALLARG_CELL(GETSTACK(-1)); \
  SETSTACK(-1, args);		     \
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP), \
		      args, rho));		     \
  RELEASE_PROMARGS(args); \
  NEXT(); \
} while(0)

#define Builtin2(do_fun,which,rho) do { \
  SEXP call = GETCONSTOP(); \
  SEXP args = CREATE_CALLARG_2CELLS(GETSTACK(-2), GETSTACK(-1)); \
  SETSTACK(-2, args);     \
  R_BCNodeStackTop--; \
  SETSTACK(-1, do_fun(call, getPrimitive(which, BUILTINSXP),	\
		      args, rho));			\
  RELEASE_PROMARGS(args); \
  NEXT(); \
} while(0)

#define NewBuiltin2(do_fun,opval,opsym,rho) do {	\
  SEXP call = GETCONSTOP(); \
  SEXP x = GETSTACK(-2); \
  SEXP y = GETSTACK(-1); \
  SETSTACK(-2, do_fun(call, opval, opsym, x, y,rho));	\
  R_BCNodeStackTop--; \
  NEXT(); \
} while(0)

#define Arith1(opsym) do {		\
  SEXP call = GETCONSTOP(); \
  SEXP x = GETSTACK(-1); \
  SETSTACK(-1, cmp_arith1(call, opsym, x, rho)); \
  NEXT(); \
} while(0)


#define Arith2(opval,opsym) NewBuiltin2(cmp_arith2,opval,opsym,rho)
#define Math1(which) Builtin1(do_math1,which,rho)
#define Relop2(opval,opsym) NewBuiltin2(cmp_relop,opval,opsym,rho)

#ifdef NO_SAVE_ALLOC
# define DO_FAST_BINOP(op,a,b) do { \
    SKIP_OP(); \
    SETSTACK_REAL(-2, (a) op (b)); \
    R_BCNodeStackTop--; \
    NEXT(); \
} while (0)

# define DO_FAST_BINOP_INT(op, a, b) do { \
    double dval = ((double) (a)) op ((double) (b)); \
    if (dval <= INT_MAX && dval >= INT_MIN + 1) { \
	SKIP_OP(); \
	SETSTACK_INTEGER(-2, (int) dval); \
	R_BCNodeStackTop--; \
	NEXT(); \
    } \
} while(0)
#else
/* these reuse one of the two values on the top of the stack if it is
   of the right type and has NAMED = 0. It is known that both of these
   will have length one and have no attributes. */
# define DO_FAST_BINOP(op,a,b) do {					\
	SKIP_OP();							\
	SEXP sa = R_BCNodeStackTop[-2];					\
	SEXP sb = R_BCNodeStackTop[-1];					\
	SEXP ans;							\
	if (NO_REFERENCES(sa) && TYPEOF(sa) == REALSXP) ans = sa;	\
	else if (NO_REFERENCES(sb) && TYPEOF(sb) == REALSXP) ans = sb;	\
	else ans = allocVector(REALSXP, 1);				\
	REAL(ans)[0] = (a) op (b);					\
	SETSTACK(-2, ans);						\
	R_BCNodeStackTop--;						\
	NEXT();								\
    } while (0)

# define DO_FAST_BINOP_INT(op, a, b) do { \
	double dval = ((double) (a)) op ((double) (b)); \
	if (dval <= INT_MAX && dval >= INT_MIN + 1) {	\
	    SKIP_OP();							\
	    SEXP sa = R_BCNodeStackTop[-2];				\
	    SEXP sb = R_BCNodeStackTop[-1];				\
	    SEXP ans;							\
	    if (NO_REFERENCES(sa) && TYPEOF(sa) == INTSXP) ans = sa;	\
	    else if (NO_REFERENCES(sb) && TYPEOF(sb) == INTSXP) ans = sb; \
	    else ans = allocVector(INTSXP, 1);				\
	    INTEGER(ans)[0] = (int) dval;				\
	    SETSTACK(-2, ans);						\
	    R_BCNodeStackTop--;						\
	    NEXT();							\
	}								\
    } while(0)
#endif

# define FastBinary(op,opval,opsym) do { \
    scalar_value_t vx; \
    scalar_value_t vy; \
    int typex = bcStackScalar(R_BCNodeStackTop - 2, &vx); \
    int typey = bcStackScalar(R_BCNodeStackTop - 1, &vy); \
    if (typex == REALSXP) { \
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.dval, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) \
	    DO_FAST_BINOP(op, vx.dval, vy.ival); \
    } \
    else if (typex == INTSXP && vx.ival != NA_INTEGER) { \
	if (typey == REALSXP) \
	    DO_FAST_BINOP(op, vx.ival, vy.dval); \
	else if (typey == INTSXP && vy.ival != NA_INTEGER) { \
	    if (opval == DIVOP) \
		DO_FAST_BINOP(op, (double) vx.ival, (double) vy.ival); \
	    else \
		DO_FAST_BINOP_INT(op, vx.ival, vy.ival); \
	} \
    } \
    Arith2(opval, opsym); \
} while (0)

#define BCNPUSH(v) do { \
  SEXP __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
  if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
  __ntop__[-1] = __value__; \
  R_BCNodeStackTop = __ntop__; \
} while (0)

#define BCNPUSH_NOCHECK(v) do { \
  SEXP __value__ = (v); \
  R_bcstack_t *__ntop__ = R_BCNodeStackTop; \
  *__ntop__ = __value__; \
  R_BCNodeStackTop = __ntop__ + 1; \
} while (0)

#define BCNDUP() do { \
    R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
    if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
    __ntop__[-1] = __ntop__[-2]; \
    R_BCNodeStackTop = __ntop__; \
} while(0)

#define BCNDUP2ND() do { \
    R_bcstack_t *__ntop__ = R_BCNodeStackTop + 1; \
    if (__ntop__ > R_BCNodeStackEnd) nodeStackOverflow(); \
    __ntop__[-1] = __ntop__[-3]; \
    R_BCNodeStackTop = __ntop__; \
} while(0)

#define BCNPOP() (R_BCNodeStackTop--, GETSTACK(0))
#define BCNPOP_IGNORE_VALUE() R_BCNodeStackTop--

#define BCNSTACKCHECK(n)  do { \
  if (R_BCNodeStackTop + 1 > R_BCNodeStackEnd) nodeStackOverflow(); \
} while (0)

#define BCIPUSHPTR(v)  do { \
  void *__value__ = (v); \
  IStackval *__ntop__ = R_BCIntStackTop + 1; \
  if (__ntop__ > R_BCIntStackEnd) intStackOverflow(); \
  *__ntop__[-1].p = __value__; \
  R_BCIntStackTop = __ntop__; \
} while (0)

#define BCIPUSHINT(v)  do { \
  int __value__ = (v); \
  IStackval *__ntop__ = R_BCIntStackTop + 1; \
  if (__ntop__ > R_BCIntStackEnd) intStackOverflow(); \
  __ntop__[-1].i = __value__; \
  R_BCIntStackTop = __ntop__; \
} while (0)

#define BCIPOPPTR() ((--R_BCIntStackTop)->p)
#define BCIPOPINT() ((--R_BCIntStackTop)->i)

#define BCCONSTS(e) BCODE_CONSTS(e)

static void nodeStackOverflow()
{
    error(_("node stack overflow"));
}

#ifdef BC_INT_STACK
static void intStackOverflow()
{
    error(_("integer stack overflow"));
}
#endif

static SEXP bytecodeExpr(SEXP e)
{
    if (isByteCode(e)) {
	if (LENGTH(BCCONSTS(e)) > 0)
	    return VECTOR_ELT(BCCONSTS(e), 0);
	else return R_NilValue;
    }
    else return e;
}

SEXP R_PromiseExpr(SEXP p)
{
    return bytecodeExpr(PRCODE(p));
}

SEXP R_ClosureExpr(SEXP p)
{
    return bytecodeExpr(BODY(p));
}
#define DUMPSTATE() dumpInterpreterState(codebase, pc, LENGTH(BCODE_CODE(body)) - 1, oldntop, R_BCNodeStackTop)

#ifdef THREADED_CODE

#ifdef RESOLVE_CONST_OP
  #ifdef DIRECT_LABELS
    typedef union bcode { void *v; int i; SEXP sexp; union bcode* label;} BCODE;
    static struct { void *addr; int argc; unsigned sexpmap; unsigned labelmap; char *instname; } opinfo[OPCOUNT];
  #else
    typedef union { void *v; int i; SEXP sexp;} BCODE;
    static struct { void *addr; int argc; unsigned sexpmap; char *instname; } opinfo[OPCOUNT];
  #endif
#else /* not RESOLVE_CONST_OP */
  typedef union { void *v; int i; } BCODE;
  static struct { void *addr; int argc; char* instname; } opinfo[OPCOUNT];
#endif

//#define DEBUG_INST(name) fprintf(stderr, "DEBUG: Added instruction %s (%d) argc=%d instname=%s\n", #name, name##_OP, opinfo[name##_OP].argc, opinfo[name##_OP].instname)
#define DEBUG_INST(name)

#define OP(name,n,cmask,lmask) \
  case name##_OP: opinfo[name##_OP].addr = (__extension__ &&op_##name); \
    SETCONSTMAP(opinfo[name##_OP].sexpmap, cmask); \
    SETLABELMAP(opinfo[name##_OP].labelmap, lmask); \
    opinfo[name##_OP].argc = (n); \
    opinfo[name##_OP].instname = #name; \
    DEBUG_INST(name); \
    goto loop; \
    op_##name

#define BEGIN_MACHINE  NEXT(); init: { loop: switch(which++)
#define LASTOP } value = R_NilValue; if (which != OPCOUNT + 1) error("some instructions are not implemented"); goto done
#define INITIALIZE_MACHINE() if (body == NULL) goto init

#define NEXT() (__extension__ ({goto *(*pc++).v;}))
/* #define GETOP() (  ((*pc).i > 100000) ? (printf("SEXP "), PrintValue((*pc).sexp), printf("\n")) : 0, (*pc++).i ) */
#define GETOP() (*pc++).i
#define SKIP_OP() (pc++)

#define BCCODE(e) (BCODE *) INTEGER(BCODE_CODE(e))
#else
typedef int BCODE;

#define OP(name,argc,sexpmap) case name##_OP

#ifdef BC_PROFILING
#define BEGIN_MACHINE  loop: current_opcode = *pc; switch(*pc++)
#else
#define BEGIN_MACHINE  loop: switch(*pc++)
#endif
#define LASTOP  default: error(_("bad opcode"))
#define INITIALIZE_MACHINE()

#define NEXT() goto loop
#define GETOP() *pc++
#define SKIP_OP() (pc++)

#define BCCODE(e) INTEGER(BCODE_CODE(e))
#endif

static R_INLINE SEXP BINDING_VALUE(SEXP loc)
{
    if (loc != R_NilValue && ! IS_ACTIVE_BINDING(loc))
	return CAR(loc);
    else
	return R_UnboundValue;
}

#define BINDING_SYMBOL(loc) TAG(loc)

/* Defining USE_BINDING_CACHE enables a cache for GETVAR, SETVAR, and
   others to more efficiently locate bindings in the top frame of the
   current environment.  The index into of the symbol in the constant
   table is used as the cache index.  Two options can be used to chose
   among implementation strategies:

       If CACHE_ON_STACK is defined the the cache is allocated on the
       byte code stack. Otherwise it is allocated on the heap as a
       VECSXP.  The stack-based approach is more efficient, but runs
       the risk of running out of stack space.

       If CACHE_MAX is defined, then a cache of at most that size is
       used. The value must be a power of 2 so a modulus computation x
       % CACHE_MAX can be done as x & (CACHE_MAX - 1). More than 90%
       of the closures in base have constant pools with fewer than 128
       entries when compiled, to that is a good value to use.

   On average about 1/3 of constant pool entries are symbols, so this
   approach wastes some space.  This could be avoided by grouping the
   symbols at the beginning of the constant pool and recording the
   number.
   
   TK: I've implemented this. The first element of the constant pool is
   still the expression (function body).  Following are symbols guessed to
   be local variables (which includes all symbols that may be referenced by
   instructions that use the cache).  Following are other constants.  The
   last element of the constant pool is the number of symbols guessed to be
   local variables.  The cache size is this number + 1 (the first element of
   the cache left unused).

   Bindings recorded may become invalid if user code removes a
   variable.  The code in envir.c has been modified to insert
   R_unboundValue as the value of a binding when it is removed, and
   code using cached bindings checks for this.

   It would be nice if we could also cache bindings for variables
   found in enclosing environments. These would become invalid if a
   new variable is defined in an intervening frame. Some mechanism for
   invalidating the cache would be needed. This is certainly possible,
   but finding an efficient mechanism does not seem to be easy.   LT */

/* Both mechanisms implemented here make use of the stack to hold
   cache information.  This is not a problem except for "safe" for()
   loops using the STARTLOOPCNTXT instruction to run the body in a
   separate bcEval call.  Since this approach expects loop setup
   information to be passed on the stack from the outer bcEval call to
   an inner one the inner one cannot put things on the stack. For now,
   bcEval takes an additional argument that disables the cache in
   calls via STARTLOOPCNTXT for all "safe" loops. It would be better
   to deal with this in some other way, for example by having a
   specific STARTFORLOOPCNTXT instruction that deals with transferring
   the information in some other way. For now disabling the cache is
   an expedient solution. LT */

#define USE_BINDING_CACHE
# ifdef USE_BINDING_CACHE
/* CACHE_MAX must be a power of 2 for modulus using & CACHE_MASK to work*/
# define CACHE_MAX 128
# ifdef CACHE_MAX
#  define CACHE_MASK (CACHE_MAX - 1)
#  define CACHEIDX(i) ((i) & CACHE_MASK)
# else
#  define CACHEIDX(i) (i)
# endif

# define CACHE_ON_STACK
# ifdef CACHE_ON_STACK
typedef R_bcstack_t * R_binding_cache_t;
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? vcache[CACHEIDX(sidx)] : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? vcache[sidx] : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) vcache[CACHEIDX(sidx)] = (cell); } while (0)
# else
typedef SEXP R_binding_cache_t;
#  define GET_CACHED_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, CACHEIDX(sidx)) : R_NilValue)
#  define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) \
    (vcache ? VECTOR_ELT(vcache, sidx) : R_NilValue)

#  define SET_CACHED_BINDING(vcache, sidx, cell) \
    do { if (vcache) SET_VECTOR_ELT(vcache, CACHEIDX(sidx), cell); } while (0)
# endif
#else
typedef void *R_binding_cache_t;
# define GET_CACHED_BINDING_CELL(vcache, sidx) R_NilValue
# define GET_SMALLCACHE_BINDING_CELL(vcache, sidx) R_NilValue

# define SET_CACHED_BINDING(vcache, sidx, cell)
#endif

static R_INLINE SEXP GET_BINDING_CELL_CACHE(SEXP symbol, SEXP rho,
					    R_binding_cache_t vcache, int idx)
{
    SEXP cell = GET_CACHED_BINDING_CELL(vcache, idx);
    /* The value returned by GET_CACHED_BINDING_CELL is either a
       binding cell or R_NilValue.  TAG(R_NilValue) is R_NilVelue, and
       that will no equal symbol. So a separate test for cell !=
       R_NilValue is not needed. */
    if (TAG(cell) == symbol && CAR(cell) != R_UnboundValue)
	return cell;
    else {
	SEXP ncell = GET_BINDING_CELL(symbol, rho);
	if (ncell != R_NilValue)
	    SET_CACHED_BINDING(vcache, idx, ncell);
	else if (cell != R_NilValue && CAR(cell) == R_UnboundValue)
	    SET_CACHED_BINDING(vcache, idx, R_NilValue);
	return ncell;
    }
}

static void MISSING_ARGUMENT_ERROR(SEXP symbol)
{
    const char *n = CHAR(PRINTNAME(symbol));
    if(*n) error(_("argument \"%s\" is missing, with no default"), n);
    else error(_("argument is missing, with no default"));
}

#define MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss) \
    do { if (! keepmiss) MISSING_ARGUMENT_ERROR(symbol); } while (0)

static void UNBOUND_VARIABLE_ERROR(SEXP symbol)
{
    error(_("object '%s' not found"), EncodeChar(PRINTNAME(symbol)));
}

static R_INLINE SEXP FORCE_PROMISE(SEXP value, SEXP symbol, SEXP rho,
				   Rboolean keepmiss)
{
    if (PRVALUE(value) == R_UnboundValue) {
	/**** R_isMissing is inefficient */
	if (keepmiss && R_isMissing(symbol, rho))
	    value = R_MissingArg;
	else value = forcePromise(value);
    }
    else value = PRVALUE(value);
    SET_NAMED(value, 2);
    return value;
}

static R_INLINE SEXP FIND_VAR_NO_CACHE(SEXP symbol, SEXP rho, SEXP cell)
{
    SEXP value;
    /* only need to search the current frame again if
       binding was special or frame is a base frame */
    if (cell != R_NilValue ||
	rho == R_BaseEnv || rho == R_BaseNamespace)
	value =  findVar(symbol, rho);
    else
	value =  findVar(symbol, ENCLOS(rho));
    return value;
}

static R_INLINE SEXP getvar(SEXP symbol, SEXP rho,
			    Rboolean dd, Rboolean keepmiss,
			    R_binding_cache_t vcache, int sidx)
{
    SEXP value;
    if (dd)
	value = ddfindVar(symbol, rho);
    else if (vcache != NULL) {
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = BINDING_VALUE(cell);
	if (value == R_UnboundValue)
	    value = FIND_VAR_NO_CACHE(symbol, rho, cell);
    }
    else
	value = findVar(symbol, rho);

    if (value == R_UnboundValue)
	UNBOUND_VARIABLE_ERROR(symbol);
    else if (value == R_MissingArg)
	MAYBE_MISSING_ARGUMENT_ERROR(symbol, keepmiss);
    else if (TYPEOF(value) == PROMSXP)
	value = FORCE_PROMISE(value, symbol, rho, keepmiss);
    else if (NAMED(value) == 0 && value != R_NilValue)
	SET_NAMED(value, 1);
    return value;
}

#ifdef USE_PROMARGS_STACK
  /* 
    Initially this definition has been used assuming that BUILTINS could possibly leak their arguments/promargs.
    But using ftype is not reliable for this in case of calls to builtins compiled to use GETBUILTIN, CALLBUILTIN.
    There may be call of another type while a call to a builtin is in progress.

    #define CREATE_CALLARG_CELL(v) ((ftype == BUILTINSXP) ? CONS_NR(v,R_NilValue) : allocatePromargsCellNoTag(v))
  */
  #define CREATE_CALLARG_CELL(v) allocatePromargsCellNoTag(v)
  #define CREATE_CALLARG_2CELLS(u,v) allocatePromargsTuple(u, v)
  #define CREATE_CALLARG_3CELLS(u,v,w) allocatePromargsTriple(u, v, w)
  #define CREATE_CALLARG_3VCELLS(u,v,w) allocatePromargsTripleLastValue(u, v, w)
  #define CREATE_CALLARG_4VCELLS(u,v,w,x) allocatePromargsQuadrupleLastValue(u, v, w, x)

#else /* not USE_PROMARGS_STACK */
  #define CREATE_CALLARG_CELL(v) CONS_NR(v, R_NilValue)
  #define CREATE_CALLARG_2CELLS(u,v) allocateCallargTuple(u, v)
  #define CREATE_CALLARG_3CELLS(u,v,w) allocateCallargTriple(u, v, w)
  #define CREATE_CALLARG_3VCELLS(u,v,w) allocateCallargTripleLastValue(u, v, w)
  #define CREATE_CALLARG_4VCELLS(u,v,w,x) allocateCallargQuadrupleLastValue(u, v, w, x)
  
#endif

#define PUSHCALLARG(v) PUSHCALLARG_CELL(CREATE_CALLARG_CELL(v))

#define PUSHCALLARG_CELL(c) do { \
  SEXP __cell__ = (c); \
  if (GETSTACK(-2) == R_NilValue) SETSTACK(-2, __cell__); \
  else SETCDR(GETSTACK(-1), __cell__); \
  SETSTACK(-1, __cell__);	       \
} while (0)

#define PUSHCALLEARG_NOTSPECIAL(v) BCNPUSH_NOCHECK(v)
#define PUSHCALLEARG(v) do { \
    if (ftype != SPECIALSXP) { \
        PUSHCALLEARG_NOTSPECIAL(v); \
    } \
} while(0)

#define INLINE_GETVAR
#ifdef INLINE_GETVAR
/* Try to handle the most common case as efficiently as possible.  If
   smallcache is true then a modulus operation on the index is not
   needed, nor is a check that a non-null value corresponds to the
   requested symbol. The symbol from the constant pool is also usually
   not needed. The test TYPOF(value) != SYMBOL rules out R_MissingArg
   and R_UnboundValue as these are implemented s symbols.  It also
   rules other symbols, but as those are rare they are handled by the
   getvar() call. */
#define DO_GETVAR_COMMON(dd,keepmiss,__fn_result) do { \
    int sidx = GETOP(); \
    if (!dd && smallcache) { \
	SEXP cell = GET_SMALLCACHE_BINDING_CELL(vcache, sidx); \
	/* try fast handling of REALSXP, INTSXP, LGLSXP */ \
	/* (cell won't be R_NilValue or an active binding) */ \
	value = CAR(cell); \
	int type = TYPEOF(value); \
	switch(type) { \
	case REALSXP: \
	case INTSXP: \
	case LGLSXP: \
	    /* may be ok to skip this test: */ \
	    if (NAMED(value) == 0) \
		SET_NAMED(value, 1); \
	    R_Visible = TRUE; \
	    __fn_result(value); \
	    NEXT(); \
	} \
	if (cell != R_NilValue && ! IS_ACTIVE_BINDING(cell)) { \
	    if (type != SYMSXP) {	\
		if (type == PROMSXP) {		\
		    SEXP pv = PRVALUE(value);		\
		    if (pv == R_UnboundValue) {		\
			SEXP symbol = VECTOR_ELT(constants, sidx);	\
			value = FORCE_PROMISE(value, symbol, rho, keepmiss); \
		    }							\
		    else value = pv;					\
		}							\
		else if (NAMED(value) == 0)				\
		    SET_NAMED(value, 1);				\
		R_Visible = TRUE;					\
		__fn_result(value);						\
		NEXT();							\
	    }								\
	}								\
    }									\
    SEXP symbol = VECTOR_ELT(constants, sidx);				\
    R_Visible = TRUE;							\
    __fn_result(getvar(symbol, rho, dd, keepmiss, vcache, sidx));		\
    NEXT();								\
} while (0)

#else /* not INLINE_GETVAR */

#define DO_GETVAR_COMMON(dd,keepmiss,__fn_result) do { \
  int sidx = GETOP(); \
  SEXP symbol = VECTOR_ELT(constants, sidx); \
  R_Visible = TRUE; \
  __fn_result(getvar(symbol, rho, dd, keepmiss, vcache, sidx));	\
  NEXT(); \
} while (0)

#endif

#define DO_GETVAR(dd, keepmiss) DO_GETVAR_COMMON(dd, keepmiss, BCNPUSH)
#define DO_GETVAR_PUSHCALLARG(dd, keepmiss) DO_GETVAR_COMMON(dd, keepmiss, PUSHCALLARG)
#define DO_GETVAR_PUSHCALLEARG_NOTSPECIAL(dd, keepmiss) DO_GETVAR_COMMON(dd, keepmiss, PUSHCALLEARG_NOTSPECIAL)

#define EARG_CALLBUILTIN(nargs, PRIMCALL) do { \
  SEXP fun = GETSTACK(-1-nargs); \
  SEXP call = GETCONSTOP(); \
  int flag; \
  const void *vmax = vmaxget(); \
  if (TYPEOF(fun) != BUILTINSXP) \
    error(_("not a BUILTIN function (CALLBUILTINEARG)")); \
  flag = PRIMPRINT(fun); \
  R_Visible = flag != 1; \
  if (R_Profiling && IS_TRUE_BUILTIN(fun)) { \
    RCNTXT cntxt; \
    SEXP oldref = R_Srcref; \
    begincontext(&cntxt, CTXT_BUILTIN, call,R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue); \
    R_Srcref = NULL; \
    value = PRIMCALL; \
    R_Srcref = oldref; \
    endcontext(&cntxt); \
  } else { \
    value = PRIMCALL; \
  } \
  if (flag < 2) R_Visible = flag != 1; \
  vmaxset(vmax); \
  R_BCNodeStackTop -= nargs; \
  SETSTACK(-1, value); \
  NEXT(); \
} while(0)

#define DO_GETFUN_COMMON(GETVALUE_ACTION, STACKINIT_ACTION) do { \
  /* get the function */ \
  SEXP symbol = GETCONSTOP(); \
  GETVALUE_ACTION; \
  if(RTRACE(value)) { \
    Rprintf("trace: "); \
    PrintValue(symbol); \
  } \
 \
  /* initialize the function type register */ \
  ftype = TYPEOF(value); \
 \
  /* the original version here would push space for creating the argument list. */ \
  STACKINIT_ACTION; \
  NEXT(); \
} while(0);

                                   

static int tryDispatch(SEXP genericSymbol, SEXP call, SEXP x, SEXP rho, SEXP *pv)
{
  RCNTXT cntxt;
  SEXP pargs, rho1;
  int dispatched = FALSE;
  SEXP op = SYMVALUE(genericSymbol);

  PROTECT(pargs = PROMISE_ARGS(CDR(call), rho));
  SET_PRVALUE_IF_PROMISE(CAR(pargs), x);

  /**** Minimal hack to try to handle the S4 case.  If we do the check
	and do not dispatch then some arguments beyond the first might
	have been evaluated; these will then be evaluated again by the
	compiled argument code. */
  if (IS_S4_OBJECT(x) && R_has_methods(op)) {
    SEXP val = R_possible_dispatch(call, op, pargs, rho, TRUE);
    if (val) {
      *pv = val;
      UNPROTECT(1);
      RELEASE_PROMARGS(pargs);
      return TRUE;
    }
  }

  /* See comment at first usemethod() call in this file. LT */
  PROTECT(rho1 = NewEnvironment(R_NilValue, R_NilValue, rho));
  begincontext(&cntxt, CTXT_RETURN, call, rho1, rho, pargs, op);
  if (usemethod(PRINTNAME(genericSymbol), x, call, pargs, rho1, rho, R_BaseEnv, pv))
    dispatched = TRUE;
  endcontext(&cntxt);
  UNPROTECT(2);
  RELEASE_PROMARGS(pargs);
  if (! dispatched) DECREMENT_REFCNT(x);
  return dispatched;
}

static int tryAssignDispatch(SEXP genericSymbol, SEXP call, SEXP lhs, SEXP rhs,
			     SEXP rho, SEXP *pv)
{
    int result;
    SEXP ncall, last, prom;

    PROTECT(ncall = duplicate(call));
    last = ncall;
    while (CDR(last) != R_NilValue)
	last = CDR(last);
    prom = mkRHSPROMISE(CAR(last), rhs);
    SETCAR(last, prom);
    result = tryDispatch(genericSymbol, ncall, lhs, rho, pv);
    UNPROTECT(1);
    return result;
}

#define DO_STARTDISPATCH(generic) do { \
  SEXP call = GETCONSTOP(); \
  LABEL_TYPE label = GETLABELOP(); \
  value = GETSTACK(-1); \
  if (isObject(value) && tryDispatch(generic, call, value, rho, &value)) {\
    SETSTACK(-1, value);						\
    BC_CHECK_SIGINT(); \
    pc = PC_FOR_LABEL(label); \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
    SEXP cell = CREATE_CALLARG_CELL(value); \
    BCNSTACKCHECK(3); \
    SETSTACK(0, call); \
    SETSTACK(1, cell); \
    SETSTACK(2, cell); \
    R_BCNodeStackTop += 3; \
    if (tag != R_NilValue) \
      SET_TAG(cell, CreateTag(tag)); \
  } \
  NEXT(); \
} while (0)

#define DO_DFLTDISPATCH(fun, symbol) do { \
  SEXP call = GETSTACK(-3); \
  SEXP args = GETSTACK(-2); \
  value = fun(call, symbol, args, rho); \
  RELEASE_PROMARGS(args); \
  R_BCNodeStackTop -= 3; \
  SETSTACK(-1, value); \
  NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH(generic) do { \
  SEXP call = GETCONSTOP(); \
  LABEL_TYPE label = GETLABELOP(); \
  SEXP lhs = GETSTACK(-2); \
  SEXP rhs = GETSTACK(-1); \
  if (MAYBE_SHARED(lhs)) { \
    lhs = shallow_duplicate(lhs); \
    SETSTACK(-2, lhs); \
    SET_NAMED(lhs, 1); \
  } \
  if (isObject(lhs) && \
      tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
    R_BCNodeStackTop--;	\
    SETSTACK(-1, value); \
    BC_CHECK_SIGINT(); \
    pc = PC_FOR_LABEL(label); \
  } \
  else { \
    SEXP tag = TAG(CDR(call)); \
    SEXP cell = CREATE_CALLARG_CELL(lhs); \
    BCNSTACKCHECK(3); \
    SETSTACK(0, call); \
    SETSTACK(1, cell); \
    SETSTACK(2, cell); \
    R_BCNodeStackTop += 3; \
    if (tag != R_NilValue) \
      SET_TAG(cell, CreateTag(tag)); \
  } \
  NEXT(); \
} while (0)

#define DO_DFLT_ASSIGN_DISPATCH(fun, symbol) do { \
  SEXP rhs = GETSTACK(-4); \
  SEXP call = GETSTACK(-3); \
  SEXP args = GETSTACK(-2); \
  PUSHCALLARG(rhs); \
  value = fun(call, symbol, args, rho); \
  RELEASE_PROMARGS(args); \
  R_BCNodeStackTop -= 4; \
  SETSTACK(-1, value);	 \
  NEXT(); \
} while (0)

#define DO_STARTDISPATCH_N(generic) do { \
    SEXP call = GETCONSTOP(); \
    LABEL_TYPE label = GETLABELOP(); \
    value = GETSTACK(-1); \
    if (isObject(value)) { \
	if (tryDispatch(generic, call, value, rho, &value)) { \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = PC_FOR_LABEL(label); \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_START_ASSIGN_DISPATCH_N(generic) do { \
    SEXP call = GETCONSTOP(); \
    LABEL_TYPE label = GETLABELOP(); \
    SEXP lhs = GETSTACK(-2); \
    if (isObject(lhs)) { \
	SEXP rhs = GETSTACK(-1); \
	if (MAYBE_SHARED(lhs)) { \
	    lhs = shallow_duplicate(lhs); \
	    SETSTACK(-2, lhs); \
	    SET_NAMED(lhs, 1); \
	} \
	if (tryAssignDispatch(generic, call, lhs, rhs, rho, &value)) { \
	    R_BCNodeStackTop--; \
	    SETSTACK(-1, value); \
	    BC_CHECK_SIGINT(); \
	    pc = PC_FOR_LABEL(label); \
	} \
    } \
    NEXT(); \
} while (0)

#define DO_ISTEST(fun) do { \
  SETSTACK(-1, fun(GETSTACK(-1)) ? R_TrueValue : R_FalseValue);	\
  NEXT(); \
} while(0)
#define DO_ISTYPE(type) do { \
  SETSTACK(-1, TYPEOF(GETSTACK(-1)) == type ? mkTrue() : mkFalse()); \
  NEXT(); \
} while (0)
#define isNumericOnly(x) (isNumeric(x) && ! isLogical(x))

#ifdef BC_PROFILING
#define NO_CURRENT_OPCODE -1
static int current_opcode = NO_CURRENT_OPCODE;
static int opcode_counts[OPCOUNT];
#endif

#define BC_COUNT_DELTA 1000

#ifndef IMMEDIATE_FINALIZERS
/* finalizers are run here since this should only be called at
   points where running arbitrary code should be safe */
#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      R_RunPendingFinalizers(); \
      evalcount = 0; \
  } \
} while (0)
#else
#define BC_CHECK_SIGINT() do { \
  if (++evalcount > BC_COUNT_DELTA) { \
      R_CheckUserInterrupt(); \
      evalcount = 0; \
  } \
} while (0)
#endif

static void loopWithContext(volatile SEXP code, volatile SEXP rho)
{
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho, R_BaseEnv, R_NilValue,
		 R_NilValue);
    if (SETJMP(cntxt.cjmpbuf) != CTXT_BREAK)
	bcEval(code, rho, FALSE);
    endcontext(&cntxt);
}

static R_INLINE int bcStackIndex(R_bcstack_t *s)
{
    SEXP idx = *s;
    switch(TYPEOF(idx)) {
    case INTSXP:
	if (LENGTH(idx) == 1 && INTEGER(idx)[0] != NA_INTEGER)
	    return INTEGER(idx)[0];
	else return -1;
    case REALSXP:
	if (LENGTH(idx) == 1) {
	    double val = REAL(idx)[0];
	    if (! ISNAN(val) && val <= INT_MAX && val > INT_MIN)
		return (int) val;
	    else return -1;
	}
	else return -1;
    default: return -1;
    }
}

static R_INLINE void VECSUBSET_PTR(R_bcstack_t *sx, R_bcstack_t *si,
				   R_bcstack_t *sv, SEXP rho)
{
    SEXP idx, args, value;
    SEXP vec = GETSTACK_PTR(sx);
    int i = bcStackIndex(si) - 1;

    if (ATTRIB(vec) == R_NilValue && i >= 0) {
	switch (TYPEOF(vec)) {
	case REALSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_REAL_PTR(sv, REAL(vec)[i]);
	    return;
	case INTSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_INTEGER_PTR(sv, INTEGER(vec)[i]);
	    return;
	case LGLSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_LOGICAL_PTR(sv, LOGICAL(vec)[i]);
	    return;
	case CPLXSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_PTR(sv, ScalarComplex(COMPLEX(vec)[i]));
	    return;
	case RAWSXP:
	    if (LENGTH(vec) <= i) break;
	    SETSTACK_PTR(sv, ScalarRaw(RAW(vec)[i]));
	    return;
	}
    }

    /* fall through to the standard default handler */
    idx = GETSTACK_PTR(si);
    args = CREATE_CALLARG_2CELLS(vec, idx);
#ifndef USE_PROMARGS_STACK    
    PROTECT(args);
#endif    
    value = do_subset_dflt(R_NilValue, R_SubsetSym, args, rho);
#ifndef USE_PROMARGS_STACK    
    UNPROTECT(1);
#endif
    RELEASE_PROMARGS(args);    
    SETSTACK_PTR(sv, value);
}

#define DO_VECSUBSET(rho) do { \
    VECSUBSET_PTR(R_BCNodeStackTop - 2, R_BCNodeStackTop - 1, \
		  R_BCNodeStackTop - 2, rho); \
    R_BCNodeStackTop--; \
} while(0)

static R_INLINE SEXP getMatrixDim(SEXP mat)
{
    if (! OBJECT(mat) &&
	TAG(ATTRIB(mat)) == R_DimSymbol &&
	CDR(ATTRIB(mat)) == R_NilValue) {
	SEXP dim = CAR(ATTRIB(mat));
	if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2)
	    return dim;
	else return R_NilValue;
    }
    else return R_NilValue;
}

static R_INLINE void DO_MATSUBSET(SEXP rho)
{
    SEXP idx, jdx, args, value;
    SEXP mat = GETSTACK(-3);
    SEXP dim = getMatrixDim(mat);

    if (dim != R_NilValue) {
	int i = bcStackIndex(R_BCNodeStackTop - 2);
	int j = bcStackIndex(R_BCNodeStackTop - 1);
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    int k = i - 1 + nrow * (j - 1);
	    switch (TYPEOF(mat)) {
	    case REALSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK_REAL(-1, REAL(mat)[k]);
		return;
	    case INTSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK_INTEGER(-1, INTEGER(mat)[k]);
		return;
	    case LGLSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK_LOGICAL(-1, LOGICAL(mat)[k]);
		return;
	    case CPLXSXP:
		if (LENGTH(mat) <= k) break;
		R_BCNodeStackTop -= 2;
		SETSTACK(-1, ScalarComplex(COMPLEX(mat)[k]));
		return;
	    }
	}
    }

    /* fall through to the standard default handler */
    idx = GETSTACK(-2);
    jdx = GETSTACK(-1);

    args = CREATE_CALLARG_3CELLS(mat, idx, jdx);
    SETSTACK(-1, args); /* for GC protection */

    value = do_subset_dflt(R_NilValue, R_SubsetSym, args, rho);

    RELEASE_PROMARGS(args);
    R_BCNodeStackTop -= 2;
    SETSTACK(-1, value);
}

#define INTEGER_TO_REAL(x) ((x) == NA_INTEGER ? NA_REAL : (x))
#define LOGICAL_TO_REAL(x) ((x) == NA_LOGICAL ? NA_REAL : (x))

static R_INLINE Rboolean setElementFromScalar(SEXP vec, int i, int typev,
					      scalar_value_t *v)
{
    if (i < 0) return FALSE;

    if (TYPEOF(vec) == REALSXP) {
	if (LENGTH(vec) <= i) return FALSE;
	switch(typev) {
	case REALSXP: REAL(vec)[i] = v->dval; return TRUE;
	case INTSXP: REAL(vec)[i] = INTEGER_TO_REAL(v->ival); return TRUE;
	case LGLSXP: REAL(vec)[i] = LOGICAL_TO_REAL(v->ival); return TRUE;
	}
    }
    else if (typev == TYPEOF(vec)) {
	if (LENGTH(vec) <= i) return FALSE;
	switch (typev) {
	case INTSXP: INTEGER(vec)[i] = v->ival; return TRUE;
	case LGLSXP: LOGICAL(vec)[i] = v->ival; return TRUE;
	}
    }
    return FALSE;
}

static R_INLINE void SETVECSUBSET_PTR(R_bcstack_t *sx, R_bcstack_t *srhs,
				      R_bcstack_t *si, R_bcstack_t *sv,
				      SEXP rho)
{
    SEXP idx, args, value;
    SEXP vec = GETSTACK_PTR(sx);

    if (MAYBE_SHARED(vec)) {
	vec = duplicate(vec);
	SETSTACK_PTR(sx, vec);
    }
    else if (NAMED(vec) == 1)
	SET_NAMED(vec, 0);

    if (ATTRIB(vec) == R_NilValue) {
	int i = bcStackIndex(si);
	if (i > 0) {
	    scalar_value_t v;
	    int typev = bcStackScalar(srhs, &v);
	    if (setElementFromScalar(vec, i - 1, typev, &v)) {
		SETSTACK_PTR(sv, vec);
		return;
	    }
	}
    }

    /* fall through to the standard default handler */
    value = GETSTACK_PTR(srhs);
    idx = GETSTACK_PTR(si);
    
    args = CREATE_CALLARG_3VCELLS(vec, idx, value);

#ifndef USE_PROMARGS_STACK
    PROTECT(args);
#endif
    
    vec = do_subassign_dflt(R_NilValue, R_SubassignSym, args, rho);
    
#ifndef USE_PROMARGS_STACK
    UNPROTECT(1);
#endif
    RELEASE_PROMARGS(args);
    SETSTACK_PTR(sv, vec);
}

static R_INLINE void DO_SETVECSUBSET(SEXP rho)
{
    SETVECSUBSET_PTR(R_BCNodeStackTop - 3, R_BCNodeStackTop - 2,
		     R_BCNodeStackTop - 1, R_BCNodeStackTop - 3, rho);
    R_BCNodeStackTop -= 2;
}

static R_INLINE void DO_SETMATSUBSET(SEXP rho)
{
    SEXP dim, idx, jdx, args, value;
    SEXP mat = GETSTACK(-4);

    if (MAYBE_SHARED(mat)) {
	mat = duplicate(mat);
	SETSTACK(-4, mat);
    }
    else if (NAMED(mat) == 1)
	SET_NAMED(mat, 0);

    dim = getMatrixDim(mat);

    if (dim != R_NilValue) {
	int i = bcStackIndex(R_BCNodeStackTop - 2);
	int j = bcStackIndex(R_BCNodeStackTop - 1);
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
	    scalar_value_t v;
	    int typev = bcStackScalar(R_BCNodeStackTop - 3, &v);
	    int k = i - 1 + nrow * (j - 1);
	    if (setElementFromScalar(mat, k, typev, &v)) {
		R_BCNodeStackTop -= 3;
		SETSTACK(-1, mat);
		return;
	    }
	}
    }

    /* fall through to the standard default handler */
    value = GETSTACK(-3);
    idx = GETSTACK(-2);
    jdx = GETSTACK(-1);

    args = CREATE_CALLARG_4VCELLS(mat, idx, jdx, value);
    SETSTACK(-1, args); /* for GC protection */
    mat = do_subassign_dflt(R_NilValue, R_SubassignSym, args, rho);
    RELEASE_PROMARGS(args);
    R_BCNodeStackTop -= 3;
    SETSTACK(-1, mat);
}

#define FIXUP_SCALAR_LOGICAL(call, arg, op) do { \
	SEXP val = GETSTACK(-1); \
	if (TYPEOF(val) != LGLSXP || LENGTH(val) != 1) { \
	    if (!isNumber(val))	\
		errorcall(call, \
			  _("invalid %s type in 'x %s y'"), arg, op);	\
	    SETSTACK(-1, ScalarLogical(asLogical(val))); \
	} \
    } while(0)

static R_INLINE void checkForMissings(SEXP args, SEXP call)
{
    SEXP a, c;
    int n, k;
    for (a = args, n = 1; a != R_NilValue; a = CDR(a), n++)
	if (CAR(a) == R_MissingArg) {
	    /* check for an empty argument in the call -- start from
	       the beginning in case of ... arguments */
	    if (call != R_NilValue) {
		for (k = 1, c = CDR(call); c != R_NilValue; c = CDR(c), k++)
		    if (CAR(c) == R_MissingArg)
			errorcall(call, "argument %d is empty", k);
	    }
	    /* An error from evaluating a symbol will already have
	       been signaled.  The interpreter, in evalList, does
	       _not_ signal an error for a call expression that
	       produces an R_MissingArg value; for example

		   c(alist(a=)$a)

	       does not signal an error. If we decide we do want an
	       error in this case we can modify evalList for the
	       interpreter and here use the code below. */
#ifdef NO_COMPUTED_MISSINGS
	    /* otherwise signal a 'missing argument' error */
	    errorcall(call, "argument %d is missing", n);
#endif
	}
}

#define GET_VEC_LOOP_VALUE(var, pos) do {		\
    (var) = GETSTACK(pos);				\
    if (MAYBE_SHARED(var)) {				\
	(var) = allocVector(TYPEOF(seq), 1);		\
	SETSTACK(pos, var);				\
	SET_NAMED(var, 1);				\
    }							\
} while (0)

/* The CALLBUILTIN instruction handles calls to both true BUILTINs and
   to .Internals of type BUILTIN. To handle profiling in a way that is
   consistent with this instruction needs to be able to distinguish a
   true BUILTIN from a .Internal. LT */
#define IS_TRUE_BUILTIN(x) ((R_FunTab[PRIMOFFSET(x)].eval % 100 )/10 == 0)

static R_INLINE SEXP BUMPREFCNT(SEXP x)
{
    INCREMENT_REFCNT(x);
    return x;
}


static void c_inspect(SEXP x) {

  do_inspect(NULL, NULL, CONS(x, R_NilValue), NULL);

}

static void dumpInterpreterState(BCODE *codebase, BCODE *codeCurrent, int codeLength, R_bcstack_t* nodeStackBase, R_bcstack_t *nodeStackTop) {
    int i;
    
    printf("-------------------------------- NODE STACK --------------------------------------------\n");
    printf("Node stack dump (stack top = %p, stack base = %p):\n", nodeStackTop, nodeStackBase);
    i = 0;
    while(nodeStackTop != nodeStackBase) {
        nodeStackTop --;
        i--;
        printf("==== [%d] at %p is %p: ", i, nodeStackTop, *nodeStackTop);
        c_inspect(*nodeStackTop);
    }
    
    printf("-------------------------------- CODE --------------------------------------------------\n");
    printf("Instruction dump (code base = %p, pc = %p, codeLength = %d, code end = %p):\n", codebase, codeCurrent, codeLength, codebase + codeLength);
    BCODE *pc = codebase + 1;
    i = 1;
    for(i = 1; i <= codeLength;) {
        int op;
        void *opAddr = pc->v;
        for (op = 0; op < OPCOUNT; op++) {
	    if (opinfo[op].addr == opAddr) {
                break;
            }
        }
        if (op == OPCOUNT) {
            printf("==== %3d: UNKNOWN INSTRUCTION (%p)\n", i, opAddr);
            break;
        }
        char *name = opinfo[op].instname;
        int nargs = opinfo[op].argc;
        printf("==== %3d [%p]: %s (%d) + %d arguments", i, pc, name, op, nargs);
        if (pc == codeCurrent) {
            printf(" <^^^^^^^^^^^^^^^^^^^^^^^^^^^ PC is at previous instruction >\n");
        } else { 
            printf("\n");
        }
        i += nargs + 1;
        pc += nargs + 1;
    }
    printf("----------------------------------------------------------------------------------------\n");
}

void inspectArgs(SEXP args) {
    int nargs = 0;
    int ndots = 0;
    int nnamed = 0;
    int nmissing = 0;
    int maxnamed = 0;
    const char *lastname = "";
    
    SEXP a;
    for(a = args; a != R_NilValue; a = CDR(a)) {
        SEXP cara = CAR(a);
        nargs++;
        if (cara == R_DotsSymbol) {
            ndots++;
        } else if (cara == R_MissingArg) {
            nmissing++;
        } else if (TAG(a) != R_NilValue) {
            nnamed++;
            maxnamed = nargs;
            lastname = CHAR(PRINTNAME(TAG(a)));
        }
    }
    fprintf(stderr, "CCALL nargs=%d ndots=%d nnamed=%d maxnamed=%d lastname=%s nmissing=%d\n", nargs, ndots, nnamed, maxnamed, lastname, nmissing);
    
}

static SEXP bcEval(SEXP body, SEXP rho, Rboolean useCache)
{
  SEXP value, constants;
  BCODE *pc, *codebase;
  int ftype = 0;
  R_bcstack_t *oldntop = R_BCNodeStackTop;
  static int evalcount = 0;
#ifdef BC_INT_STACK
  IStackval *olditop = R_BCIntStackTop;
#endif
#ifdef BC_PROFILING
  int old_current_opcode = current_opcode;
#endif
#ifdef THREADED_CODE
  int which = 0;
#endif

  BC_CHECK_SIGINT();

  INITIALIZE_MACHINE();
  codebase = pc = BCCODE(body);
  constants = BCCONSTS(body);

  /* allow bytecode to be disabled for testing */
  if (R_disable_bytecode)
      return eval(bytecodeExpr(body), rho);

  /* check version */
  {
      int version = GETOP();
      if (version < R_bcMinVersion || version > R_bcVersion) {
	  if (version >= 2) {
	      static Rboolean warned = FALSE;
	      if (! warned) {
		  warned = TRUE;
		  warning(_("bytecode version mismatch; using eval"));
	      }
	      return eval(bytecodeExpr(body), rho);
	  }
	  else if (version < R_bcMinVersion)
	      error(_("bytecode version is too old"));
	  else error(_("bytecode version is too new"));
      }
  }

  R_binding_cache_t vcache = NULL;
  Rboolean smallcache = TRUE;
#ifdef USE_BINDING_CACHE
  if (useCache) {
      R_len_t n = LENGTH(constants);
      /*  n >= 2 must hold */
      
      n = asInteger(VECTOR_ELT(constants, n - 1)) + 1; 
      /* +1 is for the first entry which is the expression/function body */
      
# ifdef CACHE_MAX
      if (n > CACHE_MAX) {
	  n = CACHE_MAX;
	  smallcache = FALSE;
      }
# endif
# ifdef CACHE_ON_STACK
      /* initialize binding cache on the stack */
      vcache = R_BCNodeStackTop;
      if (R_BCNodeStackTop + n > R_BCNodeStackEnd)
	  nodeStackOverflow();
      while (n > 0) {
	  *R_BCNodeStackTop = R_NilValue;
	  R_BCNodeStackTop++;
	  n--;
      }
# else
      /* allocate binding cache and protect on stack */
      vcache = allocVector(VECSXP, n);
      BCNPUSH(vcache);
# endif
  }
#endif

  BEGIN_MACHINE {
    OP(BCMISMATCH, 0, CONSTOP(0), LABELOP(0)): error(_("byte code version mismatch"));
    OP(RETURN, 0, CONSTOP(0), LABELOP(0)): value = GETSTACK(-1); goto done;
    OP(GOTO, 1, CONSTOP(0), LABELOP(1)):
      {
	LABEL_TYPE label = GETLABELOP();
	BC_CHECK_SIGINT();
	pc = PC_FOR_LABEL(label);
	NEXT();
      }
    OP(BRIFNOT, 2, CONSTOP(1), LABELOP(2)):
      {
	SEXP call = GETCONSTOP();	
	LABEL_TYPE label = GETLABELOP();
	int cond;
	value = BCNPOP();
	cond = asLogicalNoNA(value, call);
	if (! cond) {
	    BC_CHECK_SIGINT(); /**** only on back branch?*/
	    pc = PC_FOR_LABEL(label);
	}
	NEXT();
      }
    OP(POP, 0, CONSTOP(0), LABELOP(0)): BCNPOP_IGNORE_VALUE(); NEXT();
    OP(DUP, 0, CONSTOP(0), LABELOP(0)): BCNDUP(); NEXT();
    OP(PRINTVALUE, 0, CONSTOP(0), LABELOP(0)): PrintValue(BCNPOP()); NEXT();
    OP(STARTLOOPCNTXT, 1, CONSTOP(1), LABELOP(0)):
	{
	    SEXP code = GETCONSTOP();
	    loopWithContext(code, rho);
	    NEXT();
	}
    OP(ENDLOOPCNTXT, 0, CONSTOP(0), LABELOP(0)): value = R_NilValue; goto done;
    OP(DOLOOPNEXT, 0, CONSTOP(0), LABELOP(0)): findcontext(CTXT_NEXT, rho, R_NilValue);
    OP(DOLOOPBREAK, 0, CONSTOP(0), LABELOP(0)): findcontext(CTXT_BREAK, rho, R_NilValue);
    OP(STARTFOR, 3, CONSTOP(1)|CONSTOP(2), LABELOP(3)):
      {
	SEXP seq = GETSTACK(-1);
	SEXP call = GETCONSTOP();
	SEXP symbol = GETCONSTOP();
	LABEL_TYPE label = GETLABELOP();

	/* if we are iterating over a factor, coerce to character first */
	if (inheritsCharSXP(seq, R_FactorCharSXP)) {
	    seq = asCharacterFactor(seq);
	    SETSTACK(-1, seq);
	}

	defineVar(symbol, R_NilValue, rho);
	BCNPUSH(GET_BINDING_CELL(symbol, rho));

	value = allocVector(INTSXP, 2);
	INTEGER(value)[0] = -1;
	if (isVector(seq))
	  INTEGER(value)[1] = LENGTH(seq);
	else if (isList(seq) || isNull(seq))
	  INTEGER(value)[1] = length(seq);
	else errorcall(call,
		       _("invalid for() loop sequence"));
	BCNPUSH(value);

	/* bump up NAMED count of seq to avoid modification by loop code */
	INCREMENT_NAMED(seq);
	INCREMENT_REFCNT(seq);

	/* place initial loop variable value object on stack */
	switch(TYPEOF(seq)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    value = allocVector(TYPEOF(seq), 1);
	    BCNPUSH(value);
	    break;
	default: BCNPUSH(R_NilValue);
	}

	BC_CHECK_SIGINT();
	pc = PC_FOR_LABEL(label);
	NEXT();
      }
    OP(STEPFOR, 1, CONSTOP(0), LABELOP(1)):
      {
	LABEL_TYPE label = GETLABELOP();
	int i = ++(INTEGER(GETSTACK(-2))[0]);
	int n = INTEGER(GETSTACK(-2))[1];
	if (i < n) {
	  SEXP seq = GETSTACK(-4);
	  SEXP cell = GETSTACK(-3);
	  switch (TYPEOF(seq)) {
	  case LGLSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    LOGICAL(value)[0] = LOGICAL(seq)[i];
	    break;
	  case INTSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    INTEGER(value)[0] = INTEGER(seq)[i];
	    break;
	  case REALSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    REAL(value)[0] = REAL(seq)[i];
	    break;
	  case CPLXSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    COMPLEX(value)[0] = COMPLEX(seq)[i];
	    break;
	  case STRSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    SET_STRING_ELT(value, 0, STRING_ELT(seq, i));
	    break;
	  case RAWSXP:
	    GET_VEC_LOOP_VALUE(value, -1);
	    RAW(value)[0] = RAW(seq)[i];
	    break;
	  case EXPRSXP:
	  case VECSXP:
	    value = VECTOR_ELT(seq, i);
	    SET_NAMED(value, 2);
	    break;
	  case LISTSXP:
	    value = CAR(seq);
	    SETSTACK(-4, CDR(seq));
	    SET_NAMED(value, 2);
	    break;
	  default:
	    error(_("invalid sequence argument in for loop"));
	  }
	  if (CAR(cell) == R_UnboundValue || ! SET_BINDING_VALUE(cell, value))
	      defineVar(BINDING_SYMBOL(cell), value, rho);
	  BC_CHECK_SIGINT();
	  pc = PC_FOR_LABEL(label);
	}
	NEXT();
      }
    OP(ENDFOR, 0, CONSTOP(0), LABELOP(0)):
      {
#ifdef COMPUTE_REFCNT_VALUES
	SEXP seq = GETSTACK(-4);
	DECREMENT_REFCNT(seq);
#endif
	R_BCNodeStackTop -= 3;
	SETSTACK(-1, R_NilValue);
	NEXT();
      }
    OP(SETLOOPVAL, 0, CONSTOP(0), LABELOP(0)):
      BCNPOP_IGNORE_VALUE(); SETSTACK(-1, R_NilValue); NEXT();
    OP(INVISIBLE, 0, CONSTOP(0), LABELOP(0)): R_Visible = FALSE; NEXT();
    /**** for now LDCONST, LDTRUE, and LDFALSE duplicate/allocate to
	  be defensive against bad package C code */
    OP(LDCONST, 1, CONSTOP(1), LABELOP(0)):
      R_Visible = TRUE;
      value = GETCONSTOP();
      /* make sure NAMED = 2 -- lower values might be safe in some cases but
	 not in general, especially if the constant pool was created by
	 unserializing a compiled expression. */
      /*if (NAMED(value) < 2) SET_NAMED(value, 2);*/
      BCNPUSH(duplicate(value));
      NEXT();
    OP(LDNULL, 0, CONSTOP(0), LABELOP(0)): R_Visible = TRUE; BCNPUSH(R_NilValue); NEXT();
    OP(LDTRUE, 0, CONSTOP(0), LABELOP(0)): R_Visible = TRUE; BCNPUSH(mkTrue()); NEXT();
    OP(LDFALSE, 0, CONSTOP(0), LABELOP(0)): R_Visible = TRUE; BCNPUSH(mkFalse()); NEXT();
    OP(GETVAR, 1, CONSTOP(0), LABELOP(0)): DO_GETVAR(FALSE, FALSE);
    OP(DDVAL, 1, CONSTOP(0), LABELOP(0)): DO_GETVAR(TRUE, FALSE);
    OP(SETVAR, 1, CONSTOP(0), LABELOP(0)):
      {
	int sidx = GETOP();
	SEXP loc;
	if (smallcache)
	    loc = GET_SMALLCACHE_BINDING_CELL(vcache, sidx);
	else {
	    SEXP symbol = VECTOR_ELT(constants, sidx);
	    loc = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	}
	value = GETSTACK(-1);
	INCREMENT_NAMED(value);
	if (! SET_BINDING_VALUE(loc, value)) {
	    SEXP symbol = VECTOR_ELT(constants, sidx);
	    PROTECT(value);
	    defineVar(symbol, value, rho);
	    UNPROTECT(1);
	}
	NEXT();
      }

#define DEFAULT_STACKINIT_ACTION() do { \
  /* push the function, and push space for creating the argument list. */ \
  BCNSTACKCHECK(3); \
  SETSTACK(0, value); \
  SETSTACK(1, R_NilValue); \
  SETSTACK(2, R_NilValue); \
  R_BCNodeStackTop += 3; \
} while(0);

#define GETSYMFUN_GETVALUE_ACTION() do { \
  value = SYMVALUE(symbol); \
  if (TYPEOF(value) == PROMSXP) { \
    value = forcePromise(value); \
    SET_NAMED(value, 2); \
  } \
} while(0);

#define GETINTLBUILTIN_GETVALUE_ACTION() do { \
  value = INTERNAL(symbol); \
  if (TYPEOF(value) != BUILTINSXP) \
    error(_("there is no .Internal function '%s'"), \
      CHAR(PRINTNAME(symbol))); \
} while(0);

    OP(GETFUN, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = findFun(symbol, rho), DEFAULT_STACKINIT_ACTION() );
    OP(GETGLOBFUN, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = findFun(symbol, R_GlobalEnv), DEFAULT_STACKINIT_ACTION() );
    OP(GETSYMFUN, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETSYMFUN_GETVALUE_ACTION(), DEFAULT_STACKINIT_ACTION() );
    OP(GETBUILTIN, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), DEFAULT_STACKINIT_ACTION() );
    OP(GETINTLBUILTIN, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), DEFAULT_STACKINIT_ACTION() );
    
#define EARG_STACKINIT_ACTION(nargs) do { \
  /* push the function and check there is enough space on stack for arguments */ \
  BCNSTACKCHECK(nargs + 1); \
  SETSTACK(0, value); \
  R_BCNodeStackTop ++; \
} while(0);    
    
    OP(GETBUILTINEARG0, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(0) );
    OP(GETBUILTINEARG1, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(1) );
    OP(GETBUILTINEARG2, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(2) );
    OP(GETBUILTINEARG3, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(3) );
    OP(GETBUILTINEARG4, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(4) );
    OP(GETBUILTINEARG5, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(5) );
    OP(GETBUILTINEARG6, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(6) );
    OP(GETBUILTINEARG7, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( value = getPrimitive(symbol, BUILTINSXP), EARG_STACKINIT_ACTION(7) );
    
    OP(GETINTLBUILTINEARG0, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(0) );
    OP(GETINTLBUILTINEARG1, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(1) );
    OP(GETINTLBUILTINEARG2, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(2) );
    OP(GETINTLBUILTINEARG3, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(3) );
    OP(GETINTLBUILTINEARG4, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(4) );
    OP(GETINTLBUILTINEARG5, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(5) );
    OP(GETINTLBUILTINEARG6, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(6) );
    OP(GETINTLBUILTINEARG7, 1, CONSTOP(1), LABELOP(0)): DO_GETFUN_COMMON( GETINTLBUILTIN_GETVALUE_ACTION(), EARG_STACKINIT_ACTION(7) );    

    OP(CHECKFUN, 0, CONSTOP(0), LABELOP(0)):
      {
	/* check then the value on the stack is a function */
	value = GETSTACK(-1);
	if (TYPEOF(value) != CLOSXP && TYPEOF(value) != BUILTINSXP &&
	    TYPEOF(value) != SPECIALSXP)
	  error(_("attempt to apply non-function"));

	/* initialize the function type register, and push space for
	   creating the argument list. */
	ftype = TYPEOF(value);
	BCNSTACKCHECK(2);
	SETSTACK(0, R_NilValue);
	SETSTACK(1, R_NilValue);
	R_BCNodeStackTop += 2;
	NEXT();
      }
    OP(CHECKFUNEARG, 1, CONSTOP(0), LABELOP(0)):
      {
        int nargs = GETINTOP();
	/* check then the value on the stack is a function */
	value = GETSTACK(-1);
	if (TYPEOF(value) != CLOSXP && TYPEOF(value) != BUILTINSXP &&
	    TYPEOF(value) != SPECIALSXP)
	  error(_("attempt to apply non-function"));

	/* initialize the function type register, and push space for
	   creating the argument list. */
	ftype = TYPEOF(value);
	
	BCNSTACKCHECK(nargs);
	NEXT();
      }      
    OP(GETFUNEARG, 2, CONSTOP(1), LABELOP(0)): 
      {
        /* get the function */
        SEXP symbol = GETCONSTOP();
        int nargs = GETINTOP(); /* FIXME: turn into register so that it is not popped twice, once here and once in the call ? */
        value = findFun(symbol, rho);
        if(RTRACE(value)) {
            Rprintf("trace: ");
            PrintValue(symbol);
        }

        /* initialize the function type register */
        ftype = TYPEOF(value);

        BCNSTACKCHECK(1 + nargs);
        SETSTACK(0, value);
        R_BCNodeStackTop += 1;
        
        NEXT();
      }
    OP(MAKEPROM, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP code = GETCONSTOP();
	if (ftype != SPECIALSXP) {
	  if (ftype == BUILTINSXP)
	      value = bcEval(code, rho, TRUE);
	  else
	    value = mkPROMISE(code, rho); /* we know code is not const */
	  PUSHCALLARG(value);
	}
	NEXT();
      }
    OP(MAKEPROMEARG, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP code = GETCONSTOP();
	if (ftype != SPECIALSXP) {
	  if (ftype == BUILTINSXP)
	      value = bcEval(code, rho, TRUE);
	  else
	    value = mkPROMISE(code, rho); /* we know code is not const */
	  PUSHCALLEARG_NOTSPECIAL(value);
	}
	NEXT();
      }      
    OP(MAKEGETVARPROM, 1, CONSTOP(0), LABELOP(0)):
      {
        /* this instruction is not used for ..n and ... */
	if (ftype == BUILTINSXP) {
	    DO_GETVAR_PUSHCALLARG(FALSE, FALSE);
	    /* not reached */
        } else if (ftype != SPECIALSXP) {
            SEXP symbol = VECTOR_ELT(constants, GETOP());
            value = mkPROMISE(symbol, rho); /* we know symbol is not const */
	    PUSHCALLARG(value);
        } else {
            SKIP_OP();
        }
        NEXT(); 
      }
    OP(MAKEGETVARPROMEARG, 1, CONSTOP(0), LABELOP(0)):
      {
        /* this instruction is not used for ..n and ... */
	if (ftype == BUILTINSXP) {
	    DO_GETVAR_PUSHCALLEARG_NOTSPECIAL(FALSE, FALSE);
	    /* not reached */
        } else if (ftype != SPECIALSXP) {
            SEXP symbol = VECTOR_ELT(constants, GETOP());
            value = mkPROMISE(symbol, rho); /* we know symbol is not const */
	    PUSHCALLEARG_NOTSPECIAL(value);
        } else {
            SKIP_OP();
        }
        NEXT(); 
      }      
    OP(DOMISSING, 0, CONSTOP(0), LABELOP(0)):
      {
	if (ftype != SPECIALSXP)
	  PUSHCALLARG(R_MissingArg);
	NEXT();
      }
    OP(SETTAG, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP tag = GETCONSTOP();
	SEXP cell = GETSTACK(-1);
	if (ftype != SPECIALSXP && cell != R_NilValue)
#ifdef USE_PROMARGS_STACK
          TAG(cell) = CreateTag(tag); /* no barrier */
#else	
	  SET_TAG(cell, CreateTag(tag));
#endif	  
	NEXT();
      }
    OP(DODOTS, 0, CONSTOP(0), LABELOP(0)):
      {
	if (ftype != SPECIALSXP) {
	  SEXP h = findVar(R_DotsSymbol, rho);
	  if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
	    for (; h != R_NilValue; h = CDR(h)) {
	      SEXP val, cell;
	      if (ftype == BUILTINSXP) val = eval(CAR(h), rho);
	      else val = mkPROMISEorConst(CAR(h), rho);
	      cell = CREATE_CALLARG_CELL(val);
	      PUSHCALLARG_CELL(cell);
	      if (TAG(h) != R_NilValue) SET_TAG(cell, CreateTag(TAG(h)));
	    }
	  }
	  else if (h != R_MissingArg)
	    error(_("'...' used in an incorrect context"));
	}
	NEXT();
      }
    OP(PUSHARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLARG(BCNPOP()); NEXT();
    /**** for now PUSHCONST, PUSHTRUE, and PUSHFALSE duplicate/allocate to
	  be defensive against bad package C code */
    OP(PUSHCONSTARG, 1, CONSTOP(1), LABELOP(0)):
      value = GETCONSTOP();
      PUSHCALLARG(BUMPREFCNT(duplicate(value)));
      NEXT();
    OP(PUSHNULLARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLARG(R_NilValue); NEXT();
    OP(PUSHTRUEARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLARG(BUMPREFCNT(mkTrue())); NEXT();
    OP(PUSHFALSEARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLARG(BUMPREFCNT(mkFalse())); NEXT();
    OP(PUSHEARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLEARG(BCNPOP()); NEXT();
    /**** for now PUSHCONST, PUSHTRUE, and PUSHFALSE duplicate/allocate to
	  be defensive against bad package C code */
    OP(PUSHCONSTEARG, 1, CONSTOP(1), LABELOP(0)):
      value = GETCONSTOP();
      PUSHCALLEARG(BUMPREFCNT(duplicate(value)));
      NEXT();
    OP(PUSHNULLEARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLEARG(R_NilValue); NEXT();
    OP(PUSHTRUEEARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLEARG(BUMPREFCNT(mkTrue())); NEXT();
    OP(PUSHFALSEEARG, 0, CONSTOP(0), LABELOP(0)): PUSHCALLEARG(BUMPREFCNT(mkFalse())); NEXT();
    OP(CALL, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP fun = GETSTACK(-3);
	SEXP call = GETCONSTOP();
	SEXP args = GETSTACK(-2);
	int flag;
	switch (ftype) {
	case BUILTINSXP:
	  checkForMissings(args, call);
	  flag = PRIMPRINT(fun);
	  R_Visible = flag != 1;
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  RELEASE_PROMARGS(args);
	  if (flag < 2) R_Visible = flag != 1;
	  break;
	case SPECIALSXP:
	  flag = PRIMPRINT(fun);
	  R_Visible = flag != 1;
	  value = PRIMFUN(fun) (call, fun, CDR(call), rho);
	  if (flag < 2) R_Visible = flag != 1;
	  break;
	case CLOSXP:
	  value = applyClosure(call, fun, args, rho, R_BaseEnv);
	  RELEASE_PROMARGS(args);
	  break;
	default: error(_("bad function"));
	}
	R_BCNodeStackTop -= 2;
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }
    OP(CALLEARG, 2, CONSTOP(1), LABELOP(0)):
      {
	SEXP call = GETCONSTOP();
	int nargs = GETINTOP();
	SEXP fun; 
	SEXP args; 

	int flag;
	switch(ftype) {
        case BUILTINSXP:
          fun = GETSTACK(-1-nargs);
          args = PROTECT(buildPositionalPromargs(nargs, R_BCNodeStackTop-1));
	  checkForMissings(args, call);
	  flag = PRIMPRINT(fun);
	  R_Visible = flag != 1;
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  if (flag < 2) R_Visible = flag != 1;
          UNPROTECT(1);	  
  	  R_BCNodeStackTop -= nargs;
	  break;
	case SPECIALSXP:
          fun = GETSTACK(-1);
	  flag = PRIMPRINT(fun);
	  R_Visible = flag != 1;
	  value = PRIMFUN(fun) (call, fun, CDR(call), rho);
	  if (flag < 2) R_Visible = flag != 1;
	  /* note: there are no args stored on the node stack */
	  break;
	case CLOSXP:
          fun = GETSTACK(-1-nargs);
//        /*defensive version for testing */  args = PROTECT(buildPositionalPromargs(nargs, R_BCNodeStackTop-1));
//	                                      value = applyClosure(call, fun, args, rho, R_BaseEnv);
          value = applyPositionalClosure(call, fun, R_BCNodeStackTop-nargs, nargs, rho); 
            /* FIXME: redundantly calculates R_BCNodeStackTop-(1 + nargs) even for 0 args */
//                                            UNPROTECT(1);
  	  R_BCNodeStackTop -= nargs;
	  break;
	default: error(_("bad function"));
	}
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }                
    OP(CALLBUILTIN, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP fun = GETSTACK(-3);
	SEXP call = GETCONSTOP();
	SEXP args = GETSTACK(-2);
	int flag;
	const void *vmax = vmaxget();
	if (TYPEOF(fun) != BUILTINSXP)
	  error(_("not a BUILTIN function (CALLBUILTIN)"));
	flag = PRIMPRINT(fun);
	R_Visible = flag != 1;
	if (R_Profiling && IS_TRUE_BUILTIN(fun)) {
	    RCNTXT cntxt;
	    SEXP oldref = R_Srcref;
	    begincontext(&cntxt, CTXT_BUILTIN, call,
			 R_BaseEnv, R_BaseEnv, R_NilValue, R_NilValue);
	    R_Srcref = NULL;
	    value = PRIMFUN(fun) (call, fun, args, rho);
	    R_Srcref = oldref;
	    endcontext(&cntxt);
	} else {
	    value = PRIMFUN(fun) (call, fun, args, rho);
	}
	if (flag < 2) R_Visible = flag != 1;
	vmaxset(vmax);
	R_BCNodeStackTop -= 2;
	RELEASE_PROMARGS(args);
	SETSTACK(-1, value);
	NEXT();
      }
    OP(CALLBUILTINEARG0, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(0, PRIMEARGFUN0(fun) (call, fun, rho));
    OP(CALLBUILTINEARG1, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(1, PRIMEARGFUN1(fun) (call, fun, GETSTACK(-1), rho));
    OP(CALLBUILTINEARG2, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(2, PRIMEARGFUN2(fun) (call, fun, GETSTACK(-2), GETSTACK(-1), rho));
    OP(CALLBUILTINEARG3, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(3, PRIMEARGFUN3(fun) (call, fun, GETSTACK(-3), GETSTACK(-2), GETSTACK(-1), rho));
    OP(CALLBUILTINEARG4, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(4, PRIMEARGFUN4(fun) (call, fun, GETSTACK(-4), GETSTACK(-3), GETSTACK(-2), GETSTACK(-1), rho));
    OP(CALLBUILTINEARG5, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(5, PRIMEARGFUN5(fun) (call, fun, GETSTACK(-5), GETSTACK(-4), GETSTACK(-3), GETSTACK(-2), GETSTACK(-1), rho));
    OP(CALLBUILTINEARG6, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(6, PRIMEARGFUN6(fun) (call, fun, GETSTACK(-6), GETSTACK(-5), GETSTACK(-4), GETSTACK(-3), GETSTACK(-2), GETSTACK(-1), rho));
    OP(CALLBUILTINEARG7, 1, CONSTOP(1), LABELOP(0)): EARG_CALLBUILTIN(7, PRIMEARGFUN7(fun) (call, fun, GETSTACK(-7), GETSTACK(-6), GETSTACK(-5), GETSTACK(-4), GETSTACK(-3), GETSTACK(-2), GETSTACK(-1), rho));
    OP(CALLSPECIAL, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP call = GETCONSTOP();
	SEXP symbol = CAR(call);
	SEXP fun = getPrimitive(symbol, SPECIALSXP);
	int flag;
	const void *vmax = vmaxget();
	if (RTRACE(fun)) {
	  Rprintf("trace: ");
	  PrintValue(symbol);
	}
	BCNPUSH(fun);  /* for GC protection */
	flag = PRIMPRINT(fun);
	R_Visible = flag != 1;
	value = PRIMFUN(fun) (call, fun, CDR(call), rho);
	if (flag < 2) R_Visible = flag != 1;
	vmaxset(vmax);
	SETSTACK(-1, value); /* replaces fun on stack */
	NEXT();
      }
    OP(MAKECLOSURE, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP fb = GETCONSTOP();
	SEXP forms = VECTOR_ELT(fb, 0);
	SEXP body = VECTOR_ELT(fb, 1);
	value = mkCLOSXP(forms, body, rho);
	BCNPUSH(value);
	NEXT();
      }
    OP(UMINUS, 1, CONSTOP(1), LABELOP(0)): Arith1(R_SubSym);
    OP(UPLUS, 1, CONSTOP(1), LABELOP(0)): Arith1(R_AddSym);
    OP(ADD, 1, CONSTOP(1), LABELOP(0)): FastBinary(+, PLUSOP, R_AddSym);
    OP(SUB, 1, CONSTOP(1), LABELOP(0)): FastBinary(-, MINUSOP, R_SubSym);
    OP(MUL, 1, CONSTOP(1), LABELOP(0)): FastBinary(*, TIMESOP, R_MulSym);
    OP(DIV, 1, CONSTOP(1), LABELOP(0)): FastBinary(/, DIVOP, R_DivSym);
    OP(EXPT, 1, CONSTOP(1), LABELOP(0)): Arith2(POWOP, R_ExptSym);
    OP(SQRT, 1, CONSTOP(1), LABELOP(0)): Math1(R_SqrtSym);
    OP(EXP, 1, CONSTOP(1), LABELOP(0)): Math1(R_ExpSym);
    OP(EQ, 1, CONSTOP(1), LABELOP(0)): FastRelop2(==, EQOP, R_EqSym);
    OP(NE, 1, CONSTOP(1), LABELOP(0)): FastRelop2(!=, NEOP, R_NeSym);
    OP(LT, 1, CONSTOP(1), LABELOP(0)): FastRelop2(<, LTOP, R_LtSym);
    OP(LE, 1, CONSTOP(1), LABELOP(0)): FastRelop2(<=, LEOP, R_LeSym);
    OP(GE, 1, CONSTOP(1), LABELOP(0)): FastRelop2(>=, GEOP, R_GeSym);
    OP(GT, 1, CONSTOP(1), LABELOP(0)): FastRelop2(>, GTOP, R_GtSym);
    OP(AND, 1, CONSTOP(1), LABELOP(0)): Builtin2(do_logic, R_AndSym, rho);
    OP(OR, 1, CONSTOP(1), LABELOP(0)): Builtin2(do_logic, R_OrSym, rho);
    OP(NOT, 1, CONSTOP(1), LABELOP(0)): Builtin1(do_logic, R_NotSym, rho);
    OP(DOTSERR, 0, CONSTOP(0), LABELOP(0)): error(_("'...' used in an incorrect context"));
    OP(STARTASSIGN, 1, CONSTOP(0), LABELOP(0)):
      {
	int sidx = GETOP();
	SEXP symbol = VECTOR_ELT(constants, sidx);
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = BINDING_VALUE(cell);
	if (value == R_UnboundValue ||
	    TYPEOF(value) == PROMSXP ||
#ifdef SWITCH_TO_REFCNT
	    REFCNT(value) != 1
#else
	    NAMED(value) != 1
#endif
	    )
	    value = EnsureLocal(symbol, rho);
	BCNPUSH(value);
	BCNDUP2ND();
	/* top three stack entries are now RHS value, LHS value, RHS value */
	FIXUP_RHS_NAMED(GETSTACK(-1));
	INCREMENT_REFCNT(GETSTACK(-1));
	NEXT();
      }
    OP(ENDASSIGN, 1, CONSTOP(0), LABELOP(0)):
      {
	int sidx = GETOP();
	SEXP symbol = VECTOR_ELT(constants, sidx);
	SEXP cell = GET_BINDING_CELL_CACHE(symbol, rho, vcache, sidx);
	value = GETSTACK(-1); /* leave on stack for GC protection */
	INCREMENT_NAMED(value);
	if (! SET_BINDING_VALUE(cell, value))
	    defineVar(symbol, value, rho);
	R_BCNodeStackTop--; /* now pop LHS value off the stack */
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(GETSTACK(-1), 2);
#else
	INCREMENT_NAMED(GETSTACK(-1));
	DECREMENT_REFCNT(GETSTACK(-1));
#endif
	NEXT();
      }
    OP(STARTSUBSET, 2, CONSTOP(1), LABELOP(2)): DO_STARTDISPATCH(R_SubsetSym);
    OP(DFLTSUBSET, 0, CONSTOP(0), LABELOP(0)): DO_DFLTDISPATCH(do_subset_dflt, R_SubsetSym);
    OP(STARTSUBASSIGN, 2, CONSTOP(1), LABELOP(2)): DO_START_ASSIGN_DISPATCH(R_SubassignSym);
    OP(DFLTSUBASSIGN, 0, CONSTOP(0), LABELOP(0)):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign_dflt, R_SubassignSym);
    OP(STARTC, 2, CONSTOP(1), LABELOP(2)): DO_STARTDISPATCH(R_CSymbol);
    OP(DFLTC, 0, CONSTOP(0), LABELOP(0)): DO_DFLTDISPATCH(do_c_dflt, R_CSym);
    OP(STARTSUBSET2, 2, CONSTOP(1), LABELOP(2)): DO_STARTDISPATCH(R_Subset2Sym);
    OP(DFLTSUBSET2, 0, CONSTOP(0), LABELOP(0)): DO_DFLTDISPATCH(do_subset2_dflt, R_Subset2Sym);
    OP(STARTSUBASSIGN2, 2, CONSTOP(1), LABELOP(2)): DO_START_ASSIGN_DISPATCH(R_Subassign2Sym);
    OP(DFLTSUBASSIGN2, 0, CONSTOP(0), LABELOP(0)):
      DO_DFLT_ASSIGN_DISPATCH(do_subassign2_dflt, R_Subassign2Sym);
    OP(DOLLAR, 2, CONSTOP(1)|CONSTOP(2), LABELOP(0)):
      {
	int dispatched = FALSE;
	SEXP call = GETCONSTOP();
	SEXP symbol = GETCONSTOP();
	SEXP x = GETSTACK(-1);
	if (isObject(x)) {
	    SEXP ncall;
	    PROTECT(ncall = duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    dispatched = tryDispatch(R_DollarSymbol, ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (dispatched)
	    SETSTACK(-1, value);
	else
	    SETSTACK(-1, R_subset3_dflt(x, PRINTNAME(symbol), R_NilValue));
	NEXT();
      }
    OP(DOLLARGETS, 2, CONSTOP(1)|CONSTOP(2), LABELOP(0)):
      {
	int dispatched = FALSE;
	SEXP call = GETCONSTOP();
	SEXP symbol = GETCONSTOP();
	SEXP x = GETSTACK(-2);
	SEXP rhs = GETSTACK(-1);
	if (MAYBE_SHARED(x)) {
	    x = shallow_duplicate(x);
	    SETSTACK(-2, x);
	    SET_NAMED(x, 1);
	}
	if (isObject(x)) {
	    SEXP ncall, prom;
	    PROTECT(ncall = duplicate(call));
	    /**** hack to avoid evaluating the symbol */
	    SETCAR(CDDR(ncall), ScalarString(PRINTNAME(symbol)));
	    prom = mkRHSPROMISE(CADDDR(ncall), rhs);
	    SETCAR(CDR(CDDR(ncall)), prom);
	    dispatched = tryDispatch(R_DollarAssignSymbol, ncall, x, rho, &value);
	    UNPROTECT(1);
	}
	if (! dispatched)
	  value = R_subassign3_dflt(call, x, symbol, rhs);
	R_BCNodeStackTop--;
	SETSTACK(-1, value);
	NEXT();
      }
    OP(ISNULL, 0, CONSTOP(0), LABELOP(0)): DO_ISTEST(isNull);
    OP(ISLOGICAL, 0, CONSTOP(0), LABELOP(0)): DO_ISTYPE(LGLSXP);
    OP(ISINTEGER, 0, CONSTOP(0), LABELOP(0)): {
	SEXP arg = GETSTACK(-1);
	Rboolean test = (TYPEOF(arg) == INTSXP) && ! inheritsCharSXP(arg, R_FactorCharSXP);
	SETSTACK(-1, test ? mkTrue() : mkFalse());
	NEXT();
      }
    OP(ISDOUBLE, 0, CONSTOP(0), LABELOP(0)): DO_ISTYPE(REALSXP);
    OP(ISCOMPLEX, 0, CONSTOP(0), LABELOP(0)): DO_ISTYPE(CPLXSXP);
    OP(ISCHARACTER, 0, CONSTOP(0), LABELOP(0)): DO_ISTYPE(STRSXP);
    OP(ISSYMBOL, 0, CONSTOP(0), LABELOP(0)): DO_ISTYPE(SYMSXP); /**** S4 thingy allowed now???*/
    OP(ISOBJECT, 0, CONSTOP(0), LABELOP(0)): DO_ISTEST(OBJECT);
    OP(ISNUMERIC, 0, CONSTOP(0), LABELOP(0)): DO_ISTEST(isNumericOnly);
    OP(VECSUBSET, 0, CONSTOP(0), LABELOP(0)): DO_VECSUBSET(rho); NEXT();
    OP(MATSUBSET, 0, CONSTOP(0), LABELOP(0)): DO_MATSUBSET(rho); NEXT();
    OP(SETVECSUBSET, 0, CONSTOP(0), LABELOP(0)): DO_SETVECSUBSET(rho); NEXT();
    OP(SETMATSUBSET, 0, CONSTOP(0), LABELOP(0)): DO_SETMATSUBSET(rho); NEXT();
    OP(AND1ST, 2, CONSTOP(1), LABELOP(2)): {
	SEXP call = GETCONSTOP();
	LABEL_TYPE label = GETLABELOP();
	FIXUP_SCALAR_LOGICAL(call, "'x'", "&&");
	value = GETSTACK(-1);
	if (LOGICAL(value)[0] == FALSE)
	    pc = PC_FOR_LABEL(label);
	NEXT();
    }
    OP(AND2ND, 1, CONSTOP(1), LABELOP(0)): {
	SEXP call = GETCONSTOP();
	FIXUP_SCALAR_LOGICAL(call, "'y'", "&&");
	value = GETSTACK(-1);
	/* The first argument is TRUE or NA. If the second argument is
	   not TRUE then its value is the result. If the second
	   argument is TRUE, then the first argument's value is the
	   result. */
	if (LOGICAL(value)[0] != TRUE)
	    SETSTACK(-2, value);
	R_BCNodeStackTop -= 1;
	NEXT();
    }
    OP(OR1ST, 2, CONSTOP(1), LABELOP(2)):  {
	SEXP call = GETCONSTOP();
	LABEL_TYPE label = GETLABELOP();
	FIXUP_SCALAR_LOGICAL(call, "'x'", "||");
	value = GETSTACK(-1);
	if (LOGICAL(value)[0] != NA_LOGICAL && LOGICAL(value)[0]) /* is true */
	    pc = PC_FOR_LABEL(label);
	NEXT();
    }
    OP(OR2ND, 1, CONSTOP(1), LABELOP(0)):  {
	SEXP call = GETCONSTOP();
	FIXUP_SCALAR_LOGICAL(call, "'y'", "||");
	value = GETSTACK(-1);
	/* The first argument is FALSE or NA. If the second argument is
	   not FALSE then its value is the result. If the second
	   argument is FALSE, then the first argument's value is the
	   result. */
	if (LOGICAL(value)[0] != FALSE)
	    SETSTACK(-2, value);
	R_BCNodeStackTop -= 1;
	NEXT();
    }
    OP(GETVAR_MISSOK, 1, CONSTOP(0), LABELOP(0)): DO_GETVAR(FALSE, TRUE);
    OP(DDVAL_MISSOK, 1, CONSTOP(0), LABELOP(0)): DO_GETVAR(TRUE, TRUE);
    OP(VISIBLE, 0, CONSTOP(0), LABELOP(0)): R_Visible = TRUE; NEXT();
    OP(SETVAR2, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP symbol = GETCONSTOP();
	value = GETSTACK(-1);
	if (MAYBE_REFERENCED(value)) {
	    value = duplicate(value);
	    SETSTACK(-1, value);
	}
	setVar(symbol, value, ENCLOS(rho));
	NEXT();
      }
    OP(STARTASSIGN2, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP symbol = GETCONSTOP();
	value = GETSTACK(-1);
	BCNPUSH(getvar(symbol, ENCLOS(rho), FALSE, FALSE, NULL, 0));
	BCNPUSH(value);
	/* top three stack entries are now RHS value, LHS value, RHS value */
	FIXUP_RHS_NAMED(value);
	INCREMENT_REFCNT(value);
	NEXT();
      }
    OP(ENDASSIGN2, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP symbol = GETCONSTOP();
	value = BCNPOP();
	INCREMENT_NAMED(value);
	setVar(symbol, value, ENCLOS(rho));
	/* original right-hand side value is now on top of stack again */
#ifdef OLD_RHS_NAMED
	/* we do not duplicate the right-hand side value, so to be
	   conservative mark the value as NAMED = 2 */
	SET_NAMED(GETSTACK(-1), 2);
#else
	INCREMENT_NAMED(GETSTACK(-1));
#endif
	DECREMENT_REFCNT(GETSTACK(-1));
	NEXT();
      }
    OP(SETTER_CALL, 2, CONSTOP(1)|CONSTOP(2), LABELOP(0)):
      {
	SEXP lhs = GETSTACK(-5);
	SEXP rhs = GETSTACK(-4);
	SEXP fun = GETSTACK(-3);
	SEXP call = GETCONSTOP();
	SEXP vexpr = GETCONSTOP();
	SEXP args, prom, last;
	if (MAYBE_SHARED(lhs)) {
	  lhs = shallow_duplicate(lhs);
	  SETSTACK(-5, lhs);
	  SET_NAMED(lhs, 1);
	}
	switch (ftype) {
	case BUILTINSXP:
	  /* push RHS value onto arguments with 'value' tag */
	  PUSHCALLARG(rhs);
	  SET_TAG(GETSTACK(-1), R_valueSym);
	  /* replace first argument with LHS value */
	  args = GETSTACK(-2);
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  RELEASE_PROMARGS(args);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
	  /* promise won't be captured so don't track refrences */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* insert evaluated promise for RHS as last argument */
	  last = args;
	  while (CDR(last) != R_NilValue)
	      last = CDR(last);
	  prom = mkRHSPROMISE(vexpr, rhs);
	  SETCAR(last, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case CLOSXP:
	  /* push evaluated promise for RHS onto arguments with 'value' tag */
	  prom = mkRHSPROMISE(vexpr, rhs);
	  PUSHCALLARG(prom);
	  SET_TAG(GETSTACK(-1), R_valueSym);
	  /* replace first argument with evaluated promise for LHS */
	  /* promise might be captured, so track references */
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  args = GETSTACK(-2);
	  SETCAR(args, prom);
	  /* make the call */
	  value = applyClosure(call, fun, args, rho, R_BaseEnv);
	  RELEASE_PROMARGS(args); /* also releases prom which was allocated after args */
	  break;
	default: error(_("bad function"));
	}
	R_BCNodeStackTop -= 4;
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }
    OP(GETTER_CALL, 1, CONSTOP(1), LABELOP(0)):
      {
	SEXP lhs = GETSTACK(-5);
	SEXP fun = GETSTACK(-3);
	SEXP call = GETCONSTOP();
	SEXP args, prom;
	switch (ftype) {
	case BUILTINSXP:
	  /* replace first argument with LHS value */
	  args = GETSTACK(-2);
	  SETCAR(args, lhs);
	  /* make the call */
	  checkForMissings(args, call);
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  RELEASE_PROMARGS(args);
	  break;
	case SPECIALSXP:
	  /* duplicate arguments and put into stack for GC protection */
	  args = duplicate(CDR(call));
	  SETSTACK(-2, args);
	  /* insert evaluated promise for LHS as first argument */
	  /* promise won't be captured so don't track refrences */
	  prom = R_mkEVPROMISE_NR(R_TmpvalSymbol, lhs);
	  SETCAR(args, prom);
	  /* make the call */
	  value = PRIMFUN(fun) (call, fun, args, rho);
	  break;
	case CLOSXP:
	  /* replace first argument with evaluated promise for LHS */
	  /* promise might be captured, so track references */
	  prom = R_mkEVPROMISE(R_TmpvalSymbol, lhs);
	  args = GETSTACK(-2);
	  SETCAR(args, prom);
	  /* make the call */
	  value = applyClosure(call, fun, args, rho, R_BaseEnv);
	  RELEASE_PROMARGS(args);
	  break;
	default: error(_("bad function"));
	}
	R_BCNodeStackTop -= 2;
	SETSTACK(-1, value);
	ftype = 0;
	NEXT();
      }
    OP(SWAP, 0, CONSTOP(0), LABELOP(0)): {
	R_bcstack_t tmp = R_BCNodeStackTop[-1];
	/* This instruction only occurs between accessor calls in
	   complex assignments. [It should probably be renamed to
	   reflect this.] It needs to make sure intermediate LHS
	   values in complex assignments are not shared by duplicating
	   the extracted value in tmp when necessary. Duplicating is
	   necessary if the value might be shared _or_ if the
	   container, which is in R_BCNodeStackTop[-3], has become
	   possibly shared by going through a closure in the preceding
	   accessor call.  This is taken to indicate that the
	   corresponding replacement function might be a closure and
	   will need to see an unmodified LHS value. This heuristic
	   fails if the accessor function called here is not a closure
	   but the replacement function is. */
	if (MAYBE_REFERENCED(tmp) &&
	    (MAYBE_SHARED(tmp) || MAYBE_SHARED(R_BCNodeStackTop[-3])))
	    tmp = shallow_duplicate(tmp);
	R_BCNodeStackTop[-1] = R_BCNodeStackTop[-2];
	R_BCNodeStackTop[-2] = tmp;
	NEXT();
    }
    OP(DUP2ND, 0, CONSTOP(0), LABELOP(0)): BCNDUP2ND(); NEXT();
    OP(SWITCH, 4, CONSTOP(1)|CONSTOP(2)|CONSTOP(3)|CONSTOP(4), LABELOP(0)): {
       SEXP call = GETCONSTOP();
       SEXP names = GETCONSTOP();
       SEXP coffsets = GETCONSTOP();
       SEXP ioffsets = GETCONSTOP();
       value = BCNPOP();
       if (!isVector(value) || length(value) != 1)
	   errorcall(call, _("EXPR must be a length 1 vector"));
       if (TYPEOF(value) == STRSXP) {
	   int i, n, which;
	   if (names == R_NilValue)
	       errorcall(call, _("numeric EXPR required for 'switch' without named alternatives"));
	   if (TYPEOF(coffsets) != INTSXP)
	       errorcall(call, "bad character 'switch' offsets");
	   if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(coffsets))
	       errorcall(call, "bad 'switch' names");
	   n = LENGTH(names);
	   which = n - 1;
	   for (i = 0; i < n - 1; i++)
	       if (pmatch(STRING_ELT(value, 0),
			  STRING_ELT(names, i), 1 /* exact */)) {
		   which = i;
		   break;
	       }
	   pc = codebase + INTEGER(coffsets)[which];
       }
       else {
	   if (TYPEOF(ioffsets) != INTSXP)
	       errorcall(call, "bad numeric 'switch' offsets");
	   int which = asInteger(value);
	   if (which != NA_INTEGER) which--;
	   if (which < 0 || which >= LENGTH(ioffsets))
	       which = LENGTH(ioffsets) - 1;
	   pc = codebase + INTEGER(ioffsets)[which];
       }
       NEXT();
    }
    OP(RETURNJMP, 0, CONSTOP(0), LABELOP(0)): {
      value = BCNPOP();
      findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, value);
    }
    OP(STARTVECSUBSET, 2, CONSTOP(1), LABELOP(2)): DO_STARTDISPATCH_N(R_SubsetSym);
    OP(STARTMATSUBSET, 2, CONSTOP(1), LABELOP(2)): DO_STARTDISPATCH_N(R_SubsetSym);
    OP(STARTSETVECSUBSET, 2, CONSTOP(1), LABELOP(2)): DO_START_ASSIGN_DISPATCH_N(R_SubassignSym);
    OP(STARTSETMATSUBSET, 2, CONSTOP(1), LABELOP(2)): DO_START_ASSIGN_DISPATCH_N(R_SubassignSym);
    LASTOP;
  }

 done:
  R_BCNodeStackTop = oldntop;
#ifdef BC_INT_STACK
  R_BCIntStackTop = olditop;
#endif
#ifdef BC_PROFILING
  current_opcode = old_current_opcode;
#endif
  return value;
}

#ifdef THREADED_CODE
SEXP R_bcEncode(SEXP bytes, SEXP constants)
{
    SEXP code;
    BCODE *pc;
    int *ipc, i, j, k, nargs, n, m, v;
    unsigned map;

    
    /* check integrity of constant pool */
    
    n = LENGTH(constants);
    if (n < 2) {
      error(_("Invalid constants length: %d"), n);
    } 

    m = asInteger(VECTOR_ELT(constants, n - 1));
    if (m < 0 || m > n - 2) {
      error(_("Invalid number of variable names %d declared to be in the constant pool of length %d.\n"), m, n);
    }

    m = (sizeof(BCODE) + sizeof(int) - 1) / sizeof(int);

    n = LENGTH(bytes);
    ipc = INTEGER(bytes);

    v = ipc[0];
    if (v < R_bcMinVersion || v > R_bcVersion) {
	code = allocVector(INTSXP, m * 2);
	pc = (BCODE *) INTEGER(code);
	pc[0].i = v;
	pc[1].v = opinfo[BCMISMATCH_OP].addr;
	return code;
    }
    else {
	code = allocVector(INTSXP, m * n);
	pc = (BCODE *) INTEGER(code);

	/* disassembly (debugging */
        /* FIXME: it would be nice to move this into a separate function and run it only in case of error encountered. */
        /* FIXME: it would be nice to have a nicer disassembler which will also understand at least some of the constants */
        if (FALSE) {
            fprintf(stderr, "bcEncode: debug disassembly\n");
            for(i = 1; i < n;) {
                int op = ipc[i];
                if (op <0 || op >= OPCOUNT) {
                    fprintf(stderr, "%d: invalid instruction %d (opcount is %d)", i, ipc[i], OPCOUNT);
                    break;
                } else {
                    char *name = opinfo[op].instname;
                    nargs = opinfo[op].argc;
            
                    fprintf(stderr, "%d: %s (%d)", i, name, op);
                    for (j = 0; j < nargs; j++) {
                        fprintf(stderr, " %d", ipc[j]);
                    }
                    i += nargs + 1;
                }
                fprintf(stderr, "\n");
            }
            fprintf(stderr, "constant pool size: %d\n", length(constants));
        }

	for (i = 0; i < n; i++) pc[i].i = ipc[i];

	/* install the current version number */
	pc[0].i = R_bcVersion;

	for (i = 1; i < n;) {
	    int op = pc[i].i;
	    if (op < 0 || op >= OPCOUNT) {
		error("bcEncode: unknown instruction code %d", op);
            }
	    pc[i].v = opinfo[op].addr;
	    nargs = opinfo[op].argc;
	    
#ifdef RESOLVE_CONST_OP
            map = opinfo[op].sexpmap;
            for (j = 0; j < nargs; j++) {
                k = i + j + 1;
                if (map&1) {
                    if (pc[k].i < 0 || pc[k].i >= length(constants)) {
                        error("bcEncode: constant pool index %d out of bounds %d at code index %d (instruction %d = %s)", 
                            pc[k].i, length(constants), i, op, opinfo[op].instname);
                    }
                    pc[k].sexp = VECTOR_ELT(constants, pc[k].i);
                }
                map >>= 1;
            }
#endif /* RESOLVE_CONST_OP */

#ifdef DIRECT_LABELS
            map = opinfo[op].labelmap;
            for (j = 0; j < nargs; j++) {
                k = i + j + 1;
                if (map&1) {
                    if (pc[k].i < 0 || pc[k].i >= n) {
                      error("bcEncode: label instruction index %d out of bounds %d (instruction %d = %s)", 
                          pc[k].i, n, op, opinfo[op].instname);
                    }
                    pc[k].label = pc + pc[k].i;
                }                
                map >>= 1;
            }
#endif /* DIRECT_LABELS */
	    i += nargs + 1;
	}
	return code;
    }
}

static int findOp(void *addr)
{
    int i;

    for (i = 0; i < OPCOUNT; i++)
	if (opinfo[i].addr == addr)
	    return i;
    error(_("cannot find index for threaded code address"));
    return 0; /* not reached */
}

static int findSEXP(SEXP sexp, SEXP constants) {

  int nconstants, i;
  
  nconstants = length(constants);
  for (i = 0; i < nconstants; i++) {
    if (VECTOR_ELT(constants, i) == sexp) {
      return i;
    } 
  }
  error(_("cannot decode bytecode, invalid resolved SEXP"));
}

SEXP R_bcDecode(SEXP code, SEXP constants) {
    int n, i, j, ii, *ipc;
    BCODE *pc;
    SEXP bytes;
    unsigned map;

    int m = (sizeof(BCODE) + sizeof(int) - 1) / sizeof(int);

    n = LENGTH(code) / m;
    pc = (BCODE *) INTEGER(code);

    bytes = allocVector(INTSXP, n);
    ipc = INTEGER(bytes);

    /* copy the version number */
    ipc[0] = pc[0].i;

    for (i = 1; i < n;) {
	int op = findOp(pc[i].v);
	int argc = opinfo[op].argc;
	ipc[i] = op;
	i++;
	ii = i;
	
	for (j = 0; j < argc; j++, i++)
	    ipc[i] = pc[i].i;	
	
#ifdef RESOLVE_CONST_OP
        map = opinfo[op].sexpmap;
        for (j = 0, i = ii; j < argc; j++, i++) {
            if (map&1) {
	        ipc[i] = findSEXP(pc[i].sexp, constants);
            }
            map >>= 1;
        }
#endif	

#ifdef DIRECT_LABELS
        map = opinfo[op].labelmap;
        for (j = 0, i = ii; j < argc; j++, i++) {
            if (map&1) {
	        ipc[i] = pc[i].label - pc;
            }
            map >>= 1;
        }
#endif	    
    }

    return bytes;
}
#else
SEXP R_bcEncode(SEXP x, SEXP c) { return x; }
SEXP R_bcDecode(SEXP x, SEXP c) { return duplicate(x); }
#endif

SEXP attribute_hidden do_mkcode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP bytes, consts, ans;

    checkArity(op, args);
    bytes = CAR(args);
    consts = CADR(args);
    ans = CONS(R_bcEncode(bytes, consts), consts);
    SET_TYPEOF(ans, BCODESXP);
    return ans;
}

SEXP attribute_hidden do_bcclose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP forms, body, env;

    checkArity(op, args);
    forms = CAR(args);
    body = CADR(args);
    env = CADDR(args);

    CheckFormals(forms);

    if (! isByteCode(body))
	errorcall(call, _("invalid body"));

    if (isNull(env)) {
	error(_("use of NULL environment is defunct"));
	env = R_BaseEnv;
    } else
    if (!isEnvironment(env))
	errorcall(call, _("invalid environment"));

    return mkCLOSXP(forms, body, env);
}

SEXP attribute_hidden do_is_builtin_internal(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP symbol, i;

    checkArity(op, args);
    symbol = CAR(args);

    if (!isSymbol(symbol))
	errorcall(call, _("invalid symbol"));

    if ((i = INTERNAL(symbol)) != R_NilValue && TYPEOF(i) == BUILTINSXP)
	return R_TrueValue;
    else
	return R_FalseValue;
}

static SEXP disassemble(SEXP bc)
{
  SEXP ans, dconsts;
  int i;
  SEXP code = BCODE_CODE(bc);
  SEXP consts = BCODE_CONSTS(bc);
  SEXP expr = BCODE_EXPR(bc);
  int nc = LENGTH(consts);

  PROTECT(ans = allocVector(VECSXP, expr != R_NilValue ? 4 : 3));
  SET_VECTOR_ELT(ans, 0, install(".Code"));
  SET_VECTOR_ELT(ans, 1, R_bcDecode(code, consts));
  SET_VECTOR_ELT(ans, 2, allocVector(VECSXP, nc));
  if (expr != R_NilValue)
      SET_VECTOR_ELT(ans, 3, duplicate(expr));

  dconsts = VECTOR_ELT(ans, 2);
  for (i = 0; i < nc; i++) {
    SEXP c = VECTOR_ELT(consts, i);
    if (isByteCode(c))
      SET_VECTOR_ELT(dconsts, i, disassemble(c));
    else
      SET_VECTOR_ELT(dconsts, i, duplicate(c));
  }

  UNPROTECT(1);
  return ans;
}

SEXP attribute_hidden do_disassemble(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP code;

  checkArity(op, args);
  code = CAR(args);
  if (! isByteCode(code))
    errorcall(call, _("argument is not a byte code object"));
  return disassemble(code);
}

SEXP attribute_hidden do_bcversion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  SEXP ans = allocVector(INTSXP, 1);
  INTEGER(ans)[0] = R_bcVersion;
  return ans;
}

SEXP attribute_hidden do_loadfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file, s;
    FILE *fp;

    checkArity(op, args);

    PROTECT(file = coerceVector(CAR(args), STRSXP));

    if (! isValidStringF(file))
	errorcall(call, _("bad file name"));

    fp = RC_fopen(STRING_ELT(file, 0), "rb", TRUE);
    if (!fp)
	errorcall(call, _("unable to open 'file'"));
    s = R_LoadFromFile(fp, 0);
    fclose(fp);

    UNPROTECT(1);
    return s;
}

SEXP attribute_hidden do_savefile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    FILE *fp;

    checkArity(op, args);

    if (!isValidStringF(CADR(args)))
	errorcall(call, _("'file' must be non-empty string"));
    if (TYPEOF(CADDR(args)) != LGLSXP)
	errorcall(call, _("'ascii' must be logical"));

    fp = RC_fopen(STRING_ELT(CADR(args), 0), "wb", TRUE);
    if (!fp)
	errorcall(call, _("unable to open 'file'"));

    R_SaveToFileV(CAR(args), fp, INTEGER(CADDR(args))[0], 0);

    fclose(fp);
    return R_NilValue;
}

#ifdef UNUSED
#define R_COMPILED_EXTENSION ".Rc"

/* neither of these functions call R_ExpandFileName -- the caller
   should do that if it wants to */
char *R_CompiledFileName(char *fname, char *buf, size_t bsize)
{
    char *basename, *ext;

    /* find the base name and the extension */
    basename = Rf_strrchr(fname, FILESEP[0]);
    if (basename == NULL) basename = fname;
    ext = Rf_strrchr(basename, '.');

    if (ext != NULL && strcmp(ext, R_COMPILED_EXTENSION) == 0) {
	/* the supplied file name has the compiled file extension, so
	   just copy it to the buffer and return the buffer pointer */
	if (snprintf(buf, bsize, "%s", fname) < 0)
	    error("R_CompiledFileName: buffer too small");
	return buf;
    }
    else if (ext == NULL) {
	/* if the requested file has no extention, make a name that
	   has the extenrion added on to the expanded name */
	if (snprintf(buf, bsize, "%s%s", fname, R_COMPILED_EXTENSION) < 0)
	    error("R_CompiledFileName: buffer too small");
	return buf;
    }
    else {
	/* the supplied file already has an extension, so there is no
	   corresponding compiled file name */
	return NULL;
    }
}

FILE *R_OpenCompiledFile(char *fname, char *buf, size_t bsize)
{
    char *cname = R_CompiledFileName(fname, buf, bsize);

    if (cname != NULL && R_FileExists(cname) &&
	(strcmp(fname, cname) == 0 ||
	 ! R_FileExists(fname) ||
	 R_FileMtime(cname) > R_FileMtime(fname)))
	/* the compiled file cname exists, and either fname does not
	   exist, or it is the same as cname, or both exist and cname
	   is newer */
	return R_fopen(buf, "rb");
    else return NULL;
}
#endif

SEXP attribute_hidden do_growconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP constBuf, ans;
    int i, n;

    checkArity(op, args);
    constBuf = CAR(args);
    if (TYPEOF(constBuf) != VECSXP)
	error(_("constant buffer must be a generic vector"));

    n = LENGTH(constBuf);
    ans = allocVector(VECSXP, 2 * n);
    for (i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, VECTOR_ELT(constBuf, i));

    return ans;
}

SEXP attribute_hidden do_putconst(SEXP call, SEXP op, SEXP args, SEXP env) {
    checkArity(op, args);
    RETURN_EARG3(do_earg_putconst, call, op, args, env);
}

SEXP attribute_hidden do_earg_putconst(SEXP call, SEXP op, SEXP arg_constBuf, SEXP arg_constCount, SEXP arg_x, SEXP env)
{
    SEXP constBuf, x;
    int i, constCount;

    constBuf = arg_constBuf;
    if (TYPEOF(constBuf) != VECSXP)
	error(_("constant buffer must be a generic vector"));

    constCount = asInteger(arg_constCount);
    if (constCount < 0 || constCount >= LENGTH(constBuf))
	error("bad constCount value");

    x = arg_x;

    /* check for a match and return index if one is found */
    for (i = 0; i < constCount; i++) {
	SEXP y = VECTOR_ELT(constBuf, i);
	if (x == y || R_compute_identical(x, y, 0))
	    return ScalarInteger(i);
    }

    /* otherwise insert the constant and return index */
    SET_VECTOR_ELT(constBuf, constCount, x);
    return ScalarInteger(constCount);
}

SEXP attribute_hidden do_getconst(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP theExpression, constBuf, varnameBuf, ans;
    int i, j, constCount, varnameCount;

    checkArity(op, args);
    theExpression = CAR(args); args = CDR(args);
    varnameBuf = CAR(args); args = CDR(args);
    varnameCount = asInteger(CAR(args)); args = CDR(args);
    constBuf = CAR(args); args = CDR(args);
    constCount = asInteger(CAR(args));

    if (TYPEOF(varnameBuf) != VECSXP)
	error(_("varname buffer must be a generic vector"));
    if (varnameCount < 0 || varnameCount > LENGTH(varnameBuf))
	error(_("bad varname count %d (varname buf length is %d)"), varnameCount, varnameBuf);

    if (TYPEOF(constBuf) != VECSXP)
	error(_("constant buffer must be a generic vector"));
    if (constCount < 0 || constCount > LENGTH(constBuf))
	error(_("bad constant count %d (constant buf length is %d)"), constCount, constBuf);

    PROTECT(ans = allocVector(VECSXP, varnameCount + constCount + 2));
    j = 0;
    SET_VECTOR_ELT(ans, j++, theExpression);
    for (i = 0; i < varnameCount; i++)
	SET_VECTOR_ELT(ans, j++, VECTOR_ELT(varnameBuf, i));
    for (i = 0; i < constCount; i++)
	SET_VECTOR_ELT(ans, j++, VECTOR_ELT(constBuf, i));
    SET_VECTOR_ELT(ans, j++, ScalarInteger(varnameCount)); 
    UNPROTECT(1);
    return ans;
}

#ifdef BC_PROFILING
SEXP R_getbcprofcounts()
{
    SEXP val;
    int i;

    val = allocVector(INTSXP, OPCOUNT);
    for (i = 0; i < OPCOUNT; i++)
	INTEGER(val)[i] = opcode_counts[i];
    return val;
}

static void dobcprof(int sig)
{
    if (current_opcode >= 0 && current_opcode < OPCOUNT)
	opcode_counts[current_opcode]++;
    signal(SIGPROF, dobcprof);
}

SEXP R_startbcprof()
{
    struct itimerval itv;
    int interval;
    double dinterval = 0.02;
    int i;

    if (R_Profiling)
	error(_("profile timer in use"));
    if (bc_profiling)
	error(_("already byte code profiling"));

    /* according to man setitimer, it waits until the next clock
       tick, usually 10ms, so avoid too small intervals here */
    interval = 1e6 * dinterval + 0.5;

    /* initialize the profile data */
    current_opcode = NO_CURRENT_OPCODE;
    for (i = 0; i < OPCOUNT; i++)
	opcode_counts[i] = 0;

    signal(SIGPROF, dobcprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	error(_("setting profile timer failed"));

    bc_profiling = TRUE;

    return R_NilValue;
}

static void dobcprof_null(int sig)
{
    signal(SIGPROF, dobcprof_null);
}

SEXP R_stopbcprof()
{
    struct itimerval itv;

    if (! bc_profiling)
	error(_("not byte code profiling"));

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, dobcprof_null);

    bc_profiling = FALSE;

    return R_NilValue;
}
#endif

/* end of byte code section */

SEXP attribute_hidden do_setnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_num_math_threads, new;
    checkArity(op, args);
    new = asInteger(CAR(args));
    if (new >= 0 && new <= R_max_num_math_threads)
	R_num_math_threads = new;
    return ScalarInteger(old);
}

SEXP attribute_hidden do_setmaxnumthreads(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int old = R_max_num_math_threads, new;
    checkArity(op, args);
    new = asInteger(CAR(args));
    if (new >= 0) {
	R_max_num_math_threads = new;
	if (R_num_math_threads > R_max_num_math_threads)
	    R_num_math_threads = R_max_num_math_threads;
    }
    return ScalarInteger(old);
}

/* checks if an .Internal function supports explicit args, and if so, how many args are supported */
/* returns -1 if not supported (or not an .Internal), otherwise the number of args */

SEXP attribute_hidden do_internal_supports_earg(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP prim = CAR(args);

    SEXP sym;
    if (TYPEOF(prim) == CHARSXP) {
        if (LENGTH(prim) != 1 )
	    error(_("invalid length of '%s' argument"), "prim");
        sym = installCharSXP(prim);
    } else if (TYPEOF(prim) == SYMSXP) {
        sym = prim;
    } else {
        error(_("invalid type of '%s' argument"), "prim");
    }
    
    SEXP value = INTERNAL(sym);
       
    if (TYPEOF(value) != BUILTINSXP) {
//        printf("internal.supports.earg: %s does not support earg because its type is %s\n", CHAR(PRINTNAME(sym)), type2char(TYPEOF(value)));
        return ScalarInteger(-1);
    }
    int arity = PRIMARITY(value);
    if (arity == -1) { 
//        printf("internal.supports.earg: %s does not support earg because its arity is -1\n", CHAR(PRINTNAME(sym)));
        return ScalarInteger(-1);
    }
    EARG_CCODE fun = PRIMEARGFUN(value);
    if (fun.ptr == NULL) {
//        printf("internal.supports.earg: %s does not support earg because its earg do function is NULL\n", CHAR(PRINTNAME(sym)));
        return ScalarInteger(-1);
    }
    return ScalarInteger(arity);
}


/* checks if a true builtin (builtin which is not .Internal) supports explicit args, and if so, how many args are supported */
/* returns -1 if not supported (or not a true builtin), otherwise the number of args */

SEXP attribute_hidden do_true_builtin_supports_earg(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP prim = CAR(args);

    SEXP sym;
    if (TYPEOF(prim) == CHARSXP) {
        if (LENGTH(prim) != 1 )
	    error(_("invalid length of '%s' argument"), "prim");
        sym = installCharSXP(prim);
    } else if (TYPEOF(prim) == SYMSXP) {
        sym = prim;
    } else {
        error(_("invalid type of '%s' argument"), "prim");
    }
    
    SEXP value = INTERNAL(sym);
    if (TYPEOF(value) == BUILTINSXP) {
//        printf("true.builtin.supports.earg: %s does not support earg because it is an .Internal\n", CHAR(PRINTNAME(sym)));
        return ScalarInteger(-1);
    }
    
    SEXP symvalue = SYMVALUE(sym);
    
    if (TYPEOF(symvalue) == PROMSXP) {
        PROTECT(symvalue);
        symvalue = forcePromise(symvalue);
        SET_NAMED(symvalue, 2);
        UNPROTECT(1);
    } 
       
    if (TYPEOF(symvalue) != BUILTINSXP) {
//        printf("true.builtin.supports.earg: %s does not support earg because its type is %s (not a builtin, or overridden)\n", CHAR(PRINTNAME(sym)), type2char(TYPEOF(symvalue)));
        return ScalarInteger(-1);
    }
    int arity = PRIMARITY(symvalue);
    if (arity == -1) { 
//        printf("true.builtin.supports.earg: %s does not support earg because its arity is -1\n", CHAR(PRINTNAME(sym)));
        return ScalarInteger(-1);
    }
    EARG_CCODE fun = PRIMEARGFUN(symvalue);
    if (fun.ptr == NULL) {
//        printf("true.builtin.supports.earg: %s does not support earg because its earg do function is NULL\n", CHAR(PRINTNAME(sym)));
        return ScalarInteger(-1);
    }
    return ScalarInteger(arity);
}
