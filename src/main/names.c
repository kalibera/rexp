/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2015  The R Core Team
 *  Copyright (C) 2003--2015  The R Foundation
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

#define __R_Names__ /* used in Defn.h for extern on R_FunTab */
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>

#include <Print.h>
#include "arithmetic.h" /* for do_math[1234], do_cmathfuns */

#include <Rinterface.h>
#include <Rdynpriv.h>

static SEXP dcfun(SEXP call, SEXP op, SEXP args, SEXP env);

/* Table of  .Internal(.) and .Primitive(.)  R functions
 * =====     =========	      ==========
 *
 * Each entry is a line with
 *
 *  printname	c-entry	 offset	 eval	arity	  pp-kind   precedence	    rightassoc
 *  ---------	-------	 ------	 ----	-----	  -------   ----------	    ----------
 *2 name	cfun	 code	 eval	arity	  gram.kind gram.precedence gram.rightassoc
 *3 PRIMNAME	PRIMFUN	 PRIMVAL [*]    PRIMARITY PPINFO    PPINFO	    PPINFO
 *
 * where "2" are the component names of the FUNTAB struct (Defn.h)
 * and	 "3" are the accessor macros. [*]: PRIMPRINT(.) uses the eval component
 *
 * printname:	The function name in R
 *
 * c-entry:	The name of the corresponding C function,
 *		actually declared in ../include/Internal.h .
 *		Convention:
 *		 - all start with "do_",
 *		 - all return SEXP.
 *		 - all have argument list
 *			 (SEXP call, SEXP op, SEXP args, SEXP env)
 *
 * offset:	the 'op' (offset pointer) above; used for C functions
 *		which deal with more than one R function...
 *
 * eval:	= XYZ (three digits) --- where e.g. '1' means '001'
 *		X=1 says that we should force R_Visible off
 *		X=0 says that we should force R_Visible on
 *		X=2 says that we should switch R_Visible on but let the C
 *                  code update this.
 *		Y=1 says that this is an internal function which must
 *		    be accessed with a	.Internal(.) call, any other value is
 *		    accessible directly and printed in R as ".Primitive(..)".
 *		Z=1 says evaluate arguments before calling (BUILTINSXP) and
 *		Z=0 says don't evaluate (SPECIALSXP).
 *
 * arity:	How many arguments are required/allowed;  "-1"	meaning ``any''
 *
 * pp-kind:	Deparsing Info (-> PPkind in ../include/Defn.h )
 *
 * precedence: Operator precedence (-> PPprec in ../include/Defn.h )
 *
 * rightassoc: Right (1) or left (0) associative operator
 *
 */

FUNTAB R_FunTab[] =
{

/* printname	c-entry		offset	eval	arity	pp-kind	     precedence	rightassoc
 * ---------	-------		------	----	-----	-------      ----------	----------*/

/* Language Related Constructs */

/* Primitives */
{"if",		do_if,		0,	200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"while",	do_while,	0,	100,	-1,	{PP_WHILE,   PREC_FN,	  0}},
{"for",		do_for,		0,	100,	-1,	{PP_FOR,     PREC_FN,	  0}},
{"repeat",	do_repeat,	0,	100,	-1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break, CTXT_BREAK,	0,	-1,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_break, CTXT_NEXT,	0,	-1,	{PP_NEXT,    PREC_FN,	  0}},
{"return",	do_return,	0,	0,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set,		1,	100,	-1,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set,		3,	100,	-1,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set,		2,	100,	-1,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"{",		do_begin,	0,	200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"(",		dcfun,	0,	1,	1,	{PP_PAREN,   PREC_FN,	  0}, (DL_FUNC) dc_paren},
{".subset",	do_subset_dflt,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"[",		do_subset,	1,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2,	2,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3,	3,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"@",		do_AT,		0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"[<-",		do_subassign,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"switch",	do_switch,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"browser",	do_browser,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	do_trace,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primUntrace",do_trace,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Internal",	do_internal,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	do_primitive,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"call",	do_call,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"quote",	do_quote,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"substitute",	do_substitute,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"nargs",	do_nargs,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"on.exit",	do_onexit,	0,	100,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"forceAndCall",do_forceAndCall,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

/* .Internals */

{"stop",	do_stop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"warning",	do_warning,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"gettext",	do_gettext,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"ngettext",	do_ngettext,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"bindtextdomain",do_bindtextdomain,0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addCondHands",do_addCondHands,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{".resetCondHands",dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_resetCondHands},
{".signalCondition",dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_signalCondition},
{".dfltStop",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_dfltStop},
{".dfltWarn",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_dfltWarn},
{".addRestart",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_addRestart},
{".getRestart",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_getRestart},
{".invokeRestart",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_invokeRestart},
{".addTryHandlers",do_addTryHandlers,	0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"geterrmessage",dcfun, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_geterrmessage},
{"seterrmessage",dcfun, 0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_seterrmessage},
{"printDeferredWarnings",dcfun, 0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_printDeferredWarnings},
{"interruptsSuspended",do_interruptsSuspended, 0,	11,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"restart",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_restart},
{"as.function.default",do_asfunction,0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}},
{"debug",	do_debug,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"undebug",	do_debug,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"isdebugged",	do_debug,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"debugonce",	do_debug,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"delayedAssign",do_delayed,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"makeLazy",	do_makelazy,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{"identical",	do_identical,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	  0}},


/* Binary Operators, all primitives */
/* these are group generic and so need to eval args */
{"+",		do_arith,	PLUSOP,	1,	-1,	{PP_BINARY,  PREC_SUM,	  0}},
{"-",		do_arith,	MINUSOP,1,	-1,	{PP_BINARY,  PREC_SUM,	  0}},
{"*",		do_arith,	TIMESOP,1,	2,	{PP_BINARY,  PREC_PROD,	  0}},
{"/",		do_arith,	DIVOP,	1,	2,	{PP_BINARY2, PREC_PROD,	  0}},
{"^",		do_arith,	POWOP,	1,	2,	{PP_BINARY2, PREC_POWER,  1}},
{"%%",		do_arith,	MODOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%/%",		do_arith,	IDIVOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%*%",		do_matprod,	0,	1,	2,	{PP_BINARY,  PREC_PERCENT,0}},

{"==",		do_relop,	EQOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"!=",		do_relop,	NEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<",		do_relop,	LTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<=",		do_relop,	LEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">=",		do_relop,	GEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">",		do_relop,	GTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"&",		do_logic,	1,	1,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"|",		do_logic,	2,	1,	2,	{PP_BINARY,  PREC_OR,	  0}},
{"!",		do_logic,	3,	1,	1,	{PP_UNARY,   PREC_NOT,	  0}},

/* specials as conditionally evaluate second arg */
{"&&",		do_logic2,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_logic2,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},
{":",		do_colon,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}},
/* does not evaluate */
{"~",		do_tilde,	0,	0,	-1,	{PP_BINARY,  PREC_TILDE,  0}},


/* Logic Related Functions */
/* these are group generic and so need to eval args */
{"all",		do_logic3,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"any",		do_logic3,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},


/* Vectors, Matrices and Arrays */

/* Primitives */

{"length",	do_length,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"length<-",	do_lengthgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"c",/* bind.c:*/do_c,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass",	do_class,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass<-",	do_classgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT, 1}},
{"class",	R_do_data_class,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{".cache_class",R_do_data_class,1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"class<-",	R_do_set_class,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"unclass",	do_unclass,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"names",	do_names,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"names<-",	do_namesgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"dimnames",	do_dimnames,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dimnames<-",	do_dimnamesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"dim",		do_dim,		0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dim<-",	do_dimgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"attributes",	do_attributes,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"attributes<-",do_attributesgets,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"attr",	do_attr,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"attr<-",	do_attrgets,	0,	1,	3,	{PP_FUNCALL, PREC_LEFT,	1}},
{"@<-",		do_attrgets,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"levels<-",	do_levelsgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},

/* .Internals */

{"vector",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_makevector},
{"complex",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_complex},
{"matrix",	do_matrix,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"array",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_array},
{"diag",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_diag},
{"backsolve",	do_backsolve,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"max.col",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_maxcol},
{"row",		do_rowscols,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"col",		do_rowscols,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"unlist",	do_unlist,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cbind",	do_bind,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"drop",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_drop},
{"all.names",	do_allnames,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"comment",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_comment},
{"comment<-",	do_commentgets,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"get",		do_get,		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"get0",	do_get,		2,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"mget",	do_mget,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"exists",	do_get,		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"assign",	dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_assign},
{"list2env",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_list2env},
{"remove",	do_remove,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"duplicated",	do_duplicated,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"unique",	do_duplicated,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"anyDuplicated",do_duplicated,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"anyNA",	do_anyNA,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_which},
{"which.min",	do_first_min,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmin",	do_pmin,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	do_first_min,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"match",	do_match,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pmatch",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pmatch},
{"charmatch",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_charmatch},
{"match.call",	do_matchcall,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"crossprod",	do_matprod,	1,	11,	2,	{PP_FUNCALL, PREC_FN,   0}},
{"tcrossprod",	do_matprod,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lengths",	do_lengths,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"attach",	do_attach,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"detach",	dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_detach},
{"search",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_search},
{"setFileTime",	dcfun,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setFileTime},


/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
{"round",	do_Math2,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"signif",	do_Math2,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log",		do_log,		10003,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log10",	do_log1arg,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log2",	do_log1arg,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"abs",		do_abs,		6,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"floor",	do_math1,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ceiling",	do_math1,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sqrt",	do_math1,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sign",	do_math1,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trunc",	do_trunc,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"exp",		do_math1,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expm1",	do_math1,	11,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log1p",	do_math1,	12,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cos",		do_math1,	20,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sin",		do_math1,	21,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tan",		do_math1,	22,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acos",	do_math1,	23,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asin",	do_math1,	24,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atan",	do_math1,	25,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cosh",	do_math1,	30,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinh",	do_math1,	31,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanh",	do_math1,	32,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acosh",	do_math1,	33,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asinh",	do_math1,	34,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atanh",	do_math1,	35,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"lgamma",	do_math1,	40,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gamma",	do_math1,	41,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"digamma",	do_math1,	42,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trigamma",	do_math1,	43,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
/* see "psigamma" below !*/

{"cospi",	do_math1,	47,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinpi",	do_math1,	48,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanpi",	do_math1,	49,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/*
  Can remove all the [dpqr]xxx once the compiler knows how to optimize
  to .External.

  This is most of the do_math[23], and all of the do_math4, do_random[123]
*/
{"dchisq",	do_math2,	6,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pchisq",	do_math2,	7,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qchisq",	do_math2,	8,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dexp",	do_math2,	9,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pexp",	do_math2,	10,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qexp",	do_math2,	11,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgeom",	do_math2,	12,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgeom",	do_math2,	13,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgeom",	do_math2,	14,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dpois",	do_math2,	15,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ppois",	do_math2,	16,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qpois",	do_math2,	17,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dt",		do_math2,	18,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pt",		do_math2,	19,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qt",		do_math2,	20,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dsignrank",	do_math2,	21,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"psignrank",	do_math2,	22,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsignrank",	do_math2,	23,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselJ",	do_math2,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of a Complex Argument */
/* these are group generic and so need to eval args */

{"Re",		do_cmathfuns,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Im",		do_cmathfuns,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Mod",		do_cmathfuns,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Arg",		do_cmathfuns,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Conj",	do_cmathfuns,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3,	1,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbeta",	do_math3,	2,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbeta",	do_math3,	3,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dbinom",	do_math3,	4,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbinom",	do_math3,	5,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbinom",	do_math3,	6,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dcauchy",	do_math3,	7,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pcauchy",	do_math3,	8,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qcauchy",	do_math3,	9,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"df",		do_math3,	10,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pf",		do_math3,	11,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qf",		do_math3,	12,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgamma",	do_math3,	13,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgamma",	do_math3,	14,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgamma",	do_math3,	15,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlnorm",	do_math3,	16,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plnorm",	do_math3,	17,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlnorm",	do_math3,	18,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlogis",	do_math3,	19,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plogis",	do_math3,	20,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlogis",	do_math3,	21,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom",	do_math3,	22,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom",	do_math3,	23,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom",	do_math3,	24,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnorm",	do_math3,	25,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnorm",	do_math3,	26,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnorm",	do_math3,	27,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dunif",	do_math3,	28,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"punif",	do_math3,	29,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qunif",	do_math3,	30,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dweibull",	do_math3,	31,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pweibull",	do_math3,	32,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qweibull",	do_math3,	33,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnchisq",	do_math3,	34,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnchisq",	do_math3,	35,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnchisq",	do_math3,	36,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnt",		do_math3,	37,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnt",		do_math3,	38,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnt",		do_math3,	39,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dwilcox",	do_math3,	40,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pwilcox",	do_math3,	41,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qwilcox",	do_math3,	42,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselI",	do_math3,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"besselK",	do_math3,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom_mu",	do_math3,	45,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom_mu",	do_math3,	46,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom_mu",	do_math3,	47,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	do_math4,	1,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"phyper",	do_math4,	2,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qhyper",	do_math4,	3,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbeta",	do_math4,	4,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbeta",	do_math4,	5,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbeta",	do_math4,	6,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnf",		do_math4,	7,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnf",		do_math4,	8,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnf",		do_math4,	9,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dtukey",	do_math4,	10,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ptukey",	do_math4,	11,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qtukey",	do_math4,	12,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

/* Random Numbers */

{"rchisq",	do_random1,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rexp",	do_random1,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rgeom",	do_random1,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rpois",	do_random1,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rt",		do_random1,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rsignrank",	do_random1,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"rbeta",	do_random2,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rbinom",	do_random2,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rcauchy",	do_random2,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rf",		do_random2,	3,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rgamma",	do_random2,	4,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlnorm",	do_random2,	5,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlogis",	do_random2,	6,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom",	do_random2,	7,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom_mu",	do_random2,	13,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnchisq",	do_random2,	12,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnorm",	do_random2,	8,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"runif",	do_random2,	9,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rweibull",	do_random2,	10,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rwilcox",	do_random2,	11,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"rhyper",	do_random3,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},

{"sample",	do_sample,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sample2",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sample2},

{"RNGkind",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_RNGkind},
{"set.seed",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setseed},

/* Data Summaries */
/* these four are group generic and so need to eval args */
{"sum",		do_summary,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"min",		do_summary,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"max",		do_summary,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prod",	do_summary,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"mean",	do_summary,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"range",	do_range,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* Note that the number of arguments in this group only applies
   to the default method */
{"cumsum",	do_cum,		1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cumprod",	do_cum,		2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cummax",	do_cum,		3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cummin",	do_cum,		4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Type coercion */

{"as.character",do_asatomic,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.integer",	do_asatomic,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.double",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.numeric",	do_asatomic,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.complex",	do_asatomic,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.logical",	do_asatomic,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.raw",	do_asatomic,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.call",	do_ascall,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.environment",do_as_environment,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"storage.mode<-",do_storage_mode,0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"as.vector",	do_asvector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"paste",	do_paste,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"paste0",	do_paste,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.path",	do_filepath,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format",	do_format,	0,	11,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"format.info",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_formatinfo},
{"cat",		do_cat,		0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"do.call",	do_docall,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* String Manipulation */

{"nchar",	do_nchar,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"nzchar",	do_nzchar,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"substr",	dcfun,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_substr},
{"substr<-",	dcfun,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_substrgets},
{"strsplit",	do_strsplit,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"abbreviate",	dcfun,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_abbrev},
{"make.names",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_makenames},
{"pcre_config", dcfun,	1,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pcre_config},
{"grep",	do_grep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepl",	do_grep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepRaw",	do_grepraw,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"sub",		do_gsub,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"gsub",	do_gsub,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"regexpr",	do_regexpr,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gregexpr",	do_regexpr,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"regexec",	do_regexec,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"agrep",	do_agrep,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"agrepl",	do_agrep,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"adist",	do_adist,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"aregexec",	do_aregexec,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"tolower",	do_tolower,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"toupper",	do_tolower,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"chartr",	do_chartr,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sprintf",	do_sprintf,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"make.unique",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_makeunique},
{"charToRaw",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_charToRaw},
{"rawToChar",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rawToChar},
{"rawShift",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rawShift},
{"intToBits",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_intToBits},
{"rawToBits",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rawToBits},
{"packBits",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_packBits},
{"utf8ToInt",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_utf8ToInt},
{"intToUtf8",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_intToUtf8},
{"validUTF8",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_validUTF8},
{"validEnc",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_validEnc},
{"encodeString",dcfun,1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_encodeString},
{"iconv",	do_iconv,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"strtrim",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_strtrim},
{"strtoi",	do_strtoi,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strrep",	do_strrep,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Type Checking (typically implemented in ./coerce.c ) */

{"is.null",	do_is,		NILSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.logical",	do_is,		LGLSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.integer",	do_is,		INTSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.double",	do_is,		REALSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.complex",	do_is,		CPLXSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.character",do_is,		STRSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.symbol",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.name",	do_is,		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.environment",do_is,	ENVSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.list",	do_is,		VECSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.pairlist",	do_is,		LISTSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.expression",do_is,		EXPRSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.raw",	do_is,		RAWSXP, 1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.object",	do_is,		50,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"isS4",	do_is,		51,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.numeric",	do_is,		100,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.matrix",	do_is,		101,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.array",	do_is,		102,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.atomic",	do_is,		200,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.recursive",do_is,		201,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.call",	do_is,		300,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.language",	do_is,		301,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.function",	do_is,		302,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.single",	do_is,		999,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.na",	do_isna,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.nan",	do_isnan,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.finite",	do_isfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.infinite",	do_isinfinite,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.vector",	do_isvector,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Miscellaneous */

/* Primitive */
{"proc.time",	dcfun,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_proctime},
{"gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
#if 0
{"visibleflag", do_visibleflag,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif
{"withVisible", do_withVisible,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expression",	do_expression,	1,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"interactive",	dcfun,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_interactive},
{"invisible",	do_invisible,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep",		do_rep,		0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep.int",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rep_int},
{"rep_len",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rep_len},
{"seq.int",	do_seq,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_len",	do_seq_len,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_along",	do_seq_along,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"list",	do_makelist,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"xtfrm",	do_xtfrm,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2native",	do_enc2,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2utf8",	do_enc2,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"emptyenv",	dcfun,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_emptyenv},
{"baseenv",	dcfun,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_baseenv},
{"globalenv",	dcfun,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_globalenv},
{"environment<-",do_envirgets,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"pos.to.env",	do_pos2env,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"eapply",	do_eapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lapply",	do_lapply,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"vapply",	do_vapply,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mapply",	do_mapply,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{".C",		do_dotCode,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Fortran",	do_dotCode,	1,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".External",   do_External,    0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External2",   do_External,   1,    201,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call",       do_dotcall,     0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External.graphics", do_Externalgr, 0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Call.graphics", do_dotcallgr, 0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},

/* .Internal */
{"Version",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_version},
{"machine",	do_machine,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"commandArgs", dcfun, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_commandArgs},

#ifdef Win32
{"system",	do_system,	0,	211,	5,	{PP_FUNCALL, PREC_FN,	0}},
#else
{"system",	do_system,	0,	211,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#ifdef Win32
{"shell.exec",	do_shellexec,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.which",	do_syswhich,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mkjunction", do_mkjunction,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"tzone_name", do_tzone_name,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif

{"parse",	do_parse,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
//{"parse_Rd",	do_parseRd,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
//{"deparseRd",	do_deparseRd,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
//{"parseLatex",  do_parseLatex,  0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"save",	do_save,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"saveToConn",	do_saveToConn,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"load",	dcfun,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_load},
{"loadFromConn2",dcfun,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_loadFromConn2},
{"serializeToConn",	do_serializeToConn,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserializeFromConn",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_unserializeFromConn},
{"deparse",	do_deparse,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"dput",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dput},
{"dump",	do_dump,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"quit",	do_quit,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"readline",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readln},
{"print.default",do_printdefault,0,	111,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"print.function",do_printfunction,0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prmatrix",	do_prmatrix,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		dcfun,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gc},
{"gcinfo",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gcinfo},
{"gctorture",	dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gctorture},
{"gctorture2",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gctorture2},
{"memory.profile",dcfun, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memoryprofile},
{"split",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_split},
{"is.loaded",	do_isloaded,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{"recordGraphics", do_recordGraphics, 0, 211,     3,      {PP_FOREIGN, PREC_FN,	0}},
{"dyn.load",	do_dynload,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"dyn.unload",	do_dynunload,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ls",		dcfun,		1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_ls},
{"typeof",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_typeof},
{"eval",	do_eval,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"returnValue",   dcfun,0,     11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_returnValue},
{"sys.parent",	do_sys,		1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.call",	do_sys,		2,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frame",	do_sys,		3,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.nframe",	do_sys,		4,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.calls",	do_sys,		5,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frames",	do_sys,		6,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.on.exit",	do_sys,		7,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parents",	do_sys,		8,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.function",do_sys,		9,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"traceback",	dcfun,	0,      11,     1,      {PP_FUNCALL, PREC_FN,   0}, (DL_FUNC) dc_traceback},
{"browserText", do_sysbrowser,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserCondition", do_sysbrowser,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserSetDebug", do_sysbrowser,	3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"parent.frame",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_parentframe},
{"sort",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sort},
{"is.unsorted",	do_isunsorted,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"psort",	do_psort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsort",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_qsort},
{"radixsort",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_radixsort},
{"order",	do_order,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rank",	do_rank,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"scan",	do_scan,	0,	11,	19,	{PP_FUNCALL, PREC_FN,	0}},
{"t.default",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_transpose},
{"aperm",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_aperm},
{"builtins",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_builtins},
{"args",	do_args,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"formals",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_formals},
{"body",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_body},
{"bodyCode",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bodyCode},
{"environment",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_envir},
{"environmentName",dcfun,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_envirName},
{"env2list",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_env2list},
{"reg.finalizer",dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_regFinaliz},
{"options",	do_options,	0,	211,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"getOption",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getOption},
{"sink",	dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sink},
{"sink.number",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sinknumber},
{"rapply",	do_rapply,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"islistfactor",dcfun,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_islistfactor},
{"colSums",	do_colsum,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"colMeans",	do_colsum,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowSums",	do_colsum,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowMeans",	do_colsum,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tracemem",    do_tracemem,    0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"retracemem",  do_retracemem,  0,      201,     -1,      {PP_FUNCALL, PREC_FN,	0}},
{"untracemem",  do_untracemem,  0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"inspect",	do_inspect,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"address",     dcfun,     0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_address},
{"refcnt",      do_refcnt,      0,       11,     1,     {PP_FUNCALL, PREC_FN, 0}},
{"mem.limits",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memlimits},
{"merge",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_merge},
{"capabilities",dcfun,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_capabilities},
{"capabilitiesX11",dcfun,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_capabilitiesX11},
{"new.env",	do_newenv,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env",  dcfun,   0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_parentenv},
{"parent.env<-",dcfun, 0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}, (DL_FUNC) dc_parentenvgets},
{"topenv",	do_topenv,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"l10n_info",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_l10n_info},
{"Cstack_info", dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_Cstack_info},

/* Functions To Interact with the Operating System */

{"file.show",	do_fileshow,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"file.create",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filecreate},
{"file.remove",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileremove},
{"file.rename",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filerename},
{"file.append",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileappend},
{"file.symlink",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filesymlink},
{"file.link",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filelink},
{"file.copy",	do_filecopy,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"list.files",	do_listfiles,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"list.dirs",	do_listdirs,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.exists", dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileexists},
{"file.choose", dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filechoose},
{"file.info",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileinfo},
{"file.access",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileaccess},
{"dir.exists",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_direxists},
{"dir.create",	dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dircreate},
{"tempfile",	do_tempfile,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"tempdir",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_tempdir},
{"R.home",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_Rhome},
{"date",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_date},
{"Sys.getenv",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getenv},
{"Sys.setenv",	dcfun,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setenv},
{"Sys.unsetenv",dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_unsetenv},
{"getwd",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getwd},
{"setwd",	do_setwd,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"basename",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_basename},
{"dirname",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dirname},
{"Sys.chmod",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_syschmod},
{"Sys.umask",	dcfun,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sysumask},
{"Sys.readlink", dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readlink},
{"Sys.info",	do_sysinfo,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.sleep",	do_syssleep,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getlocale",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getlocale},
{"Sys.setlocale",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setlocale},
{"Sys.localeconv",dcfun,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_localeconv},
{"path.expand",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pathexpand},
{"Sys.getpid",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sysgetpid},
{"normalizePath",do_normalizepath,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.glob",	dcfun,	0,      11,	2,      {PP_FUNCALL, PREC_FN,   0}, (DL_FUNC) dc_glob},
{"unlink",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_unlink},

/* Complex Valued Functions */
{"polyroot",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_polyroot},


/* Objects */
{"inherits",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_inherits},
{"UseMethod",	do_usemethod,	0,     200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"NextMethod",	do_nextmethod,	0,     210,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"standardGeneric",do_standardGeneric,0, 201,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* date-time manipulations */
{"Sys.time",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_systime},
{"as.POSIXct",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_asPOSIXct},
{"as.POSIXlt",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_asPOSIXlt},
{"format.POSIXlt",dcfun,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_formatPOSIXlt},
{"strptime",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_strptime},
{"Date2POSIXlt",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_D2POSIXlt},
{"POSIXlt2Date",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_POSIXlt2D},

{"mkCode",     dcfun,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_mkcode},
{"bcClose",    do_bcclose,      0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"is.builtin.internal",    do_is_builtin_internal,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"disassemble", do_disassemble, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"bcVersion", dcfun,     0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_bcversion},
{"load.from.file", do_loadfile, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"save.to.file", do_savefile,   0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"growconst", dcfun,     0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_growconst},
{"putconst", dcfun,       0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_putconst},
{"getconst", dcfun,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_getconst},
{"enableJIT",    dcfun,  0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_enablejit},
{"compilePKGS", dcfun, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_compilepkgs},

{"setNumMathThreads", dcfun,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_setnumthreads},
{"setMaxNumMathThreads", dcfun,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_setmaxnumthreads},

/* Connections */
{"stdin",	dcfun,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_stdin},
{"stdout",	dcfun,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_stdout},
{"stderr",	dcfun,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_stderr},
{"isatty",	dcfun,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isatty},
{"readLines",	do_readLines,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeLines",	dcfun,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_writelines},
{"readBin",	do_readbin,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeBin",	dcfun,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_writebin},
{"readChar",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readchar},
{"writeChar",	dcfun,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_writechar},
{"open",	dcfun,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_open},
{"isOpen",	dcfun,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isopen},
{"isIncomplete",dcfun,0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isincomplete},
{"isSeekable",	dcfun,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isseekable},
{"close",	do_close,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"flush",	dcfun,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_flush},
{"file",	do_url,		1,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"url",		do_url,		0,      11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"pipe",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pipe},
{"fifo",	dcfun,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fifo},
{"gzfile",	do_gzfile,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"bzfile",	do_gzfile,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"xzfile",	do_gzfile,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"unz",		dcfun,		0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_unz},
{"seek",	dcfun,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_seek},
{"truncate",	dcfun,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_truncate},
{"pushBack",	dcfun,	0,     111,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pushback},
{"clearPushBack",dcfun,0,   111,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_clearpushback},
{"pushBackLength",dcfun,0,  11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pushbacklength},
{"rawConnection",dcfun,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rawconnection},
{"rawConnectionValue",dcfun,0, 11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rawconvalue},
{"textConnection",dcfun,0,	11,     5,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_textconnection},
{"textConnectionValue",dcfun,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_textconvalue},
{"socketConnection",do_sockconn,0,	11,     7,      {PP_FUNCALL, PREC_FN,	0}},
{"sockSelect",dcfun,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sockselect},
{"getConnection",dcfun,0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getconnection},
{"getAllConnections",dcfun,0,11, 0,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getallconnections},
{"summary.connection",dcfun,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sumconnection},
{"gzcon",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gzcon},
{"memCompress",dcfun,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memCompress},
{"memDecompress",dcfun,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memDecompress},


{"readDCF",	do_readDCF,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},


{"lockEnvironment", dcfun,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lockEnv},
{"environmentIsLocked",	dcfun,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_envIsLocked},
{"lockBinding", do_lockBnd,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unlockBinding", do_lockBnd,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsLocked", dcfun,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bndIsLocked},
{"makeActiveBinding", dcfun,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_mkActiveBnd},
{"bindingIsActive", dcfun,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bndIsActive},
/* looks like mkUnbound is unused in base R */
{"mkUnbound",	dcfun,		0, 111,	1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_mkUnbound},
{"isNamespaceEnv",dcfun,		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isNSEnv},
{"registerNamespace",do_regNS,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unregisterNamespace",do_unregNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredNamespace",do_getRegNS,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"isRegisteredNamespace", do_getRegNS,	1, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceRegistry",dcfun, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getNSRegistry},
{"importIntoEnv",do_importIntoEnv, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"env.profile",  dcfun,    0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_envprofile},

{"Encoding",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_encoding},
{"setEncoding",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setencoding},
{"setTimeLimit",dcfun,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setTimeLimit},
{"setSessionTimeLimit",dcfun,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setSessionTimeLimit},
{"icuSetCollate",do_ICUset,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"icuGetCollate",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_ICUget},
{"readRenviron",do_readEnviron,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"shortRowNames",dcfun,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_shortRowNames},
{"copyDFattr",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_copyDFattr},
{"getRegisteredRoutines",dcfun,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getRegisteredRoutines},
{"getLoadedDLLs",dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getDllTable},
{"getSymbolInfo",dcfun,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getSymbolInfo},
{".isMethodsDispatchOn",do_S4on,0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBfetch",do_lazyLoadDBfetch,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBflush",dcfun,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lazyLoadDBflush},
{"getVarsFromFrame",dcfun, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getVarsFromFrame},
{"lazyLoadDBinsertValue",do_lazyLoadDBinsertValue, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"bincode",	do_bincode,	 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tabulate",	dcfun,	 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_tabulate},
{"findInterval",do_findinterval, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pretty",	do_pretty,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"formatC",	do_formatC,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"crc64",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_crc64},
{"bitwiseAnd",	do_bitwise,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseNot",	do_bitwise,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseOr",	do_bitwise,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseXor",	do_bitwise,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftL", do_bitwise,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftR",  do_bitwise,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"serialize",	do_serialize,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeb",	do_serialize,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserialize",	do_serialize,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_matrix",do_rowsum,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_df",	do_rowsum,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"setS4Object",	dcfun, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setS4Object},
{"traceOnOff",	do_traceOnOff,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"debugOnOff",	do_traceOnOff,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"La_qr_cmplx",	do_lapack,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",do_lapack,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg",	do_lapack,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg_cmplx",do_lapack,	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",	do_lapack,	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dlange",	do_lapack,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgecon",	do_lapack,	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon",	do_lapack,	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgecon",	do_lapack,	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon",	do_lapack,	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve_cmplx",do_lapack,    11,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve",	do_lapack,	100,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_qr",	do_lapack,	101,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol",	do_lapack,	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol2inv",	do_lapack,	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"qr_coef_real",do_lapack,	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_real",	do_lapack,	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"det_ge_real",	do_lapack,	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_coef_cmplx",do_lapack,	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_cmplx",	do_lapack,	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"La_svd",	do_lapack,	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_svd_cmplx",do_lapack,	401,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_version",	do_lapack,	1000,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

{"bcprofcounts",dcfun,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bcprofcounts},
{"bcprofstart",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bcprofstart},
{"bcprofstop",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bcprofstop},

{"eSoftVersion",dcfun, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_eSoftVersion},
{"curlVersion", do_curlVersion, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"curlGetHeaders",do_curlGetHeaders,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"curlDownload",do_curlDownload, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}},
};


/* Table of special names.  These are marked as special with
   SET_SPECIAL_SYMBOL.  Environments on the function call stack that
   have never contained such a symbol are marked as such, so they can
   be quickly skipped when searching for a function named by such a
   special symbol.

   Any symbols can be put here, but ones that contain special
   characters, or are reserved words, are the ones unlikely to be
   defined in any environment other than base, and hence the ones
   where this is most likely to help. */

static char *Spec_name[] = {
    "if", "while", "repeat", "for", "break", "next", "return", "function",
    "(", "{",
    "+", "-", "*", "/", "^", "%%", "%/%", "%*%", ":",
    "==", "!=", "<", ">", "<=", ">=",
    "&", "|", "&&", "||", "!",
    "<-", "<<-", "=",
    "$", "[", "[[",
    "$<-", "[<-", "[[<-",
    0
};


/* also used in eval.c */
SEXP attribute_hidden R_Primitive(const char *primname)
{
    for (int i = 0; R_FunTab[i].name; i++)
	if (strcmp(primname, R_FunTab[i].name) == 0) { /* all names are ASCII */
	    if ((R_FunTab[i].eval % 100 )/10)
		return R_NilValue; /* it is a .Internal */
	    else
		return mkPRIMSXP(i, R_FunTab[i].eval % 10);
	}
    return R_NilValue;
}

SEXP attribute_hidden do_primitive(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, prim;
    checkArity(op, args);
    name = CAR(args);
    if (!isString(name) || LENGTH(name) != 1 ||
	STRING_ELT(name, 0) == R_NilValue)
	errorcall(call, _("string argument required"));
    prim = R_Primitive(CHAR(STRING_ELT(name, 0)));
    if (prim == R_NilValue)
	errorcall(call, _("no such primitive function"));
    return prim;
}

attribute_hidden
int StrToInternal(const char *s)
{
    int i;
    for (i = 0; R_FunTab[i].name; i++)
	if (strcmp(s, R_FunTab[i].name) == 0) return i;
    return NA_INTEGER;
}

static void installFunTab(int i)
{
    SEXP prim;
    /* mkPRIMSXP caches its results, thus prim does not need protection */
    prim = mkPRIMSXP(i, R_FunTab[i].eval % 10);
    if ((R_FunTab[i].eval % 100 )/10)
	SET_INTERNAL(install(R_FunTab[i].name), prim);
    else
	SET_SYMVALUE(install(R_FunTab[i].name), prim);
}

static void SymbolShortcuts(void)
{  /* ../include/Rinternals.h : */
    R_Bracket2Symbol = install("[[");
    R_BracketSymbol = install("[");
    R_BraceSymbol = install("{");
    R_ClassSymbol = install("class");
    R_DeviceSymbol = install(".Device");
    R_DimNamesSymbol = install("dimnames");
    R_DimSymbol = install("dim");
    R_DollarSymbol = install("$");
    R_DotsSymbol = install("...");
    R_DropSymbol = install("drop");

    /* The last value symbol is used by the interpreter for recording
       the value of the most recently evaluated top level
       expression. To avoid creating an additional reference that
       would requires duplicating on modification this symbol does not
       increment reference counts on its symbol value.  This is safe
       since the symbol value corresponds to the base environment
       where complex assignments are not allowed.  */
    R_LastvalueSymbol = install(".Last.value");
    DISABLE_REFCNT(R_LastvalueSymbol);

    R_LevelsSymbol = install("levels");
    R_ModeSymbol = install("mode");
    R_NameSymbol  = install("name");
    R_NamesSymbol = install("names");
    R_NaRmSymbol = install("na.rm");
    R_PackageSymbol = install("package");
    R_PreviousSymbol = install("previous");
    R_QuoteSymbol = install("quote");
    R_RowNamesSymbol = install("row.names");
    R_SeedsSymbol = install(".Random.seed");
    R_SortListSymbol = install("sort.list");
    R_SourceSymbol = install("source");   /* Still present for use in methods package, not used elsewhere */
    R_TspSymbol = install("tsp");
    /* ../include/Defn.h , i.e. non-public : */
    R_CommentSymbol = install("comment");
    R_DotEnvSymbol = install(".Environment");
    R_ExactSymbol = install("exact");
    R_RecursiveSymbol = install("recursive");
    R_SrcfileSymbol = install("srcfile");
    R_SrcrefSymbol = install("srcref");
    R_WholeSrcrefSymbol = install("wholeSrcref");
    R_TmpvalSymbol = install("*tmp*");
    R_UseNamesSymbol = install("use.names");
    R_ColonSymbol = install(":");
    R_DoubleColonSymbol = install("::");
    R_TripleColonSymbol = install(":::");
    R_ConnIdSymbol = install("conn_id");
    R_DevicesSymbol = install(".Devices");
    R_baseSymbol = // <- back compatible, "deprecated"
    R_BaseSymbol = install("base");
    R_SpecSymbol = install("spec");
    R_NamespaceEnvSymbol = install(".__NAMESPACE__.");

    R_dot_Generic = install(".Generic");
    R_dot_Method = install(".Method");
    R_dot_Methods = install(".Methods");
    R_dot_defined = install(".defined");
    R_dot_target = install(".target");
    R_dot_Group = install(".Group");
    R_dot_Class = install(".Class");
    R_dot_GenericCallEnv = install(".GenericCallEnv");
    R_dot_GenericDefEnv = install(".GenericDefEnv");
    R_dot_packageName = install(".packageName");
}


#define N_DDVAL_SYMBOLS 65

static SEXP DDVALSymbols[N_DDVAL_SYMBOLS];

static SEXP createDDVALSymbol(int n) {
    char buf[10];
    snprintf(buf, 10, "..%d", n);
    return install(buf);
}

static void initializeDDVALSymbols() {
    for(int i = 0; i < N_DDVAL_SYMBOLS; i++) {
	DDVALSymbols[i] = createDDVALSymbol(i);
    }
}

SEXP attribute_hidden installDDVAL(int n) {
    if (n < N_DDVAL_SYMBOLS)
	return DDVALSymbols[n];

    return createDDVALSymbol(n);
}


/* initialize the symbol table */
void attribute_hidden InitNames()
{
    /* allocate the symbol table */
    if (!(R_SymbolTable = (SEXP *) calloc(HSIZE, sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for symbol table");

    /* R_UnboundValue */
    R_UnboundValue = allocSExp(SYMSXP);
    SET_SYMVALUE(R_UnboundValue, R_UnboundValue);
    SET_PRINTNAME(R_UnboundValue, R_NilValue);
    SET_ATTRIB(R_UnboundValue, R_NilValue);
    /* R_MissingArg */
    R_MissingArg = allocSExp(SYMSXP);
    SET_SYMVALUE(R_MissingArg, R_MissingArg);
    SET_PRINTNAME(R_MissingArg, mkChar(""));
    SET_ATTRIB(R_MissingArg, R_NilValue);
    /* R_RestartToken */
    R_RestartToken = allocSExp(SYMSXP);
    SET_SYMVALUE(R_RestartToken, R_RestartToken);
    SET_PRINTNAME(R_RestartToken, mkChar(""));
    SET_ATTRIB(R_RestartToken, R_NilValue);
    /* String constants (CHARSXP values) */
    /* Note: we don't want NA_STRING to be in the CHARSXP cache, so that
       mkChar("NA") is distinct from NA_STRING */
    /* NA_STRING */
    NA_STRING = allocCharsxp(strlen("NA"));
    strcpy(CHAR_RW(NA_STRING), "NA");
    SET_CACHED(NA_STRING);  /* Mark it */
    R_print.na_string = NA_STRING;
    /* R_BlankString */
    R_BlankString = mkChar("");
    R_BlankScalarString = ScalarString(R_BlankString);
    MARK_NOT_MUTABLE(R_BlankScalarString);

    /* Initialize the symbol Table */
    for (int i = 0; i < HSIZE; i++) R_SymbolTable[i] = R_NilValue;

    /* Set up a set of globals so that a symbol table search can be
       avoided when matching something like dim or dimnames. */
    SymbolShortcuts();

    /*  Builtin Functions */
    for (int i = 0; R_FunTab[i].name; i++) installFunTab(i);

    /* Special base functions */
    for (int i = 0; Spec_name[i]; i++)
	SET_SPECIAL_SYMBOL(install(Spec_name[i]));

    R_initAsignSymbols();
    initializeDDVALSymbols();
    R_initialize_bcode();
}


/*  install - probe the symbol table */
/*  If "name" is not found, it is installed in the symbol table.
    The symbol corresponding to the string "name" is returned. */

SEXP install(const char *name)
{
    SEXP sym;
    int i, hashcode;

    hashcode = R_Newhashpjw(name);
    i = hashcode % HSIZE;
    /* Check to see if the symbol is already present;  if it is, return it. */
    for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym))
	if (strcmp(name, CHAR(PRINTNAME(CAR(sym)))) == 0) return (CAR(sym));
    /* Create a new symbol node and link it into the table. */
    if (*name == '\0')
	error(_("attempt to use zero-length variable name"));
    if (strlen(name) > MAXIDSIZE)
	error(_("variable names are limited to %d bytes"), MAXIDSIZE);
    sym = mkSYMSXP(mkChar(name), R_UnboundValue);
    SET_HASHVALUE(PRINTNAME(sym), hashcode);
    SET_HASHASH(PRINTNAME(sym), 1);

    R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
    return (sym);
}

SEXP installChar(SEXP charSXP)
{
    SEXP sym;
    int i, hashcode;

    if( !HASHASH(charSXP) ) {
	hashcode = R_Newhashpjw(CHAR(charSXP));
	SET_HASHVALUE(charSXP, hashcode);
	SET_HASHASH(charSXP, 1);
    } else {
	hashcode = HASHVALUE(charSXP);
    }
    i = hashcode % HSIZE;
    /* Check to see if the symbol is already present;  if it is, return it. */
    for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym))
	if (strcmp(CHAR(charSXP), CHAR(PRINTNAME(CAR(sym)))) == 0) return (CAR(sym));
    /* Create a new symbol node and link it into the table. */
    int len = LENGTH(charSXP);
    if (len == 0)
	error(_("attempt to use zero-length variable name"));
    if (len > MAXIDSIZE)
	error(_("variable names are limited to %d bytes"), MAXIDSIZE);
    if (IS_ASCII(charSXP) || (IS_UTF8(charSXP) && utf8locale) ||
					(IS_LATIN1(charSXP) && latin1locale) )
	sym = mkSYMSXP(charSXP, R_UnboundValue);
    else {
	/* This branch is to match behaviour of install (which is older):
	   symbol C-string names are always interpreted as if
	   in the native locale, even when they are not in the native locale */
	PROTECT(charSXP);
	sym = mkSYMSXP(mkChar(CHAR(charSXP)), R_UnboundValue);
	SET_HASHVALUE(PRINTNAME(sym), hashcode);
	SET_HASHASH(PRINTNAME(sym), 1);
	UNPROTECT(1);
    }

    R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
    return (sym);
}

#define maxLength 512
attribute_hidden
SEXP installS3Signature(const char *className, const char *methodName) {

    const char *src;
    char signature[maxLength];

    int i = 0;
    for(src = className; *src; src++) {
	if (i == maxLength)
	    error(_("class name too long in '%s'"), className);
	signature[i++] = *src;
    }

    if (i == maxLength)
	error(_("class name too long in '%s'"), className);
    signature[i++] = '.';

    for(src = methodName; *src; src++) {
	if (i == maxLength)
	    error(_("class name too long in '%s'"), className);
	signature[i++] = *src;
    }

    if (i == maxLength)
	error(_("class name too long in '%s'"), className);
    signature[i] = 0;

    return install(signature);
}

/* invokes .Call function using args given as pairlist */
/* in case of a builtin, the arguments already have been evaluated */

static SEXP dcfun(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC dcfun = PRIMDCFUN(op);

    if (!dcfun)
        errorcall(call, "dcfun: `%s` is not a .Call function",
            PRIMNAME(op));

    R_len_t nargs = length(args);
    checkArityCallLength(op, call, nargs);

    /* convert list to new list */
    SEXP pargs = PROTECT(allocVector(VECSXP, nargs));
    R_len_t i = 0;
    SEXP a = args;
    while(a != R_NilValue) {
        SET_VECTOR_ELT(pargs, i++, CAR(a));
        a = CDR(a);
    }

    SEXP ans = R_doDotCall( dcfun, nargs, (SEXP *) DATAPTR(pargs), call );
    UNPROTECT(1); /* pargs */
    return ans;
 }


/*  do_internal - This is the code for .Internal(). */

SEXP attribute_hidden do_internal(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, fun, ans;
    int save = R_PPStackTop;
    int flag;
    const void *vmax = vmaxget();

    checkArity(op, args);
    s = CAR(args);
    if (!isPairList(s))
	errorcall(call, _("invalid .Internal() argument"));
    fun = CAR(s);
    if (!isSymbol(fun))
	errorcall(call, _("invalid .Internal() argument"));
    if (INTERNAL(fun) == R_NilValue)
	errorcall(call, _("there is no .Internal function '%s'"),
		  CHAR(PRINTNAME(fun)));

#ifdef CHECK_INTERNALS
    if(R_Is_Running > 1 && getenv("_R_CHECK_INTERNALS2_")) {
	// find out if we were called from a namespace.
	// inlining by the compiler can defeat this.
	const char *ns = "";
	SEXP e = env;
	for (int i = 0; i < 10; i++) {
	    if(R_IsNamespaceEnv(e)) {
		ns = CHAR(STRING_ELT(R_NamespaceEnvSpec(e), 0));
		break;
	    }
	    e = ENCLOS(e);
	    if (isNull(e)) break;
	}
	const char *fn = CHAR(PRINTNAME(fun));
	// nspackloader.R contained a .Internal call, so need this
	// until all packages have been re-installed.
	if (!strlen(ns) && strcmp(fn, "getRegisteredNamespace"))
	    errorcall(call,
		      ".Internal(%s()) not called from a base namespace\n", fn);
	if (strlen(ns)
#if CHECK_INTERNALS < 2
	    && strcmp(ns, "Matrix")
#endif
	    && strcmp(ns, "base") && strcmp(ns, "tools")
	    && strcmp(ns, "utils") && strcmp(ns, "compiler"))
	    errorcall(call,
		      ".Internal(%s()) called from namespace '%s'\n", fn, ns);
    }
#endif


    PROTECT(args = prepareArgsForFun(call, INTERNAL(fun), CDR(s), env));
    flag = PRIMPRINT(INTERNAL(fun));
    R_Visible = flag != 1;
    ans = callFun(call, INTERNAL(fun), args, env);
    /* This resetting of R_Visible = FALSE was to fix PR#7397,
       now fixed in GEText */
    if (flag < 2) R_Visible = flag != 1;
#ifdef CHECK_VISIBILITY
    if(flag < 2 && flag == R_Visible) {
	char *nm = CHAR(PRINTNAME(fun));
	if(strcmp(nm, "eval") && strcmp(nm, "options") && strcmp(nm, "Recall")
	   && strcmp(nm, "do.call") && strcmp(nm, "switch")
	   && strcmp(nm, "recordGraphics") && strcmp(nm, "writeBin")
	   && strcmp(nm, "NextMethod"))
	    printf("vis: internal %s\n", nm);
    }
#endif
    UNPROTECT(1);
    check_stack_balance(INTERNAL(fun), save);
    vmaxset(vmax);
    return (ans);
}
#undef __R_Names__

	/* Internal code for the ~ operator */

SEXP attribute_hidden do_tilde(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (isObject(call))
	return duplicate(call);
    else {
	SEXP klass;
	PROTECT(call = duplicate(call));
	PROTECT(klass = mkString("formula"));
	setAttrib(call, R_ClassSymbol, klass);
	setAttrib(call, R_DotEnvSymbol, rho);
	UNPROTECT(2);
	return call;
    }
}

/* For use in packages */
const char *getPRIMNAME(SEXP object)
{
    return PRIMNAME(object);
}
