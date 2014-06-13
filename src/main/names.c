/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2014  The R Core Team
 *  Copyright (C) 2003, 2004  The R Foundation
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
{"if",		do_if, NULL,		0,	200,	-1,	{PP_IF,	     PREC_FN,	  1}},
{"while",	do_while, NULL,	0,	100,	-1,	{PP_WHILE,   PREC_FN,	  0}},
{"for",		do_for, NULL,		0,	100,	-1,	{PP_FOR,     PREC_FN,	  0}},
{"repeat",	do_repeat, NULL,	0,	100,	-1,	{PP_REPEAT,  PREC_FN,	  0}},
{"break",	do_break, NULL, CTXT_BREAK,	0,	-1,	{PP_BREAK,   PREC_FN,	  0}},
{"next",	do_break, NULL, CTXT_NEXT,	0,	-1,	{PP_NEXT,    PREC_FN,	  0}},
{"return",	do_return, NULL,	0,	0,	-1,	{PP_RETURN,  PREC_FN,	  0}},
{"function",	do_function, NULL,	0,	0,	-1,	{PP_FUNCTION,PREC_FN,	  0}},
{"<-",		do_set, NULL,		1,	100,	-1,	{PP_ASSIGN,  PREC_LEFT,	  1}},
{"=",		do_set, NULL,		3,	100,	-1,	{PP_ASSIGN,  PREC_EQ,	  1}},
{"<<-",		do_set, NULL,		2,	100,	-1,	{PP_ASSIGN2, PREC_LEFT,	  1}},
{"{",		do_begin, NULL,	0,	200,	-1,	{PP_CURLY,   PREC_FN,	  0}},
{"(",		do_paren, NULL,	0,	1,	1,	{PP_PAREN,   PREC_FN,	  0}},
{".subset",	do_subset_dflt, NULL,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".subset2",	do_subset2_dflt, NULL,2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"[",		do_subset, NULL,	1,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"[[",		do_subset2, NULL,	2,	0,	-1,	{PP_SUBSET,  PREC_SUBSET, 0}},
{"$",		do_subset3, NULL,	3,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"@",		do_AT, NULL,		0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}},
{"[<-",		do_subassign, NULL,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2, NULL,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3, NULL,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"switch",	do_switch, NULL,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"browser",	do_browser, NULL,	0,	101,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	do_trace, NULL,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primUntrace",do_trace, NULL,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Internal",	do_internal, NULL,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	do_primitive, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"call",	do_call, NULL,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"quote",	do_quote, NULL,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"substitute",	do_substitute, NULL,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing, NULL,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"nargs",	do_nargs, NULL,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"on.exit",	do_onexit, NULL,	0,	100,	1,	{PP_FUNCALL, PREC_FN,	  0}},

/* .Internals */

{"stop",	do_stop, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"warning",	do_warning, NULL,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"gettext",	do_gettext, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"ngettext",	do_ngettext, NULL,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"bindtextdomain",do_bindtextdomain, NULL,0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addCondHands",do_addCondHands, {.args5 = do_earg_addCondHands},	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{".resetCondHands",do_resetCondHands, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".signalCondition",do_signalCondition, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltStop",do_dfltStop, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".dfltWarn",do_dfltWarn, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addRestart",do_addRestart, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".getRestart",do_getRestart, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".invokeRestart",do_invokeRestart, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{".addTryHandlers",do_addTryHandlers, NULL,	0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"geterrmessage",do_geterrmessage, NULL, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"seterrmessage",do_seterrmessage, NULL, 0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"printDeferredWarnings",do_printDeferredWarnings, NULL, 0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}},
{"interruptsSuspended",do_interruptsSuspended, NULL, 0,	11,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"restart",	do_restart, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"as.function.default",do_asfunction, NULL,0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}},
{"debug",	do_debug, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"undebug",	do_debug, NULL,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"isdebugged",	do_debug, NULL,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{"debugonce",	do_debug, NULL,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}},
{"Recall",	do_recall, NULL,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"delayedAssign",do_delayed, {.args4 = do_earg_delayed},	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"makeLazy",	do_makelazy, NULL,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}},
{"identical",	do_identical, {.args7 = do_earg_identical},	0,	11,	7,	{PP_FUNCALL, PREC_FN,	  0}},


/* Binary Operators, all primitives */
/* these are group generic and so need to eval args */
{"+",		do_arith, NULL,	PLUSOP,	1,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"-",		do_arith, NULL,	MINUSOP,1,	2,	{PP_BINARY,  PREC_SUM,	  0}},
{"*",		do_arith, NULL,	TIMESOP,1,	2,	{PP_BINARY,  PREC_PROD,	  0}},
{"/",		do_arith, NULL,	DIVOP,	1,	2,	{PP_BINARY2, PREC_PROD,	  0}},
{"^",		do_arith, NULL,	POWOP,	1,	2,	{PP_BINARY2, PREC_POWER,  1}},
{"%%",		do_arith, NULL,	MODOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%/%",		do_arith, NULL,	IDIVOP,	1,	2,	{PP_BINARY2, PREC_PERCENT,0}},
{"%*%",		do_matprod, {.args2 = do_earg_matprod_star},	0,	1,	2,	{PP_BINARY,  PREC_PERCENT,0}},

{"==",		do_relop, NULL,	EQOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"!=",		do_relop, NULL,	NEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<",		do_relop, NULL,	LTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"<=",		do_relop, NULL,	LEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">=",		do_relop, NULL,	GEOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{">",		do_relop, NULL,	GTOP,	1,	2,	{PP_BINARY,  PREC_COMPARE,0}},
{"&",		do_logic, NULL,	1,	1,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"|",		do_logic, NULL,	2,	1,	2,	{PP_BINARY,  PREC_OR,	  0}},
{"!",		do_logic, NULL,	3,	1,	1,	{PP_UNARY,   PREC_NOT,	  0}},

/* specials as conditionally evaluate second arg */
{"&&",		do_logic2, NULL,	1,	0,	2,	{PP_BINARY,  PREC_AND,	  0}},
{"||",		do_logic2, NULL,	2,	0,	2,	{PP_BINARY,  PREC_OR,	  0}},
{":",		do_colon, NULL,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}},
/* does not evaluate */
{"~",		do_tilde, NULL,	0,	0,	2,	{PP_BINARY,  PREC_TILDE,  0}},


/* Logic Related Functions */
/* these are group generic and so need to eval args */
{"all",		do_logic3, NULL,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"any",		do_logic3, NULL,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	  0}},


/* Vectors, Matrices and Arrays */

/* Primitives */

{"length",	do_length, {.args1 = do_earg_length},	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"length<-",	do_lengthgets, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"c",/* bind.c:*/do_c, NULL,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass",	do_class, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"oldClass<-",	do_classgets, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT, 1}},
{"class",	R_do_data_class, NULL,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{".cache_class",	R_do_data_class, NULL,	1,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"class<-",	R_do_set_class, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"unclass",	do_unclass, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"names",	do_names, {.args1 = do_earg_names},	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"names<-",	do_namesgets, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"dimnames",	do_dimnames, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dimnames<-",	do_dimnamesgets, NULL,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"dim",		do_dim, {.args1 = do_earg_dim},		0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dim<-",	do_dimgets, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"attributes",	do_attributes, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"attributes<-",do_attributesgets, NULL,0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"attr",	do_attr, NULL,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"attr<-",	do_attrgets, NULL,	0,	1,	3,	{PP_FUNCALL, PREC_LEFT,	1}},
{"@<-",		do_attrgets, NULL,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"levels<-",	do_levelsgets, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},

/* .Internals */

{"vector",	do_makevector, {.args2 = do_earg_makevector},	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"complex",	do_complex, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"matrix",	do_matrix, {.args7 = do_earg_matrix},	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"array",	do_array, {.args3 = do_earg_array},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"diag",	do_diag, {.args3 = do_earg_diag},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"backsolve",	do_backsolve, NULL,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"max.col",	do_maxcol, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"row",		do_rowscols, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"col",		do_rowscols, NULL,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"unlist",	do_unlist, {.args3 = do_earg_unlist},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cbind",	do_bind, NULL,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind, NULL,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"drop",	do_drop, {.args1 = do_earg_drop},	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"all.names",	do_allnames, NULL,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"comment",	do_comment, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"comment<-",	do_commentgets, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"get",		do_get, {.args4 = do_earg_get},		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mget",	do_mget, NULL,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"exists",	do_get, {.args4 = do_earg_get},		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"assign",	do_assign, {.args4 = do_earg_assign},	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"list2env",	do_list2env, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"remove",	do_remove, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"duplicated",	do_duplicated, {.args4 = do_earg_duplicated},	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"unique",	do_duplicated, {.args4 = do_earg_duplicated},	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"anyDuplicated",do_duplicated, {.args3 = do_earg_anyDuplicated},	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"anyNA",	do_anyNA, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	do_which, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.min",	do_first_min, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmin",	do_pmin, NULL,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin, NULL,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	do_first_min, {.args1 = do_earg_first_min},	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"match",	do_match, {.args4 = do_earg_match},	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pmatch",	do_pmatch, {.args4 = do_earg_pmatch},	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"charmatch",	do_charmatch, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"match.call",	do_matchcall, {.args3 = do_earg_matchcall},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"crossprod",	do_matprod, {.args2 = do_earg_matprod},	1,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"tcrossprod",	do_matprod, {.args2 = do_earg_matprod},	2,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},

{"attach",	do_attach, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"detach",	do_detach, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"search",	do_search, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setFileTime",	do_setFileTime, NULL,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Mathematical Functions */
/* primitives: these are group generic and so need to eval args (possibly internally) */
{"round",	do_Math2, NULL,	10001,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"signif",	do_Math2, NULL,	10004,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log",		do_log, NULL,		10003,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"log10",	do_log1arg, NULL,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log2",	do_log1arg, NULL,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"abs",		do_abs, NULL,		6,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"floor",	do_math1, NULL,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ceiling",	do_math1, NULL,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sqrt",	do_math1, NULL,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sign",	do_math1, NULL,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trunc",	do_trunc, NULL,	5,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"exp",		do_math1, NULL,	10,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expm1",	do_math1, NULL,	11,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"log1p",	do_math1, NULL,	12,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cos",		do_math1, NULL,	20,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sin",		do_math1, NULL,	21,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tan",		do_math1, NULL,	22,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acos",	do_math1, NULL,	23,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asin",	do_math1, NULL,	24,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atan",	do_math1, NULL,	25,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"cosh",	do_math1, NULL,	30,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinh",	do_math1, NULL,	31,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanh",	do_math1, NULL,	32,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"acosh",	do_math1, NULL,	33,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"asinh",	do_math1, NULL,	34,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"atanh",	do_math1, NULL,	35,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"lgamma",	do_math1, NULL,	40,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gamma",	do_math1, NULL,	41,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"digamma",	do_math1, NULL,	42,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"trigamma",	do_math1, NULL,	43,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
/* see "psigamma" below !*/

{"cospi",	do_math1, NULL,	47,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sinpi",	do_math1, NULL,	48,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"tanpi",	do_math1, NULL,	49,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"lbeta",	do_math2, NULL,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"beta",	do_math2, NULL,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lchoose",	do_math2, NULL,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"choose",	do_math2, NULL,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/*
  Can remove all the [dpqr]xxx once the compiler knows how to optimize
  to .External.

  This is most of the do_math[23], NULL, and all of the do_math4, NULL, do_random[123], NULL
*/
{"dchisq",	do_math2, NULL,	6,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pchisq",	do_math2, NULL,	7,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qchisq",	do_math2, NULL,	8,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dexp",	do_math2, NULL,	9,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pexp",	do_math2, NULL,	10,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qexp",	do_math2, NULL,	11,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgeom",	do_math2, NULL,	12,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgeom",	do_math2, NULL,	13,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgeom",	do_math2, NULL,	14,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dpois",	do_math2, NULL,	15,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ppois",	do_math2, NULL,	16,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qpois",	do_math2, NULL,	17,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dt",		do_math2, NULL,	18,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pt",		do_math2, NULL,	19,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qt",		do_math2, NULL,	20,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dsignrank",	do_math2, NULL,	21,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}},
{"psignrank",	do_math2, NULL,	22,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsignrank",	do_math2, NULL,	23,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselJ",	do_math2, NULL,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"besselY",	do_math2, NULL,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"psigamma",	do_math2, NULL,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of a Complex Argument */
/* these are group generic and so need to eval args */

{"Re",		do_cmathfuns, NULL,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Im",		do_cmathfuns, NULL,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Mod",		do_cmathfuns, NULL,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Arg",		do_cmathfuns, NULL,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Conj",	do_cmathfuns, NULL,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3, NULL,	1,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbeta",	do_math3, NULL,	2,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbeta",	do_math3, NULL,	3,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dbinom",	do_math3, NULL,	4,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pbinom",	do_math3, NULL,	5,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qbinom",	do_math3, NULL,	6,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dcauchy",	do_math3, NULL,	7,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pcauchy",	do_math3, NULL,	8,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qcauchy",	do_math3, NULL,	9,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"df",		do_math3, NULL,	10,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pf",		do_math3, NULL,	11,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qf",		do_math3, NULL,	12,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dgamma",	do_math3, NULL,	13,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pgamma",	do_math3, NULL,	14,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qgamma",	do_math3, NULL,	15,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlnorm",	do_math3, NULL,	16,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plnorm",	do_math3, NULL,	17,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlnorm",	do_math3, NULL,	18,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dlogis",	do_math3, NULL,	19,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"plogis",	do_math3, NULL,	20,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qlogis",	do_math3, NULL,	21,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom",	do_math3, NULL,	22,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom",	do_math3, NULL,	23,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom",	do_math3, NULL,	24,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnorm",	do_math3, NULL,	25,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnorm",	do_math3, NULL,	26,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnorm",	do_math3, NULL,	27,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dunif",	do_math3, NULL,	28,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"punif",	do_math3, NULL,	29,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qunif",	do_math3, NULL,	30,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dweibull",	do_math3, NULL,	31,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pweibull",	do_math3, NULL,	32,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qweibull",	do_math3, NULL,	33,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnchisq",	do_math3, NULL,	34,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnchisq",	do_math3, NULL,	35,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnchisq",	do_math3, NULL,	36,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnt",		do_math3, NULL,	37,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnt",		do_math3, NULL,	38,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnt",		do_math3, NULL,	39,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dwilcox",	do_math3, NULL,	40,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pwilcox",	do_math3, NULL,	41,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qwilcox",	do_math3, NULL,	42,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},

{"besselI",	do_math3, NULL,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"besselK",	do_math3, NULL,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbinom_mu",	do_math3, NULL,	45,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbinom_mu",	do_math3, NULL,	46,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbinom_mu",	do_math3, NULL,	47,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	do_math4, NULL,	1,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"phyper",	do_math4, NULL,	2,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qhyper",	do_math4, NULL,	3,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnbeta",	do_math4, NULL,	4,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnbeta",	do_math4, NULL,	5,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnbeta",	do_math4, NULL,	6,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dnf",		do_math4, NULL,	7,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"pnf",		do_math4, NULL,	8,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qnf",		do_math4, NULL,	9,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

{"dtukey",	do_math4, NULL,	10,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}},
{"ptukey",	do_math4, NULL,	11,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},
{"qtukey",	do_math4, NULL,	12,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}},

/* Random Numbers */

{"rchisq",	do_random1, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rexp",	do_random1, NULL,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rgeom",	do_random1, NULL,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rpois",	do_random1, NULL,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rt",		do_random1, NULL,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rsignrank",	do_random1, NULL,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"rbeta",	do_random2, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rbinom",	do_random2, NULL,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rcauchy",	do_random2, NULL,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rf",		do_random2, NULL,	3,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rgamma",	do_random2, NULL,	4,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlnorm",	do_random2, NULL,	5,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rlogis",	do_random2, NULL,	6,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom",	do_random2, NULL,	7,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnbinom_mu",	do_random2, NULL,	13,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnchisq",	do_random2, NULL,	12,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rnorm",	do_random2, NULL,	8,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"runif",	do_random2, NULL,	9,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rweibull",	do_random2, NULL,	10,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rwilcox",	do_random2, NULL,	11,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"rhyper",	do_random3, NULL,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},

{"sample",	do_sample, {.args4 = do_earg_sample},	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sample2",	do_sample2, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"RNGkind",	do_RNGkind, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"set.seed",	do_setseed, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Data Summaries */
/* these four are group generic and so need to eval args */
{"sum",		do_summary, NULL,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"min",		do_summary, NULL,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"max",		do_summary, NULL,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prod",	do_summary, NULL,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

{"mean",	do_summary, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"range",	do_range, NULL,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* Note that the number of arguments in this group only applies
   to the default method */
{"cumsum",	do_cum, NULL,		1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cumprod",	do_cum, NULL,		2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cummax",	do_cum, NULL,		3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"cummin",	do_cum, NULL,		4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Type coercion */

{"as.character",do_ascharacter, NULL,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.integer",	do_ascharacter, NULL,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.double",	do_ascharacter, NULL,	2,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.complex",	do_ascharacter, NULL,	3,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.logical",	do_ascharacter, NULL,	4,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.raw",	do_ascharacter, NULL,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.call",	do_ascall, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"as.environment",do_as_environment, NULL,0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"storage.mode<-",do_storage_mode, NULL,0,	1,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"as.vector",	do_asvector, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"paste",	do_paste, {.args3 = do_earg_paste},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"paste0",	do_paste, {.args2 = do_earg_paste0},	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.path",	do_filepath, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format",	do_format, NULL,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"format.info",	do_formatinfo, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cat",		do_cat, {.args6 = do_earg_cat},		0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"do.call",	do_docall, NULL,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* String Manipulation */

{"nchar",	do_nchar, NULL,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"nzchar",	do_nzchar, NULL,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"substr",	do_substr, NULL,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"substr<-",	do_substrgets, NULL,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"strsplit",	do_strsplit, NULL,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"abbreviate",	do_abbrev, NULL,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"make.names",	do_makenames, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"grep",	do_grep, NULL,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepl",	do_grep, NULL,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"grepRaw",	do_grepraw, NULL,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"sub",		do_gsub, NULL,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"gsub",	do_gsub, NULL,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"regexpr",	do_regexpr, NULL,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gregexpr",	do_regexpr, NULL,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"regexec",	do_regexec, NULL,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"agrep",	do_agrep, NULL,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"agrepl",	do_agrep, NULL,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"adist",	do_adist, NULL,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"aregexec",	do_aregexec, NULL,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"tolower",	do_tolower, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"toupper",	do_tolower, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"chartr",	do_chartr, NULL,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sprintf",	do_sprintf, NULL,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"make.unique",	do_makeunique, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"charToRaw",	do_charToRaw, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToChar",	do_rawToChar, NULL,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rawShift",	do_rawShift, NULL,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"intToBits",	do_intToBits, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rawToBits",	do_rawToBits, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"packBits",	do_packBits, NULL,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"utf8ToInt",	do_utf8ToInt, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"intToUtf8",	do_intToUtf8, NULL,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"encodeString",do_encodeString, NULL,1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"iconv",	do_iconv, NULL,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"strtrim",	do_strtrim, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"strtoi",	do_strtoi, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Type Checking (typically implemented in ./coerce.c ) */

{"is.null",	do_is, {.args1 = do_earg_is},		NILSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.logical",	do_is, {.args1 = do_earg_is},		LGLSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.integer",	do_is, {.args1 = do_earg_is},		INTSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.double",	do_is, {.args1 = do_earg_is},		REALSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.complex",	do_is, {.args1 = do_earg_is},		CPLXSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.character",do_is, {.args1 = do_earg_is},		STRSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.symbol",	do_is, {.args1 = do_earg_is},		SYMSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.environment",do_is, {.args1 = do_earg_is},	ENVSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.list",	do_is, {.args1 = do_earg_is},		VECSXP,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.pairlist",	do_is, {.args1 = do_earg_is},		LISTSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.expression",do_is, {.args1 = do_earg_is},		EXPRSXP,1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.raw",	do_is, {.args1 = do_earg_is},		RAWSXP, 1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.object",	do_is, {.args1 = do_earg_is},		50,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"isS4",	do_is, {.args1 = do_earg_is},		51,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.numeric",	do_is, {.args1 = do_earg_is},		100,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.matrix",	do_is, {.args1 = do_earg_is},		101,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.array",	do_is, {.args1 = do_earg_is},		102,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.atomic",	do_is, {.args1 = do_earg_is},		200,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.recursive",do_is, {.args1 = do_earg_is},		201,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.call",	do_is, {.args1 = do_earg_is},		300,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.language",	do_is, {.args1 = do_earg_is},		301,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.function",	do_is, {.args1 = do_earg_is},		302,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.single",	do_is, {.args1 = do_earg_is},		999,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.na",	do_isna, {.args1 = do_earg_isna},	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.nan",	do_isnan, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.finite",	do_isfinite, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"is.infinite",	do_isinfinite, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"is.vector",	do_isvector, {.args2 = do_earg_isvector},	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

/* Miscellaneous */

/* Primitive */
{"proc.time",	do_proctime, NULL,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"gc.time",	do_gctime, NULL,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
#if 0
{"visibleflag", do_visibleflag, NULL,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif
{"withVisible", do_withVisible, NULL,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"expression",	do_expression, NULL,	1,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"interactive",	do_interactive, NULL,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"invisible",	do_invisible, NULL,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep",		do_rep, NULL,		0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rep.int",	do_rep_int, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rep_len",	do_rep_len, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"seq.int",	do_seq, NULL,		0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_len",	do_seq_len, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"seq_along",	do_seq_along, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"list",	do_makelist, NULL,	1,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"xtfrm",	do_xtfrm, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2native",	do_enc2, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"enc2utf8",	do_enc2, NULL,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"emptyenv",	do_emptyenv, NULL,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"baseenv",	do_baseenv, NULL,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"globalenv",	do_globalenv, NULL,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"environment<-",do_envirgets, NULL,	0,	1,	2,	{PP_FUNCALL, PREC_LEFT,	1}},
{"pos.to.env",	do_pos2env, NULL,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"eapply",	do_eapply, NULL,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lapply",	do_lapply, NULL,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"vapply",	do_vapply, NULL,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"mapply",	do_mapply, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{".C",		do_dotCode, NULL,	0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Fortran",	do_dotCode, NULL,	1,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".External",   do_External, NULL,    0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".External2",   do_External, NULL,   1,    201,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call",       do_dotcall, NULL,     0,      1,      -1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple0", do_dotcall_simple, {.args1 = do_earg_dotcall_simple0},     0,      1,      1,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple1", do_dotcall_simple, {.args2 = do_earg_dotcall_simple1},     0,      1,      2,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple2", do_dotcall_simple, {.args3 = do_earg_dotcall_simple2},     0,      1,      3,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple3", do_dotcall_simple, {.args4 = do_earg_dotcall_simple3},     0,      1,      4,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple4", do_dotcall_simple, {.args5 = do_earg_dotcall_simple4},     0,      1,      5,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple5", do_dotcall_simple, {.args6 = do_earg_dotcall_simple5},     0,      1,      6,     {PP_FOREIGN, PREC_FN,	0}},
{".Call.Simple6", do_dotcall_simple, {.args7 = do_earg_dotcall_simple6},     0,      1,      7,     {PP_FOREIGN, PREC_FN,	0}},
{".External.graphics", do_Externalgr, NULL, 0, 1,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{".Call.graphics", do_dotcallgr, NULL, 0,	1,	-1,	{PP_FOREIGN, PREC_FN,	0}},

/* .Internal */
{"Version",	do_version, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"machine",	do_machine, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"commandArgs", do_commandArgs, NULL, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

#ifdef Win32
{"system",	do_system, NULL,	0,	211,	5,	{PP_FUNCALL, PREC_FN,	0}},
#else
{"system",	do_system, NULL,	0,	211,	2,	{PP_FUNCALL, PREC_FN,	0}},
#endif

#ifdef Win32
{"shell.exec",	do_shellexec, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.which",	do_syswhich, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"useInternet2",do_setInternet2, NULL,0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mkjunction", do_mkjunction, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"tzone_name", do_tzone_name, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif

{"parse",	do_parse, NULL,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
//{"parse_Rd", 	do_parseRd, NULL,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
//{"deparseRd", 	do_deparseRd, NULL, 	0, 	11, 	2,	{PP_FUNCALL, PREC_FN, 	0}},
//{"parseLatex",  do_parseLatex, NULL,  0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"save",	do_save, NULL,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"saveToConn",	do_saveToConn, NULL,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"load",	do_load, NULL,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"loadFromConn2",do_loadFromConn2, NULL,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeToConn",	do_serializeToConn, NULL,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserializeFromConn",	do_unserializeFromConn, NULL,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"deparse",	do_deparse, {.args5 = do_earg_deparse},	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"dput",	do_dput, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"dump",	do_dump, NULL,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"quit",	do_quit, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"readline",	do_readln, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"print.default",do_printdefault, NULL,0,	111,	9,	{PP_FUNCALL, PREC_FN,	0}},
{"print.function",do_printfunction, NULL,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"prmatrix",	do_prmatrix, NULL,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"gc",		do_gc, NULL,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"gcinfo",	do_gcinfo, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture",	do_gctorture, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"gctorture2",	do_gctorture2, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"memory.profile",do_memoryprofile, NULL, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"split",	do_split, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.loaded",	do_isloaded, NULL,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{"recordGraphics", do_recordGraphics, NULL, 0, 211,     3,      {PP_FOREIGN, PREC_FN,	0}},
{"dyn.load",	do_dynload, NULL,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"dyn.unload",	do_dynunload, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"ls",		do_ls, {.args2 = do_earg_ls},		1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"typeof",	do_typeof, {.args1 = do_earg_typeof},	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"eval",	do_eval, NULL,	0,	211,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parent",	do_sys, {.args1 = do_earg_sys},		1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.call",	do_sys, {.args1 = do_earg_sys},		2,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frame",	do_sys, {.args1 = do_earg_sys},		3,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.nframe",	do_sys, {.args1 = do_earg_sys},		4,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.calls",	do_sys, {.args1 = do_earg_sys},		5,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.frames",	do_sys, {.args1 = do_earg_sys},		6,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.on.exit",	do_sys, {.args1 = do_earg_sys},		7,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.parents",	do_sys, {.args1 = do_earg_sys},		8,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sys.function",do_sys, {.args1 = do_earg_sys},		9,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"traceback",	do_traceback, NULL,  	0,      11,     1,      {PP_FUNCALL, PREC_FN,   0}},
{"browserText", do_sysbrowser, NULL,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserCondition", do_sysbrowser, NULL,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"browserSetDebug", do_sysbrowser, NULL,	3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"parent.frame",do_parentframe, {.args1 = do_earg_parentframe},	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"sort",	do_sort, {.args2 = do_earg_sort},	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"is.unsorted",	do_isunsorted, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"psort",	do_psort, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsort",	do_qsort, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"radixsort",	do_radixsort, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"order",	do_order, NULL,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rank",	do_rank, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"scan",	do_scan, NULL,	0,	11,	19,	{PP_FUNCALL, PREC_FN,	0}},
{"t.default",	do_transpose, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"aperm",	do_aperm, {.args3 = do_earg_aperm},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"builtins",	do_builtins, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"args",	do_args, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"formals",	do_formals, {.args1 = do_earg_formals},	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"body",	do_body, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bodyCode",	do_bodyCode, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environment",	do_envir, {.args1 = do_earg_envir},	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"environmentName",do_envirName, NULL,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"env2list",	do_env2list, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"reg.finalizer",do_regFinaliz, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"options",	do_options, NULL,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"sink",	do_sink, NULL,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"sink.number",	do_sinknumber, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"rapply",	do_rapply, NULL,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"islistfactor",do_islistfactor, {.args2 = do_earg_islistfactor} ,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"colSums",	do_colsum, NULL,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"colMeans",	do_colsum, NULL,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowSums",	do_colsum, NULL,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"rowMeans",	do_colsum, NULL,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tracemem",    do_tracemem, NULL,    0,      1,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"retracemem",  do_retracemem, NULL,  0,      201,     -1,      {PP_FUNCALL, PREC_FN,	0}},
{"untracemem",  do_untracemem, NULL,  0,      101,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"inspect",	do_inspect, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"mem.limits",	do_memlimits, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"merge",	do_merge, NULL,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilities",do_capabilities, NULL,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"capabilitiesX11",do_capabilitiesX11, NULL,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"new.env",	do_newenv, {.args3 = do_earg_newenv},	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env",  do_parentenv, {.args1 = do_earg_parentenv},   0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"parent.env<-",do_parentenvgets, NULL, 0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}},
{"l10n_info",	do_l10n_info, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Cstack_info", do_Cstack_info, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"internal.supports.earg", do_internal_supports_earg, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"true.builtin.supports.earg", do_true_builtin_supports_earg, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},

/* Functions To Interact with the Operating System */

{"file.show",	do_fileshow, NULL,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"file.create",	do_filecreate, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.remove",	do_fileremove, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.rename",	do_filerename, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.append",	do_fileappend, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.symlink",do_filesymlink, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.link",	do_filelink, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"file.copy",	do_filecopy, NULL,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}},
{"list.files",	do_listfiles, NULL,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}},
{"list.dirs",	do_listdirs, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"file.exists", do_fileexists, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.choose", do_filechoose, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.info",	do_fileinfo, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"file.access",	do_fileaccess, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"dir.create",	do_dircreate, NULL,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tempfile",	do_tempfile, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"tempdir",	do_tempdir, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"R.home",	do_Rhome, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"date",	do_date, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getenv",	do_getenv, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setenv",	do_setenv, NULL,	0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.unsetenv",do_unsetenv, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getwd",	do_getwd, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"setwd",	do_setwd, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"basename",	do_basename, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"dirname",	do_dirname, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.chmod",	do_syschmod, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.umask",	do_sysumask, NULL,	0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.readlink", do_readlink, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.info",	do_sysinfo, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.sleep",	do_syssleep, NULL,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getlocale",do_getlocale, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.setlocale",do_setlocale, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.localeconv",do_localeconv, NULL,0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"path.expand",	do_pathexpand, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.getpid",	do_sysgetpid, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"normalizePath",do_normalizepath, NULL,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Sys.glob",	do_glob, NULL,	0,      11,	2,      {PP_FUNCALL, PREC_FN,   0}},
{"unlink",	do_unlink, NULL,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},

/* Complex Valued Functions */
{"polyroot",	do_polyroot, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},


/* Objects */
{"inherits",	do_inherits, {.args3 = do_earg_inherits},	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"UseMethod",	do_usemethod, NULL,	0,     200,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"NextMethod",	do_nextmethod, NULL,	0,     210,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"standardGeneric",do_standardGeneric, NULL,0, 201,	-1,	{PP_FUNCALL, PREC_FN,	0}},

/* date-time manipulations */
{"Sys.time",	do_systime, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXct",	do_asPOSIXct, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"as.POSIXlt",	do_asPOSIXlt, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"format.POSIXlt",do_formatPOSIXlt, NULL,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"strptime",	do_strptime, NULL,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"Date2POSIXlt",do_D2POSIXlt, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"POSIXlt2Date",do_POSIXlt2D, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"mkCode",     do_mkcode, NULL,       0,      11,     2,      {PP_FUNCALL, PREC_FN, 0}},
{"bcClose",    do_bcclose, NULL,      0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"is.builtin.internal",    do_is_builtin_internal, NULL,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"disassemble", do_disassemble, NULL, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"bcVersion", do_bcversion, NULL,     0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}},
{"load.from.file", do_loadfile, NULL, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"save.to.file", do_savefile, NULL,   0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"growconst", do_growconst, NULL,     0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"putconst", do_putconst, {.args3 = do_earg_putconst},       0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}},
{"getconst", do_getconst, NULL,       0,      11,     5,      {PP_FUNCALL, PREC_FN, 0}},
{"enableJIT",    do_enablejit, NULL,  0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"compilePKGS", do_compilepkgs, NULL, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

{"setNumMathThreads", do_setnumthreads, NULL,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},
{"setMaxNumMathThreads", do_setmaxnumthreads, NULL,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}},

/* Connections */
{"stdin",	do_stdin, NULL,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stdout",	do_stdout, {.args0 = do_earg_stdout},	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"stderr",	do_stderr, NULL,	0,      11,     0,      {PP_FUNCALL, PREC_FN,	0}},
{"isatty",	do_isatty, NULL,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"readLines",	do_readLines, NULL,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeLines",	do_writelines, NULL,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"readBin",	do_readbin, NULL,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}},
{"writeBin",	do_writebin, NULL,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"readChar",	do_readchar, NULL,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"writeChar",	do_writechar, NULL,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}},
{"open",	do_open, NULL,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"isOpen",	do_isopen, NULL,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"isIncomplete",do_isincomplete, NULL,0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"isSeekable",	do_isseekable, NULL,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"close",	do_close, NULL,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"flush",	do_flush, NULL,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"file",	do_url, NULL,		1,      11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"url",		do_url, NULL,		0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"pipe",	do_pipe, NULL,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"fifo",	do_fifo, NULL,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"gzfile",	do_gzfile, NULL,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"bzfile",	do_gzfile, NULL,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"xzfile",	do_gzfile, NULL,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"unz",		do_unz, NULL,		0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"seek",	do_seek, NULL,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"truncate",	do_truncate, NULL,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBack",	do_pushback, NULL,	0,     111,     4,      {PP_FUNCALL, PREC_FN,	0}},
{"clearPushBack",do_clearpushback, NULL,0,   111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"pushBackLength",do_pushbacklength, NULL,0,  11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"rawConnection",do_rawconnection, NULL,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"rawConnectionValue",do_rawconvalue, NULL,0, 11,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnection",do_textconnection, NULL,0,	11,     5,      {PP_FUNCALL, PREC_FN,	0}},
{"textConnectionValue",do_textconvalue, NULL,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"socketConnection",do_sockconn, NULL,0,	11,     7,      {PP_FUNCALL, PREC_FN,	0}},
{"sockSelect",do_sockselect, NULL,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"getConnection",do_getconnection, NULL,0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"getAllConnections",do_getallconnections, NULL,0,11, 0,      {PP_FUNCALL, PREC_FN,	0}},
{"summary.connection",do_sumconnection, NULL,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}},
{"gzcon",	do_gzcon, NULL,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},
{"memCompress",do_memCompress, NULL,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"memDecompress",do_memDecompress, NULL,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}},


{"readDCF",	do_readDCF, NULL,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}},


{"lockEnvironment", do_lockEnv, NULL,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}},
{"environmentIsLocked",	do_envIsLocked, NULL,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"lockBinding", do_lockBnd, NULL,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unlockBinding", do_lockBnd, NULL,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsLocked", do_bndIsLocked, NULL,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"makeActiveBinding", do_mkActiveBnd, NULL,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}},
{"bindingIsActive", do_bndIsActive, NULL,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
/* looks like mkUnbound is unused in base R */
{"mkUnbound",	do_mkUnbound, NULL,		0, 111,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"isNamespaceEnv",do_isNSEnv, {.args1 = do_earg_isNSEnv},		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}},
{"registerNamespace",do_regNS, NULL,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}},
{"unregisterNamespace",do_unregNS, NULL,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredNamespace",do_getRegNS, {.args1 = do_earg_getRegNS},	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{"getNamespaceRegistry",do_getNSRegistry, {.args0 = do_earg_getNSRegistry}, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}},
{"importIntoEnv",do_importIntoEnv, NULL, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"env.profile",  do_envprofile, NULL,    0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"Encoding",	do_encoding, NULL,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"setEncoding",	do_setencoding, NULL,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"setTimeLimit",do_setTimeLimit, NULL,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"setSessionTimeLimit",do_setSessionTimeLimit, NULL,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"icuSetCollate",do_ICUset, NULL,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"readRenviron",do_readEnviron, NULL,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}},
{"shortRowNames",do_shortRowNames, {.args2 = do_earg_shortRowNames}, 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"copyDFattr",do_copyDFattr, {.args2 = do_earg_copyDFattr},	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredRoutines",do_getRegisteredRoutines, NULL,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getLoadedDLLs",do_getDllTable, NULL,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},
{"getSymbolInfo",do_getSymbolInfo, NULL,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"getRegisteredSymbolInfo",do_getRegisteredSymbolInfo, {.args1 = do_earg_getRegisteredSymbolInfo},	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}},
{".isMethodsDispatchOn",do_S4on, NULL,0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBfetch",do_lazyLoadDBfetch, NULL,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBflush",do_lazyLoadDBflush, NULL,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"getVarsFromFrame",do_getVarsFromFrame, NULL, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBinsertValue",do_lazyLoadDBinsertValue, NULL, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"bincode",	do_bincode, NULL,	 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"tabulate",	do_tabulate, NULL,	 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"findInterval",do_findinterval, NULL, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}},
{"pretty",	do_pretty, NULL, 	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"formatC",	do_formatC, NULL, 	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}},
{"crc64",	do_crc64, NULL, 	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseAnd",	do_bitwise, NULL, 	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseNot",	do_bitwise, NULL, 	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseOr",	do_bitwise, NULL, 	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseXor",	do_bitwise, NULL, 	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftL", do_bitwise, NULL, 	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"bitwiseShiftR",  do_bitwise, NULL, 	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"serialize",	do_serialize, NULL, 	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"serializeb",	do_serialize, NULL, 	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"unserialize",	do_serialize, NULL, 	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_matrix",do_rowsum, NULL, 	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"rowsum_df",	do_rowsum, NULL, 	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"setS4Object",	do_setS4Object, NULL, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"traceOnOff",	do_traceOnOff, NULL, 	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},

{"La_qr_cmplx",	do_lapack, NULL,     	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack, NULL,     	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",do_lapack, NULL,     	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg",	do_lapack, NULL,     	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rg_cmplx",do_lapack, NULL,     	41,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs",	do_lapack, NULL,     	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_rs_cmplx",	do_lapack, NULL,     	51,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dlange",	do_lapack, NULL,     	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dgecon",	do_lapack, NULL,     	7,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_dtrcon",	do_lapack, NULL,     	8,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_zgecon",	do_lapack, NULL,     	9,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_ztrcon",	do_lapack, NULL,     	10,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve_cmplx",do_lapack, NULL,    11,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"La_solve",	do_lapack, NULL,     	100,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_qr",	do_lapack, NULL,     	101,	11,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol",	do_lapack, NULL,     	200,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"La_chol2inv",	do_lapack, NULL,     	201,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},

{"qr_coef_real",do_lapack, NULL,     	300,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_real",	do_lapack, NULL,     	301,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"det_ge_real",	do_lapack, NULL,     	302,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_coef_cmplx",do_lapack, NULL,    	303,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qr_qy_cmpl",	do_lapack, NULL,     	304,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},

{"La_svd",	do_lapack, NULL,     	400,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_svd_cmplx",do_lapack, NULL,     	401,	11,	5,	{PP_FUNCALL, PREC_FN,	0}},
{"La_version",	do_lapack, NULL,     	1000,	11,	0,	{PP_FUNCALL, PREC_FN,	0}},

{NULL,		NULL, NULL,		0,	0,	0,	{PP_INVALID, PREC_FN,	0}},
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
    if (!isString(name) || length(name) != 1 ||
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
    /* prim needs to be protected since install can (and does here) allocate */
    PROTECT(prim = mkPRIMSXP(i, R_FunTab[i].eval % 10));
    SEXP sym = install(R_FunTab[i].name);
    if ((R_FunTab[i].eval % 100 )/10)
	SET_INTERNAL(sym, prim);
    else
	SET_SYMVALUE(sym, prim);
    R_FunTab[i].symbol = sym;
    UNPROTECT(1);
}

static void SymbolShortcuts(void)
{  /* ../include/Rinternals.h : */
    R_Bracket2Symbol = install("[[");
    R_BracketSymbol = install("[");
    R_BraceSymbol = install("{");
    R_CSymbol = install("c");
    R_ClassSymbol = install("class");
    R_DeviceSymbol = install(".Device");
    R_DimNamesSymbol = install("dimnames");
    R_DimSymbol = install("dim");
    R_DollarSymbol = install("$");
    R_DollarAssignSymbol = install("$<-");
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
    R_QuoteSymbol = install("quote");
    R_RowNamesSymbol = install("row.names");
    R_SeedsSymbol = install(".Random.seed");
    R_SourceSymbol = install("source");   /* Still present for back compatibility, but not used */
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
    R_DoubleColonSymbol = install("::");
    R_TripleColonSymbol = install(":::");
    R_ConnIdSymbol = install("conn_id");
    R_DevicesSymbol = install(".Devices");

    R_dot_Generic = install(".Generic");
    R_dot_Method = install(".Method");
    R_dot_Methods = install(".Methods");
    R_dot_defined = install(".defined");
    R_dot_target = install(".target");
    R_dot_Group = install(".Group");
    R_dot_Class = install(".Class");
    R_dot_GenericCallEnv = install(".GenericCallEnv");
    R_dot_GenericDefEnv = install(".GenericDefEnv");

    R_native_symbol = install("native symbol");
    R_registered_native_symbol = install("registered native symbol");
    R_PACKAGESymbol = install("PACKAGE");
    R_NAOKSymbol = install("NAOK");
    R_DUPSymbol = install("DUP");
    R_ENCODINGSymbol = install("ENCODING");
    R_CsingleSymbol = install("Csingle");
    R_OriginSymbol = install("origin");

    R_XSymbol = install("x");
    R_FSymbol = install("f");
    R_dot_NAME = install(".NAME");
    R_ObjectSymbol = install("object");
    R_OnSymbol = install("on");
    R_AlongWithSymbol = install("along.with");
    R_LengthOutSymbol = install("length.out");
    R_EXPRSymbol = install("EXPR");
    R_ExprSymbol = install("expr");
    R_ZSymbol = install("z");
    R_GenericSymbol = install("generic");

    R_S3MethodsTableSymbol = install(".__S3MethodsTable__.");
    R_S3MethodsClassesSymbol = install(".S3MethodsClasses");
    R_SortListSymbol = install("sort.list");
    R_PreviousSymbol = install("previous");
    R_ContainsSymbol = install("contains");
    R_SelectSuperClassesSymbol = install(".selectSuperClasses");
    R_ClassEnvSymbol = install(".classEnv");
    R_InitMethodDispatchSymbol = install("initMethodDispatch");
}

static void CharSXPShortcuts(void) {
    R_PreserveObject(R_NativeSymbolInfoCharSXP = mkChar("NativeSymbolInfo"));
    R_PreserveObject(R_FactorCharSXP = mkChar("factor"));
    R_PreserveObject(R_OrderedCharSXP = mkChar("ordered"));
    R_PreserveObject(R_ConnectionCharSXP = mkChar("connection"));
    R_PreserveObject(R_RawConnectionCharSXP = mkChar("rawConnection"));
    R_PreserveObject(R_TextConnectionCharSXP = mkChar("textConnection"));
    R_PreserveObject(R_DLLInfoCharSXP = mkChar("DLLInfo"));
    R_PreserveObject(R_UserDefinedDatabaseCharSXP = mkChar("UserDefinedDatabase"));
    R_PreserveObject(R_POSIXltCharSXP = mkChar("POSIXlt"));
    R_PreserveObject(R_OpsCharSXP = mkChar("Ops"));
    R_PreserveObject(R_MathCharSXP = mkChar("Math"));
    R_PreserveObject(R_ComplexCharSXP = mkChar("Complex"));
    R_PreserveObject(R_SummaryCharSXP = mkChar("Summary"));

    R_PreserveObject(R_LengthCharSXP = mkChar("length"));
    R_PreserveObject(R_LengthAssignCharSXP = mkChar("length<-"));
    R_PreserveObject(R_NamesCharSXP = mkChar("names"));
    R_PreserveObject(R_NamesAssignCharSXP = mkChar("names<-"));
    R_PreserveObject(R_DimNamesCharSXP = mkChar("dimnames"));
    R_PreserveObject(R_DimNamesAssignCharSXP = mkChar("dimnames<-"));
    R_PreserveObject(R_DimCharSXP = mkChar("dim"));
    R_PreserveObject(R_DimAssignCharSXP = mkChar("dim<-"));
    R_PreserveObject(R_LevelsAssignCharSXP = mkChar("levels<-"));
    R_PreserveObject(R_AtAssignCharSXP = mkChar("@<-"));
    R_PreserveObject(R_CCharSXP = mkChar("c"));
    R_PreserveObject(R_UnlistCharSXP = mkChar("unlist"));
    R_PreserveObject(R_AsVectorCharSXP = mkChar("as.vector"));
    R_PreserveObject(R_IsNACharSXP = mkChar("is.na"));
    R_PreserveObject(R_AnyNACharSXP = mkChar("anyNA"));
    R_PreserveObject(R_IsNaNCharSXP = mkChar("is.nan"));
    R_PreserveObject(R_IsFiniteCharSXP = mkChar("is.finite"));
    R_PreserveObject(R_IsInfiniteCharSXP = mkChar("is.infinite"));
    R_PreserveObject(R_AsEnvironmentCharSXP = mkChar("as.environment"));
    R_PreserveObject(R_RepCharSXP = mkChar("rep"));
    R_PreserveObject(R_SeqCharSXP = mkChar("seq"));
    R_PreserveObject(R_IsUnsortedCharSXP = mkChar("is.unsorted"));
    R_PreserveObject(R_XtfrmCharSXP = mkChar("xtfrm"));
    R_PreserveObject(R_SubsetCharSXP = mkChar("["));
    R_PreserveObject(R_Subset2CharSXP = mkChar("[["));
    R_PreserveObject(R_SubassignCharSXP = mkChar("[<-"));
    R_PreserveObject(R_Subassign2CharSXP = mkChar("[[<-"));
    R_PreserveObject(R_DollarCharSXP = mkChar("$"));
    R_PreserveObject(R_DollarAssignCharSXP = mkChar("$<-"));
    R_PreserveObject(R_AsCharacterCharSXP = mkChar("as.character"));
    R_PreserveObject(R_AsIntegerCharSXP = mkChar("as.integer"));
    R_PreserveObject(R_AsDoubleCharSXP = mkChar("as.double"));
    R_PreserveObject(R_AsComplexCharSXP = mkChar("as.complex"));
    R_PreserveObject(R_AsLogicalCharSXP = mkChar("as.logical"));
    R_PreserveObject(R_AsRawCharSXP = mkChar("as.raw"));
    R_PreserveObject(R_IsNumericCharSXP = mkChar("is.numeric"));
    R_PreserveObject(R_IsMatrixCharSXP = mkChar("is.matrix"));
    R_PreserveObject(R_IsArrayCharSXP = mkChar("is.array"));
    R_PreserveObject(R_EmptyCharSXP = mkChar(""));
    R_PreserveObject(R_MatrixCharSXP = mkChar("matrix"));
    R_PreserveObject(R_ArrayCharSXP = mkChar("array"));
    R_PreserveObject(R_FunctionCharSXP = mkChar("function"));
    R_PreserveObject(R_NumericCharSXP = mkChar("numeric"));
    R_PreserveObject(R_NameCharSXP = mkChar("name"));
}

#define N_DDVAL_SYMBOLS 65

static SEXP DDVALSymbols[N_DDVAL_SYMBOLS];

static SEXP createDDVALSymbol(int n) {
    char buf[10];
    snprintf(buf, 10, "..%d", n);
    return mkSYMSXP(mkChar(buf), R_UnboundValue);
}

static void initializeDDVALSymbols() {
    for(int i = 0; i < N_DDVAL_SYMBOLS; i++) {
        SEXP s = createDDVALSymbol(i);
        R_PreserveObject(s);
        MARK_NOT_MUTABLE(s);
        DDVALSymbols[i] = s;
    }
}

SEXP getDDVALSymbol(int n) {
    if (n < N_DDVAL_SYMBOLS) {
        return DDVALSymbols[n];
    }

    return createDDVALSymbol(n);
}


/* initialize the symbol table */
void attribute_hidden InitNames() {

    /* allocate the symbol table */
    if (!(R_SymbolTable = (SEXP *) calloc(HSIZE, sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for symbol table");

    /* R_UnboundValue */
    initializeOffHeapSEXP(R_UnboundValue, SYMSXP);
    SET_SYMVALUE(R_UnboundValue, R_UnboundValue);
    SET_PRINTNAME(R_UnboundValue, R_NilValue);
    SET_ATTRIB(R_UnboundValue, R_NilValue);
    /* R_MissingArg */
    initializeOffHeapSEXP(R_MissingArg, SYMSXP);
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

    CharSXPShortcuts();
}


/*  install - probe the symbol table */
/*  If "name" is not found, it is installed in the symbol table.
    The symbol corresponding to the string "name" is returned. */

SEXP install(const char *name)
{
    SEXP sym;
    int i, hashcode;

    if (*name == '\0')
	error(_("attempt to use zero-length variable name"));
    if (strlen(name) > MAXIDSIZE)
	error(_("variable names are limited to %d bytes"), MAXIDSIZE);
    hashcode = Newhashpjw(name);
    i = hashcode % HSIZE;
    /* Check to see if the symbol is already present;  if it is, return it. */
    for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym))
	if (strcmp(name, CHAR(PRINTNAME(CAR(sym)))) == 0) return (CAR(sym));
    /* Create a new symbol node and link it into the table. */
    sym = mkSYMSXP(mkChar(name), R_UnboundValue);
    SET_HASHVALUE(PRINTNAME(sym), hashcode);
    SET_HASHASH(PRINTNAME(sym), 1);

    R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
    return (sym);
}


/*
  like install, but the string is given as a CHARSXP
  one could use install(CHAR(charSXP)) instead, but this is more efficient

  knownNative can be set to TRUE for efficiency, when the caller knows that
  charSXP is in the native encoding
*/

SEXP installNativeCharSXP(SEXP charSXP, Rboolean knownNative) {
    int len = LENGTH(charSXP);

    if (len == 0) {
        error(_("attempt to use zero-length variable name"));
    }
    if (len > MAXIDSIZE) {
	error(_("variable names are limited to %d bytes"), MAXIDSIZE);
    }

    int i = hashCharSXP(charSXP) % HSIZE;

    SEXP symList;
    for (symList = R_SymbolTable[i]; symList != R_NilValue; symList = CDR(symList)) {
        SEXP sym = CAR(symList);
        if (PRINTNAME(sym) == charSXP) { /* we assume all CHARSXPs are interned/cached */

            /*
               Symbols are only allowed to have _native_ (not set) encoding,
               at least that is the behaviour of the original install
               function.
             */

            if (knownNative || IS_ASCII(charSXP)) { /* the common case */
                return sym;
            }

            int isUTF8 = IS_UTF8(charSXP);
            int isLatin1 = IS_LATIN1(charSXP);

            if (isUTF8) {
                if (utf8locale) {
                    return sym;
                }
            } else if (isLatin1) {
                if (latin1locale) {
                    return sym;
                }
            } else {
                return sym;
            }

            /*
              The charSXP has a different encoding from PRINTNAME(sym), but
              they have the same bytes.  This is perhaps just a
              philosophical option.
             */

            return install(CHAR(charSXP));
        }
    }

    if (!knownNative && (IS_UTF8(charSXP) || IS_LATIN1(charSXP))) {

        /* this is slow, has questionable semantics, but keeps behaviour of
         * the old install function */

        charSXP = mkChar(CHAR(charSXP));
    }

    SEXP sym = mkSYMSXP(charSXP, R_UnboundValue);
    R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
    return sym;
}

SEXP installCharSXP(SEXP charSXP) {
    return installNativeCharSXP(charSXP, FALSE);
}


/*

  Returns a symbol for a string "<name1><sep><name2><sep>...<name_nelems>".
  Elements are given as CHARSXPs.
  charSXPs is an input array of elements of length nelems.

  This is equivalent to creating a C string for the signature and then
  calling install on it, but more efficient.

*/
SEXP installCharSXPSignature(SEXP *charSXPs, int nelems, char sep) {

    if (nelems == 1) {
        return installCharSXP(*charSXPs);
    }

    if (nelems == 0) {  /* but this should not happen */
        return R_MissingArg;
    }

    /* hash the first element - take it from the CHARSXP */
    int h = hashCharSXP(charSXPs[0]);

    int i;
    int signatureLength = LENGTH(charSXPs[0]);

    for(i = 1; i < nelems; i++) {
        h = NewhashpjwAppendChar(sep, h);
        h = NewhashpjwAppend(CHAR(charSXPs[i]), h);
        signatureLength += 1 + LENGTH(charSXPs[i]);
    }

    SEXP symList;
    int tableIndex = h % HSIZE;
    for (symList = R_SymbolTable[tableIndex]; symList != R_NilValue; symList = CDR(symList)) {
        SEXP sym = CAR(symList);
        SEXP symCharSXP = PRINTNAME(sym);
        int symLength = LENGTH(symCharSXP);

        if (symLength != signatureLength) {
            /* FIXME: if the hashing is very good, this check will not be paying off */
            goto differs;
        }

        /* check if sym corresponds to the signature */
        const char *symStr = CHAR(symCharSXP);
        const char *str = CHAR(charSXPs[0]);

            /* check the first element */
        while(*str)  {
            if (*str++ != *symStr++) {
                goto differs;
            }
        }

            /* check separators and remaining elements */

        for (i = 1; i < nelems; i++) {
            if (*symStr++ != sep) {
                goto differs;
            }
            str = CHAR(charSXPs[i]);
            while(*str)  {
                if (*str++ != *symStr++) {
                    goto differs;
                }
            }
        }

        if (*symStr == 0) {
            return sym;
        }

        differs: ; /* continue */
    }

    /* the symbol does not exist */


        /* create a CharSXP for the symbol, this is slowpath, so does not have to be fast */
    char sbuf[signatureLength];
    char *s = sbuf;
    for (i = 0; i < nelems; i++) {
        if (i > 0) {
            *s++ = sep;
        }
        strcpy(s, CHAR(charSXPs[i]));
        s += LENGTH(charSXPs[i]);
    }
    SEXP charSXP = mkChar(sbuf);

    SET_HASHVALUE(charSXP, h);
    SET_HASHASH(charSXP, 1);
    SEXP symbol = mkSYMSXP(charSXP, R_UnboundValue);
    R_SymbolTable[tableIndex] = CONS(symbol, R_SymbolTable[tableIndex]);

    return symbol;
}

SEXP installSignature(SEXP *sxps, int nelems, char sep) {

    SEXP charSXPs[nelems];

    for (int i = 0; i < nelems; i++) {
        charSXPs[i] = asChar(sxps[i]);
    }

    return installCharSXPSignature(charSXPs, nelems, sep);
}

/*
  Equivalent to install( className . "." methodName )
 
  The returned value is the symbol.
 
*/

/* this is a naive version which always copies strings */
SEXP installS3MethodSignatureSlow(const char *className, const char *methodName) {

    const char *src;
    const int maxLength = 512;
    char signature[maxLength];

    int i = 0;
    for(src = className; *src; src++) {
        if (i == maxLength) {
            error(_("class name too long in '%s'"), className);
        }
        signature[i++] = *src;
    }

    if (i == maxLength) {
        error(_("class name too long in '%s'"), className);
    }
    signature[i++] = '.';

    for(src = methodName; *src; src++) {
        if (i == maxLength) {
            error(_("class name too long in '%s'"), className);
        }
        signature[i++] = *src;
    }

    if (i == maxLength) {
        error(_("class name too long in '%s'"), className);
    }
    signature[i] = 0;

    return install(signature);
}

SEXP R_INLINE static installNewS3MethodSignature(const char *className, const char *methodName, int h, int tableIndex) {
    /* create a CharSXP for the symbol, this is slowpath, so does not have to be fast */

    char sbuf[strlen(className) + strlen(methodName) + 2];
    sprintf(sbuf, "%s.%s", className, methodName);
    SEXP charSXP = mkChar(sbuf);

    SET_HASHVALUE(charSXP, h);
    SET_HASHASH(charSXP, 1);
    SEXP symbol = mkSYMSXP(charSXP, R_UnboundValue);
    R_SymbolTable[tableIndex] = CONS(symbol, R_SymbolTable[tableIndex]);

    return symbol;
}

/* this is an on-the-fly version (no copying) */
/* this has to be kept in sync with Newhashpjw */
/* FIXME: extract parts common with installCharSXPSignature */
SEXP installS3MethodSignature(const char *className, const char *methodName) {

    int h = Newhashpjw(className);
    h = NewhashpjwAppendChar('.', h);
    h = NewhashpjwAppend(methodName, h);

    SEXP symList;
    int tableIndex = h % HSIZE;
    for (symList = R_SymbolTable[tableIndex]; symList != R_NilValue; symList = CDR(symList)) {
        SEXP sym = CAR(symList);
        SEXP symCharSXP = PRINTNAME(sym);

        if (equalS3Signature(CHAR(symCharSXP), className, methodName)) {
            return sym;
        }
    }

    /* the symbol does not exist */
    return installNewS3MethodSignature(className, methodName, h, tableIndex);
}

SEXP installS3MethodSignatureNativeCharSXP(SEXP classNameCharSXP, const char *methodName) {

    int h = hashCharSXP(classNameCharSXP);
    h = NewhashpjwAppendChar('.', h);
    h = NewhashpjwAppend(methodName, h);

    SEXP symList;
    int tableIndex = h % HSIZE;
    const char *className = CHAR(classNameCharSXP);
    for (symList = R_SymbolTable[tableIndex]; symList != R_NilValue; symList = CDR(symList)) {
        SEXP sym = CAR(symList);

        if (equalS3Signature(CHAR(PRINTNAME(sym)), className, methodName)) {
            return sym;
        }
    }

    /* the symbol does not exist */
    return installNewS3MethodSignature(className, methodName, h, tableIndex);
}



/* this is for debugging only, can be called from a debugger  */

static int cmpstringp(const void *p1, const void *p2) {
    return strcmp(* (char * const *) p1, * (char * const *) p2);
}

int checkSymbolTable() {

    int i;
    int nsymbols = 0;

    for(i = 0; i < HSIZE; i++) {
        nsymbols += length(R_SymbolTable[i]);
    }

    fprintf(stderr, "Symbol table entries: %d\n", nsymbols);

    char *names[nsymbols];
    int j = 0;

    for(i = 0; i < HSIZE; i++) {
        SEXP s;
        for(s = R_SymbolTable[i]; s != R_NilValue; s = CDR(s)) {
            SEXP sym = CAR(s);
            const char *pname = CHAR(PRINTNAME(sym));
            int len = strlen(pname);

            char *str = (char *) malloc(len * sizeof(char) + 1);
            strcpy(str, pname);
            names[j++] = str;
        }
    }

    fprintf(stderr, "Copied entries: %d\n", j);
    qsort(names, nsymbols, sizeof(char *), cmpstringp);

    int nerrors = 0;
    for(j = 1; j < nsymbols ; j++) {
        if (!strcmp(names[j], names[j-1])) {
            fprintf(stderr, "ERROR in symbol table, duplicated entry %s.\n", names[j]);
            nerrors++;
        }
    }

    fprintf(stderr, "Done checking.\n");
    for(j = 0; j < nsymbols ; j++) {
        free(names[j]);
    }

    return nerrors;
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
	if (!*ns && strcmp(fn, "getRegisteredNamespace"))
	    errorcall(call,
		      ".Internal(%s()) not called from a base namespace\n", fn);
	if (*ns
#if CHECK_INTERNALS < 2
	    && strcmp(ns, "Matrix")
#endif
	    && strcmp(ns, "base") && strcmp(ns, "tools")
	    && strcmp(ns, "utils") && strcmp(ns, "compiler"))
	    errorcall(call,
		      ".Internal(%s()) called from namespace '%s'\n", fn, ns);
    }
#endif


    args = CDR(s);
    if (TYPEOF(INTERNAL(fun)) == BUILTINSXP)
	args = evalList(args, env, call, 0);
    PROTECT(args);
    flag = PRIMPRINT(INTERNAL(fun));
    R_Visible = flag != 1;
    ans = PRIMFUN(INTERNAL(fun)) (s, INTERNAL(fun), args, env);
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
