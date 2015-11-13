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
{"break",	dcfun, CTXT_BREAK,	0,	0,	{PP_BREAK,   PREC_FN,	  0}, (DL_FUNC) dc_break, DC_COE},
{"next",	dcfun, CTXT_NEXT,	0,	0,	{PP_NEXT,    PREC_FN,	  0}, (DL_FUNC) dc_break, DC_COE},
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
{"@",		dcfun,		0,	0,	2,	{PP_DOLLAR,  PREC_DOLLAR, 0}, (DL_FUNC) dc_AT, DC_COE},
{"[<-",		do_subassign,	0,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"[[<-",	do_subassign2,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"$<-",		do_subassign3,	1,	0,	3,	{PP_SUBASS,  PREC_LEFT,	  1}},
{"switch",	do_switch,	0,	200,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"browser",	do_browser,	0,	101,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{".primTrace",	dcfun,	0,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_trace, DC_COE},
{".primUntrace",dcfun,	1,	101,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_trace, DC_COE},
{".Internal",	do_internal,	0,	200,	1,	{PP_FUNCALL, PREC_FN,	  0}},
{".Primitive",	dcfun,	0,	1,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_primitive, DC_COE},
{"call",	do_call,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"quote",	do_quote,	0,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"substitute",	do_substitute,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"missing",	do_missing,	1,	0,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"nargs",	dcfun,	1,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_nargs, DC_COE},
{"on.exit",	do_onexit,	0,	100,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"forceAndCall",do_forceAndCall,	0,	0,	-1,	{PP_FUNCALL, PREC_FN,	  0}},

/* .Internals */

{"stop",	do_stop,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}},
{"warning",	do_warning,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}},
{"gettext",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_gettext, DC_COE},
{"ngettext",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_ngettext, DC_COE},
{"bindtextdomain",dcfun,0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_bindtextdomain, DC_COE},
{".addCondHands",dcfun,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_addCondHands},
{".resetCondHands",dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_resetCondHands},
{".signalCondition",dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_signalCondition},
{".dfltStop",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_dfltStop},
{".dfltWarn",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_dfltWarn},
{".addRestart",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_addRestart},
{".getRestart",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_getRestart},
{".invokeRestart",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_invokeRestart},
{".addTryHandlers",dcfun,	0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_addTryHandlers, DC_COE},
{"geterrmessage",dcfun, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_geterrmessage},
{"seterrmessage",dcfun, 0,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_seterrmessage},
{"printDeferredWarnings",dcfun, 0,	111,	0,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_printDeferredWarnings},
{"interruptsSuspended",do_interruptsSuspended, 0,	11,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"restart",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_restart},
{"as.function.default",dcfun,0,	11,	2,	{PP_FUNCTION,PREC_FN,	  0}, (DL_FUNC) dc_asfunction, DC_COE},
{"debug",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_debug, DC_COE},
{"undebug",	dcfun,	1,	111,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_debug, DC_COE},
{"isdebugged",	dcfun,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_debug, DC_COE},
{"debugonce",	dcfun,	3,	111,	3,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_debug, DC_COE},
{"Recall",	do_recall,	0,	210,	-1,	{PP_FUNCALL, PREC_FN,	  0}},
{"delayedAssign",dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_delayed, DC_COE},
{"makeLazy",	dcfun,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	  0}, (DL_FUNC) dc_makelazy},
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
{":",		dcfun,	0,	1,	2,	{PP_BINARY2, PREC_COLON,  0}, (DL_FUNC) dc_colon, DC_COE},
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
{"matrix",	dcfun,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_matrix},
{"array",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_array},
{"diag",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_diag},
{"backsolve",	dcfun,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_backsolve},
{"max.col",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_maxcol},
{"row",		dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rowscols, DC_COE},
{"col",		dcfun,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rowscols, DC_COE},
{"unlist",	do_unlist,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}},
{"cbind",	do_bind,	1,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rbind",	do_bind,	2,	10,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"drop",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_drop},
{"all.names",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_allnames},
{"comment",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_comment},
{"comment<-",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_LEFT,	1}, (DL_FUNC) dc_commentgets},
{"get",		dcfun,		1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_get, DC_COE},
{"get0",	dcfun,		2,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_get, DC_COE},
{"mget",	dcfun,		1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_mget, DC_COE},
{"exists",	dcfun,		0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_get, DC_COE},
{"assign",	dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_assign},
{"list2env",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_list2env},
{"remove",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_remove},
{"duplicated",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_duplicated, DC_COE},
{"unique",	dcfun,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_duplicated, DC_COE},
{"anyDuplicated",dcfun,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_duplicated, DC_COE},
{"anyNA",	do_anyNA,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_which},
{"which.min",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_first_min, DC_COE},
{"pmin",	do_pmin,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"pmax",	do_pmin,	1,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"which.max",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_first_min, DC_COE},
{"match",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_match, DC_COE},
{"pmatch",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pmatch},
{"charmatch",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_charmatch},
{"match.call",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_matchcall, DC_COE},
{"crossprod",	do_matprod,	1,	11,	2,	{PP_FUNCALL, PREC_FN,   0}},
{"tcrossprod",	do_matprod,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"lengths",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lengths, DC_COE},

{"attach",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_attach},
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

{"atan2",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"lbeta",	dcfun,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"beta",	dcfun,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"lchoose",	dcfun,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"choose",	dcfun,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

/*
  Can remove all the [dpqr]xxx once the compiler knows how to optimize
  to .External.

  This is most of the do_math[23], and all of the do_math4, do_random[123]
*/
{"dchisq",	dcfun,	6,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"pchisq",	dcfun,	7,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"qchisq",	dcfun,	8,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"dexp",	dcfun,	9,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"pexp",	dcfun,	10,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"qexp",	dcfun,	11,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"dgeom",	dcfun,	12,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"pgeom",	dcfun,	13,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"qgeom",	dcfun,	14,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"dpois",	dcfun,	15,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"ppois",	dcfun,	16,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"qpois",	dcfun,	17,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"dt",		dcfun,	18,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"pt",		dcfun,	19,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"qt",		dcfun,	20,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"dsignrank",	dcfun,	21,	11,	2+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"psignrank",	dcfun,	22,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"qsignrank",	dcfun,	23,	11,	2+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"besselJ",	dcfun,	24,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},
{"besselY",	dcfun,	25,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},

{"psigamma",	dcfun,	26,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math2, DC_COE},


/* Mathematical Functions of a Complex Argument */
/* these are group generic and so need to eval args */

{"Re",		do_cmathfuns,	1,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Im",		do_cmathfuns,	2,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Mod",		do_cmathfuns,	3,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Arg",		do_cmathfuns,	4,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},
{"Conj",	do_cmathfuns,	5,	1,	1,	{PP_FUNCALL, PREC_FN,	0}},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	dcfun,	1,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pbeta",	dcfun,	2,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qbeta",	dcfun,	3,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dbinom",	dcfun,	4,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pbinom",	dcfun,	5,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qbinom",	dcfun,	6,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dcauchy",	dcfun,	7,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pcauchy",	dcfun,	8,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qcauchy",	dcfun,	9,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"df",		dcfun,	10,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pf",		dcfun,	11,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qf",		dcfun,	12,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dgamma",	dcfun,	13,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pgamma",	dcfun,	14,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qgamma",	dcfun,	15,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dlnorm",	dcfun,	16,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"plnorm",	dcfun,	17,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qlnorm",	dcfun,	18,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dlogis",	dcfun,	19,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"plogis",	dcfun,	20,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qlogis",	dcfun,	21,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dnbinom",	dcfun,	22,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pnbinom",	dcfun,	23,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qnbinom",	dcfun,	24,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dnorm",	dcfun,	25,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pnorm",	dcfun,	26,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qnorm",	dcfun,	27,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dunif",	dcfun,	28,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"punif",	dcfun,	29,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qunif",	dcfun,	30,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dweibull",	dcfun,	31,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pweibull",	dcfun,	32,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qweibull",	dcfun,	33,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dnchisq",	dcfun,	34,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pnchisq",	dcfun,	35,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qnchisq",	dcfun,	36,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dnt",		dcfun,	37,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pnt",		dcfun,	38,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qnt",		dcfun,	39,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dwilcox",	dcfun,	40,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pwilcox",	dcfun,	41,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qwilcox",	dcfun,	42,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"besselI",	dcfun,	43,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"besselK",	dcfun,	44,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},

{"dnbinom_mu",	dcfun,	45,	11,	3+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"pnbinom_mu",	dcfun,	46,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},
{"qnbinom_mu",	dcfun,	47,	11,	3+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math3, DC_COE},


/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	dcfun,	1,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"phyper",	dcfun,	2,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"qhyper",	dcfun,	3,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},

{"dnbeta",	dcfun,	4,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"pnbeta",	dcfun,	5,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"qnbeta",	dcfun,	6,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},

{"dnf",		dcfun,	7,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"pnf",		dcfun,	8,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"qnf",		dcfun,	9,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},

{"dtukey",	dcfun,	10,	11,	4+1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"ptukey",	dcfun,	11,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},
{"qtukey",	dcfun,	12,	11,	4+2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_math4, DC_COE},

/* Random Numbers */

{"rchisq",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random1, DC_COE},
{"rexp",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random1, DC_COE},
{"rgeom",	dcfun,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random1, DC_COE},
{"rpois",	dcfun,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random1, DC_COE},
{"rt",		dcfun,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random1, DC_COE},
{"rsignrank",	dcfun,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random1, DC_COE},

{"rbeta",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rbinom",	dcfun,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rcauchy",	dcfun,	2,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rf",		dcfun,	3,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rgamma",	dcfun,	4,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rlnorm",	dcfun,	5,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rlogis",	dcfun,	6,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rnbinom",	dcfun,	7,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rnbinom_mu",	dcfun,	13,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rnchisq",	dcfun,	12,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rnorm",	dcfun,	8,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"runif",	dcfun,	9,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rweibull",	dcfun,	10,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},
{"rwilcox",	dcfun,	11,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random2, DC_COE},

{"rhyper",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_random3, DC_COE},

{"sample",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sample},
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
{"paste",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_paste, DC_COE},
{"paste0",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_paste, DC_COE},
{"file.path",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filepath, DC_COE},
{"format",	dcfun,	0,	11,	9,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_format},
{"format.info",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_formatinfo},
{"cat",		dcfun,		0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_cat, DC_COE},
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
{"grep",	dcfun,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_grep, DC_COE},
{"grepl",	dcfun,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_grep, DC_COE},
{"grepRaw",	dcfun,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_grepraw},
{"sub",		dcfun,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gsub, DC_COE},
{"gsub",	dcfun,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gsub, DC_COE},
{"regexpr",	dcfun,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_regexpr, DC_COE},
{"gregexpr",	dcfun,	1,	11,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_regexpr, DC_COE},
{"regexec",	dcfun,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_regexec},
{"agrep",	dcfun,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_agrep, DC_COE},
{"agrepl",	dcfun,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_agrep, DC_COE},
{"adist",	dcfun,	1,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_adist},
{"aregexec",	dcfun,	1,	11,	7,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_aregexec, DC_COE},
{"tolower",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_tolower, DC_COE},
{"toupper",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_tolower, DC_COE},
{"chartr",	dcfun,	1,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_chartr},
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
{"iconv",	dcfun,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_iconv},
{"strtrim",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_strtrim},
{"strtoi",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_strtoi},
{"strrep",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_strrep},

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

{"is.vector",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isvector, DC_COE},

/* Miscellaneous */

/* Primitive */
{"proc.time",	dcfun,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_proctime},
{"gc.time",	do_gctime,	0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
#if 0
{"visibleflag", do_visibleflag,	0,	1,	0,	{PP_FUNCALL, PREC_FN,	0}},
#endif
{"withVisible", dcfun,	1,	10,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_withVisible, DC_COE},
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

{"eapply",	dcfun,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_eapply, DC_COE},
{"lapply",	dcfun,	0,	10,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lapply, DC_COE},
{"vapply",	dcfun,	0,	10,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_vapply, DC_COE},
{"mapply",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_mapply, DC_COE},

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

{"parse",	dcfun,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_parse, DC_COE},
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
{"dump",	dcfun,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dump, DC_COE},
{"quit",	dcfun,	0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_quit, DC_COE},
{"readline",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readln},
{"print.default",dcfun,	0,	111,	9,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_printdefault, DC_COE},
{"print.function",do_printfunction,0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"prmatrix",	dcfun,	0,	111,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_prmatrix},
{"gc",		dcfun,		0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gc},
{"gcinfo",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gcinfo},
{"gctorture",	dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gctorture},
{"gctorture2",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gctorture2},
{"memory.profile",dcfun, 0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memoryprofile},
{"split",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_split},
{"is.loaded",	do_isloaded,	0,	11,	-1,	{PP_FOREIGN, PREC_FN,	0}},
{"recordGraphics", do_recordGraphics, 0, 211,     3,      {PP_FOREIGN, PREC_FN,	0}},
{"dyn.load",	dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dynload, DC_COE},
{"dyn.unload",	dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dynunload, DC_COE},
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
{"browserText", dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sysbrowser, DC_COE},
{"browserCondition", dcfun,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sysbrowser, DC_COE},
{"browserSetDebug", dcfun,	3,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sysbrowser, DC_COE},
{"parent.frame",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_parentframe},
{"sort",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sort},
{"is.unsorted",	do_isunsorted,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"psort",	do_psort,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}},
{"qsort",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_qsort},
{"radixsort",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_radixsort},
{"order",	do_order,	0,	11,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"rank",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rank, DC_COE},
{"scan",	dcfun,	0,	11,	19,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_scan, DC_COE},
{"t.default",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_transpose},
{"aperm",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_aperm},
{"builtins",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_builtins},
{"args",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_args, DC_COE},
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
{"rapply",	dcfun,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rapply, DC_COE},
{"islistfactor",dcfun,  0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_islistfactor},
{"colSums",	dcfun,	0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_colsum, DC_COE},
{"colMeans",	dcfun,	1,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_colsum, DC_COE},
{"rowSums",	dcfun,	2,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_colsum, DC_COE},
{"rowMeans",	dcfun,	3,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_colsum, DC_COE},
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
{"new.env",	dcfun,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_newenv},
{"parent.env",  dcfun,   0,	11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_parentenv},
{"parent.env<-",dcfun, 0,	11,     2,      {PP_FUNCALL, PREC_LEFT,	1}, (DL_FUNC) dc_parentenvgets},
{"topenv",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_topenv, DC_COE},
{"l10n_info",	dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_l10n_info},
{"Cstack_info", dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_Cstack_info},

/* Functions To Interact with the Operating System */

{"file.show",	dcfun,	0,	111,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileshow},
{"file.create",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filecreate},
{"file.remove",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileremove},
{"file.rename",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filerename},
{"file.append",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileappend},
{"file.symlink",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filesymlink},
{"file.link",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filelink},
{"file.copy",	dcfun,	0,	11,	6,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filecopy},
{"list.files",	dcfun,	0,	11,	8,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_listfiles},
{"list.dirs",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_listdirs},
{"file.exists", dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileexists},
{"file.choose", dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_filechoose},
{"file.info",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileinfo},
{"file.access",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fileaccess},
{"dir.exists",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_direxists},
{"dir.create",	dcfun,	0,	111,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_dircreate},
{"tempfile",	dcfun,	0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_tempfile},
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
{"Sys.sleep",	dcfun,	0,	111,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_syssleep, DC_COE},
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
{"bcClose",    dcfun,      0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_bcclose, DC_COE},
{"is.builtin.internal",    dcfun,      0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_is_builtin_internal, DC_COE},
{"disassemble", dcfun, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_disassemble, DC_COE},
{"bcVersion", dcfun,     0,      11,     0,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_bcversion},
{"load.from.file", dcfun, 0,      11,     1,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_loadfile, DC_COE},
{"save.to.file", dcfun,   0,      11,     3,      {PP_FUNCALL, PREC_FN, 0}, (DL_FUNC) dc_savefile, DC_COE},
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
{"readLines",	dcfun,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readLines},
{"writeLines",	dcfun,	0,      111,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_writelines},
{"readBin",	dcfun,	0,      11,     6,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readbin},
{"writeBin",	dcfun,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_writebin},
{"readChar",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readchar},
{"writeChar",	dcfun,	0,      211,    5,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_writechar},
{"open",	dcfun,	0,      111,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_open},
{"isOpen",	dcfun,	0,      11,     2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isopen},
{"isIncomplete",dcfun,0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isincomplete},
{"isSeekable",	dcfun,	0,      11,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isseekable},
{"close",	do_close,	0,      111,     2,      {PP_FUNCALL, PREC_FN,	0}},
{"flush",	dcfun,	0,      111,     1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_flush},
{"file",	dcfun,		1,      11,     6,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_url, DC_COE},
{"url",		dcfun,		0,      11,     5,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_url, DC_COE},
{"pipe",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pipe},
{"fifo",	dcfun,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_fifo},
{"gzfile",	dcfun,	0,      11,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gzfile, DC_COE},
{"bzfile",	dcfun,	1,      11,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gzfile, DC_COE},
{"xzfile",	dcfun,	2,      11,     4,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gzfile, DC_COE},
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
{"socketConnection",dcfun,0,	11,     7,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sockconn},
{"sockSelect",dcfun,	0,	11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sockselect},
{"getConnection",dcfun,0,	11,	1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getconnection},
{"getAllConnections",dcfun,0,11, 0,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getallconnections},
{"summary.connection",dcfun,0,11,    1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_sumconnection},
{"gzcon",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_gzcon},
{"memCompress",dcfun,	0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memCompress},
{"memDecompress",dcfun,0,	11,     2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_memDecompress},


{"readDCF",	dcfun,	0,      11,     3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readDCF},


{"lockEnvironment", dcfun,		0, 111,  2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lockEnv},
{"environmentIsLocked",	dcfun,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_envIsLocked},
{"lockBinding", dcfun,		0, 111,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lockBnd, DC_COE},
{"unlockBinding", dcfun,		1, 111,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lockBnd, DC_COE},
{"bindingIsLocked", dcfun,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bndIsLocked},
{"makeActiveBinding", dcfun,	0, 111,	3,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_mkActiveBnd},
{"bindingIsActive", dcfun,	0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bndIsActive},
/* looks like mkUnbound is unused in base R */
{"mkUnbound",	dcfun,		0, 111,	1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_mkUnbound},
{"isNamespaceEnv",dcfun,		0, 11,	1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_isNSEnv},
{"registerNamespace",dcfun,		0, 11,	2,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_regNS, DC_COE},
{"unregisterNamespace",dcfun,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_unregNS, DC_COE},
{"getRegisteredNamespace",dcfun,	0, 11,  1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getRegNS, DC_COE},
{"isRegisteredNamespace", dcfun,	1, 11,  1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getRegNS, DC_COE},
{"getNamespaceRegistry",dcfun, 0, 11, 0,     {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getNSRegistry},
{"importIntoEnv",dcfun, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_importIntoEnv},
{"env.profile",  dcfun,    0,	211,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_envprofile},

{"Encoding",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_encoding},
{"setEncoding",	dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setencoding},
{"setTimeLimit",dcfun,0,	111,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setTimeLimit},
{"setSessionTimeLimit",dcfun,0,	111,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setSessionTimeLimit},
{"icuSetCollate",do_ICUset,	0,	111,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"icuGetCollate",dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_ICUget},
{"readRenviron",dcfun,	0,      111,    1,      {PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_readEnviron, DC_COE},
{"shortRowNames",dcfun,0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_shortRowNames},
{"copyDFattr",dcfun,	0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_copyDFattr},
{"getRegisteredRoutines",dcfun,0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getRegisteredRoutines},
{"getLoadedDLLs",dcfun,	0,	11,	0,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getDllTable},
{"getSymbolInfo",dcfun,0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getSymbolInfo},
{".isMethodsDispatchOn",do_S4on,0,	1,	-1,	{PP_FUNCALL, PREC_FN,	0}},
{"lazyLoadDBfetch",dcfun,0,1,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lazyLoadDBfetch},
{"lazyLoadDBflush",dcfun,0,11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lazyLoadDBflush},
{"getVarsFromFrame",dcfun, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_getVarsFromFrame},
{"lazyLoadDBinsertValue",dcfun, 0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_lazyLoadDBinsertValue},
{"bincode",	dcfun,	 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bincode},
{"tabulate",	dcfun,	 0,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_tabulate},
{"findInterval",dcfun, 0,	11,	4,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_findinterval},
{"pretty",	dcfun,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_pretty},
{"formatC",	dcfun,	0,	11,	7,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_formatC},
{"crc64",	dcfun,	0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_crc64},
{"bitwiseAnd",	dcfun,	1,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bitwise, DC_COE},
{"bitwiseNot",	dcfun,	2,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bitwise, DC_COE},
{"bitwiseOr",	dcfun,	3,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bitwise, DC_COE},
{"bitwiseXor",	dcfun,	4,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bitwise, DC_COE},
{"bitwiseShiftL", dcfun,	5,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bitwise, DC_COE},
{"bitwiseShiftR", dcfun,	6,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_bitwise, DC_COE},
{"serialize",	dcfun,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_serialize, DC_COE},
{"serializeb",	dcfun,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_serialize, DC_COE},
{"unserialize",	dcfun,	2,	11,	2,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_serialize, DC_COE},
{"rowsum_matrix",dcfun,	0,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rowsum, DC_COE},
{"rowsum_df",	dcfun,	1,	11,	5,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_rowsum, DC_COE},
{"setS4Object",	dcfun, 0,	11,	3,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_setS4Object},
{"traceOnOff",	dcfun, 0,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_traceOnOff, DC_COE},
{"debugOnOff",	dcfun,	1,	11,	1,	{PP_FUNCALL, PREC_FN,	0}, (DL_FUNC) dc_traceOnOff, DC_COE},

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

SEXP attribute_hidden dc_primitive(SEXP call, SEXP op, SEXP env, SEXP name)
{
    SEXP prim;
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

    SEXP pargs;
    R_len_t i;
    if (PRIMDCKIND(op) == DC_COE) {
        pargs = PROTECT(allocVector(VECSXP, nargs + 3));
        SET_VECTOR_ELT(pargs, 0, call);
        SET_VECTOR_ELT(pargs, 1, op);
        SET_VECTOR_ELT(pargs, 2, env);
        i = 3;
        nargs += 3;
    } else {
        pargs = PROTECT(allocVector(VECSXP, nargs));
        i = 0;
    }

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
    /* "s" below cannot be changed to "call" as some code expects name
       of the internal like "rbind" as opposed to ".Internal" */
    ans = callFun(s, INTERNAL(fun), args, env);
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
