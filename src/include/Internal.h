/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2016  The R Core Team
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

/* Names of  .Internal(.) and .Primitive(.)  R functions
 *
 * Must all return SEXP because of CCODE in Defn.h.
 * do_math*() and do_cmathfuns are in ../main/arithmetic.h
 */

#ifndef R_INTERNAL_H
#define R_INTERNAL_H


/* Function Names */

#if Win32
SEXP do_mkjunction(SEXP, SEXP, SEXP, SEXP);
SEXP do_shellexec(SEXP, SEXP, SEXP, SEXP);
SEXP do_syswhich(SEXP, SEXP, SEXP, SEXP);
SEXP do_tzone_name(SEXP, SEXP, SEXP, SEXP);
#else
SEXP do_X11(SEXP, SEXP, SEXP, SEXP);
#endif

SEXP do_abs(SEXP, SEXP, SEXP, SEXP);
SEXP do_addTryHandlers(SEXP, SEXP, SEXP, SEXP);
SEXP do_agrep(SEXP, SEXP, SEXP, SEXP);
SEXP do_anyNA(SEXP, SEXP, SEXP, SEXP);
SEXP do_aregexec(SEXP, SEXP, SEXP, SEXP);
SEXP do_args(SEXP, SEXP, SEXP, SEXP);
SEXP do_arith(SEXP, SEXP, SEXP, SEXP);
SEXP do_ascall(SEXP, SEXP, SEXP, SEXP);
SEXP do_as_environment(SEXP, SEXP, SEXP, SEXP);
SEXP do_asatomic(SEXP, SEXP, SEXP, SEXP);
SEXP do_asfunction(SEXP, SEXP, SEXP, SEXP);
SEXP do_asmatrixdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_asvector(SEXP, SEXP, SEXP, SEXP);
SEXP do_asCharacterFactor(SEXP, SEXP, SEXP, SEXP);
SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP do_attach(SEXP,SEXP,SEXP,SEXP);
SEXP do_attr(SEXP, SEXP, SEXP, SEXP);
SEXP do_attrgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_attributes(SEXP, SEXP, SEXP, SEXP);
SEXP do_attributesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_begin(SEXP, SEXP, SEXP, SEXP);
SEXP do_bind(SEXP, SEXP, SEXP, SEXP);
SEXP do_bindtextdomain(SEXP, SEXP, SEXP, SEXP);
SEXP do_bitwise(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_break(SEXP, SEXP, SEXP, SEXP);
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);
SEXP do_c(SEXP, SEXP, SEXP, SEXP);
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_call(SEXP, SEXP, SEXP, SEXP);
SEXP do_cat(SEXP, SEXP, SEXP, SEXP);
SEXP do_class(SEXP, SEXP, SEXP, SEXP);
SEXP do_classgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_colon(SEXP, SEXP, SEXP, SEXP);
SEXP do_colsum(SEXP, SEXP, SEXP, SEXP);
SEXP do_commentgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_contourLines(SEXP, SEXP, SEXP, SEXP);
SEXP do_cum(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlDownload(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlGetHeaders(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_debug(SEXP, SEXP, SEXP, SEXP);
SEXP do_delayed(SEXP, SEXP, SEXP, SEXP);
SEXP do_deparse(SEXP, SEXP, SEXP, SEXP);
SEXP do_dim(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimnames(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimnamesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_docall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcallgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotCode(SEXP, SEXP, SEXP, SEXP);
SEXP do_dump(SEXP, SEXP, SEXP, SEXP);
SEXP do_duplicated(SEXP, SEXP, SEXP, SEXP);
SEXP do_dynload(SEXP, SEXP, SEXP, SEXP);
SEXP do_dynunload(SEXP, SEXP, SEXP, SEXP);
SEXP do_eapply(SEXP, SEXP, SEXP, SEXP);
SEXP do_edit(SEXP, SEXP, SEXP, SEXP);
SEXP do_enc2(SEXP, SEXP, SEXP, SEXP);
SEXP do_envirgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_External(SEXP, SEXP, SEXP, SEXP);
SEXP do_Externalgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_eval(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);
SEXP do_filepath(SEXP, SEXP, SEXP, SEXP);
SEXP do_first_min(SEXP, SEXP, SEXP, SEXP);
SEXP do_for(SEXP, SEXP, SEXP, SEXP);
SEXP do_forceAndCall(SEXP, SEXP, SEXP, SEXP);
SEXP do_function(SEXP, SEXP, SEXP, SEXP);
SEXP do_gctime(SEXP, SEXP, SEXP, SEXP);
SEXP do_get(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEvent(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_gettext(SEXP, SEXP, SEXP, SEXP);
SEXP do_grep(SEXP, SEXP, SEXP, SEXP);
SEXP do_gsub(SEXP, SEXP, SEXP, SEXP);
SEXP do_ICUset(SEXP, SEXP, SEXP, SEXP);
SEXP do_identical(SEXP, SEXP, SEXP, SEXP);
SEXP do_if(SEXP, SEXP, SEXP, SEXP);
SEXP do_inspect(SEXP, SEXP, SEXP, SEXP);
SEXP do_internal(SEXP, SEXP, SEXP, SEXP);
SEXP do_interruptsSuspended(SEXP, SEXP, SEXP, SEXP);
SEXP do_invisible(SEXP, SEXP, SEXP, SEXP);
SEXP do_is(SEXP, SEXP, SEXP, SEXP);
SEXP do_isfinite(SEXP, SEXP, SEXP, SEXP);
SEXP do_isinfinite(SEXP, SEXP, SEXP, SEXP);
SEXP do_isloaded(SEXP, SEXP, SEXP, SEXP);
SEXP do_isna(SEXP, SEXP, SEXP, SEXP);
SEXP do_isnan(SEXP, SEXP, SEXP, SEXP);
SEXP do_isunsorted(SEXP, SEXP, SEXP, SEXP);
SEXP do_isvector(SEXP, SEXP, SEXP, SEXP);
SEXP do_lapack(SEXP, SEXP, SEXP, SEXP);
SEXP do_lapply(SEXP, SEXP, SEXP, SEXP);
SEXP do_length(SEXP, SEXP, SEXP, SEXP);
SEXP do_lengthgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_lengths(SEXP, SEXP, SEXP, SEXP);
SEXP do_levelsgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_log(SEXP, SEXP, SEXP, SEXP);
SEXP do_log1arg(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic2(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic3(SEXP, SEXP, SEXP, SEXP);
SEXP do_machine(SEXP, SEXP, SEXP, SEXP);
SEXP do_makelist(SEXP, SEXP, SEXP, SEXP);
SEXP do_mapply(SEXP, SEXP, SEXP, SEXP);
SEXP do_match(SEXP, SEXP, SEXP, SEXP);
SEXP do_matchcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_matprod(SEXP, SEXP, SEXP, SEXP);
SEXP do_Math2(SEXP, SEXP, SEXP, SEXP);
SEXP do_mget(SEXP, SEXP, SEXP, SEXP);
SEXP do_missing(SEXP, SEXP, SEXP, SEXP);
SEXP do_names(SEXP, SEXP, SEXP, SEXP);
SEXP do_namesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_nargs(SEXP, SEXP, SEXP, SEXP);
SEXP do_nchar(SEXP,SEXP,SEXP,SEXP);
SEXP do_nextmethod(SEXP,SEXP,SEXP,SEXP);
SEXP do_ngettext(SEXP, SEXP, SEXP, SEXP);
SEXP do_normalizepath(SEXP, SEXP, SEXP, SEXP);
SEXP do_nzchar(SEXP,SEXP,SEXP,SEXP);
SEXP do_onexit(SEXP, SEXP, SEXP, SEXP);
SEXP do_options(SEXP, SEXP, SEXP, SEXP);
SEXP do_order(SEXP, SEXP, SEXP, SEXP);
SEXP do_parse(SEXP, SEXP, SEXP, SEXP);
SEXP do_paste(SEXP, SEXP, SEXP, SEXP);
SEXP do_pmin(SEXP, SEXP, SEXP, SEXP);
SEXP do_pos2env(SEXP, SEXP, SEXP, SEXP);
SEXP do_printdefault(SEXP, SEXP, SEXP, SEXP);
SEXP do_printfunction(SEXP, SEXP, SEXP, SEXP);
SEXP do_psort(SEXP, SEXP, SEXP, SEXP);
SEXP do_quit(SEXP, SEXP, SEXP, SEXP);
SEXP do_quote(SEXP, SEXP, SEXP, SEXP);
SEXP do_random1(SEXP, SEXP, SEXP, SEXP);
SEXP do_random2(SEXP, SEXP, SEXP, SEXP);
SEXP do_random3(SEXP, SEXP, SEXP, SEXP);
SEXP do_range(SEXP, SEXP, SEXP, SEXP);
SEXP do_rank(SEXP, SEXP, SEXP, SEXP);
SEXP do_rapply(SEXP, SEXP, SEXP, SEXP);
SEXP do_readEnviron(SEXP, SEXP, SEXP, SEXP);
SEXP do_recall(SEXP, SEXP, SEXP, SEXP);
SEXP do_refcnt(SEXP, SEXP, SEXP, SEXP);
SEXP do_recordGraphics(SEXP, SEXP, SEXP, SEXP);
SEXP do_regexpr(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_rep(SEXP, SEXP, SEXP, SEXP);
SEXP do_repeat(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_return(SEXP, SEXP, SEXP, SEXP);
SEXP do_rgb(SEXP, SEXP, SEXP, SEXP);
SEXP do_rowsum(SEXP, SEXP, SEXP, SEXP);
SEXP do_rowscols(SEXP, SEXP, SEXP, SEXP);
SEXP do_S4on(SEXP, SEXP, SEXP, SEXP);
SEXP do_save(SEXP, SEXP, SEXP, SEXP);
SEXP do_saveToConn(SEXP, SEXP, SEXP, SEXP);
SEXP do_saveplot(SEXP, SEXP, SEXP, SEXP);
SEXP do_scan(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq_along(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq_len(SEXP, SEXP, SEXP, SEXP);
SEXP do_serialize(SEXP, SEXP, SEXP, SEXP);
SEXP do_serializeToConn(SEXP, SEXP, SEXP, SEXP);
SEXP do_set(SEXP, SEXP, SEXP, SEXP);
SEXP do_setGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_setwd(SEXP, SEXP, SEXP, SEXP);
SEXP do_sprintf(SEXP, SEXP, SEXP, SEXP);
SEXP do_standardGeneric(SEXP, SEXP, SEXP, SEXP);
SEXP do_startsWith(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_stop(SEXP, SEXP, SEXP, SEXP);
SEXP do_storage_mode(SEXP, SEXP, SEXP, SEXP);
SEXP do_strsplit(SEXP,SEXP,SEXP,SEXP);
SEXP do_sysinfo(SEXP,SEXP,SEXP,SEXP);
SEXP do_syssleep(SEXP,SEXP,SEXP,SEXP);
SEXP do_subassign(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign3(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset3(SEXP, SEXP, SEXP, SEXP);
SEXP do_substitute(SEXP, SEXP, SEXP, SEXP);
SEXP do_summary(SEXP, SEXP, SEXP, SEXP);
SEXP do_switch(SEXP, SEXP, SEXP, SEXP);
SEXP do_sys(SEXP, SEXP, SEXP, SEXP);
SEXP do_sysbrowser(SEXP, SEXP, SEXP, SEXP);
SEXP do_system(SEXP, SEXP, SEXP, SEXP);
SEXP do_tilde(SEXP, SEXP, SEXP, SEXP);
SEXP do_tolower(SEXP, SEXP, SEXP, SEXP);
SEXP do_topenv(SEXP, SEXP, SEXP, SEXP);
SEXP do_trace(SEXP, SEXP, SEXP, SEXP);
SEXP do_traceOnOff(SEXP, SEXP, SEXP, SEXP);
SEXP do_trunc(SEXP, SEXP, SEXP, SEXP);
SEXP do_unclass(SEXP, SEXP, SEXP, SEXP);
SEXP do_unlist(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_usemethod(SEXP, SEXP, SEXP, SEXP);
SEXP do_vapply(SEXP, SEXP, SEXP, SEXP);
SEXP do_warning(SEXP, SEXP, SEXP, SEXP);
SEXP do_while(SEXP, SEXP, SEXP, SEXP);
SEXP do_withVisible(SEXP, SEXP, SEXP, SEXP);
SEXP do_xtfrm(SEXP, SEXP, SEXP, SEXP);

SEXP do_getSnapshot(SEXP, SEXP, SEXP, SEXP);
SEXP do_playSnapshot(SEXP, SEXP, SEXP, SEXP);

SEXP R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_do_set_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

/* bytecode */
SEXP do_bcclose(SEXP, SEXP, SEXP, SEXP);
SEXP do_is_builtin_internal(SEXP, SEXP, SEXP, SEXP);
SEXP do_disassemble(SEXP, SEXP, SEXP, SEXP);
SEXP do_loadfile(SEXP, SEXP, SEXP, SEXP);
SEXP do_savefile(SEXP, SEXP, SEXP, SEXP);

/* Connections */
SEXP do_lockBnd(SEXP, SEXP, SEXP, SEXP);
SEXP do_regNS(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_unregNS(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_getRegNS(SEXP call, SEXP op, SEXP args, SEXP rho);

SEXP do_tracemem(SEXP, SEXP, SEXP, SEXP);
SEXP do_retracemem(SEXP, SEXP, SEXP, SEXP);
SEXP do_untracemem(SEXP, SEXP, SEXP, SEXP);

/* .Call implementations of simple do_functions */

SEXP dc_abbrev(SEXP x, SEXP argminlen, SEXP argusecl);
SEXP dc_addCondHands(SEXP classes, SEXP handlers, SEXP parentenv, SEXP target, SEXP argcalling);
SEXP dc_address(SEXP p);
SEXP dc_addRestart(SEXP r);
SEXP dc_adist(SEXP x, SEXP y, SEXP opt_costs, SEXP argcounts, SEXP argfixed, SEXP argpartial, SEXP argicase, SEXP arguseBytes);
SEXP dc_allnames(SEXP expr, SEXP argfunctions, SEXP argmaxNames, SEXP argunique);
SEXP dc_aperm(SEXP a, SEXP perm, SEXP argresize);
SEXP dc_array(SEXP vals, SEXP dims, SEXP dimnames);
SEXP dc_asPOSIXct(SEXP argx, SEXP argstz);
SEXP dc_asPOSIXlt(SEXP argx, SEXP argstz);
SEXP dc_assign(SEXP argname, SEXP val, SEXP aenv, SEXP arginherits);
SEXP dc_backsolve(SEXP r, SEXP b, SEXP argk, SEXP argupper, SEXP argtrans);
SEXP dc_baseenv();
SEXP dc_basename(SEXP s);
SEXP dc_bcprofcounts();
SEXP dc_bcprofstart();
SEXP dc_bcprofstop();
SEXP dc_bcversion();
SEXP dc_bincode(SEXP x, SEXP breaks, SEXP right, SEXP lowest);
SEXP dc_bndIsActive(SEXP sym, SEXP env);
SEXP dc_bndIsLocked(SEXP sym, SEXP env);
SEXP dc_bodyCode(SEXP fun);
SEXP dc_body(SEXP fun);
SEXP dc_builtins(SEXP argintern);
SEXP dc_capabilities();
SEXP dc_capabilitiesX11();
SEXP dc_charmatch(SEXP input, SEXP target, SEXP argno_match);
SEXP dc_charToRaw(SEXP x);
SEXP dc_chartr(SEXP old, SEXP _new, SEXP x);
SEXP dc_clearpushback(SEXP argcon);
SEXP dc_commandArgs();
SEXP dc_comment(SEXP x);
SEXP dc_compilepkgs(SEXP argnew);
SEXP dc_complex(SEXP argna, SEXP argre, SEXP argim);
SEXP dc_copyDFattr(SEXP in, SEXP out);
SEXP dc_crc64(SEXP in);
SEXP dc_Cstack_info();
SEXP dc_D2POSIXlt(SEXP argx);
SEXP dc_date();
SEXP dc_detach(SEXP argpos);
SEXP dc_dfltWarn(SEXP argmsg, SEXP ecall);
SEXP dc_diag(SEXP x, SEXP snr, SEXP snc);
SEXP dc_dircreate(SEXP path, SEXP argshow, SEXP argrecursive, SEXP argmode);
SEXP dc_direxists(SEXP fn);
SEXP dc_dirname(SEXP s);
SEXP dc_dput(SEXP x, SEXP file, SEXP argopts);
SEXP dc_drop(SEXP x);
SEXP dc_emptyenv();
SEXP dc_enablejit(SEXP argnew);
SEXP dc_encodeString(SEXP x, SEXP argw, SEXP s, SEXP argjustify, SEXP argna);
SEXP dc_encoding(SEXP x);
SEXP dc_env2list(SEXP env, SEXP argall, SEXP argsort_nms);
SEXP dc_envirName(SEXP env);
SEXP dc_envir(SEXP fun);
SEXP dc_envIsLocked(SEXP env);
SEXP dc_envprofile(SEXP env);
SEXP dc_eSoftVersion();
SEXP dc_fifo(SEXP sfile, SEXP sopen, SEXP argblock, SEXP enc);
SEXP dc_fileaccess(SEXP fn, SEXP argmode);
SEXP dc_fileappend(SEXP f1, SEXP f2);
SEXP dc_filechoose(SEXP argnew);
SEXP dc_filecopy(SEXP fn, SEXP to, SEXP argover, SEXP argrecursive, SEXP argperms, SEXP argdates);
SEXP dc_filecreate(SEXP fn, SEXP argshow);
SEXP dc_fileexists(SEXP file);
SEXP dc_fileinfo(SEXP fn, SEXP argextras);
SEXP dc_filelink(SEXP f1, SEXP f2);
SEXP dc_fileremove(SEXP f);
SEXP dc_filerename(SEXP f1, SEXP f2);
SEXP dc_fileshow(SEXP fn, SEXP hd, SEXP tl, SEXP argdl, SEXP pg);
SEXP dc_filesymlink(SEXP f1, SEXP f2);
SEXP dc_findinterval(SEXP xt, SEXP x, SEXP right, SEXP inside);
SEXP dc_flush(SEXP argcon);
SEXP dc_formals(SEXP fun);
SEXP dc_formatC(SEXP x, SEXP argtype, SEXP argwidth, SEXP argdigits, SEXP argfmt, SEXP argflag, SEXP argstrlen);
SEXP dc_formatinfo(SEXP x, SEXP argdigits, SEXP argnsmall);
SEXP dc_formatPOSIXlt(SEXP argx, SEXP argsformat, SEXP argUseTZ);
SEXP dc_format(SEXP x, SEXP argtrim, SEXP argdigits, SEXP argnsmall, SEXP swd, SEXP argadj, SEXP argna, SEXP argsci, SEXP argdec);
SEXP dc_gcinfo(SEXP arggc_reporting);
SEXP dc_gc(SEXP arggc_reporting, SEXP argreset_max);
SEXP dc_gctorture2(SEXP arggap, SEXP argwait, SEXP arginhibit);
SEXP dc_gctorture(SEXP arggap);
SEXP dc_getallconnections();
SEXP dc_getconnection(SEXP argwhat);
SEXP dc_getconst(SEXP constBuf, SEXP argn);
SEXP dc_getDllTable();
SEXP dc_getenv(SEXP argx, SEXP argunset);
SEXP dc_geterrmessage();
SEXP dc_getlocale(SEXP argcat);
SEXP dc_getNSRegistry();
SEXP dc_getOption(SEXP x);
SEXP dc_getRegisteredRoutines(SEXP dll);
SEXP dc_getRestart(SEXP argi);
SEXP dc_getSymbolInfo(SEXP sname, SEXP spackage, SEXP withRegistrationInfo);
SEXP dc_getVarsFromFrame(SEXP vars, SEXP env, SEXP forcesxp);
SEXP dc_getwd();
SEXP dc_globalenv();
SEXP dc_glob(SEXP x, SEXP argdirmark);
SEXP dc_grepraw(SEXP pat, SEXP text, SEXP argoffset, SEXP argigcase, SEXP argfixed, SEXP argvalue, SEXP argall, SEXP arginvert);
SEXP dc_growconst(SEXP constBuf);
SEXP dc_gzcon(SEXP argcon, SEXP arglevel, SEXP argallow);
SEXP dc_iconv(SEXP x, SEXP argfrom, SEXP argto, SEXP argsub, SEXP argmark, SEXP argtoraw);
SEXP dc_ICUget(SEXP argtype);
SEXP dc_importIntoEnv(SEXP impenv, SEXP impnames, SEXP expenv, SEXP expnames);
SEXP dc_inherits(SEXP x, SEXP what, SEXP which);
SEXP dc_interactive();
SEXP dc_intToBits(SEXP x);
SEXP dc_intToUtf8(SEXP x, SEXP argmultiple);
SEXP dc_isatty(SEXP argcon);
SEXP dc_isincomplete(SEXP argcon);
SEXP dc_islistfactor(SEXP X, SEXP argrecursive);
SEXP dc_isNSEnv(SEXP rho);
SEXP dc_isopen(SEXP argcon, SEXP argrw);
SEXP dc_isseekable(SEXP argcon);
SEXP dc_l10n_info();
SEXP dc_lazyLoadDBfetch(SEXP key, SEXP file, SEXP compsxp, SEXP hook);
SEXP dc_lazyLoadDBflush(SEXP argcfile);
SEXP dc_lazyLoadDBinsertValue(SEXP value, SEXP file, SEXP ascii, SEXP compsxp, SEXP hook);
SEXP dc_list2env(SEXP x, SEXP envir);
SEXP dc_listdirs(SEXP d, SEXP argfullnames, SEXP argrecursive);
SEXP dc_listfiles(SEXP d, SEXP p, SEXP argallfiles, SEXP argfullnames, SEXP argrecursive, SEXP argigcase, SEXP argidirs, SEXP argnodots);
SEXP dc_loadFromConn2(SEXP argcon, SEXP aenv, SEXP argverbose);
SEXP dc_load(SEXP fname, SEXP aenv);
SEXP dc_localeconv();
SEXP dc_lockEnv(SEXP frame, SEXP argbindings);
SEXP dc_ls(SEXP env, SEXP argall, SEXP argsort_nms);
SEXP dc_makelazy(SEXP names, SEXP values, SEXP expr, SEXP eenv, SEXP aenv);
SEXP dc_makenames(SEXP names, SEXP argallow_);
SEXP dc_makeunique(SEXP names, SEXP sep);
SEXP dc_makevector(SEXP argmode, SEXP arglen);
SEXP dc_matrix(SEXP vals, SEXP snr, SEXP snc, SEXP argbyrow, SEXP dimnames, SEXP argmiss_nr, SEXP argmiss_nc);
SEXP dc_maxcol(SEXP m, SEXP argmethod);
SEXP dc_memCompress(SEXP argfrom, SEXP argtype);
SEXP dc_memDecompress(SEXP argfrom, SEXP argtype);
SEXP dc_memlimits(SEXP argnsize, SEXP argvsize);
SEXP dc_memoryprofile();
SEXP dc_merge(SEXP xi, SEXP yi, SEXP argall_x, SEXP argall_y);
SEXP dc_mkActiveBnd(SEXP sym, SEXP fun, SEXP env);
SEXP dc_mkcode(SEXP bytes, SEXP consts);
SEXP dc_mkUnbound(SEXP sym);
SEXP dc_newenv(SEXP arghash, SEXP enclos, SEXP argsize);
SEXP dc_open(SEXP argcon, SEXP sopen, SEXP argblock);
SEXP dc_packBits(SEXP x, SEXP stype);
SEXP dc_paren(SEXP x);
SEXP dc_parentenvgets(SEXP argenv, SEXP parent);
SEXP dc_parentenv(SEXP env);
SEXP dc_parentframe(SEXP argn);
SEXP dc_pathexpand(SEXP fn);
SEXP dc_pcre_config();
SEXP dc_pipe(SEXP scmd, SEXP sopen, SEXP enc);
SEXP dc_pmatch(SEXP input, SEXP target, SEXP argno_match, SEXP argdups_ok);
SEXP dc_polyroot(SEXP z);
SEXP dc_POSIXlt2D(SEXP argx);
SEXP dc_pretty(SEXP argl, SEXP argu, SEXP argn, SEXP argmin_n, SEXP argshrink, SEXP arghi, SEXP argeps);
SEXP dc_printDeferredWarnings();
SEXP dc_prmatrix(SEXP x, SEXP rowlab, SEXP collab, SEXP argquote, SEXP argright, SEXP naprint);
SEXP dc_proctime();
SEXP dc_pushbacklength(SEXP argcon);
SEXP dc_pushback(SEXP stext, SEXP argcon, SEXP argnewLine, SEXP argtype);
SEXP dc_putconst(SEXP constBuf, SEXP argconstCount, SEXP x);
SEXP dc_qsort(SEXP x, SEXP argindx_ret);
SEXP dc_radixsort(SEXP x, SEXP argnalast, SEXP argdecreasing);
SEXP dc_rawconnection(SEXP sfile, SEXP sraw, SEXP sopen);
SEXP dc_rawconvalue(SEXP argcon);
SEXP dc_rawShift(SEXP x, SEXP argshift);
SEXP dc_rawToBits(SEXP x);
SEXP dc_rawToChar(SEXP x, SEXP argmultiple);
SEXP dc_readbin(SEXP argcon, SEXP swhat, SEXP argn, SEXP argsize, SEXP argsignd, SEXP argswap);
SEXP dc_readchar(SEXP argcon, SEXP nchars, SEXP arguseBytes);
SEXP dc_readDCF(SEXP file, SEXP argwhat, SEXP argfold_excludes);
SEXP dc_readLines(SEXP argcon, SEXP argn, SEXP argok, SEXP argwarn, SEXP argencoding, SEXP argskipNul);
SEXP dc_readlink(SEXP paths);
SEXP dc_readln(SEXP prompt);
SEXP dc_regexec(SEXP pat, SEXP text, SEXP argicase, SEXP argfixed, SEXP arguseBytes);
SEXP dc_regFinaliz(SEXP args, SEXP argfun, SEXP argonexit);
SEXP dc_remove(SEXP name, SEXP envarg, SEXP arginherits);
SEXP dc_rep_int(SEXP s, SEXP ncopy);
SEXP dc_rep_len(SEXP s, SEXP len);
SEXP dc_resetCondHands(SEXP newstack);
SEXP dc_restart(SEXP dummy);
SEXP dc_returnValue(SEXP dflt);
SEXP dc_Rhome();
SEXP dc_RNGkind (SEXP rng, SEXP norm);
SEXP dc_sample2(SEXP argdn, SEXP argk);
SEXP dc_sample(SEXP sn, SEXP sk, SEXP sreplace, SEXP prob);
SEXP dc_search();
SEXP dc_seek(SEXP argcon, SEXP argwhere, SEXP argorigin, SEXP argrw);
SEXP dc_setencoding(SEXP x, SEXP enc);
SEXP dc_setenv(SEXP nm, SEXP vars);
SEXP dc_seterrmessage(SEXP msg);
SEXP dc_setFileTime(SEXP argfn, SEXP argftime);
SEXP dc_setlocale(SEXP argcat, SEXP locale);
SEXP dc_setmaxnumthreads(SEXP argnew);
SEXP dc_setnumthreads(SEXP argnew);
SEXP dc_setS4Object(SEXP object, SEXP argflag, SEXP argcomplete);
SEXP dc_setseed (SEXP argseed, SEXP skind, SEXP nkind);
SEXP dc_setSessionTimeLimit(SEXP argcpu, SEXP argelapsed);
SEXP dc_setTimeLimit(SEXP argcpu, SEXP argelapsed, SEXP argtransient);
SEXP dc_shortRowNames(SEXP x, SEXP argtype);
SEXP dc_signalCondition(SEXP cond, SEXP msg, SEXP ecall);
SEXP dc_sinknumber(SEXP argcon);
SEXP dc_sink(SEXP argcon, SEXP argcloseOnExit, SEXP argerrcon, SEXP argtee);
SEXP dc_sockconn(SEXP scmd, SEXP argport, SEXP argserver, SEXP argblocking, SEXP sopen, SEXP enc, SEXP argtimeout);
SEXP dc_sockselect(SEXP insock, SEXP write, SEXP argtimeout);
SEXP dc_sort(SEXP x, SEXP argdecreasing);
SEXP dc_split(SEXP x, SEXP f);
SEXP dc_stderr();
SEXP dc_stdin();
SEXP dc_stdout();
SEXP dc_strptime(SEXP x, SEXP sformat, SEXP stz);
SEXP dc_strrep(SEXP x, SEXP n);
SEXP dc_strtoi(SEXP x, SEXP b);
SEXP dc_strtrim(SEXP x, SEXP argwidth);
SEXP dc_substrgets(SEXP x, SEXP sa, SEXP so, SEXP value);
SEXP dc_substr(SEXP x, SEXP sa, SEXP so);
SEXP dc_sumconnection(SEXP argcon);
SEXP dc_syschmod(SEXP paths, SEXP argsmode, SEXP arguseMask);
SEXP dc_sysgetpid();
SEXP dc_systime();
SEXP dc_sysumask(SEXP argmode);
SEXP dc_tabulate(SEXP in, SEXP nbin);
SEXP dc_tempdir();
SEXP dc_tempfile(SEXP pattern, SEXP tempdir, SEXP fileext);
SEXP dc_textconnection(SEXP sfile, SEXP stext, SEXP sopen, SEXP venv, SEXP argtype);
SEXP dc_textconvalue(SEXP argcon);
SEXP dc_traceback(SEXP argskip);
SEXP dc_transpose(SEXP a);
SEXP dc_truncate(SEXP argcon);
SEXP dc_typeof(SEXP x);
SEXP dc_unlink(SEXP fn, SEXP argrecursive, SEXP argforce);
SEXP dc_unserializeFromConn(SEXP argcon, SEXP fun);
SEXP dc_unsetenv(SEXP vars);
SEXP dc_unz(SEXP sfile, SEXP sopen, SEXP enc);
SEXP dc_utf8ToInt(SEXP x);
SEXP dc_validEnc(SEXP x);
SEXP dc_validUTF8(SEXP x);
SEXP dc_version();
SEXP dc_which(SEXP v);
SEXP dc_writebin(SEXP object, SEXP argcon, SEXP argsize, SEXP argswap, SEXP arguseBytes);
SEXP dc_writechar(SEXP object, SEXP argcon, SEXP nchars, SEXP sep, SEXP arguseBytes);
SEXP dc_writelines(SEXP text, SEXP argcon, SEXP sep, SEXP arguseBytes);
SEXP NORET dc_dfltStop(SEXP argmsg, SEXP ecall);
SEXP NORET dc_invokeRestart(SEXP r, SEXP arglist);

/* .Call implementations of do_functions requiring call, op, env */

SEXP dc_primitive(SEXP call, SEXP op, SEXP env, SEXP name);
SEXP do_primitive(SEXP call, SEXP op, SEXP args, SEXP env);

#endif /* not R_INTERNAL_H */
