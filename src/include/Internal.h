/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2015  The R Core Team
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
SEXP do_anyNA(SEXP, SEXP, SEXP, SEXP);
SEXP do_arith(SEXP, SEXP, SEXP, SEXP);
SEXP do_ascall(SEXP, SEXP, SEXP, SEXP);
SEXP do_as_environment(SEXP, SEXP, SEXP, SEXP);
SEXP do_asatomic(SEXP, SEXP, SEXP, SEXP);
SEXP do_asvector(SEXP, SEXP, SEXP, SEXP);
SEXP do_attr(SEXP, SEXP, SEXP, SEXP);
SEXP do_attrgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_attributes(SEXP, SEXP, SEXP, SEXP);
SEXP do_attributesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_begin(SEXP, SEXP, SEXP, SEXP);
SEXP do_bind(SEXP, SEXP, SEXP, SEXP);
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);
SEXP do_c(SEXP, SEXP, SEXP, SEXP);
SEXP do_c_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_call(SEXP, SEXP, SEXP, SEXP);
SEXP do_class(SEXP, SEXP, SEXP, SEXP);
SEXP do_classgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_contourLines(SEXP, SEXP, SEXP, SEXP);
SEXP do_cum(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlDownload(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlGetHeaders(SEXP, SEXP, SEXP, SEXP);
SEXP do_curlVersion(SEXP, SEXP, SEXP, SEXP);
SEXP do_dim(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimnames(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimnamesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcallgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotCode(SEXP, SEXP, SEXP, SEXP);
SEXP do_edit(SEXP, SEXP, SEXP, SEXP);
SEXP do_enc2(SEXP, SEXP, SEXP, SEXP);
SEXP do_envirgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_External(SEXP, SEXP, SEXP, SEXP);
SEXP do_Externalgr(SEXP, SEXP, SEXP, SEXP);
SEXP do_eval(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);
SEXP do_forceAndCall(SEXP, SEXP, SEXP, SEXP);
SEXP do_function(SEXP, SEXP, SEXP, SEXP);
SEXP do_gctime(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEvent(SEXP, SEXP, SEXP, SEXP);
SEXP do_getGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
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
SEXP do_lapack(SEXP, SEXP, SEXP, SEXP);
SEXP do_length(SEXP, SEXP, SEXP, SEXP);
SEXP do_lengthgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_levelsgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_log(SEXP, SEXP, SEXP, SEXP);
SEXP do_log1arg(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic3(SEXP, SEXP, SEXP, SEXP);
SEXP do_makelist(SEXP, SEXP, SEXP, SEXP);
SEXP do_matprod(SEXP, SEXP, SEXP, SEXP);
SEXP do_Math2(SEXP, SEXP, SEXP, SEXP);
SEXP do_missing(SEXP, SEXP, SEXP, SEXP);
SEXP do_names(SEXP, SEXP, SEXP, SEXP);
SEXP do_namesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_nchar(SEXP,SEXP,SEXP,SEXP);
SEXP do_nzchar(SEXP,SEXP,SEXP,SEXP);
SEXP do_onexit(SEXP, SEXP, SEXP, SEXP);
SEXP do_options(SEXP, SEXP, SEXP, SEXP);
SEXP do_order(SEXP, SEXP, SEXP, SEXP);
SEXP do_pmin(SEXP, SEXP, SEXP, SEXP);
SEXP do_pos2env(SEXP, SEXP, SEXP, SEXP);
SEXP do_quote(SEXP, SEXP, SEXP, SEXP);
SEXP do_range(SEXP, SEXP, SEXP, SEXP);
SEXP do_recall(SEXP, SEXP, SEXP, SEXP);
SEXP do_refcnt(SEXP, SEXP, SEXP, SEXP);
SEXP do_recordGraphics(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP do_rep(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_return(SEXP, SEXP, SEXP, SEXP);
SEXP do_S4on(SEXP, SEXP, SEXP, SEXP);
SEXP do_saveplot(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq_along(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq_len(SEXP, SEXP, SEXP, SEXP);
SEXP do_set(SEXP, SEXP, SEXP, SEXP);
SEXP do_setGraphicsEventEnv(SEXP, SEXP, SEXP, SEXP);
SEXP do_sprintf(SEXP, SEXP, SEXP, SEXP);
SEXP do_standardGeneric(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_stop(SEXP, SEXP, SEXP, SEXP);
SEXP do_storage_mode(SEXP, SEXP, SEXP, SEXP);
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
SEXP do_tilde(SEXP, SEXP, SEXP, SEXP);
SEXP do_trunc(SEXP, SEXP, SEXP, SEXP);
SEXP do_unclass(SEXP, SEXP, SEXP, SEXP);
SEXP do_unlist(SEXP, SEXP, SEXP, SEXP);
SEXP NORET do_usemethod(SEXP, SEXP, SEXP, SEXP);
SEXP do_warning(SEXP, SEXP, SEXP, SEXP);
SEXP do_xtfrm(SEXP, SEXP, SEXP, SEXP);

SEXP do_getSnapshot(SEXP, SEXP, SEXP, SEXP);
SEXP do_playSnapshot(SEXP, SEXP, SEXP, SEXP);

SEXP R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_do_set_class(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type);

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
SEXP dc_attach(SEXP what, SEXP argpos, SEXP name);
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
SEXP dc_close(SEXP argcon, SEXP dummy);
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
SEXP dc_machine();
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
SEXP dc_psort(SEXP x, SEXP p);
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
SEXP dc_refcnt(SEXP x);
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
SEXP dc_save(SEXP list, SEXP argfile, SEXP argascii, SEXP argversion, SEXP source, SEXP argep);
SEXP dc_saveToConn(SEXP list, SEXP argconn, SEXP argascii, SEXP argversion, SEXP source, SEXP argep);
SEXP dc_serializeToConn(SEXP object, SEXP argconn, SEXP argascii, SEXP argversion, SEXP fun);
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
SEXP dc_setwd(SEXP s);
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
SEXP dc_strsplit(SEXP x, SEXP tok, SEXP argfixed, SEXP argperl, SEXP arguseBytes);
SEXP dc_strtoi(SEXP x, SEXP b);
SEXP dc_strtrim(SEXP x, SEXP argwidth);
SEXP dc_substrgets(SEXP x, SEXP sa, SEXP so, SEXP value);
SEXP dc_substr(SEXP x, SEXP sa, SEXP so);
SEXP dc_sumconnection(SEXP argcon);
SEXP dc_syschmod(SEXP paths, SEXP argsmode, SEXP arguseMask);
SEXP dc_sysgetpid();
SEXP dc_sysinfo();
#ifndef Win32
SEXP dc_system(SEXP argcmd, SEXP argintern);
#endif
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
SEXP dc_warning(SEXP argcall, SEXP argimmediate, SEXP argnobreak, SEXP argmsg);
SEXP dc_which(SEXP v);
SEXP dc_writebin(SEXP object, SEXP argcon, SEXP argsize, SEXP argswap, SEXP arguseBytes);
SEXP dc_writechar(SEXP object, SEXP argcon, SEXP nchars, SEXP sep, SEXP arguseBytes);
SEXP dc_writelines(SEXP text, SEXP argcon, SEXP sep, SEXP arguseBytes);
SEXP NORET dc_dfltStop(SEXP argmsg, SEXP ecall);
SEXP NORET dc_invokeRestart(SEXP r, SEXP arglist);

/* .Call implementations of do_functions requiring call, op, env */

SEXP dc_addTryHandlers(SEXP call, SEXP op, SEXP rho);
SEXP dc_agrep(SEXP call, SEXP op, SEXP env, SEXP pat, SEXP vec, SEXP argicase, SEXP argvalue, SEXP opt_costs, SEXP opt_bounds, SEXP arguseBytes, SEXP argfixed);
SEXP dc_aregexec(SEXP call, SEXP op, SEXP env, SEXP pat, SEXP vec, SEXP opt_bounds, SEXP opt_costs, SEXP argicase, SEXP argfixed, SEXP arguseBytes);
SEXP dc_args(SEXP call, SEXP op, SEXP rho, SEXP name);
SEXP dc_asfunction(SEXP call, SEXP op, SEXP rho, SEXP arglist, SEXP envir);
SEXP dc_AT(SEXP call, SEXP op, SEXP env, SEXP argobj, SEXP nlist);
SEXP dc_bcclose(SEXP call, SEXP op, SEXP rho, SEXP forms, SEXP body, SEXP env);
SEXP dc_bindtextdomain(SEXP call, SEXP op, SEXP rho, SEXP argdomain, SEXP argdirname);
SEXP dc_bitwise(SEXP call, SEXP op, SEXP env, SEXP a, SEXP b);
SEXP NORET dc_break(SEXP call, SEXP op, SEXP rho);
SEXP dc_cat(SEXP call, SEXP op, SEXP rho, SEXP objs, SEXP file, SEXP sepr, SEXP fill, SEXP labs, SEXP argappend);
SEXP dc_colon(SEXP call, SEXP op, SEXP rho, SEXP s1, SEXP s2);
SEXP dc_colsum(SEXP call, SEXP op, SEXP rho, SEXP x, SEXP argn, SEXP argp, SEXP argNaRm);
SEXP dc_commentgets(SEXP x, SEXP value);
SEXP dc_debug(SEXP call, SEXP op, SEXP rho, SEXP fun, SEXP text, SEXP condition);
SEXP dc_delayed(SEXP call, SEXP op, SEXP rho, SEXP argname, SEXP expr, SEXP eenv, SEXP aenv);
SEXP dc_deparse(SEXP expr, SEXP argcutoff, SEXP argbacktick, SEXP argopts, SEXP argnlines);
SEXP dc_disassemble(SEXP call, SEXP op, SEXP rho, SEXP code);
SEXP dc_docall(SEXP fun, SEXP args, SEXP envir);
SEXP dc_dump(SEXP call, SEXP op, SEXP rho, SEXP names, SEXP file, SEXP source, SEXP argopts, SEXP argevaluate);
SEXP dc_duplicated(SEXP call, SEXP op, SEXP env, SEXP x, SEXP incomp, SEXP argfromLast, SEXP argnmax);
SEXP dc_dynload(SEXP call, SEXP op, SEXP env, SEXP x, SEXP local, SEXP now, SEXP path);
SEXP dc_dynunload(SEXP call, SEXP op, SEXP env, SEXP x);
SEXP dc_eapply(SEXP call, SEXP op, SEXP rho, SEXP argenv, SEXP FUN, SEXP argall, SEXP arguseNms);
SEXP dc_filepath(SEXP call, SEXP op, SEXP env, SEXP x, SEXP sep);
SEXP dc_first_min(SEXP call, SEXP op, SEXP rho, SEXP sx);
SEXP dc_for(SEXP call, SEXP op, SEXP rho, SEXP sym, SEXP value, SEXP body);
SEXP dc_getRegNS(SEXP call, SEXP op, SEXP rho, SEXP argname);
SEXP dc_get(SEXP call, SEXP op, SEXP rho, SEXP argx, SEXP argenvir, SEXP argmode, SEXP arginherits, SEXP value_if_not_exists);
SEXP dc_gettext(SEXP call, SEXP op, SEXP rho, SEXP argdomain, SEXP string);
SEXP dc_grep(SEXP call, SEXP op, SEXP env, SEXP pat, SEXP text, SEXP argigcase, SEXP argvalue, SEXP argperl, SEXP argfixed, SEXP arguseBytes, SEXP arginvert);
SEXP dc_gsub(SEXP call, SEXP op, SEXP env, SEXP pat, SEXP rep, SEXP text, SEXP argigcase, SEXP argperl, SEXP argfixed, SEXP arguseBytes);
SEXP dc_gzfile(SEXP call, SEXP op, SEXP env, SEXP sfile, SEXP sopen, SEXP enc, SEXP argcompress);
SEXP dc_is_builtin_internal(SEXP call, SEXP op, SEXP rho, SEXP symbol);
SEXP dc_isvector(SEXP call, SEXP op, SEXP rho, SEXP x, SEXP argtype);
SEXP dc_lapply(SEXP call, SEXP op, SEXP rho, SEXP X, SEXP FUN);
SEXP dc_lengths(SEXP call, SEXP op, SEXP rho, SEXP x, SEXP arguseNames);
SEXP dc_loadfile(SEXP call, SEXP op, SEXP env, SEXP argfile);
SEXP dc_lockBnd(SEXP call, SEXP op, SEXP rho, SEXP sym, SEXP env);
SEXP dc_logic2(SEXP call, SEXP op, SEXP env, SEXP s1, SEXP s2);
SEXP dc_mapply(SEXP call, SEXP op, SEXP rho, SEXP f, SEXP varyingArgs, SEXP constantArgs);
SEXP dc_matchcall(SEXP call, SEXP op, SEXP env, SEXP b, SEXP funcall, SEXP argexpdots, SEXP sysp);
SEXP dc_match(SEXP call, SEXP op, SEXP env, SEXP x, SEXP table, SEXP argnomatch, SEXP incomp);
SEXP dc_mget(SEXP call, SEXP op, SEXP rho, SEXP x, SEXP env, SEXP mode, SEXP argifnotfound, SEXP arginherits);
SEXP dc_nargs(SEXP call, SEXP op, SEXP rho);
SEXP dc_nextmethod(SEXP call, SEXP op, SEXP env, SEXP arggeneric, SEXP dummy, SEXP argdots);
SEXP dc_ngettext(SEXP call, SEXP op, SEXP rho, SEXP argn, SEXP msg1, SEXP msg2, SEXP sdom);
SEXP dc_normalizepath(SEXP call, SEXP op, SEXP rho, SEXP paths, SEXP slash, SEXP argmustWork);
SEXP dc_parse(SEXP call, SEXP op, SEXP env, SEXP argfile, SEXP argnum, SEXP argtext, SEXP prompt, SEXP source, SEXP argencoding);
SEXP dc_paste(SEXP call, SEXP op, SEXP env, SEXP x, SEXP argsepcoll, SEXP argcollapse);
SEXP dc_primitive(SEXP call, SEXP op, SEXP env, SEXP name);
SEXP dc_printdefault(SEXP call, SEXP op, SEXP rho, SEXP x, SEXP argdigits, SEXP argquote, SEXP naprint, SEXP argprintgap, SEXP argright, SEXP argmax, SEXP arguseSource, SEXP argtryS4);
SEXP dc_printfunction(SEXP call, SEXP op, SEXP rho, SEXP s, SEXP arguseSource);
SEXP dc_quit(SEXP call, SEXP op, SEXP rho, SEXP argsave, SEXP argstatus, SEXP argrunLast);
SEXP dc_random1(SEXP call, SEXP op, SEXP rho, SEXP argn, SEXP arga);
SEXP dc_random2(SEXP call, SEXP op, SEXP rho, SEXP argn, SEXP arga, SEXP argb);
SEXP dc_random3(SEXP call, SEXP op, SEXP rho, SEXP argn, SEXP a, SEXP b, SEXP c);
SEXP dc_rank(SEXP call, SEXP op, SEXP rho, SEXP x, SEXP sn, SEXP argties);
SEXP dc_rapply(SEXP call, SEXP op, SEXP rho, SEXP X, SEXP fun, SEXP classes, SEXP deflt, SEXP how);
SEXP dc_readEnviron(SEXP call, SEXP op, SEXP env, SEXP x);
SEXP dc_regexpr(SEXP call, SEXP op, SEXP env, SEXP pat, SEXP text, SEXP argigcase, SEXP argperl, SEXP argfixed, SEXP arguseBytes);
SEXP dc_regNS(SEXP call, SEXP op, SEXP rho, SEXP argname, SEXP val);
SEXP dc_repeat(SEXP call, SEXP op, SEXP rho, SEXP body);
SEXP dc_rowscols(SEXP call, SEXP op, SEXP rho, SEXP x);
SEXP dc_rowsum(SEXP call, SEXP op, SEXP env, SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP rn);
SEXP dc_savefile(SEXP call, SEXP op, SEXP env, SEXP code, SEXP argfile, SEXP argascii);
SEXP dc_scan(SEXP call, SEXP op, SEXP rho, SEXP file, SEXP what, SEXP argnmax, SEXP sep, SEXP dec, SEXP quotes, SEXP argskip, SEXP argnlines, SEXP nastrings, SEXP argflush, SEXP argfill, SEXP stripwhite, SEXP argquiet, SEXP argblskip, SEXP argmultiline, SEXP comstr, SEXP argescapes, SEXP argencoding, SEXP argskipNul);
SEXP dc_serialize(SEXP call, SEXP op, SEXP env, SEXP object, SEXP icon, SEXP type, SEXP ver, SEXP fun);
SEXP dc_sys(SEXP call, SEXP op, SEXP rho, SEXP argn);
SEXP dc_sysbrowser(SEXP call, SEXP op, SEXP rho, SEXP argn);
SEXP dc_syssleep(SEXP call, SEXP op, SEXP rho, SEXP argtime);
#ifdef Win32
SEXP dc_system(SEXP call, SEXP op, SEXP rho, SEXP cmd, SEXP argflag, SEXP fin, SEXP Stdout, SEXP Stderr);
#endif
SEXP dc_tolower(SEXP call, SEXP op, SEXP env, SEXP x);
SEXP dc_topenv(SEXP call, SEXP op, SEXP rho, SEXP envir, SEXP target);
SEXP dc_trace(SEXP call, SEXP op, SEXP rho, SEXP fun);
SEXP dc_traceOnOff(SEXP call, SEXP op, SEXP rho, SEXP onOff);
SEXP dc_unregNS(SEXP call, SEXP op, SEXP rho, SEXP argname);
SEXP dc_url(SEXP call, SEXP op, SEXP env, SEXP scmp, SEXP sopen, SEXP argblock, SEXP enc, SEXP argmeth, SEXP argraw);
SEXP dc_vapply(SEXP call, SEXP op, SEXP rho, SEXP X, SEXP FUN, SEXP argvalue, SEXP arguseNames);
SEXP dc_while(SEXP call, SEXP op, SEXP rho, SEXP value, SEXP body);
SEXP dc_withVisible(SEXP call, SEXP op, SEXP rho, SEXP x);

#endif /* not R_INTERNAL_H */
