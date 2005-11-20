/*
 * Lisp interface to libgit for XEmacs.
 *
 * Copyright (C) 1998, 1999 J. Kean Johnston. All rights reserved.
 * Copyright (C) 2002 Jerry James.
 * Copyright (C) 2005 Free Software Foundation, Inc.
 *
 * Author: Stephen J. Turnbull <stephen@xemacs.org>
 */

#include <config.h>
#include "lisp.h"

/*
 * Each dynamically loaded Emacs module is given a name at compile
 * time. This is a short name, and must be a valid part of a C
 * identifier.  This name is used to construct the name of several
 * functions which must appear in the module source code.
 * The first such function, modules_of_XXXX, should load in any dependent
 * modules. This function is optional, and the module will still load if
 * it is not present in the module.
 *
 * The second function, which is NOT optional, is syms_of_XXXX, in which
 * all functions that the module will be provided are declared. This
 * function will contain calls to DEFSUBR().
 *
 * The third function, which is also NOT optional, is vars_of_XXXX, in
 * which you declare all variables that the module provides. This
 * function will contain calls to DEFVAR_LISP(), DEFVAR_BOOL() etc.
 *
 * When declaring functions and variables in the syms_of_XXXX and
 * vars_of_XXXX functions, you use the exact same syntax that you
 * would as if this module were being compiled into the pure Emacs.
 *
 * The fourth function, which is optional, is unload_XXXX, in which actions
 * that must be taken to unload the module are listed.  XEmacs will unbind
 * functions and variables for you.  Anything else that must be done should
 * appear in this function.
 *
 * All four of these functions are declared as void functions,
 * taking no parameters. Since this sample module is called 'sample',
 * the functions will be named 'modules_of_sample', 'syms_of_sample',
 * 'vars_of_sample', and 'unload_sample'.
 */

void
modules_of_git_api ()
{
  /*
   * This function isn't actually required as we will not be loading
   * in any dependent modules, but if we were, we would do something like:
   * emodules_load ("dependent.ell", "sample2", "1.0.0");
   */
}

void
syms_of_git_api ()
{
  DEFSUBR(Fsample_function);
}

void
vars_of_git_api ()
{
  DEFVAR_LISP ("sample-string", &Vsample_string /*
This is a sample string, declared in a dynamic module.

The syntax and conventions used for all normal Emacs variables
apply equally to modules, using an identical syntax.
*/ );

  DEFVAR_BOOL ("sample-boolean", &sample_bool /*
*Sample boolean value, in a dynamic module.

This is a user-settable variable, as indicated by the *
as the first character of the description. Declared in
a module exactly as it would be internally in Emacs.
*/ );
}

#ifdef HAVE_SHLIB
void
unload_git_api ()
{
  /* We don't need to do anything here in the sample case.  However, if you
     create any new types with INIT_LRECORD_IMPLEMENTATION (sample_type), then
     UNDEF_LRECORD_IMPLEMENTATION (sample_type) must appear here.  Also, any
     symbols declared with DEFSYMBOL (Qsample_var), or one of its variants,
     must have a corresponding unstaticpro_nodump (&Qsample_var) here. */
}
#endif
