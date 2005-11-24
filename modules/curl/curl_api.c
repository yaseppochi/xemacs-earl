/*
 * Lisp interface to libcurl for XEmacs.
 *
 * Copyright (C) 1998, 1999 J. Kean Johnston. All rights reserved.
 * Copyright (C) 2002 Jerry James.
 * Copyright (C) 2005 Stephen J. Turnbull <stephen@xemacs.org>
 *
 * All rights reserved, except as expressly indicated below.
 *
 * This program is not considered part of XEmacs.
 *
 * You may use, copy, modify, and distribute this software under the terms
 * of the GNU General Public License, version 2 or later at your option.
 *
 * Author:		Stephen J. Turnbull <stephen@xemacs.org>
 * Creation-Date:	2005-11-20
 */

#include <config.h>
#include "lisp.h"
#ifdef HAVE_SHLIB
# include "emodules.h"
#endif
#include "lstream.h"
#include "elhash.h"

#include "curl_api.h"		/* include <curl/curl.h> */

/************************************************************************/
/*			   Module-specific stuff			*/
/************************************************************************/

/* Local references to Lisp symbols */
static Lisp_Object Qcurl_api, Qcurl, Qurl_handlep,
  Qlong, Qfunctionpoint, Qobjectpoint, Qoff_t;

static Lisp_Object Vcurl_option_hash_table;

/************************************************************************/
/*                  url_handle lrecord basic functions                  */
/************************************************************************/

static const struct memory_description url_handle_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_URL_Handle, type) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_URL_Handle, property_list) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_URL_Handle, coding_system) },
  { XD_END }
};

static Lisp_Object
mark_url_handle (Lisp_Object obj)
{
  if (NILP (XURL_HANDLE (obj)->property_list))
    return XURL_HANDLE (obj)->type;

  mark_object (XURL_HANDLE (obj)->coding_system);
  mark_object (XURL_HANDLE (obj)->type);
  return XURL_HANDLE (obj)->property_list;
}

static void
print_url_handle (Lisp_Object obj,
		  Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Lisp_URL_Handle *url_handle = XURL_HANDLE (obj);

  if (print_readably)
    printing_unreadable_object ("#<url_handle %s>", url_handle->url);

  write_c_string (printcharfun, "#<url_handle ");
  if (NILP(url_handle->type))
    write_c_string (printcharfun, "(dead) ");
  else
    write_fmt_string_lisp (printcharfun, "%S ", 1, url_handle->type);
  if (url_handle->url)
    write_c_string (printcharfun, url_handle->url);
  write_fmt_string (printcharfun, " 0x%lx>", (unsigned long) url_handle);
}

static Lisp_URL_Handle *
allocate_url_handle (void)
{
  Lisp_URL_Handle *url_handle =
    ALLOC_LCRECORD_TYPE (Lisp_URL_Handle, &lrecord_url_handle);

  url_handle->type = Qnil;
  url_handle->property_list = Qnil;
  url_handle->coding_system = Qnil;
  url_handle->url = NULL;
  url_handle->curl_handle = NULL;
  /* #### UNIMPLEMENTED we need to initialize the big_ball_of_strings here. */
  return url_handle;
}

static void
finalize_url_handle (void *header, int for_disksave)
{
  Lisp_URL_Handle *url_handle = (Lisp_URL_Handle *) header;

  if (for_disksave)
    invalid_operation ("Can't dump an emacs containing URL_HANDLE objects",
		       wrap_url_handle (url_handle));

  /* #### UNIMPLEMENTED we need to free the big_ball_of_strings here. */
  if (url_handle->curl_handle)
    curl_easy_cleanup (url_handle->curl_handle);
  url_handle->curl_handle = NULL;
  if (url_handle->url)
    xfree (url_handle->url, Extbyte *);
  url_handle->url = NULL;
}

DEFINE_LRECORD_IMPLEMENTATION ("url_handle", url_handle, 0,
                               mark_url_handle, print_url_handle,
			       finalize_url_handle,
                               NULL, NULL,
			       url_handle_description, Lisp_URL_Handle);


/************************************************************************/
/*                        Basic url_handle accessors                          */
/************************************************************************/

/* ###autoload */
DEFUN ("url-handle-p", Furl_handle_p, 1, 1, 0, /*
Return t if OBJECT is a URL_HANDLE connection.
*/
       (object))
{
  return URL_HANDLEP (object) ? Qt : Qnil;
}

DEFUN ("url-handle-type", Furl_handle_type, 1, 1, 0, /*
Return the type of URL-HANDLE, a symbol.
*/
       (url_handle))
{
  CHECK_URL_HANDLE (url_handle);
  return XURL_HANDLE (url_handle)->type;
}

DEFUN ("url-handle-live-p", Furl_handle_live_p, 1, 1, 0, /*
Return non-nil if URL_HANDLE is an active URL_HANDLE connection.
*/
       (url_handle))
{
  CHECK_URL_HANDLE (url_handle);
  return Furl_handle_type (url_handle);
}

DEFUN ("url-handle-property-list", Furl_handle_host, 1, 1, 0, /*
Return the property list of URL-HANDLE.
*/
       (url_handle))
{
  Lisp_Object retval;

  CHECK_URL_HANDLE (url_handle);
  retval = XURL_HANDLE (url_handle)->property_list;
  /* #### extract properties from the curl_handle and add to retval here */
  return retval;
}

#if 0
DEFUN ("url-handle-host", Furl_handle_host, 1, 1, 0, /*
Return the server host of the connection URL-HANDLE, as a string.
*/
       (url_handle))
{
  CHECK_URL_HANDLE (url_handle);
  return WHAT?
}
#endif


/************************************************************************/
/*			    Lisp API functions				*/
/************************************************************************/

DEFUN ("curl-make-url-handle", Fcurl_make_url_handle, 1, 3, 0, /*
Return a cURL handle for URL, wrapped in an url-handle.
URL is a string, which must be a URI scheme known to cURL.
URL is encoded according to optional argument CODESYS.  (Of course cURL will
URL-encode it before sending it off.)
PLIST is a property list.  These properties are set on the cURL handle.

#### This interface may change to (&rest PLIST).

#### This is not called "make-curl-handle" because a more general module,
which will call this function, is planned.  That will call this function
from a function `make-url-handle'.
*/
      (url, codesys, plist))
{
  Lisp_URL_Handle *url_handle = allocate_url_handle ();

  url_handle->type = Qcurl;

  if (NILP (codesys))
    /* #### Quick hack, should be Qnative? */
    codesys = Ffind_coding_system (Qutf_8);
  CHECK_CODING_SYSTEM (codesys);
  url_handle->coding_system = codesys;

  /* Do this *before* the plist because later we will be initializing
     curl_handle options from the plist. */
  CHECK_STRING (url);
  url_handle->url = NEW_LISP_STRING_TO_EXTERNAL_MALLOC(url, codesys);
  url_handle->curl_handle = curl_easy_init ();
  curl_easy_setopt (url_handle->curl_handle, CURLOPT_URL, url_handle->url);

  /* skeleton - currently simply copies the plist
     This has the effect of checking well-formedness, and we may want
     to handle some properties specially. */
  url_handle->property_list = Qnil;
  {
    EXTERNAL_PROPERTY_LIST_LOOP_3(key, value, plist)
      {
	url_handle->property_list =
	  Fcons (key, Fcons (value, url_handle->property_list));
      }
  }

  return wrap_url_handle (url_handle);
}

#define UNIMPLEMENTED(reason) signal_error (Qunimplemented, reason, Qunbound)

DEFUN ("curl-easy-setopt", Fcurl_easy_setopt, 3, 3, 0, /*
Set OPTION to VALUE on curl url-handle HANDLE and return t.
OPTION is a string denoting an option in `curl-option-hash-table'.
VALUE must be of the appropriate type.
HANDLE must be an url-handle object of type `curl'.
A wrapper with some validation for libcurl's `curl_easy_setopt'.
*/
       (option, value, handle))
{
  Lisp_Object optdata = Fgethash (option, Vcurl_option_hash_table, Qnil);
  Lisp_Object opttype = Fcar (optdata);
  Lisp_Object optindex = Fcar (Fcdr (optdata));
  CURLoption index;
  CURL *curl;
  CURLcode code;
  Lisp_URL_Handle *h;

  CHECK_URL_HANDLE (handle);
  h = XURL_HANDLE (handle);
  if (!EQ (h->type, Qcurl))
    wtaerror ("handle is not a curl handle", handle);
  curl = h->curl_handle;
  CHECK_INT (optindex);
  index = XINT (optindex);

  if (EQ (opttype, Qlong))
    {
      CHECK_INT (value);
      index += CURLOPTTYPE_LONG;  /* CURLoptions encode type */
      code = curl_easy_setopt (curl, (CURLoption) index, XINT (value));
      /* #### Do something more useful with the error codes! */
      if (code)
	{
	  signal_error_2 (Qio_error,
			  /* #### MEMORY LEAK!
			     We probably don't need to copy at all, but if
			     we do, add it to big_ball_of_strings. */
			  strdup (curl_easy_strerror (code)),
			  make_int (index), handle);
	}
    }
  else if (EQ (opttype, Qobjectpoint))
    {
      Extbyte *s;
      /* HA-A-ACK!  Currently we only support strings here, and we
	 don't check a string is a sane value for the option. */
      CHECK_STRING (value);
      index += CURLOPTTYPE_OBJECTPOINT;  /* CURLoptions encode type */
      /* #### MEMORY LEAK!
	 We need to add a pointer to the converted data here to a Dynarr
	 which is part of the handle.  That will be freed when the handle
	 is collected. */
      /* #### Is this the right coding system? */
      s = NEW_LISP_STRING_TO_EXTERNAL_MALLOC (value, h->coding_system);
      /* #### Do something more useful with the error codes! */
      code = curl_easy_setopt (curl, (CURLoption) index, s);
      if (code)
	{
	  signal_error_2 (Qio_error,
			  /* #### MEMORY LEAK!
			     We probably don't need to copy at all, but if
			     we do, add it to big_ball_of_strings. */
			  strdup (curl_easy_strerror (code)),
			  make_int (index), handle);
	}
      if (index == CURLOPT_URL)
	{
	  h->url = s;
	}
    }
  else if (EQ (opttype, Qfunctionpoint))
    {
      index += CURLOPTTYPE_FUNCTIONPOINT;  /* CURLoptions encode type */
      UNIMPLEMENTED ("curl_api function pointer options");
    }
  else if (EQ (opttype, Qoff_t))
    {
      index += CURLOPTTYPE_OFF_T;  /* CURLoptions encode type */
      UNIMPLEMENTED ("curl_api large file offset options");
    }
  else
    {
      invalid_state ("invalid option type in `curl-option-hash-table",
		     opttype);
    }
  return Qt;
}

static size_t curl_write_function (void *data, size_t size, size_t nmemb,
				   void *stream)
{
  size_t count = size * nmemb;
  Lstream *s = XLSTREAM ((Lisp_Object) stream);
  /* #### the return code should be checked */
  return (Lstream_write (s, data, count)) ? --count : count;
}

DEFUN ("curl-easy-perform", Fcurl_easy_perform, 1, 2, 0, /*
Read from the URL represented by HANDLE into BUFFER at point.
Optional BUFFER defaults to the current buffer.
Returns t.
#### Maybe we should create a new buffer here?
*/
       (handle, buffer))
{
  CHECK_URL_HANDLE (handle);
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  CHECK_BUFFER (buffer);
  
  if (EQ (XURL_HANDLE (handle)->type, Qcurl))
    {
      CURL *curl = XURL_HANDLE (handle)->curl_handle;
      if (curl)
	{
	  CURLcode code;
	  struct buffer *buf = XBUFFER(buffer);
	  /* #### I don't see how to generalize this.
	     We'd like to be able to the Lstream in from Lisp, and somehow
	     figure out what the type is so as to use the proper
	     CURLOPT_WRITEFUNCTION.
	     Ditto for CURLOPT_READFUNCTION and CURLOPT_READDATA. */
	  Lisp_Object s = make_lisp_buffer_output_stream (buf, buf->bufpt,
							  LSTR_ALLOW_QUIT);
	  curl_easy_setopt (curl, CURLOPT_WRITEFUNCTION, &curl_write_function);
	  curl_easy_setopt (curl, CURLOPT_WRITEDATA, s);
	  code = curl_easy_perform (curl);
	  Lstream_close (XLSTREAM (s));
	  if (code)
	    {
	      signal_error (Qio_error,
			    /* #### MEMORY LEAK!
			       We probably don't need to copy at all, but if
			       we do, add it to big_ball_of_strings. */
			    strdup (curl_easy_strerror (code)),
			    handle);
	    }
	}
      else
	{
	  invalid_state ("URL handle is uninitialized", handle);
	}
    }
  else
    {
      wtaerror ("URL handle is not a cURL handle", handle);
    }
  return Qt;
}

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
 * taking no parameters.
 */

#if 0
void
modules_of_curl_api ()
{
  /*
   * This function isn't actually required as we will not be loading
   * in any dependent modules, but if we were, we would do something like:
   * emodules_load ("dependent.ell", "sample2", "1.0.0");
   */

  /* MAYBE: emodules_load ("earl.ell", "earl", "0.0.1"); */
}
#endif

void
syms_of_curl_api ()
{
  INIT_LRECORD_IMPLEMENTATION (url_handle);

  /* #### These functions will move to the earl module. */
  DEFSUBR (Furl_handle_p);
  DEFSUBR (Furl_handle_live_p);
  DEFSUBR (Furl_handle_host);
  DEFSUBR (Furl_handle_type);

  /* cURL-specific functions. */
  DEFSUBR (Fcurl_make_url_handle);
  DEFSUBR (Fcurl_easy_perform);
  DEFSUBR (Fcurl_easy_setopt);

  /* #### These symbols will move to the earl module. */
  DEFSYMBOL_MULTIWORD_PREDICATE (Qurl_handlep);

  /* cURL-specific symbols. */
  DEFSYMBOL (Qcurl_api);	/* feature symbol */
  DEFSYMBOL (Qcurl);
  DEFSYMBOL (Qlong);
  DEFSYMBOL (Qfunctionpoint);
  DEFSYMBOL (Qobjectpoint);
  defsymbol (&Qoff_t, "off_t");	/* Yes, it IS worth conforming to cURL's
				   spelling of this symbol. */
}

void
vars_of_curl_api ()
{

  Fprovide (Qcurl_api);

  DEFVAR_LISP ("curl-option-hash-table", &Vcurl_option_hash_table /*
Table of options available for `curl-easy-setopt'.
Key are strings naming options.  The option names are taken from enum
CURLoption in <curl/curl.h>.  They are all uppercase, and the "CURLOPT_"
prefix is omitted.
Values are lists containing a type symbol \(one of `long', `objectpoint',
`functionpoint', and `off_t') and an integer, which is the option index.
It is planned to add a list of Lisp types that can be converted to something
that is useful for the option as the 3rd element of the value list.
It is planned to add the leading comments as docstrings, to be the 4th
element of the value list corresponding to each key.
*/ );
  Vcurl_option_hash_table = Qnil;

#ifdef HAVE_SHLIB
  /* Need to initialize cURL if loaded as a module.
     Could do this lazily (when creating the first cURL handle), but since
     this is a module it probably got autoloaded in response to a call to
     `make-curl-handle'. */
  if (curl_global_init (CURL_GLOBAL_XEMACS))
    {
      /* unload_curl_api (); */ 	/* #### can we do this? */
      signal_error (Qio_error, "libcurl initialization failed", Qunbound);
    }
#endif
}

#ifdef HAVE_SHLIB
void
unload_curl_api ()
{
  /* If we create any new types by INIT_LRECORD_IMPLEMENTATION (sample_type),
     then UNDEF_LRECORD_IMPLEMENTATION (sample_type) must appear here.  Also,
     any symbols declared with DEFSYMBOL (Qsample_var) or a variant, must
     have a corresponding unstaticpro_nodump (&Qsample_var) here. */

  /* Shut down libcurl and free internal structures.
     #### Handle return code? Nothing interesting is documented. */
  (void) curl_global_cleanup ();

  unstaticpro_nodump (&Qcurl_api);
  unstaticpro_nodump (&Qcurl);
  unstaticpro_nodump (&Qurl_handlep);
  unstaticpro_nodump (&Qlong);
  unstaticpro_nodump (&Qfunctionpoint);
  unstaticpro_nodump (&Qobjectpoint);
  unstaticpro_nodump (&Qoff_t);
}
#endif
