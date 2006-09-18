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

#include "curl_api.h"		/* include <curl/curl.h> and "earl.h" */

/************************************************************************/
/*			   Module-specific stuff			*/
/************************************************************************/

/* Local references to Lisp symbols */
static Lisp_Object Qcurl_api, Qcurl,
  Qlong, Qfunctionpoint, Qobjectpoint, Qoff_t, Qdouble;

static Lisp_Object Vcurl_option_hash_table, Vcurl_info_hash_table;


/************************************************************************/
/*                   cURL-specific structure handling                   */
/* Contents:								*/
/*   struct earl_transport_implementation curl_transport		*/
/*   allocate_curl_data							*/
/*   finalize_curl_data							*/
/************************************************************************/

static void finalize_curl_data (struct earl_transport_data *data);

static struct earl_transport_implementation curl_transport =
  {
    &finalize_curl_data
  };

static struct curl_data *
allocate_curl_data (void)
{
  struct curl_data *data = xmalloc (sizeof (struct curl_data));
  data->transport_implementation = &curl_transport;
  return data;
}

static void 
finalize_curl_data (struct earl_transport_data *data)
{
  struct curl_data *curl = (struct curl_data *) data;
  if (curl != NULL)
    {
      curl_easy_cleanup (curl->curl_handle);
      curl->curl_handle = NULL;
      xfree (curl, struct curl_data *);
    }
}


/************************************************************************/
/*			    Lisp API functions				*/
/************************************************************************/

/* #### Do something more useful with the error codes! */
#define CHECK_CURL_ERROR(code, index, handle)                         \
  do { if (code)                                                      \
         Fsignal (Qio_error,                                          \
                  list3 (build_ext_string (curl_easy_strerror (code), \
					   Qbinary),                  \
			 index,                                       \
			 handle)); } while (0)

#undef LAZY_INITIALIZATION_IN_REQUEST 
#ifndef LAZY_INITIALIZATION_IN_REQUEST
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
  /* validates URL and CODESYS */
  Lisp_Object session;		/* return value */
  Lisp_Session_Handle *handle;
  {
    /* due to module design, can't call Fmake_session_handle directly */
    Lisp_Object args[3];
    args[0] = intern ("make-session-handle");
    args[1] = url;
    args[2] = codesys;
    session = Ffuncall (3, args);
  }
  handle = XSESSION_HANDLE (session);

  if (NILP (codesys))
    /* #### Quick hack, should be Qnative? */
    codesys = Ffind_coding_system (Qutf_8);
  CHECK_CODING_SYSTEM (codesys);
  handle->coding_system = codesys;

  /* cURL-specific
     #### maybe all of this can be done lazily? */
  /* #### This needs to be wrapped in a condition-case so that cleanup happens
     if anything fails.  Maybe moving plist handling up is enough, but that
     assumes there's no cURL-specific stuff in the plist. */
  /* Do this *before* the plist because later we will be initializing
     curl_handle options from the plist. */
  CHECK_STRING (url);
  handle->url = url;
  handle->transport_data = (struct earl_transport_data *) allocate_curl_data ();
  CURL_DATA (handle)->curl_handle = curl_easy_init ();
  curl_easy_setopt (CURL_DATA (handle)->curl_handle,
		    CURLOPT_URL,
		    /* #### Probably needs to be in the big_ball_of_string. */
		    NEW_LISP_STRING_TO_EXTERNAL_MALLOC(url, codesys));
  /* end cURL-specific stuff */
  
  /* skeleton - currently simply copies the plist
     This has the effect of checking well-formedness, and we may want
     to handle some properties specially. */
  handle->plist = Qnil;
  {
    EXTERNAL_PROPERTY_LIST_LOOP_3(key, value, plist)
      {
	handle->plist =
	  Fcons (key, Fcons (value, handle->plist));
      }
  }

  handle->transport = Qcurl;

  /* return wrap_session_handle (handle); */
  return session;
}
#endif /* LAZY_INITIALIZATION_IN_REQUEST */

DEFUN ("curl-easy-setopt", Fcurl_easy_setopt, 3, 3, 0, /*
Set OPTION to VALUE on curl url-handle HANDLE and return t.
OPTION is a string denoting an option in `curl-option-hash-table'.
VALUE must be of the appropriate type.
HANDLE must be an url-handle object of type `curl'.
A wrapper with some validation for libcurl's `curl_easy_setopt'.
Errors without useful explanations probably mean `curl-option-hash-table'
is corrupt.
*/
       (option, value, handle))
{
  Lisp_Object optdata = Fgethash (option, Vcurl_option_hash_table, Qnil);
  Lisp_Object opttype = Fcar (optdata);
  Lisp_Object optindex = Fcar (Fcdr (optdata));
  CURLoption index;
  CURL *curl;
  CURLcode code;
  Lisp_Session_Handle *session;

  if (NILP (optdata))
    invalid_argument ("unrecognized cURL option", option);
  CHECK_SESSION_HANDLE (handle);
  session = XSESSION_HANDLE (handle);
  if (!EQ (session->transport, Qcurl))
    wtaerror ("handle is not a curl handle", handle);
  curl = CURL_DATA (session)->curl_handle;
  CHECK_INT (optindex);
  index = XINT (optindex);

  if (EQ (opttype, Qlong))
    {
      CHECK_INT (value);
      index += CURLOPTTYPE_LONG;  /* CURLoptions encode type */
      code = curl_easy_setopt (curl, (CURLoption) index, XINT (value));
      CHECK_CURL_ERROR (code, option, handle);
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
      s = NEW_LISP_STRING_TO_EXTERNAL_MALLOC (value, session->coding_system);
      code = curl_easy_setopt (curl, (CURLoption) index, s);
      CHECK_CURL_ERROR (code, option, handle);
      if (index == CURLOPT_URL)
	{
	  session->url = value;
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
      invalid_state ("invalid option type in `curl-option-hash-table'",
		     opttype);
    }
  return Qt;
}

static size_t curl_write_function (void *data, size_t size, size_t nmemb,
				   void *stream)
{
  size_t byte_count = size * nmemb;
  /* #### Gotta be a better way!  Cf. similar code in neon_api.c. */
  Lisp_Object temp;
  XPNTRVAL (temp) = (EMACS_UINT) stream;
  Lstream *s = XLSTREAM (temp);
  /* #### the return code should be checked */
  return (Lstream_write (s, data, byte_count)) ? --byte_count : byte_count;
}

/* #### Should this return the buffer, by analogy to `neon-request-request'? */
DEFUN ("curl-easy-perform", Fcurl_easy_perform, 1, 2, 0, /*
Read from the URL represented by HANDLE into BUFFER at point.
Optional BUFFER defaults to the current buffer.
Returns t.
*/
       (handle, buffer))
{
  CHECK_SESSION_HANDLE (handle);
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();
  CHECK_BUFFER (buffer);
  
  if (EQ (XSESSION_HANDLE (handle)->transport, Qcurl))
    {
      CURL *curl = CURL_DATA (XSESSION_HANDLE (handle))->curl_handle;
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
	  CHECK_CURL_ERROR (code, intern ("perform"), handle);
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

DEFUN ("curl-easy-getinfo", Fcurl_easy_getinfo, 2, 2, 0, /*
Return the value of ATTRIBUTE for HANDLE.
ATTRIBUTE is a string denoting an attribute in `curl-info-hash-table'.
HANDLE must be an url-handle object of transport `curl'.
A wrapper with some validation for libcurl's `curl_easy_getinfo'.
Errors without useful explanations probably mean `curl-info-hash-table'
is corrupt.
String returns are encoded with the `binary' coding system.
*/
       (attribute, handle))
{
  Lisp_Object attdata = Fgethash (attribute, Vcurl_info_hash_table, Qnil);
  Lisp_Object atttype = Fcar (attdata);
  Lisp_Object attindex = Fcar (Fcdr (attdata));
  CURL *curl;
  CURLcode code;
  CURLoption index;
  Lisp_Session_Handle *h;
  Lisp_Object value;

  if (NILP (attdata))
    invalid_argument ("unrecognized cURL attribute", attribute);
  CHECK_SESSION_HANDLE (handle);
  h = XSESSION_HANDLE (handle);
  if (!EQ (h->transport, Qcurl))
    wtaerror ("handle is not a curl handle", handle);
  curl = CURL_DATA (h)->curl_handle;
  CHECK_INT (attindex);
  index = XINT (attindex);

  if (EQ (atttype, Qlong))
    {
      long retval;
      code = curl_easy_getinfo (curl, index, &retval);
      CHECK_CURL_ERROR (code, attribute, handle);
      /* #### can this overflow? */
      value = make_int (retval);
    }
  else if (EQ (atttype, Qstring))
    {
      Extbyte *retval;
      code = curl_easy_getinfo (curl, index, &retval);
      CHECK_CURL_ERROR (code, attribute, handle);
      value = build_ext_string (retval, Qbinary);
    }
  else if (EQ (atttype, Qdouble))
    {
      double retval;
      code = curl_easy_getinfo (curl, index, &retval);
      CHECK_CURL_ERROR (code, attribute, handle);
      value = make_float (retval);
    }
  else if (EQ (atttype, Qlist))
    {
      UNIMPLEMENTED ("curl_api slist attributes");
    }
  else
    {
      invalid_state ("invalid attribute type in `curl-info-hash-table'",
		     atttype);
    }
  return value;  
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

  /* cURL-specific functions. */
  DEFSUBR (Fcurl_make_url_handle);
  DEFSUBR (Fcurl_easy_perform);
  DEFSUBR (Fcurl_easy_setopt);
  DEFSUBR (Fcurl_easy_getinfo);

  /* cURL-specific symbols. */
  DEFSYMBOL (Qcurl_api);	/* feature symbol */
  DEFSYMBOL (Qcurl);
  DEFSYMBOL (Qlong);
  DEFSYMBOL (Qfunctionpoint);
  DEFSYMBOL (Qobjectpoint);
  DEFSYMBOL (Qdouble);
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

  DEFVAR_LISP ("curl-info-hash-table", &Vcurl_info_hash_table /*
Table of attributes accessible via `curl-easy-getinfo'.
Keys are strings naming attributes.  The attribute names are taken from
enum CURLinfo in <curl/curl.h>.  They are all uppercase, and the "CURLINFO_"
prefix is omitted.
Values are lists containing a type symbol (one of `long', `string',
`double', and `list') and an integer, which is the attribute index.
It is planned to add docstrings, to be the 3rd element of the value list
corresponding to each key.
*/ );
  Vcurl_info_hash_table = Qnil;

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
  unstaticpro_nodump (&Qlong);
  unstaticpro_nodump (&Qfunctionpoint);
  unstaticpro_nodump (&Qobjectpoint);
  unstaticpro_nodump (&Qoff_t);
  unstaticpro_nodump (&Qdouble);
}
#endif
