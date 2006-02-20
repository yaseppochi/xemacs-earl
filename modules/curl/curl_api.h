#ifndef include_CURL_API_H
#define include_CURL_API_H
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
 * Creation-Date:	2005-11-23
 * Last-Modified:	2006-01-15
 */

#include <curl/curl.h>

/************************************************************************/
/*			XEmacs-specific cURL stuff			*/
/************************************************************************/

/* Presumably we initialize Windows socket stuff correctly.  If not, I don't
   think it's a good idea to do it in a module.  If you know better, feel
   free to allow CURL_GLOBAL_WIN32 in this macro. */
#define CURL_GLOBAL_XEMACS (CURL_GLOBAL_ALL & ~CURL_GLOBAL_WIN32)

/************************************************************************/
/*				Structures				*/
/************************************************************************/

#ifdef HAVE_EARL
#include "../earl/earl.h"
#else
/* #### This needs to be updated to correspond to earl.h. */
struct Lisp_URL_Handle
{
  struct LCRECORD_HEADER header;
  /* type of the handle; READ-ONLY from Lisp */
  Lisp_Object type;
  /* property list for properties not contained in the cURL handle;
     READ-ONLY from Lisp
     This could be used to maintain state for a handler based on url.el or
     an external process (eg, curl, wget). */
  Lisp_Object property_list;
  /* the coding system used to convery url; READ-ONLY from Lisp */
  Lisp_Object coding_system;
  /* the URL string in external format
     cURL expects the caller to allocate storage and clean it up. */
  Extbyte *url;
  /* the cURL handle used by the libcurl API */
  CURL *curl_handle;
  /* #### UNIMPLEMENTED array of pointers to string data we need to free */
  Dynarr *big_ball_of_strings;
};
typedef struct Lisp_URL_Handle Lisp_URL_Handle;

DECLARE_LRECORD (url_handle, Lisp_URL_Handle);
#define XURL_HANDLE(x) XRECORD (x, url_handle, Lisp_URL_Handle)
#define wrap_url_handle(p) wrap_record (p, url_handle)
#define URL_HANDLEP(x) RECORDP (x, url_handle)
#define CHECK_URL_HANDLE(x) CHECK_RECORD (x, url_handle)
#define CONCHECK_URL_HANDLE(x) CONCHECK_RECORD (x, url_handle)
#endif /* HAVE_EARL */

#endif include_CURL_API_H
