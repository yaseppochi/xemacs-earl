#ifndef include_CURLAPI_H
#define include_CURLAPI_H
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

#include "../earl/earl.h"
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

struct curl_data {
  /* REQUIRED MEMBER: SEE earl.h */
   struct earl_transport_implementation *transport_implementation;

  /* cURL-specific data */
  /* the cURL handle used by the libcurl API */
  CURL *curl_handle;
};

#if 0 /* not needed? */
DECLARE_LRECORD (curl_data, Lisp_Curl_Data);
#define XCURL_DATA(x) XRECORD (x, curl_data, Lisp_Curl_Data)
#define wrap_curl_data(p) wrap_record (p, curl_data)
#define CURL_DATAP(x) RECORDP (x, curl_data)
#define CHECK_CURL_DATA(x) CHECK_RECORD (x, curl_data)
#define CONCHECK_CURL_DATA(x) CONCHECK_RECORD (x, curl_data)
#endif

#define CURL_DATA(handle) ((struct curl_data *) (handle->transport_data))

#endif /* include_CURLAPI_H */
