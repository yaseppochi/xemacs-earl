;;; curl.el --- utilities for use with the curl_api module

;; Copyright (C) 2005  Stephen J. Turnbull  <stephen@xemacs.org>
;; Copyright (C) 1998 - 2005, Daniel Stenberg, <daniel@haxx.se>, et al.

;; All rights reserved, except as expressly indicated below.

;; This program is not considered part of XEmacs.

;; You may use, copy, modify, and distribute this software under the terms
;; of the GNU General Public License, version 2 or later at your option.

;; Author:		Stephen J. Turnbull <stephen@xemacs.org>
;; Creation-Date:	2005-11-24

(require 'curl-api "curl/curl_api")

;; Version information from the cURL I built the first version from.
;; This should be fixed to get the information at build time.
(put 'curl-api 'libcurl-version "7.15.0")
(put 'curl-api 'libcurl_version_major 7)
(put 'curl-api 'libcurl_version_minor 15)
(put 'curl-api 'libcurl_version_patch 0)
;; This is the numeric version of the libcurl version number, meant for easier
;; parsing and comparions by programs. The LIBCURL_VERSION_NUM define will
;; always follow this syntax:

;;       0xXXYYZZ

;; Where XX, YY and ZZ are the main version, release and patch numbers in
;; hexadecimal (using 8 bits each). All three numbers are always represented
;; using two digits.  1.2 would appear as "0x010200" while version 9.11.7
;; appears as "0x090b07".

;; This 6-digit (24 bits) hexadecimal number does not show pre-release number,
;; and it is always a greater number in a more recent release. It makes
;; comparisons with greater than and less than work.
;; #define LIBCURL_VERSION_NUM ((LIBCURL_VERSION_MAJOR << 16) | \
;;                              (LIBCURL_VERSION_MINOR << 8) | \
;;                              LIBCURL_VERSION_PATCH)

;; **** various includes and system-specific conditional code omitted ****

;; struct curl_httppost {
;;   struct curl_httppost *next;       /* next entry in the list */
;;   char *name;                       /* pointer to allocated name */
;;   long namelength;                  /* length of name length */
;;   char *contents;                   /* pointer to allocated data contents */
;;   long contentslength;              /* length of contents field */
;;   char *buffer;                     /* pointer to allocated buffer contents */
;;   long bufferlength;                /* length of buffer field */
;;   char *contenttype;                /* Content-Type */
;;   struct curl_slist* contentheader; /* list of extra headers for this form */
;;   struct curl_httppost *more;       /* if one field name has more than one
;;                                        file, this link should link to following
;;                                        files */
;;   long flags;                       /* as defined below */
;; #define HTTPPOST_FILENAME (1<<0)    /* specified content is a file name */
;; #define HTTPPOST_READFILE (1<<1)    /* specified content is a file name */
;; #define HTTPPOST_PTRNAME (1<<2)     /* name is only stored pointer
;;                                        do not free in formfree */
;; #define HTTPPOST_PTRCONTENTS (1<<3) /* contents is only stored pointer
;;                                        do not free in formfree */
;; #define HTTPPOST_BUFFER (1<<4)      /* upload file from buffer */
;; #define HTTPPOST_PTRBUFFER (1<<5)   /* upload file from pointer contents */
;; 
;;   char *showfilename;               /* The file name to show. If not set, the
;;                                        actual file name will be used (if this
;;                                        is a file part) */
;; };
;; 
;; typedef int (*curl_progress_callback)(void *clientp,
;;                                       double dltotal,
;;                                       double dlnow,
;;                                       double ultotal,
;;                                       double ulnow);
;; 
;;   /* Tests have proven that 20K is a very bad buffer size for uploads on
;;      Windows, while 16K for some odd reason performed a lot better. */
;; #define CURL_MAX_WRITE_SIZE 16384
;; 
;; typedef size_t (*curl_write_callback)(char *buffer,
;;                                       size_t size,
;;                                       size_t nitems,
;;                                       void *outstream);
;; 
;; /* This is a return code for the read callback that, when returned, will
;;    signal libcurl to immediately abort the current transfer. */
;; #define CURL_READFUNC_ABORT 0x10000000
;; typedef size_t (*curl_read_callback)(char *buffer,
;;                                       size_t size,
;;                                       size_t nitems,
;;                                       void *instream);
;; 
;; 
;; #ifndef CURL_NO_OLDIES
;;   /* not used since 7.10.8, will be removed in a future release */
;; typedef int (*curl_passwd_callback)(void *clientp,
;;                                     const char *prompt,
;;                                     char *buffer,
;;                                     int buflen);
;; #endif
;; 
;; typedef enum {
;;   CURLIOE_OK,            /* I/O operation successful */
;;   CURLIOE_UNKNOWNCMD,    /* command was unknown to callback */
;;   CURLIOE_FAILRESTART,   /* failed to restart the read */
;;   CURLIOE_LAST           /* never use */
;; } curlioerr;
;; 
;; typedef enum  {
;;   CURLIOCMD_NOP,         /* no operation */
;;   CURLIOCMD_RESTARTREAD, /* restart the read stream from start */
;;   CURLIOCMD_LAST         /* never use */
;; } curliocmd;
;; 
;; typedef curlioerr (*curl_ioctl_callback)(CURL *handle,
;;                                          int cmd,
;;                                          void *clientp);
;; 
;; /*
;;  * The following typedef's are signatures of malloc, free, realloc, strdup and
;;  * calloc respectively.  Function pointers of these types can be passed to the
;;  * curl_global_init_mem() function to set user defined memory management
;;  * callback routines.
;;  */
;; typedef void *(*curl_malloc_callback)(size_t size);
;; typedef void (*curl_free_callback)(void *ptr);
;; typedef void *(*curl_realloc_callback)(void *ptr, size_t size);
;; typedef char *(*curl_strdup_callback)(const char *str);
;; typedef void *(*curl_calloc_callback)(size_t nmemb, size_t size);
;; 
;; /* the kind of data that is passed to information_callback*/
;; typedef enum {
;;   CURLINFO_TEXT = 0,
;;   CURLINFO_HEADER_IN,    /* 1 */
;;   CURLINFO_HEADER_OUT,   /* 2 */
;;   CURLINFO_DATA_IN,      /* 3 */
;;   CURLINFO_DATA_OUT,     /* 4 */
;;   CURLINFO_SSL_DATA_IN,  /* 5 */
;;   CURLINFO_SSL_DATA_OUT, /* 6 */
;;   CURLINFO_END
;; } curl_infotype;
;; 
;; typedef int (*curl_debug_callback)
;;        (CURL *handle,      /* the handle/transfer this concerns */
;;         curl_infotype type, /* what kind of data */
;;         char *data,        /* points to the data */
;;         size_t size,       /* size of the data pointed to */
;;         void *userptr);    /* whatever the user please */
;; 
;; /* All possible error codes from all sorts of curl functions. Future versions
;;    may return other values, stay prepared.
;; 
;;    Always add new return codes last. Never *EVER* remove any. The return
;;    codes must remain the same!
;;  */
;; 
;; typedef enum {
;;   CURLE_OK = 0,
;;   CURLE_UNSUPPORTED_PROTOCOL,    /* 1 */
;;   CURLE_FAILED_INIT,             /* 2 */
;;   CURLE_URL_MALFORMAT,           /* 3 */
;;   CURLE_URL_MALFORMAT_USER,      /* 4 (NOT USED) */
;;   CURLE_COULDNT_RESOLVE_PROXY,   /* 5 */
;;   CURLE_COULDNT_RESOLVE_HOST,    /* 6 */
;;   CURLE_COULDNT_CONNECT,         /* 7 */
;;   CURLE_FTP_WEIRD_SERVER_REPLY,  /* 8 */
;;   CURLE_FTP_ACCESS_DENIED,       /* 9 a service was denied by the FTP server
;;                                     due to lack of access - when login fails
;;                                     this is not returned. */
;;   CURLE_FTP_USER_PASSWORD_INCORRECT, /* 10 */
;;   CURLE_FTP_WEIRD_PASS_REPLY,    /* 11 */
;;   CURLE_FTP_WEIRD_USER_REPLY,    /* 12 */
;;   CURLE_FTP_WEIRD_PASV_REPLY,    /* 13 */
;;   CURLE_FTP_WEIRD_227_FORMAT,    /* 14 */
;;   CURLE_FTP_CANT_GET_HOST,       /* 15 */
;;   CURLE_FTP_CANT_RECONNECT,      /* 16 */
;;   CURLE_FTP_COULDNT_SET_BINARY,  /* 17 */
;;   CURLE_PARTIAL_FILE,            /* 18 */
;;   CURLE_FTP_COULDNT_RETR_FILE,   /* 19 */
;;   CURLE_FTP_WRITE_ERROR,         /* 20 */
;;   CURLE_FTP_QUOTE_ERROR,         /* 21 */
;;   CURLE_HTTP_RETURNED_ERROR,     /* 22 */
;;   CURLE_WRITE_ERROR,             /* 23 */
;;   CURLE_MALFORMAT_USER,          /* 24 - NOT USED */
;;   CURLE_FTP_COULDNT_STOR_FILE,   /* 25 - failed FTP upload */
;;   CURLE_READ_ERROR,              /* 26 - could open/read from file */
;;   CURLE_OUT_OF_MEMORY,           /* 27 */
;;   CURLE_OPERATION_TIMEOUTED,     /* 28 - the timeout time was reached */
;;   CURLE_FTP_COULDNT_SET_ASCII,   /* 29 - TYPE A failed */
;;   CURLE_FTP_PORT_FAILED,         /* 30 - FTP PORT operation failed */
;;   CURLE_FTP_COULDNT_USE_REST,    /* 31 - the REST command failed */
;;   CURLE_FTP_COULDNT_GET_SIZE,    /* 32 - the SIZE command failed */
;;   CURLE_HTTP_RANGE_ERROR,        /* 33 - RANGE "command" didn't work */
;;   CURLE_HTTP_POST_ERROR,         /* 34 */
;;   CURLE_SSL_CONNECT_ERROR,       /* 35 - wrong when connecting with SSL */
;;   CURLE_BAD_DOWNLOAD_RESUME,     /* 36 - couldn't resume download */
;;   CURLE_FILE_COULDNT_READ_FILE,  /* 37 */
;;   CURLE_LDAP_CANNOT_BIND,        /* 38 */
;;   CURLE_LDAP_SEARCH_FAILED,      /* 39 */
;;   CURLE_LIBRARY_NOT_FOUND,       /* 40 */
;;   CURLE_FUNCTION_NOT_FOUND,      /* 41 */
;;   CURLE_ABORTED_BY_CALLBACK,     /* 42 */
;;   CURLE_BAD_FUNCTION_ARGUMENT,   /* 43 */
;;   CURLE_BAD_CALLING_ORDER,       /* 44 - NOT USED */
;;   CURLE_INTERFACE_FAILED,        /* 45 - CURLOPT_INTERFACE failed */
;;   CURLE_BAD_PASSWORD_ENTERED,    /* 46 - NOT USED */
;;   CURLE_TOO_MANY_REDIRECTS ,     /* 47 - catch endless re-direct loops */
;;   CURLE_UNKNOWN_TELNET_OPTION,   /* 48 - User specified an unknown option */
;;   CURLE_TELNET_OPTION_SYNTAX ,   /* 49 - Malformed telnet option */
;;   CURLE_OBSOLETE,                /* 50 - NOT USED */
;;   CURLE_SSL_PEER_CERTIFICATE,    /* 51 - peer's certificate wasn't ok */
;;   CURLE_GOT_NOTHING,             /* 52 - when this is a specific error */
;;   CURLE_SSL_ENGINE_NOTFOUND,     /* 53 - SSL crypto engine not found */
;;   CURLE_SSL_ENGINE_SETFAILED,    /* 54 - can not set SSL crypto engine as
;;                                     default */
;;   CURLE_SEND_ERROR,              /* 55 - failed sending network data */
;;   CURLE_RECV_ERROR,              /* 56 - failure in receiving network data */
;;   CURLE_SHARE_IN_USE,            /* 57 - share is in use */
;;   CURLE_SSL_CERTPROBLEM,         /* 58 - problem with the local certificate */
;;   CURLE_SSL_CIPHER,              /* 59 - couldn't use specified cipher */
;;   CURLE_SSL_CACERT,              /* 60 - problem with the CA cert (path?) */
;;   CURLE_BAD_CONTENT_ENCODING,    /* 61 - Unrecognized transfer encoding */
;;   CURLE_LDAP_INVALID_URL,        /* 62 - Invalid LDAP URL */
;;   CURLE_FILESIZE_EXCEEDED,       /* 63 - Maximum file size exceeded */
;;   CURLE_FTP_SSL_FAILED,          /* 64 - Requested FTP SSL level failed */
;;   CURLE_SEND_FAIL_REWIND,        /* 65 - Sending the data requires a rewind
;;                                     that failed */
;;   CURLE_SSL_ENGINE_INITFAILED,   /* 66 - failed to initialise ENGINE */
;;   CURLE_LOGIN_DENIED,            /* 67 - user, password or similar was not
;;                                     accepted and we failed to login */
;;   CURLE_TFTP_NOTFOUND,           /* 68 - file not found on server */
;;   CURLE_TFTP_PERM,               /* 69 - permission problem on server */
;;   CURLE_TFTP_DISKFULL,           /* 70 - out of disk space on server */
;;   CURLE_TFTP_ILLEGAL,            /* 71 - Illegal TFTP operation */
;;   CURLE_TFTP_UNKNOWNID,          /* 72 - Unknown transfer ID */
;;   CURLE_TFTP_EXISTS,             /* 73 - File already exists */
;;   CURLE_TFTP_NOSUCHUSER,         /* 74 - No such user */
;;   CURL_LAST /* never use! */
;; } CURLcode;
;; 
;; typedef CURLcode (*curl_ssl_ctx_callback)(CURL *curl,    /* easy handle */
;;                                           void *ssl_ctx, /* actually an
;;                                                             OpenSSL SSL_CTX */
;;                                           void *userptr);
;; 
;; /* Make a spelling correction for the operation timed-out define */
;; #define CURLE_OPERATION_TIMEDOUT CURLE_OPERATION_TIMEOUTED
;; 
;; #ifndef CURL_NO_OLDIES /* define this to test if your app builds with all
;;                           the obsolete stuff removed! */
;; /* backwards compatibility with older names */
;; #define CURLE_HTTP_NOT_FOUND CURLE_HTTP_RETURNED_ERROR
;; #define CURLE_HTTP_PORT_FAILED CURLE_INTERFACE_FAILED
;; #endif
;; 
;; typedef enum {
;;   CURLPROXY_HTTP = 0,
;;   CURLPROXY_SOCKS4 = 4,
;;   CURLPROXY_SOCKS5 = 5
;; } curl_proxytype;
;; 
;; #define CURLAUTH_NONE         0       /* nothing */
;; #define CURLAUTH_BASIC        (1<<0)  /* Basic (default) */
;; #define CURLAUTH_DIGEST       (1<<1)  /* Digest */
;; #define CURLAUTH_GSSNEGOTIATE (1<<2)  /* GSS-Negotiate */
;; #define CURLAUTH_NTLM         (1<<3)  /* NTLM */
;; #define CURLAUTH_ANY ~0               /* all types set */
;; #define CURLAUTH_ANYSAFE (~CURLAUTH_BASIC)
;; 
;; #ifndef CURL_NO_OLDIES /* define this to test if your app builds with all
;;                           the obsolete stuff removed! */
;; /* this was the error code 50 in 7.7.3 and a few earlier versions, this
;;    is no longer used by libcurl but is instead #defined here only to not
;;    make programs break */
;; #define CURLE_ALREADY_COMPLETE 99999
;; 
;; /* These are just to make older programs not break: */
;; #define CURLE_FTP_PARTIAL_FILE CURLE_PARTIAL_FILE
;; #define CURLE_FTP_BAD_DOWNLOAD_RESUME CURLE_BAD_DOWNLOAD_RESUME
;; #endif
;; 
;; #define CURL_ERROR_SIZE 256
;; 
;; /* parameter for the CURLOPT_FTP_SSL option */
;; typedef enum {
;;   CURLFTPSSL_NONE,    /* do not attempt to use SSL */
;;   CURLFTPSSL_TRY,     /* try using SSL, proceed anyway otherwise */
;;   CURLFTPSSL_CONTROL, /* SSL for the control connection or fail */
;;   CURLFTPSSL_ALL,     /* SSL for all communication or fail */
;;   CURLFTPSSL_LAST     /* not an option, never use */
;; } curl_ftpssl;
;; 
;; /* parameter for the CURLOPT_FTPSSLAUTH option */
;; typedef enum {
;;   CURLFTPAUTH_DEFAULT, /* let libcurl decide */
;;   CURLFTPAUTH_SSL,     /* use "AUTH SSL" */
;;   CURLFTPAUTH_TLS,     /* use "AUTH TLS" */
;;   CURLFTPAUTH_LAST /* not an option, never use */
;; } curl_ftpauth;
;; 
;; /* long may be 32 or 64 bits, but we should never depend on anything else
;;    but 32 */
;; #define CURLOPTTYPE_LONG          0
;; #define CURLOPTTYPE_OBJECTPOINT   10000
;; #define CURLOPTTYPE_FUNCTIONPOINT 20000
;; #define CURLOPTTYPE_OFF_T         30000
;; 
;; /* name is uppercase CURLOPT_<name>,
;;    type is one of the defined CURLOPTTYPE_<type>
;;    number is unique identifier */
;; #ifdef CINIT
;; #undef CINIT
;; #endif
;; /*
;;  * Figure out if we can use the ## operator, which is supported by ISO/ANSI C
;;  * and C++. Some compilers support it without setting __STDC__ or __cplusplus
;;  * so we need to carefully check for them too. We don't use configure-checks
;;  * for these since we want these headers to remain generic and working for all
;;  * platforms.
;;  */
;; #if defined(__STDC__) || defined(_MSC_VER) || defined(__cplusplus) || \
;;   defined(__HP_aCC) || defined(__BORLANDC__) || defined(__LCC__)
;;   /* This compiler is believed to have an ISO compatible preprocessor */
;; #define CURL_ISOCPP
;; #else
;;   /* This compiler is believed NOT to have an ISO compatible preprocessor */
;; #undef CURL_ISOCPP
;; #endif
;; 
;; #ifdef CURL_ISOCPP
;; #define CINIT(name,type,number) CURLOPT_ ## name = CURLOPTTYPE_ ## type + number
;; #else
;; /* The macro "##" is ISO C, we assume pre-ISO C doesn't support it. */
;; #define LONG          CURLOPTTYPE_LONG
;; #define OBJECTPOINT   CURLOPTTYPE_OBJECTPOINT
;; #define FUNCTIONPOINT CURLOPTTYPE_FUNCTIONPOINT
;; #define OFF_T         CURLOPTTYPE_OFF_T
;; #define CINIT(name,type,number) CURLOPT_/**/name = type + number
;; #endif
;;
;; These are the options that can be used in curl-easy-setopt.

;; #### Probably should adapt this to the method below for
;; `curl-info-hash-table'.
;; Actually, probably should make the names symbols and put them in C,
;; but that's another story.

(unless (and (boundp 'curl-option-hash-table)
	     (hash-table-p curl-option-hash-table))
  (setq curl-option-hash-table (make-hash-table :test #'equal :size 150))

  ;; typedef enum {
  ;; This is the FILE * or void * the regular output should be written to.
  (puthash "FILE" '(objectpoint 1) curl-option-hash-table)
  ;; The full URL to get/put
  (puthash "URL" '(objectpoint 2) curl-option-hash-table)
  ;; Port number to connect to, if other than default.
  (puthash "PORT" '(long 3) curl-option-hash-table)
  ;; Name of proxy to use.
  (puthash "PROXY" '(objectpoint 4) curl-option-hash-table)
  ;; "name:password" to use when fetching.
  (puthash "USERPWD" '(objectpoint 5) curl-option-hash-table)
  ;; "name:password" to use with proxy.
  (puthash "PROXYUSERPWD" '(objectpoint 6) curl-option-hash-table)
  ;; Range to get, specified as an ASCII string.
  (puthash "RANGE" '(objectpoint 7) curl-option-hash-table)
  ;; not used
  ;; Specified file stream to upload from (use as input):
  (puthash "INFILE" '(objectpoint 9) curl-option-hash-table)
  ;; Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
  ;; bytes big. If this is not used, error messages go to stderr instead:
  (puthash "ERRORBUFFER" '(objectpoint 10) curl-option-hash-table)
  ;; Function that will be called to store the output (instead of fwrite). The
  ;; parameters will use fwrite() syntax, make sure to follow them.
  (puthash "WRITEFUNCTION" '(functionpoint 11) curl-option-hash-table)
  ;; Function that will be called to read the input (instead of fread). The
  ;; parameters will use fread() syntax, make sure to follow them.
  (puthash "READFUNCTION" '(functionpoint 12) curl-option-hash-table)
  ;; Time-out the read operation after this amount of seconds
  (puthash "TIMEOUT" '(long 13) curl-option-hash-table)
  ;; If the CURLOPT_INFILE is used, this can be used to inform libcurl about
  ;; how large the file being sent really is. That allows better error
  ;; checking and better verifies that the upload was succcessful. -1 means
  ;; unknown size.
  ;;
  ;; For large file support, there is also a _LARGE version of the key
  ;; which takes an off_t type, allowing platforms with larger off_t
  ;; sizes to handle larger files.  See below for INFILESIZE_LARGE.
  (puthash "INFILESIZE" '(long 14) curl-option-hash-table)
  ;; POST input fields.
  (puthash "POSTFIELDS" '(objectpoint 15) curl-option-hash-table)
  ;; Set the referer page (needed by some CGIs)
  (puthash "REFERER" '(objectpoint 16) curl-option-hash-table)
  ;; Set the FTP PORT string (interface name, named or numerical IP address)
  ;; Use i.e '-' to use default address.
  (puthash "FTPPORT" '(objectpoint 17) curl-option-hash-table)
  ;; Set the User-Agent string (examined by some CGIs)
  (puthash "USERAGENT" '(objectpoint 18) curl-option-hash-table)
  ;; If the download receives less than "low speed limit" bytes/second
  ;; during "low speed time" seconds, the operations is aborted.
  ;; You could i.e if you have a pretty high speed connection, abort if
  ;; it is less than 2000 bytes/sec during 20 seconds.
  ;;
  ;; Set the "low speed limit"
  (puthash "LOW_SPEED_LIMIT" '(long  19) curl-option-hash-table)
  ;; Set the "low speed time"
  (puthash "LOW_SPEED_TIME" '(long 20) curl-option-hash-table)
  ;; Set the continuation offset.
  ;;
  ;; Note there is also a _LARGE version of this key which uses
  ;; off_t types, allowing for large file offsets on platforms which
  ;; use larger-than-32-bit off_t's.  Look below for RESUME_FROM_LARGE.
  (puthash "RESUME_FROM" '(long 21) curl-option-hash-table)
  ;; Set cookie in request:
  (puthash "COOKIE" '(objectpoint 22) curl-option-hash-table)
  ;; This points to a linked list of headers, struct curl_slist kind
  (puthash "HTTPHEADER" '(objectpoint 23) curl-option-hash-table)
  ;; This points to a linked list of post entries, struct HttpPost
  (puthash "HTTPPOST" '(objectpoint 24) curl-option-hash-table)
  ;; name of the file keeping your private SSL-certificate
  (puthash "SSLCERT" '(objectpoint 25) curl-option-hash-table)
  ;; password for the SSL-private key, keep this for compatibility
  (puthash "SSLCERTPASSWD" '(objectpoint 26) curl-option-hash-table)
  ;; password for the SSL private key
  (puthash "SSLKEYPASSWD" '(objectpoint 26) curl-option-hash-table)
  ;; send TYPE parameter?
  (puthash "CRLF" '(long 27) curl-option-hash-table)
  ;; send linked-list of QUOTE commands
  (puthash "QUOTE" '(objectpoint 28) curl-option-hash-table)
  ;; send FILE * or void * to store headers to, if you use a callback it
  ;; is simply passed to the callback unmodified
  (puthash "WRITEHEADER" '(objectpoint 29) curl-option-hash-table)
  ;; point to a file to read the initial cookies from, also enables
  ;; "cookie awareness"
  (puthash "COOKIEFILE" '(objectpoint 31) curl-option-hash-table)
  ;; What version to specifly try to use.
  ;; See CURL_SSLVERSION defines below.
  (puthash "SSLVERSION" '(long 32) curl-option-hash-table)
  ;; What kind of HTTP time condition to use, see defines
  (puthash "TIMECONDITION" '(long 33) curl-option-hash-table)
  ;; Time to use with the above condition. Specified in number of seconds
  ;; since 1 Jan 1970
  (puthash "TIMEVALUE" '(long 34) curl-option-hash-table)
  ;; 35 = OBSOLETE
  ;; Custom request, for customizing the get command like
  ;; HTTP: DELETE, TRACE and others
  ;; FTP: to use a different list command
  (puthash "CUSTOMREQUEST" '(objectpoint 36) curl-option-hash-table)
  ;; HTTP request, for odd commands like DELETE, TRACE and others
  (puthash "STDERR" '(objectpoint 37) curl-option-hash-table)
  ;; 38 is not used
  ;; send linked-list of post-transfer QUOTE commands
  (puthash "POSTQUOTE" '(objectpoint 39) curl-option-hash-table)
  ;; Pass a pointer to string of the output using full variable-replacement
  ;; as described elsewhere.
  (puthash "WRITEINFO" '(objectpoint 40) curl-option-hash-table)
  ;; talk a lot
  (puthash "VERBOSE" '(long 41) curl-option-hash-table)
  ;; throw the header out too
  (puthash "HEADER" '(long 42) curl-option-hash-table)
  ;; shut off the progress meter
  (puthash "NOPROGRESS" '(long 43) curl-option-hash-table)
  ;; use HEAD to get http document
  (puthash "NOBODY" '(long 44) curl-option-hash-table)
  ;; no output on http error codes >= 300
  (puthash "FAILONERROR" '(long 45) curl-option-hash-table)
  ;; this is an upload
  (puthash "UPLOAD" '(long 46) curl-option-hash-table)
  ;; HTTP POST method
  (puthash "POST" '(long 47) curl-option-hash-table)
  ;; Use NLST when listing ftp dir
  (puthash "FTPLISTONLY" '(long 48) curl-option-hash-table)
  ;; Append instead of overwrite on upload!
  (puthash "FTPAPPEND" '(long 50) curl-option-hash-table)
  ;; Specify whether to read the user+password from the .netrc or the URL.
  ;; This must be one of the CURL_NETRC_* enums below.
  (puthash "NETRC" '(long 51) curl-option-hash-table)
  ;; use Location: Luke!
  (puthash "FOLLOWLOCATION" '(LONG 52) curl-option-hash-table)
  ;;transfer data in text/ASCII format
  (puthash "TRANSFERTEXT" '(LONG 53) curl-option-hash-table)
  ;; HTTP PUT
  (puthash "PUT" '(LONG 54) curl-option-hash-table)
  ;; 55 = OBSOLETE
  ;; Function that will be called instead of the internal progress display
  ;; function. This function should be defined as the curl_progress_callback
  ;; prototype defines.
  (puthash "PROGRESSFUNCTION" '(functionpoint 56) curl-option-hash-table)
  ;; Data passed to the progress callback
  (puthash "PROGRESSDATA" '(objectpoint 57) curl-option-hash-table)
  ;; We want the referer field set automatically when following locations
  (puthash "AUTOREFERER" '(long 58) curl-option-hash-table)
  ;; Port of the proxy, can be set in the proxy string as well with:
  ;; "[host]:[port]"
  (puthash "PROXYPORT" '(long 59) curl-option-hash-table)
  ;; size of the POST input data, if strlen() is not good to use
  (puthash "POSTFIELDSIZE" '(long 60) curl-option-hash-table)
  ;; tunnel non-http operations through a HTTP proxy
  (puthash "HTTPPROXYTUNNEL" '(long 61) curl-option-hash-table)
  ;; Set the interface string to use as outgoing network interface
  (puthash "INTERFACE" '(objectpoint 62) curl-option-hash-table)
  ;; Set the krb4 security level, this also enables krb4 awareness.  This is a
  ;; string, 'clear', 'safe', 'confidential' or 'private'.  If the string is
  ;; set but doesn't match one of these, 'private' will be used. 
  (puthash "KRB4LEVEL" '(objectpoint 63) curl-option-hash-table)
  ;; Set if we should verify the peer in ssl handshake, set 1 to verify.
  (puthash "SSL_VERIFYPEER" '(long 64) curl-option-hash-table)
  ;; The CApath or CAfile used to validate the peer certificate
  ;; this option is used only if SSL_VERIFYPEER is true
  (puthash "CAINFO" '(objectpoint 65) curl-option-hash-table)
  ;; 66 = OBSOLETE
  ;; 67 = OBSOLETE
  ;; Maximum number of http redirects to follow
  (puthash "MAXREDIRS" '(long 68) curl-option-hash-table)
  ;; Pass a long set to 1 to get the date of the requested document (if
  ;; possible)! Pass a zero to shut it off.
  (puthash "FILETIME" '(long 69) curl-option-hash-table)
  ;; This points to a linked list of telnet options
  (puthash "TELNETOPTIONS" '(objectpoint 70) curl-option-hash-table)
  ;; Max amount of cached alive connections
  (puthash "MAXCONNECTS" '(long 71) curl-option-hash-table)
  ;; What policy to use when closing connections when the cache is filled
  ;; up
  (puthash "CLOSEPOLICY" '(long 72) curl-option-hash-table)
  ;; 73 = OBSOLETE
  ;; Set to explicitly use a new connection for the upcoming transfer.
  ;; Do not use this unless you're absolutely sure of this, as it makes the
  ;; operation slower and is less friendly for the network.
  (puthash "FRESH_CONNECT" '(long 74) curl-option-hash-table)
  ;; Set to explicitly forbid the upcoming transfer's connection to be re-used
  ;; when done. Do not use this unless you're absolutely sure of this, as it
  ;; makes the operation slower and is less friendly for the network.
  (puthash "FORBID_REUSE" '(long 75) curl-option-hash-table)
  ;; Set to a file name that contains random data for libcurl to use to
  ;; seed the random engine when doing SSL connects.
  (puthash "RANDOM_FILE" '(objectpoint 76) curl-option-hash-table)
  ;; Set to the Entropy Gathering Daemon socket pathname
  (puthash "EGDSOCKET" '(objectpoint 77) curl-option-hash-table)
  ;; Time-out connect operations after this amount of seconds, if connects
  ;; are OK within this time, then fine... This only aborts the connect
  ;; phase. [Only works on unix-style/SIGALRM operating systems]
  (puthash "CONNECTTIMEOUT" '(long 78) curl-option-hash-table)
  ;; Function that will be called to store headers (instead of fwrite). The
  ;; parameters will use fwrite() syntax, make sure to follow them.
  (puthash "HEADERFUNCTION" '(functionpoint 79) curl-option-hash-table)
  ;; Set this to force the HTTP request to get back to GET. Only really usable
  ;; if POST, PUT or a custom request have been used first.
  (puthash "HTTPGET" '(long 80) curl-option-hash-table)
  ;; Set if we should verify the Common name from the peer certificate in ssl
  ;; handshake, set 1 to check existence, 2 to ensure that it matches the
  ;; provided hostname.
  (puthash "SSL_VERIFYHOST" '(long 81) curl-option-hash-table)
  ;; Specify which file name to write all known cookies in after completed
  ;; operation. Set file name to "-" (dash) to make it go to stdout.
  (puthash "COOKIEJAR" '(objectpoint 82) curl-option-hash-table)
  ;; Specify which SSL ciphers to use
  (puthash "SSL_CIPHER_LIST" '(objectpoint 83) curl-option-hash-table)
  ;; Specify which HTTP version to use! This must be set to one of the
  ;; CURL_HTTP_VERSION* enums set below.
  (puthash "HTTP_VERSION" '(long 84) curl-option-hash-table)
  ;; Specificly switch on or off the FTP engine's use of the EPSV command. By
  ;; default, that one will always be attempted before the more traditional
  ;; PASV command.
  (puthash "FTP_USE_EPSV" '(long 85) curl-option-hash-table)
  ;; type of the file keeping your SSL-certificate ("DER", "PEM", "ENG")
  (puthash "SSLCERTTYPE" '(objectpoint 86) curl-option-hash-table)
  ;; name of the file keeping your private SSL-key
  (puthash "SSLKEY" '(objectpoint 87) curl-option-hash-table)
  ;; type of the file keeping your private SSL-key ("DER", "PEM", "ENG")
  (puthash "SSLKEYTYPE" '(objectpoint 88) curl-option-hash-table)
  ;; crypto engine for the SSL-sub system
  (puthash "SSLENGINE" '(objectpoint 89) curl-option-hash-table)
  ;; set the crypto engine for the SSL-sub system as default
  ;; the param has no meaning...
  (puthash "SSLENGINE_DEFAULT" '(long 90) curl-option-hash-table)
  ;; Non-zero value means to use the global dns cache
  (puthash "DNS_USE_GLOBAL_CACHE" '(long 91) curl-option-hash-table)
  ;; DNS cache timeout
  (puthash "DNS_CACHE_TIMEOUT" '(long 92) curl-option-hash-table)
  ;; send linked-list of pre-transfer QUOTE commands (Wesley Laxton)*/
  (puthash "PREQUOTE" '(objectpoint 93) curl-option-hash-table)
  ;; set the debug function
  (puthash "DEBUGFUNCTION" '(functionpoint 94) curl-option-hash-table)
  ;; set the data for the debug function
  (puthash "DEBUGDATA" '(objectpoint 95) curl-option-hash-table)
  ;; mark this as start of a cookie session
  (puthash "COOKIESESSION" '(long 96) curl-option-hash-table)
  ;; The CApath directory used to validate the peer certificate
  ;; this option is used only if SSL_VERIFYPEER is true
  (puthash "CAPATH" '(objectpoint 97) curl-option-hash-table)
  ;; Instruct libcurl to use a smaller receive buffer
  (puthash "BUFFERSIZE" '(long 98) curl-option-hash-table)
  ;; Instruct libcurl to not use any signal/alarm handlers, even when using
  ;; timeouts. This option is useful for multi-threaded applications.
  ;; See libcurl-the-guide for more background information.
  (puthash "NOSIGNAL" '(long 99) curl-option-hash-table)
  ;; Provide a CURLShare for mutexing non-ts data
  (puthash "SHARE" '(objectpoint 100) curl-option-hash-table)
  ;; indicates type of proxy. accepted values are CURLPROXY_HTTP (default),
  ;; CURLPROXY_SOCKS4 and CURLPROXY_SOCKS5.
  (puthash "PROXYTYPE" '(long 101) curl-option-hash-table)
  ;; Set the Accept-Encoding string. Use this to tell a server you would like
  ;; the response to be compressed.
  (puthash "ENCODING" '(objectpoint 102) curl-option-hash-table)
  ;; Set pointer to private data
  (puthash "PRIVATE" '(objectpoint 103) curl-option-hash-table)
  ;; Set aliases for HTTP 200 in the HTTP Response header
  (puthash "HTTP200ALIASES" '(objectpoint 104) curl-option-hash-table)
  ;; Continue to send authentication (user+password) when following locations,
  ;; even when hostname changed. This can potentionally send off the name
  ;; and password to whatever host the server decides.
  (puthash "UNRESTRICTED_AUTH" '(long 105) curl-option-hash-table)
  ;; Specificly switch on or off the FTP engine's use of the EPRT command ( it
  ;; also disables the LPRT attempt). By default, those ones will always be
  ;; attempted before the good old traditional PORT command.
  (puthash "FTP_USE_EPRT" '(long 106) curl-option-hash-table)
  ;; Set this to a bitmask value to enable the particular authentications
  ;; methods you like. Use this in combination with CURLOPT_USERPWD.
  ;; Note that setting multiple bits may cause extra network round-trips.
  (puthash "HTTPAUTH" '(long 107) curl-option-hash-table)
  ;; Set the ssl context callback function, currently only for OpenSSL ssl_ctx
  ;; in second argument. The function must be matching the
  ;; curl_ssl_ctx_callback proto.
  (puthash "SSL_CTX_FUNCTION" '(functionpoint 108) curl-option-hash-table)
  ;; Set the userdata for the ssl context callback function's third
  ;; argument
  (puthash "SSL_CTX_DATA" '(objectpoint 109) curl-option-hash-table)
  ;; FTP Option that causes missing dirs to be created on the remote server
  (puthash "FTP_CREATE_MISSING_DIRS" '(long 110) curl-option-hash-table)
  ;; Set this to a bitmask value to enable the particular authentications
  ;; methods you like. Use this in combination with CURLOPT_PROXYUSERPWD.
  ;; Note that setting multiple bits may cause extra network round-trips.
  (puthash "PROXYAUTH" '(long 111) curl-option-hash-table)
  ;; FTP option that changes the timeout, in seconds, associated with
  ;; getting a response.  This is different from transfer timeout time and
  ;; essentially places a demand on the FTP server to acknowledge commands
  ;; in a timely manner.
  (puthash "FTP_RESPONSE_TIMEOUT" '(long  112) curl-option-hash-table)
  ;; Set this option to one of the CURL_IPRESOLVE_* defines (see below) to
  ;; tell libcurl to resolve names to those IP versions only. This only has
  ;; affect on systems with support for more than one, i.e IPv4 _and_ IPv6.
  (puthash "IPRESOLVE" '(long 113) curl-option-hash-table)
  ;; Set this option to limit the size of a file that will be downloaded from
  ;; an HTTP or FTP server.
  ;; Note there is also _LARGE version which adds large file support for
  ;; platforms which have larger off_t sizes.  See MAXFILESIZE_LARGE below.
  (puthash "MAXFILESIZE" '(long 114) curl-option-hash-table)
  ;; See the comment for INFILESIZE above, but in short, specifies
  ;; the size of the file being uploaded.  -1 means unknown.
  (puthash "INFILESIZE_LARGE" '(off_t 115) curl-option-hash-table)
  ;; Sets the continuation offset.  There is also a LONG version of this;
  ;; look above for RESUME_FROM.
  (puthash "RESUME_FROM_LARGE" '(off_t 116) curl-option-hash-table)
  ;; Sets the maximum size of data that will be downloaded from
  ;; an HTTP or FTP server.  See MAXFILESIZE above for the LONG version.   
  (puthash "MAXFILESIZE_LARGE" '(off_t 117) curl-option-hash-table)
  ;; Set this option to the file name of your .netrc file you want libcurl
  ;; to parse (using the CURLOPT_NETRC option). If not set, libcurl will do
  ;; a poor attempt to find the user's home directory and check for a .netrc
  ;; file in there.
  (puthash "NETRC_FILE" '(objectpoint 118) curl-option-hash-table)
  ;; Enable SSL/TLS for FTP, pick one of:
  ;; CURLFTPSSL_TRY     - try using SSL, proceed anyway otherwise
  ;; CURLFTPSSL_CONTROL - SSL for the control connection or fail
  ;; CURLFTPSSL_ALL     - SSL for all communication or fail
  (puthash "FTP_SSL" '(long 119) curl-option-hash-table)
  ;; The _LARGE version of the standard POSTFIELDSIZE option
  (puthash "POSTFIELDSIZE_LARGE" '(off_t 120) curl-option-hash-table)
  ;; Enable/disable the TCP Nagle algorithm
  (puthash "TCP_NODELAY" '(long 121) curl-option-hash-table)
  ;; 122 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  ;; When doing 3rd party transfer, set the source user and password with
  ;; this
  (puthash "SOURCE_USERPWD" '(objectpoint 123) curl-option-hash-table)
  ;; 124 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  ;; 125 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  ;; 126 OBSOLETE, used in 7.12.3. Gone in 7.13.0
  ;; When doing 3rd party transfer, set the source pre-quote linked list
  ;; of commands with this
  (puthash "SOURCE_PREQUOTE" '(objectpoint 127) curl-option-hash-table)
  ;; When doing 3rd party transfer, set the source post-quote linked list
  ;; of commands with this
  (puthash "SOURCE_POSTQUOTE" '(objectpoint 128) curl-option-hash-table)
  ;; When FTP over SSL/TLS is selected (with CURLOPT_FTP_SSL), this option
  ;; can be used to change libcurl's default action which is to first try
  ;; "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK
  ;; response has been received.
  ;; Available parameters are:
  ;; CURLFTPAUTH_DEFAULT - let libcurl decide
  ;; CURLFTPAUTH_SSL     - try "AUTH SSL" first, then TLS
  ;; CURLFTPAUTH_TLS     - try "AUTH TLS" first, then SSL
  (puthash "FTPSSLAUTH" '(long 129) curl-option-hash-table)
  (puthash "IOCTLFUNCTION" '(functionpoint 130) curl-option-hash-table)
  (puthash "IOCTLDATA" '(objectpoint 131) curl-option-hash-table)
  ;; To make a 3rd party transfer, set the source URL with this
  (puthash "SOURCE_URL" '(objectpoint 132) curl-option-hash-table)
  ;; When doing 3rd party transfer, set the source quote linked list of
  ;; commands with this
  (puthash "SOURCE_QUOTE" '(objectpoint 133) curl-option-hash-table)
  ;; zero terminated string for pass on to the FTP server when asked for
  ;; "account" info
  (puthash "FTP_ACCOUNT" '(objectpoint 134) curl-option-hash-table)
  ;; feed cookies into cookie engine
  (puthash "COOKIELIST" '(objectpoint 135) curl-option-hash-table)
  ;; ignore Content-Length
  (puthash "IGNORE_CONTENT_LENGTH" '(long 136) curl-option-hash-table)
  ;; Set to non-zero to skip the IP address received in a 227 PASV FTP server
  ;; response. Typically used for FTP-SSL purposes but is not restricted to
  ;; that. libcurl will then instead use the same IP address it used for the
  ;; control connection.
  (puthash "FTP_SKIP_PASV_IP" '(long 137) curl-option-hash-table)
  ;;   CURLOPT_LASTENTRY ; the last unused
  ;; } CURLoption;
  )

;;   /* Below here follows defines for the CURLOPT_IPRESOLVE option. If a host
;;      name resolves addresses using more than one IP protocol version, this
;;      option might be handy to force libcurl to use a specific IP version. */
;; #define CURL_IPRESOLVE_WHATEVER 0 /* default, resolves addresses to all IP
;;                                      versions that your system allows */
;; #define CURL_IPRESOLVE_V4       1 /* resolve to ipv4 addresses */
;; #define CURL_IPRESOLVE_V6       2 /* resolve to ipv6 addresses */
;; 
;;   /* three convenient "aliases" that follow the name scheme better */
;; #define CURLOPT_WRITEDATA CURLOPT_FILE
;; #define CURLOPT_READDATA  CURLOPT_INFILE
;; #define CURLOPT_HEADERDATA CURLOPT_WRITEHEADER
;; 
;; #ifndef CURL_NO_OLDIES /* define this to test if your app builds with all
;;                           the obsolete stuff removed! */
;; #define CURLOPT_HTTPREQUEST    -1
;; #define CURLOPT_FTPASCII       CURLOPT_TRANSFERTEXT
;; #define CURLOPT_MUTE           -2
;; #define CURLOPT_PASSWDFUNCTION -3
;; #define CURLOPT_PASSWDDATA     -4
;; #define CURLOPT_CLOSEFUNCTION  -5
;; 
;; #define CURLOPT_SOURCE_HOST    -6
;; #define CURLOPT_SOURCE_PATH    -7
;; #define CURLOPT_SOURCE_PORT    -8
;; #define CURLOPT_PASV_HOST      -9
;; 
;; #else
;; /* This is set if CURL_NO_OLDIES is defined at compile-time */
;; #undef CURLOPT_DNS_USE_GLOBAL_CACHE /* soon obsolete */
;; #endif
;; 
;; 
;;   /* These enums are for use with the CURLOPT_HTTP_VERSION option. */
;; enum {
;;   CURL_HTTP_VERSION_NONE, /* setting this means we don't care, and that we'd
;;                              like the library to choose the best possible
;;                              for us! */
;;   CURL_HTTP_VERSION_1_0,  /* please use HTTP 1.0 in the request */
;;   CURL_HTTP_VERSION_1_1,  /* please use HTTP 1.1 in the request */
;; 
;;   CURL_HTTP_VERSION_LAST /* *ILLEGAL* http version */
;; };
;; 
;;   /* These enums are for use with the CURLOPT_NETRC option. */
;; enum CURL_NETRC_OPTION {
;;   CURL_NETRC_IGNORED,     /* The .netrc will never be read.
;;                            * This is the default. */
;;   CURL_NETRC_OPTIONAL,    /* A user:password in the URL will be preferred
;;                            * to one in the .netrc. */
;;   CURL_NETRC_REQUIRED,    /* A user:password in the URL will be ignored.
;;                            * Unless one is set programmatically, the .netrc
;;                            * will be queried. */
;;   CURL_NETRC_LAST
;; };
;; 
;; enum {
;;   CURL_SSLVERSION_DEFAULT,
;;   CURL_SSLVERSION_TLSv1,
;;   CURL_SSLVERSION_SSLv2,
;;   CURL_SSLVERSION_SSLv3,
;; 
;;   CURL_SSLVERSION_LAST /* never use, keep last */
;; };
;; 
;; 
;; typedef enum {
;;   CURL_TIMECOND_NONE,
;; 
;;   CURL_TIMECOND_IFMODSINCE,
;;   CURL_TIMECOND_IFUNMODSINCE,
;;   CURL_TIMECOND_LASTMOD,
;; 
;;   CURL_TIMECOND_LAST
;; } curl_TimeCond;
;; 
;; #ifdef __BEOS__
;; #include <support/SupportDefs.h>
;; #endif
;; 
;; 
;; /* curl_strequal() and curl_strnequal() are subject for removal in a future
;;    libcurl, see lib/README.curlx for details */
;; CURL_EXTERN int (curl_strequal)(const char *s1, const char *s2);
;; CURL_EXTERN int (curl_strnequal)(const char *s1, const char *s2, size_t n);
;; 
;; /* name is uppercase CURLFORM_<name> */
;; #ifdef CFINIT
;; #undef CFINIT
;; #endif
;; 
;; #ifdef CURL_ISOCPP
;; #define CFINIT(name) CURLFORM_ ## name
;; #else
;; /* The macro "##" is ISO C, we assume pre-ISO C doesn't support it. */
;; #define CFINIT(name) CURLFORM_/**/name
;; #endif
;; 
;; typedef enum {
;;   CFINIT(NOTHING),        /********* the first one is unused ************/
;; 
;;   /*  */
;;   CFINIT(COPYNAME),
;;   CFINIT(PTRNAME),
;;   CFINIT(NAMELENGTH),
;;   CFINIT(COPYCONTENTS),
;;   CFINIT(PTRCONTENTS),
;;   CFINIT(CONTENTSLENGTH),
;;   CFINIT(FILECONTENT),
;;   CFINIT(ARRAY),
;;   CFINIT(OBSOLETE),
;;   CFINIT(FILE),
;; 
;;   CFINIT(BUFFER),
;;   CFINIT(BUFFERPTR),
;;   CFINIT(BUFFERLENGTH),
;; 
;;   CFINIT(CONTENTTYPE),
;;   CFINIT(CONTENTHEADER),
;;   CFINIT(FILENAME),
;;   CFINIT(END),
;;   CFINIT(OBSOLETE2),
;; 
;;   CURLFORM_LASTENTRY /* the last unusued */
;; } CURLformoption;
;; 
;; #undef CFINIT /* done */
;; 
;; /* structure to be used as parameter for CURLFORM_ARRAY */
;; struct curl_forms {
;;   CURLformoption option;
;;   const char     *value;
;; };
;; 
;; /* use this for multipart formpost building */
;; /* Returns code for curl_formadd()
;;  *
;;  * Returns:
;;  * CURL_FORMADD_OK             on success
;;  * CURL_FORMADD_MEMORY         if the FormInfo allocation fails
;;  * CURL_FORMADD_OPTION_TWICE   if one option is given twice for one Form
;;  * CURL_FORMADD_NULL           if a null pointer was given for a char
;;  * CURL_FORMADD_MEMORY         if the allocation of a FormInfo struct failed
;;  * CURL_FORMADD_UNKNOWN_OPTION if an unknown option was used
;;  * CURL_FORMADD_INCOMPLETE     if the some FormInfo is not complete (or error)
;;  * CURL_FORMADD_MEMORY         if a HttpPost struct cannot be allocated
;;  * CURL_FORMADD_MEMORY         if some allocation for string copying failed.
;;  * CURL_FORMADD_ILLEGAL_ARRAY  if an illegal option is used in an array
;;  *
;;  ***************************************************************************/
;; typedef enum {
;;   CURL_FORMADD_OK, /* first, no error */
;; 
;;   CURL_FORMADD_MEMORY,
;;   CURL_FORMADD_OPTION_TWICE,
;;   CURL_FORMADD_NULL,
;;   CURL_FORMADD_UNKNOWN_OPTION,
;;   CURL_FORMADD_INCOMPLETE,
;;   CURL_FORMADD_ILLEGAL_ARRAY,
;;   CURL_FORMADD_DISABLED, /* libcurl was built with this disabled */
;; 
;;   CURL_FORMADD_LAST /* last */
;; } CURLFORMcode;
;; 
;; /*
;;  * NAME curl_formadd()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Pretty advanved function for building multi-part formposts. Each invoke
;;  * adds one part that together construct a full post. Then use
;;  * CURLOPT_HTTPPOST to send it off to libcurl.
;;  */
;; CURL_EXTERN CURLFORMcode curl_formadd(struct curl_httppost **httppost,
;;                                       struct curl_httppost **last_post,
;;                                       ...);
;; 
;; /*
;;  * NAME curl_formfree()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Free a multipart formpost previously built with curl_formadd().
;;  */
;; CURL_EXTERN void curl_formfree(struct curl_httppost *form);
;; 
;; /*
;;  * NAME curl_getenv()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Returns a malloc()'ed string that MUST be curl_free()ed after usage is
;;  * complete. DEPRECATED - see lib/README.curlx
;;  */
;; CURL_EXTERN char *curl_getenv(const char *variable);
;; 
;; /*
;;  * NAME curl_version()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Returns a static ascii string of the libcurl version.
;;  */
;; CURL_EXTERN char *curl_version(void);
;; 
;; /*
;;  * NAME curl_escape()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Escapes URL strings (converts all letters consider illegal in URLs to their
;;  * %XX versions). This function returns a new allocated string or NULL if an
;;  * error occurred.
;;  */
;; CURL_EXTERN char *curl_escape(const char *string, int length);
;; 
;; /*
;;  * NAME curl_unescape()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Unescapes URL encoding in strings (converts all %XX codes to their 8bit
;;  * versions). This function returns a new allocated string or NULL if an error
;;  * occurred.
;;  */
;; CURL_EXTERN char *curl_unescape(const char *string, int length);
;; 
;; /*
;;  * NAME curl_free()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Provided for de-allocation in the same translation unit that did the
;;  * allocation. Added in libcurl 7.10
;;  */
;; CURL_EXTERN void curl_free(void *p);
;; 
;; /*
;;  * NAME curl_global_init()
;;  *
;;  * DESCRIPTION
;;  *
;;  * curl_global_init() should be invoked exactly once for each application that
;;  * uses libcurl
;;  */
;; CURL_EXTERN CURLcode curl_global_init(long flags);
;; 
;; /*
;;  * NAME curl_global_init_mem()
;;  *
;;  * DESCRIPTION
;;  *
;;  * curl_global_init() or curl_global_init_mem() should be invoked exactly once
;;  * for each application that uses libcurl.  This function can be used to
;;  * initialize libcurl and set user defined memory management callback
;;  * functions.  Users can implement memory management routines to check for
;;  * memory leaks, check for mis-use of the curl library etc.  User registered
;;  * callback routines with be invoked by this library instead of the system
;;  * memory management routines like malloc, free etc.
;;  */
;; CURL_EXTERN CURLcode curl_global_init_mem(long flags,
;;                                           curl_malloc_callback m,
;;                                           curl_free_callback f,
;;                                           curl_realloc_callback r,
;;                                           curl_strdup_callback s,
;;                                           curl_calloc_callback c);
;; 
;; /*
;;  * NAME curl_global_cleanup()
;;  *
;;  * DESCRIPTION
;;  *
;;  * curl_global_cleanup() should be invoked exactly once for each application
;;  * that uses libcurl
;;  */
;; CURL_EXTERN void curl_global_cleanup(void);
;; 
;; /* linked-list structure for the CURLOPT_QUOTE option (and other) */
;; struct curl_slist {
;;   char *data;
;;   struct curl_slist *next;
;; };
;; 
;; /*
;;  * NAME curl_slist_append()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Appends a string to a linked list. If no list exists, it will be created
;;  * first. Returns the new list, after appending.
;;  */
;; CURL_EXTERN struct curl_slist *curl_slist_append(struct curl_slist *,
;;                                                  const char *);
;; 
;; /*
;;  * NAME curl_slist_free_all()
;;  *
;;  * DESCRIPTION
;;  *
;;  * free a previously built curl_slist.
;;  */
;; CURL_EXTERN void curl_slist_free_all(struct curl_slist *);
;; 
;; /*
;;  * NAME curl_getdate()
;;  *
;;  * DESCRIPTION
;;  *
;;  * Returns the time, in seconds since 1 Jan 1970 of the time string given in
;;  * the first argument. The time argument in the second parameter is unused
;;  * and should be set to NULL.
;;  */
;; CURL_EXTERN time_t curl_getdate(const char *p, const time_t *unused);
;;

(unless (and (boundp 'curl-info-hash-table)
	     (hash-table-p curl-info-hash-table))
  (setq curl-info-hash-table (make-hash-table :test #'equal :size 30))

  (let ((curlinfo-string #x100000)
	(curlinfo-long   #x200000)
	(curlinfo-double #x300000)
	(curlinfo-slist  #x400000))
    ;; #define CURLINFO_MASK     0x0fffff
    ;; #define CURLINFO_TYPEMASK 0xf00000
    ;; typedef enum {
    (puthash "NONE" '(nil nil) curl-info-hash-table) ; first, never use this
    (puthash "EFFECTIVE_URL" `(string ,(+ curlinfo-string 1))
	     curl-info-hash-table)
    (puthash "RESPONSE_CODE" `(long ,(+ curlinfo-long + 2))
	     curl-info-hash-table)
    (puthash "TOTAL_TIME" `(double ,(+ curlinfo-double 3))
	     curl-info-hash-table)
    (puthash "NAMELOOKUP_TIME" `(double ,(+ curlinfo-double 4))
	     curl-info-hash-table)
    (puthash "CONNECT_TIME" `(double ,(+ curlinfo-double 5))
	     curl-info-hash-table)
    (puthash "PRETRANSFER_TIME" `(double ,(+ curlinfo-double 6))
	     curl-info-hash-table)
    (puthash "SIZE_UPLOAD" `(double ,(+ curlinfo-double 7))
	     curl-info-hash-table)
    (puthash "SIZE_DOWNLOAD" `(double ,(+ curlinfo-double 8))
	     curl-info-hash-table)
    (puthash "SPEED_DOWNLOAD" `(double ,(+ curlinfo-double 9))
	     curl-info-hash-table)
    (puthash "SPEED_UPLOAD" `(double ,(+ curlinfo-double 10))
	     curl-info-hash-table)
    (puthash "HEADER_SIZE" `(long ,(+ curlinfo-long + 11))
	     curl-info-hash-table)
    (puthash "REQUEST_SIZE" `(long ,(+ curlinfo-long + 12))
	     curl-info-hash-table)
    (puthash "SSL_VERIFYRESULT" `(long ,(+ curlinfo-long + 13))
	     curl-info-hash-table)
    (puthash "FILETIME" `(long ,(+ curlinfo-long + 14))
	     curl-info-hash-table)
    (puthash "CONTENT_LENGTH_DOWNLOAD" `(double ,(+ curlinfo-double 15))
	     curl-info-hash-table)
    (puthash "CONTENT_LENGTH_UPLOAD" `(double ,(+ curlinfo-double 16))
	     curl-info-hash-table)
    (puthash "STARTTRANSFER_TIME" `(double ,(+ curlinfo-double 17))
	     curl-info-hash-table)
    (puthash "CONTENT_TYPE" `(string ,(+ curlinfo-string 18))
	     curl-info-hash-table)
    (puthash "REDIRECT_TIME" `(double ,(+ curlinfo-double 19))
	     curl-info-hash-table)
    (puthash "REDIRECT_COUNT" `(long ,(+ curlinfo-long + 20))
	     curl-info-hash-table)
    (puthash "PRIVATE" `(string ,(+ curlinfo-string 21))
	     curl-info-hash-table)
    (puthash "HTTP_CONNECTCODE" `(long ,(+ curlinfo-long + 22))
	     curl-info-hash-table)
    (puthash "HTTPAUTH_AVAIL" `(long ,(+ curlinfo-long + 23))
	     curl-info-hash-table)
    (puthash "PROXYAUTH_AVAIL" `(long ,(+ curlinfo-long + 24))
	     curl-info-hash-table)
    (puthash "OS_ERRNO" `(long ,(+ curlinfo-long + 25))
	     curl-info-hash-table)
    (puthash "NUM_CONNECTS" `(long ,(+ curlinfo-long + 26))
	     curl-info-hash-table)
    (puthash "SSL_ENGINES" `(list ,(+ curlinfo-slist+ 27))
	     curl-info-hash-table)
    (puthash "COOKIELIST" `(list ,(+ curlinfo-slist+ 28))
	     curl-info-hash-table)
    ;; Fill in new entries below here!
    (puthash "LASTONE" '(nil 28) curl-info-hash-table)
    ;; } CURLINFO;
    ))

;; /* CURLINFO_RESPONSE_CODE is the new name for the option previously known as
;;    CURLINFO_HTTP_CODE */
;; #define CURLINFO_HTTP_CODE CURLINFO_RESPONSE_CODE
;; 
;; typedef enum {
;;   CURLCLOSEPOLICY_NONE, /* first, never use this */
;; 
;;   CURLCLOSEPOLICY_OLDEST,
;;   CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
;;   CURLCLOSEPOLICY_LEAST_TRAFFIC,
;;   CURLCLOSEPOLICY_SLOWEST,
;;   CURLCLOSEPOLICY_CALLBACK,
;; 
;;   CURLCLOSEPOLICY_LAST /* last, never use this */
;; } curl_closepolicy;
;; 
;; #define CURL_GLOBAL_SSL (1<<0)
;; #define CURL_GLOBAL_WIN32 (1<<1)
;; #define CURL_GLOBAL_ALL (CURL_GLOBAL_SSL|CURL_GLOBAL_WIN32)
;; #define CURL_GLOBAL_NOTHING 0
;; #define CURL_GLOBAL_DEFAULT CURL_GLOBAL_ALL
;; 
;; 
;; /*****************************************************************************
;;  * Setup defines, protos etc for the sharing stuff.
;;  */
;; 
;; /* Different data locks for a single share */
;; typedef enum {
;;   CURL_LOCK_DATA_NONE = 0,
;;   /*  CURL_LOCK_DATA_SHARE is used internaly to say that
;;    *  the locking is just made to change the internal state of the share
;;    *  itself.
;;    */
;;   CURL_LOCK_DATA_SHARE,
;;   CURL_LOCK_DATA_COOKIE,
;;   CURL_LOCK_DATA_DNS,
;;   CURL_LOCK_DATA_SSL_SESSION,
;;   CURL_LOCK_DATA_CONNECT,
;;   CURL_LOCK_DATA_LAST
;; } curl_lock_data;
;; 
;; /* Different lock access types */
;; typedef enum {
;;   CURL_LOCK_ACCESS_NONE = 0,   /* unspecified action */
;;   CURL_LOCK_ACCESS_SHARED = 1, /* for read perhaps */
;;   CURL_LOCK_ACCESS_SINGLE = 2, /* for write perhaps */
;;   CURL_LOCK_ACCESS_LAST        /* never use */
;; } curl_lock_access;
;; 
;; typedef void (*curl_lock_function)(CURL *handle,
;;                                    curl_lock_data data,
;;                                    curl_lock_access locktype,
;;                                    void *userptr);
;; typedef void (*curl_unlock_function)(CURL *handle,
;;                                      curl_lock_data data,
;;                                      void *userptr);
;; 
;; typedef void CURLSH;
;; 
;; typedef enum {
;;   CURLSHE_OK,  /* all is fine */
;;   CURLSHE_BAD_OPTION, /* 1 */
;;   CURLSHE_IN_USE,     /* 2 */
;;   CURLSHE_INVALID,    /* 3 */
;;   CURLSHE_NOMEM,      /* out of memory */
;;   CURLSHE_LAST /* never use */
;; } CURLSHcode;
;; 
;; typedef enum {
;;   CURLSHOPT_NONE,  /* don't use */
;;   CURLSHOPT_SHARE,   /* specify a data type to share */
;;   CURLSHOPT_UNSHARE, /* specify shich data type to stop sharing */
;;   CURLSHOPT_LOCKFUNC,   /* pass in a 'curl_lock_function' pointer */
;;   CURLSHOPT_UNLOCKFUNC, /* pass in a 'curl_unlock_function' pointer */
;;   CURLSHOPT_USERDATA,   /* pass in a user data pointer used in the lock/unlock
;;                            callback functions */
;;   CURLSHOPT_LAST  /* never use */
;; } CURLSHoption;
;; 
;; CURL_EXTERN CURLSH *curl_share_init(void);
;; CURL_EXTERN CURLSHcode curl_share_setopt(CURLSH *, CURLSHoption option, ...);
;; CURL_EXTERN CURLSHcode curl_share_cleanup(CURLSH *);
;; 
;; /****************************************************************************
;;  * Structures for querying information about the curl library at runtime.
;;  */
;; 
;; typedef enum {
;;   CURLVERSION_FIRST,
;;   CURLVERSION_SECOND,
;;   CURLVERSION_THIRD,
;;   CURLVERSION_LAST /* never actually use this */
;; } CURLversion;
;; 
;; /* The 'CURLVERSION_NOW' is the symbolic name meant to be used by
;;    basicly all programs ever, that want to get version information. It is
;;    meant to be a built-in version number for what kind of struct the caller
;;    expects. If the struct ever changes, we redefine the NOW to another enum
;;    from above. */
;; #define CURLVERSION_NOW CURLVERSION_THIRD
;; 
;; typedef struct {
;;   CURLversion age;          /* age of the returned struct */
;;   const char *version;      /* LIBCURL_VERSION */
;;   unsigned int version_num; /* LIBCURL_VERSION_NUM */
;;   const char *host;         /* OS/host/cpu/machine when configured */
;;   int features;             /* bitmask, see defines below */
;;   const char *ssl_version;  /* human readable string */
;;   long ssl_version_num;     /* not used anymore, always 0 */
;;   const char *libz_version; /* human readable string */
;;   /* protocols is terminated by an entry with a NULL protoname */
;;   const char * const *protocols;
;; 
;;   /* The fields below this were added in CURLVERSION_SECOND */
;;   const char *ares;
;;   int ares_num;
;; 
;;   /* This field was aded in CURLVERSION_THIRD */
;;   const char *libidn;
;; } curl_version_info_data;
;; 
;; #define CURL_VERSION_IPV6      (1<<0)  /* IPv6-enabled */
;; #define CURL_VERSION_KERBEROS4 (1<<1)  /* kerberos auth is supported */
;; #define CURL_VERSION_SSL       (1<<2)  /* SSL options are present */
;; #define CURL_VERSION_LIBZ      (1<<3)  /* libz features are present */
;; #define CURL_VERSION_NTLM      (1<<4)  /* NTLM auth is supported */
;; #define CURL_VERSION_GSSNEGOTIATE (1<<5) /* Negotiate auth support */
;; #define CURL_VERSION_DEBUG     (1<<6)  /* built with debug capabilities */
;; #define CURL_VERSION_ASYNCHDNS (1<<7)  /* asynchronous dns resolves */
;; #define CURL_VERSION_SPNEGO    (1<<8)  /* SPNEGO auth */
;; #define CURL_VERSION_LARGEFILE (1<<9)  /* supports files bigger than 2GB */
;; #define CURL_VERSION_IDN       (1<<10) /* International Domain Names support */
;; #define CURL_VERSION_SSPI      (1<<11) /* SSPI is supported */
;; 
;; /*
;;  * NAME curl_version_info()
;;  *
;;  * DESCRIPTION
;;  *
;;  * This function returns a pointer to a static copy of the version info
;;  * struct. See above.
;;  */
;; CURL_EXTERN curl_version_info_data *curl_version_info(CURLversion);
;; 
;; /*
;;  * NAME curl_easy_strerror()
;;  *
;;  * DESCRIPTION
;;  *
;;  * The curl_easy_strerror function may be used to turn a CURLcode value
;;  * into the equivalent human readable error string.  This is useful
;;  * for printing meaningful error messages.
;;  */
;; CURL_EXTERN const char *curl_easy_strerror(CURLcode);
;; 
;; /*
;;  * NAME curl_share_strerror()
;;  *
;;  * DESCRIPTION
;;  *
;;  * The curl_share_strerror function may be used to turn a CURLSHcode value
;;  * into the equivalent human readable error string.  This is useful
;;  * for printing meaningful error messages.
;;  */
;; CURL_EXTERN const char *curl_share_strerror(CURLSHcode);

(provide 'curl)

;;; curl.el ends here
