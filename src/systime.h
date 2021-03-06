/* systime.h - System-dependent definitions for time manipulations.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 2001, 2002 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_systime_h_
#define INCLUDED_systime_h_

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifdef HAVE_SYS_TIMES_H
/* Need this for struct tms */
# include <sys/times.h>
#endif

/* select() is supposed to be (Unix98) defined in sys/time.h,
   but FreeBSD and Irix 5 put it in unistd.h instead.
   If we have it, including it can't hurt. */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef WIN32_NATIVE

/* This defines struct timeval */
#include <winsock.h>

struct timezone 
  {
    int	tz_minuteswest;	/* minutes west of Greenwich */
    int	tz_dsttime;	/* type of dst correction */
  };

#ifdef HAVE_X_WINDOWS
/* Provides gettimeofday etc */
#include <X11/Xw32defs.h>
#include <X11/Xos.h>
#else
/* X11R6 on NT provides the single parameter version of this command */
void gettimeofday (struct timeval *, struct timezone *);
#endif /* HAVE_X_WINDOWS */

#endif /* WIN32_NATIVE */

/* struct utimbuf */

#ifdef HAVE_UTIME
# include <utime.h>
#endif

#ifdef WIN32_NATIVE
# include <sys/utime.h>
#ifdef emacs
int mswindows_utime (Lisp_Object path, struct utimbuf *thymes);
#endif
#endif

#if defined(HAVE_TZNAME) && !defined(WIN32_NATIVE) && !defined(CYGWIN)
#ifndef tzname		/* For SGI.  */
extern char *tzname[];	/* RS6000 and others want it this way.  */
#endif
#endif

/* On some configurations (hpux8.0, X11R4), sys/time.h and X11/Xos.h
   disagree about the name of the guard symbol.  */
#ifdef HPUX
#ifdef _STRUCT_TIMEVAL
#ifndef __TIMEVAL__
#define __TIMEVAL__
#endif
#endif
#endif

/* EMACS_TIME is the type to use to represent temporal intervals.
   At one point this was `struct timeval' on some systems, int on others.
   But this is stupid.  Other things than select() code like to
   manipulate time values, and so microsecond precision should be
   maintained.  Separate typedefs and conversion functions are provided
   for select().

   EMACS_SECS (TIME) is an rvalue for the seconds component of TIME.
   EMACS_SET_SECS (TIME, SECONDS) sets that to SECONDS.

   EMACS_USECS (TIME) is an rvalue for the microseconds component of TIME.
   EMACS_SET_USECS (TIME, MICROSECONDS) sets that to MICROSECONDS.

   Note that all times are returned in "normalized" format (i.e. the
   usecs value is in the range 0 <= value < 1000000) and are assumed
   to be passed in in this format.

   EMACS_SET_SECS_USECS (TIME, SECS, USECS) sets both components of TIME.

   EMACS_GET_TIME (TIME) stores the current system time in TIME, which
	should be an lvalue.

   set_file_times (PATH, ATIME, MTIME) changes the last-access and
	last-modification times of the file named PATH to ATIME and
	MTIME, which are EMACS_TIMEs.

   EMACS_NORMALIZE_TIME (TIME) coerces TIME into normalized format.

   EMACS_ADD_TIME (DEST, SRC1, SRC2) adds SRC1 to SRC2 and stores the
	result in DEST.  Either or both may be negative.

   EMACS_SUB_TIME (DEST, SRC1, SRC2) subtracts SRC2 from SRC1 and
	stores the result in DEST.  Either or both may be negative.

   EMACS_TIME_NEG_P (TIME) is true iff TIME is negative.

   EMACS_TIME_EQUAL (TIME1, TIME2) is true iff TIME1 is the same as TIME2.
   EMACS_TIME_GREATER (TIME1, TIME2) is true iff TIME1 is greater than
        TIME2.
   EMACS_TIME_EQUAL_OR_GREATER (TIME1, TIME2) is true iff TIME1 is
        greater than or equal to TIME2.

*/

#ifdef HAVE_TIMEVAL

#define EMACS_SELECT_TIME struct timeval
#define EMACS_TIME_TO_SELECT_TIME(time, select_time) ((select_time) = (time))

#else /* not HAVE_TIMEVAL */

struct timeval
{
  long tv_sec;                /* seconds */
  long tv_usec;               /* microseconds */
};

#define EMACS_SELECT_TIME int
#define EMACS_TIME_TO_SELECT_TIME(time, select_time) \
  EMACS_TIME_TO_INT (time, select_time)

#endif /* not HAVE_TIMEVAL */

#define EMACS_TIME_TO_INT(time, intvar)		\
do {						\
  EMACS_TIME tmptime = time;			\
						\
  if (tmptime.tv_usec > 0)			\
    (intvar) = tmptime.tv_sec + 1;		\
  else						\
    (intvar) = tmptime.tv_sec;			\
} while (0)

#define EMACS_TIME struct timeval
#define EMACS_SECS(time)		    ((time).tv_sec  + 0)
#define EMACS_USECS(time)		    ((time).tv_usec + 0)
#define EMACS_SET_SECS(time, seconds)	    ((time).tv_sec  = (seconds))
#define EMACS_SET_USECS(time, microseconds) ((time).tv_usec = (microseconds))

#if !defined (HAVE_GETTIMEOFDAY)
int gettimeofday (struct timeval *, void *);
#endif

/* On SVR4, the compiler may complain if given this extra BSD arg.  */
#ifdef GETTIMEOFDAY_ONE_ARGUMENT
#define EMACS_GETTIMEOFDAY(time) gettimeofday(time)
#else
#define EMACS_GETTIMEOFDAY(time) gettimeofday(time,0)
#endif

/* According to the Xt sources, some NTP daemons on some systems may
   return non-normalized values. */
#define EMACS_GET_TIME(time)					\
do {								\
  EMACS_GETTIMEOFDAY (&(time));					\
  EMACS_NORMALIZE_TIME (time);					\
} while (0)

#define EMACS_NORMALIZE_TIME(time)				\
do {								\
  while ((time).tv_usec >= 1000000)				\
    {								\
      (time).tv_usec -= 1000000;				\
      (time).tv_sec++;						\
    }								\
  while ((time).tv_usec < 0)					\
    {								\
      (time).tv_usec += 1000000;				\
      (time).tv_sec--;						\
    }								\
} while (0)

#define EMACS_ADD_TIME(dest, src1, src2)			\
do {								\
  (dest).tv_sec  = (src1).tv_sec  + (src2).tv_sec;		\
  (dest).tv_usec = (src1).tv_usec + (src2).tv_usec;		\
  EMACS_NORMALIZE_TIME (dest);					\
} while (0)

#define EMACS_SUB_TIME(dest, src1, src2)			\
do {								\
  (dest).tv_sec  = (src1).tv_sec  - (src2).tv_sec;		\
  (dest).tv_usec = (src1).tv_usec - (src2).tv_usec;		\
  EMACS_NORMALIZE_TIME (dest);					\
} while (0)

#define EMACS_TIME_NEG_P(time) ((long)(time).tv_sec < 0)

#define EMACS_TIME_EQUAL(time1, time2)				\
  ((time1).tv_sec == (time2).tv_sec &&				\
   (time1).tv_usec == (time2).tv_usec)

#define EMACS_TIME_GREATER(time1, time2)			\
  ((time1).tv_sec > (time2).tv_sec ||				\
   ((time1).tv_sec == (time2).tv_sec &&				\
    (time1).tv_usec > (time2).tv_usec))

#define EMACS_TIME_EQUAL_OR_GREATER(time1, time2)		\
  ((time1).tv_sec > (time2).tv_sec ||				\
   ((time1).tv_sec == (time2).tv_sec &&				\
    (time1).tv_usec >= (time2).tv_usec))

#define EMACS_SET_SECS_USECS(time, secs, usecs) 		\
  (EMACS_SET_SECS (time, secs), EMACS_SET_USECS (time, usecs))

#ifdef emacs
int set_file_times (Lisp_Object path, EMACS_TIME atime, EMACS_TIME mtime);
void get_process_times (double *user_time, double *system_time,
			double *real_time);
Ibyte *qxe_ctime (const time_t *value);

#endif

#ifdef WIN32_NATIVE

/* setitimer emulation for Win32 (see win32.c) */

struct itimerval
{
  struct timeval it_value;
  struct timeval it_interval;
};

#define ITIMER_REAL 1
#define ITIMER_PROF 2

#endif /* WIN32_NATIVE */

#ifdef WIN32_ANY

int mswindows_setitimer (int kind, const struct itimerval *itnew,
			 struct itimerval *itold);

#endif /* WIN32_ANY */

/* #### Move this comment elsewhere when we figure out the place.

   "qxe" is a unique prefix used to identify encapsulations of standard
   library functions.  We used to play pre-processing games but in
   general this leads to nothing but trouble because someone first
   encountering the code will have no idea that what appears to be a
   call to a library function has actually been redefined to be a call
   somewhere else.  This is doubly true when the redefinition occurs
   in out-of-the way s+m files and only on certainly systems.

   The name "qxe" was chosen because it is a unique string that is not
   going to be found anywhere else in the sources (unlike, for example,
   the prefixes "xemacs" or "sys") and is easy to type.  Alternative
   names are certainly possible, and suggestions are welcome.

   By making the encapsulation explicit we might be making the code
   that uses is slightly less pretty, but this is more than compensated
   for by the huge increase in clarity.

   "Standard library function" can refer to any function in any
   standard library.  If we are explicitly changing the semantics
   (e.g. Mule-encapsulating), we should use an extended version of
   the prefix, e.g. perhaps "qxe_xlat_" for functions that Mule-
   encapsulate, or "qxe_retry_" for functions that automatically
   retry a system call interrupted by EINTR.  In general, if there
   is no prefix extension, it means the function is trying to
   provide (more or less) the same semantics as the standard library
   function; but be aware that the reimplementation may be incomplete
   or differ in important respects.  This is especially the case
   when attempts are made to implement Unix functions on MS Windows.

   (The comment on the particular encapsulation should describe what
   standard function is being emulated, if this is not obvious, and
   what the differences, if any, from that standard function are.)

   An example of this is the qxe_setitimer() function.  This attempts
   to emulate the POSIX (Unix98?) standard setitimer(), as found on
   all modern versions of Unix.  Normally, we just call the system-
   provided setitimer() function.  When emulated on MS Windows and
   Cygwin, however, the ITNEW and ITOLD values cannot be different
   from each other if both are non-zero, due to limitations in the
   underlying multimedia-timer API.  By simply using setitimer() with
   preprocessor tricks, a programmer would almost have to be a
   mind-reader to figure this out.  With the explicit encapsulation, a
   programmer need only look at the definition of qxe_setitimer() to
   see what its semantics are.
*/

int qxe_setitimer (int kind, const struct itimerval *itnew,
		   struct itimerval *itold);

#endif /* INCLUDED_systime_h_ */
