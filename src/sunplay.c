/* play.c - play a sound file on the speaker
 **
 ** Copyright (C) 1989 by Jef Poskanzer.
 **
 ** Modified 24-May-91 by Jamie Zawinski (for Lucid Emacs).
 ** Modified 17-Dec-92 by Jamie Zawinski (largely rewritten for SunOS 4.1.3).
 **
 ** Permission to use, copy, modify, and distribute this software and its
 ** documentation for any purpose and without fee is hereby granted, provided
 ** that the above copyright notice appear in all copies and that both that
 ** copyright notice and this permission notice appear in supporting
 ** documentation.  This software is provided "as is" without express or
 ** implied warranty.
 */

/* Synched up with: Not in FSF. */

/* This file Mule-ized by Ben Wing, 5-15-01. */

#include <config.h>
#include "lisp.h"
#include "sound.h"

#include "sysdep.h"
#include "sysfile.h"
#include "syssignal.h"

#include <multimedia/libaudio.h>
#include <multimedia/audio_device.h>

static SIGTYPE (*sighup_handler) (int sig);
static SIGTYPE (*sigint_handler) (int sig);
static SIGTYPE sighandler (int sig);

static int audio_fd;

#define audio_open()	open ("/dev/audio", (O_WRONLY | O_NONBLOCK), 0)

static int initialized_device_p;
static int reset_volume_p, reset_device_p;
static double old_volume;
static Audio_hdr dev_hdr;

static int
init_device (int volume, Binbyte *data, int fd,
	     unsigned int *header_length)
{
#ifdef SUNOS4_0_3
  if (header_length) *header_length = 0;
  return 0;
#else
  Audio_hdr file_hdr;

  reset_volume_p = 0;
  reset_device_p = 0;

  if (data && fd) ABORT (); /* one or the other */

  if (AUDIO_SUCCESS != audio_get_play_config (audio_fd, &dev_hdr))
    {
      sound_perror ("Not a valid audio device");
      return 1;
    }

  if (AUDIO_SUCCESS != (data
			? audio_decode_filehdr (data, &file_hdr, header_length)
			: audio_read_filehdr (fd, &file_hdr, 0, 0)))
    {
      if (data)
	sound_perror ("invalid audio data");
      else
	sound_perror ("invalid audio file");
      return 1;
    }

  audio_flush_play (audio_fd);

  if (!initialized_device_p || (0 != audio_cmp_hdr (&dev_hdr, &file_hdr)))
    {
      Audio_hdr new_hdr;
      new_hdr = file_hdr;
      reset_device_p = 1;
      initialized_device_p = 1;
      if (AUDIO_SUCCESS != audio_set_play_config (audio_fd, &new_hdr))
	{
	  Extbyte buf1 [100], buf2 [100], buf3 [250];
	  audio_enc_to_str (&file_hdr, buf1);
	  audio_enc_to_str (&new_hdr, buf2);
	  sprintf (buf3, "wanted %s, got %s", buf1, buf2);
	  sound_warn (buf3);
	  return 1;
	}
    }

  if (volume < 0 || volume > 100)
    {
      Extbyte buf [255];
      sprintf (buf, "volume must be between 0 and 100 (not %d)", volume);
      sound_warn (buf);
      return 1;
    }
  {
    /* set the volume; scale it to 0.0 - 1.0 */
    double V = (volume / 100.0);
    audio_get_play_gain (audio_fd, &old_volume);
    reset_volume_p = 1;
    audio_set_play_gain (audio_fd, &V);
  }

  return 0;
#endif
}


static void
reset_device (int wait_p)
{
  if (wait_p)
    audio_drain (audio_fd, 1);
  else
    audio_flush_play (audio_fd);
  if (reset_device_p)
    audio_set_play_config (audio_fd, &dev_hdr);
  if (reset_volume_p)
    audio_set_play_gain (audio_fd, &old_volume);
}


void
play_sound_file (Extbyte *sound_file, int volume)
{
  int rrtn, wrtn;
  Binbyte buf [255];
  int file_fd;

  audio_fd = audio_open ();

  if (audio_fd < 0)
    {
      sound_perror ("open /dev/audio");
      return;
    }

  /* where to find the proto for signal()... */
  sighup_handler = (SIGTYPE (*) (int)) EMACS_SIGNAL (SIGHUP, sighandler);
  sigint_handler = (SIGTYPE (*) (int)) EMACS_SIGNAL (SIGINT, sighandler);

  file_fd = open (sound_file, O_RDONLY, 0);
  if (file_fd < 0)
    {
      sound_perror (sound_file);
      goto END_OF_PLAY;
    }

  if (init_device (volume, (Binbyte *) 0, file_fd, (unsigned int *) 0))
    goto END_OF_PLAY;

  while (1)
    {
      rrtn = read (file_fd, (CBinbyte *) buf, sizeof (buf));
      if (rrtn < 0)
	{
	  sound_perror ("read");
	  goto END_OF_PLAY;
	}
      if (rrtn == 0)
	break;

      while (1)
	{
	  wrtn = write (audio_fd, (CBinbyte *) buf, rrtn);
	  if (wrtn < 0)
	    {
	      sound_perror ("write");
	      goto END_OF_PLAY;
	    }
	  if (wrtn != 0)
	    break;

	  if (AUDIO_ERR_INTERRUPTED == audio_drain (audio_fd, 1))
	    goto END_OF_PLAY;
	}
      if (wrtn != rrtn)
	{
	  Extbyte warn_buf [255];
	  sprintf (warn_buf, "play: rrtn = %d, wrtn = %d", rrtn, wrtn);
	  sound_warn (warn_buf);
	  goto END_OF_PLAY;
	}
    }

 END_OF_PLAY:

  if (file_fd > 0)
    close (file_fd);

  if (audio_fd > 0)
    {
      reset_device (1);
      close (audio_fd);
    }

  EMACS_SIGNAL (SIGHUP, sighup_handler);
  EMACS_SIGNAL (SIGINT, sigint_handler);
}


int
play_sound_data (Binbyte *data, int length, int volume)
{
  int wrtn, start = 0;
  unsigned int ilen;
  int result = 0;

  audio_fd = -1;

  if (length == 0) return 0;

  /* this is just to get a better error message */
  if (strncmp (".snd\0", (CBinbyte *) data, 4))
    {
      sound_warn ("Not valid audio data (bad magic number)");
      goto END_OF_PLAY;
    }
  if (length <= sizeof (Audio_hdr))
    {
      sound_warn ("Not valid audio data (too short)");
      goto END_OF_PLAY;
    }

  audio_fd = audio_open ();
  if (audio_fd < 0)
      return 0;

  /* where to find the proto for signal()... */
  sighup_handler = (SIGTYPE (*) (int)) EMACS_SIGNAL (SIGHUP, sighandler);
  sigint_handler = (SIGTYPE (*) (int)) EMACS_SIGNAL (SIGINT, sighandler);

  if (init_device (volume, data, 0, &ilen))
    goto END_OF_PLAY;

  data   += (ilen<<2);
  length -= (ilen<<2);
  if (length <= 1)
    goto END_OF_PLAY;

  while (1)
    {
      wrtn = write (audio_fd, (CBinbyte *) (data+start), length-start);
      if (wrtn < 0)
	{
	  sound_perror ("write");
	  goto END_OF_PLAY;
	}
      if (wrtn != 0)
	{
	  start += wrtn;
	  break;
	}
      if (AUDIO_ERR_INTERRUPTED == audio_drain (audio_fd, 1))
	goto END_OF_PLAY;
    }
  if (wrtn != length)
    {
      Extbyte buf [255];
      sprintf (buf, "play: rrtn = %d, wrtn = %d", length, wrtn);
      sound_warn (buf);
      goto END_OF_PLAY;
    }

 result = 1;
  
 END_OF_PLAY:

  if (audio_fd > 0)
    {
      reset_device (1);
      close (audio_fd);
    }

  EMACS_SIGNAL (SIGHUP, sighup_handler);
  EMACS_SIGNAL (SIGINT, sigint_handler);

  return result;
}

/* #### sigcontext doesn't exist in Solaris.  This should be updated
   to be correct for Solaris. */
static SIGTYPE
sighandler (int sig)
{
  if (audio_fd > 0)
    {
      reset_device (0);
      close (audio_fd);
    }
  if (sig == SIGHUP && sighup_handler)
    sighup_handler (sig);
  else if (sig == SIGINT && sigint_handler)
    sigint_handler (sig);
  else
    exit (1);
}
