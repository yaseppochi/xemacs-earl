/* Copyright (C) 1993 Free Software Foundation, Inc.

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

/* Synched up with: Not in FSF. */

/* This file Mule-ized by Ben Wing, 5-15-01. */


/***
   NAME
     hpplay
   PURPOSE
     Play .au sound files on hp9000s700
   BUGS
     I have been unable to figure out how to use the volume feature, so no
     attempt has been made to honor the volume arg of play_sound_*
     This means that all sounds are played at 100%.
     The gain parameter can be set by using the hp-play-gain variable.

   NOTES
     This file is mostly based on the player program found in the examples
     directory of the audio software delivered on our machines. The path I
     found it under was /usr/audio/examples/player.c
     This file contained no credits and no copyrights. The original fileheader
     is given below.
   HISTORY
     lynbech - Feb 10, 1993: Created.
***/

/* ORIGINAL FILEHEADER:
 * player - command-line audio file player
 *   Aug. 28 1991
 *    by three unknown, unsung audio programmers
 *     (well, only two are unsung)
 */

#include <config.h>
#include "lisp.h"

#include "sound.h"

#ifdef HPUX10
#include <Alib.h>
#include <CUlib.h>
#else /* !HPUX 10 */
#include <audio/Alib.h>
#include <audio/CUlib.h>
#endif /* !HPUX 10 */


Lisp_Object Vhp_play_server;
Lisp_Object Vhp_play_speaker;
Fixnum      hp_play_gain;

/* Functions */

/* error handling */
void
player_error_internal (Audio * audio, Ascbyte * text, long errorCode)
{
  Extbyte errorbuff[132];
  Ibyte *interr;

  AGetErrorText (audio, errorCode, errorbuff, 131);
  EXTERNAL_TO_C_STRING (errorbuf, interr, Qnative);
  
  signal_error (Qsound_error, text, build_string (interr));
}

long
myHandler( Audio * audio, AErrorEvent * err_event)
{
  player_error_internal(audio, "Internal sound error", err_event->error_code);
  return 1;			/* Must return something, was orig. an exit */
}

/* Playing */
void
play_bucket_internal( Audio *audio, SBucket *pSBucket, long UNUSED (volume))
{
  SBPlayParams playParams;
  AGainEntry gainEntry;
  ATransID xid;
  long status;

  playParams.priority = APriorityNormal;          /* normal priority */

  /*
   * We can't signal an error, because all h*ll would break loose if
   * we did.
   */
  if (EQ (Vhp_play_speaker, Qexternal))
    gainEntry.u.o.out_dst = AODTMonoJack;
  else
    gainEntry.u.o.out_dst = AODTMonoIntSpeaker;

  gainEntry.u.o.out_ch = AOCTMono;
  gainEntry.gain = AUnityGain;
  playParams.gain_matrix.type = AGMTOutput;       /* gain matrix */
  playParams.gain_matrix.num_entries = 1;
  playParams.gain_matrix.gain_entries = &gainEntry;
  playParams.play_volume = hp_play_gain;          /* play volume */
  playParams.pause_first = False;                 /* don't pause */
  playParams.start_offset.type = ATTSamples;      /* start offset 0 */
  playParams.start_offset.u.samples = 0;
  playParams.duration.type = ATTFullLength;       /* play entire sample */
  playParams.loop_count = 1;                      /* play sample just once */
  playParams.previous_transaction = 0;            /* no linked transaction */
  playParams.event_mask = 0;                      /* don't solicit any events */

  /*
   * play the sound bucket
   */
  xid = APlaySBucket( audio, pSBucket, &playParams, NULL );

  /*
   * set close mode to prevent playback from stopping
   *  when we close audio connection
   */
  ASetCloseDownMode( audio, AKeepTransactions, &status );

  /*
   *  That's all, folks!
   *  Always destroy bucket and close connection.
   */
  ADestroySBucket( audio, pSBucket, &status );
  ACloseAudio( audio, &status );
}

void
play_sound_file (Extbyte * sound_file, int volume)
{
  sbucket *pSBucket;
  Audio *audio;
  long status;
  AErrorHandler prevHandler;  /* pointer to previous handler */
  Extbyte *server;

  if (STRINGP (Vhp_play_server))
    LISP_STRING_TO_EXTERNAL (Vhp_play_server, server, Qnative);
  else
    server = "";

  /*
   *  open audio connection
   */
  audio = AOpenAudio( server, &status );
  if( status )
    {
      player_error_internal( audio, "Open audio failed", status );
    }

  /* replace default error handler */
  prevHandler = ASetErrorHandler(myHandler);

  /*
   *  Load the audio file into a sound bucket
   */

  pSBucket = ALoadAFile( audio, sound_file, AFFUnknown, 0, NULL, NULL );

  /*
   * Play the bucket
   */

  play_bucket_internal(audio, pSBucket, volume);

  ASetErrorHandler(prevHandler);    
}


int
play_sound_data (Binbyte * data, int UNUSED (length), int volume)
{
  SBucket *pSBucket;
  Audio *audio;
  AErrorHandler prevHandler;
  SunHeader *header;
  long status;
  Extbyte *server;
  int result;

  /* #### Finish this to return an error code.
     This function signal a lisp error. How consistent with the rest.
     What if this function is needed in doing the beep for the error?

     Apparently the author of this didn't read the comment in
     Fplay_sound.
  */
       
    
  if (STRINGP (Vhp_play_server))
    LISP_STRING_TO_EXTERNAL (Vhp_play_server, server, Qnative);
  else
    server = "";

  /* open audio connection */
  audio = AOpenAudio( server, &status );
  if(status)
    {
      player_error_internal( audio, "Open audio failed", status );
    }

  /* replace default error handler */
  prevHandler = ASetErrorHandler (myHandler);

  /* Create sound bucket */
  header = (SunHeader *) data;

  pSBucket = ACreateSBucket(audio, NULL, NULL, &status);
  if (status)
    player_error_internal( audio, "Bucket creation failed", status );

  APutSBucketData(audio, pSBucket, 0, (CBinbyte *) (data + header->header_size), header->data_length, &status);

  if (status)
    player_error_internal( audio, "Audio data copy failed", status );

  /* Play sound */
  play_bucket_internal(audio, pSBucket, volume);

  ASetErrorHandler(prevHandler);
  if (status)
    player_error_internal( audio, "Audio data copy failed", status );

  return 1;
}

void
vars_of_hpplay (void)
{
  DEFVAR_LISP ("hp-play-server", &Vhp_play_server /*
A string, determining which server to play sound at.
Note that this is specific to the HP sound implementation, and you should
not make your functions depend on it.
*/ );

  Vhp_play_server = Qnil;

  DEFVAR_LISP ("hp-play-speaker", &Vhp_play_speaker /*
If this variable is the symbol `external', sound is played externally.
If the environment variable SPEAKER is set, that value is used for
initializing this variable.
Note that this is specific to the HP sound implementation, and you should
not make your functions depend on it.
*/ );

  Vhp_play_speaker = intern ("internal");

  DEFVAR_INT ("hp-play-gain", &hp_play_gain /*
Global gain value for playing sounds.
Default value is AUnityGain which means keep level.
Please refer to the HP documentation, for instance in
`Using the Audio Application Program Interface', for details on how to
interpret this variable.
Note that this is specific to the HP sound implementation, and you should
not make your functions depend on it.
*/ );

  hp_play_gain = AUnityGain;
}

void
init_hpplay (void)
{
  if (egetenv ("SPEAKER"))
    Vhp_play_speaker = intern (egetenv ("SPEAKER"));
}
