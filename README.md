# xemacs-earl
## EARL: _Emacs Augmented Resource Locators_
A fork of XEmacs, the Lisp environment and programmer's editor, to add remote editing on the web using `libcurl` and `libneon`.

`libcurl` is the library that supports Daniel Stenberg's "Swiss Army knife" for the web, `curl`.  `libneon` is a library that supports __WebDAV__, the _Distributed Authoring and Versioning extensions to HTTP_ (RFC 2518, since obsoleted by RFC 4918).

These extensions are provided as modules (dynamically loadable objects), which at that time (2006) were a major XEmacs innovation.  Although I didn't have a real need for remote editing on the web, I thought it would be a good, practical project to learn how to use the module API.  These capabilities might also be useful for heavy users of remote resources, and possibly useful for cross-platform, editor-agnostic collaborative editing via WebDAV.  This code was tested, at least somewhat useful, and almost ready to merge to the mainline when I paused development.

Unfortunately, I ran out of spare time around then because I became the release engineer for the beta series, and was also helping to support a major contributor's participation while he was suffering from RSI and other health issues by refactoring his mega patches (they literally came in as `diff -u` patches of several megabytes, every couple of months).  In preparing this repository, I determined that __WebDAV__ is still a viable protocol, although perhaps not living up to its mid-naughties promise, `libcurl` is in active development, and `libneon` is still at least somewhat actively maintained.  So it might be interesting to pick up here where I left off.

Also unfortunately, XEmacs, while still maintained (if only sporadically), is primarily updated via Mercurial (https://foss.heptpod.net/xemacs/xemacs).

## XEmacs display of SVG images, and a cairo display device

The work described above is on the __main__ branch of this repository.  I also did some work on porting XEmacs to use `cairo` the lowlevel graphics library in GTK+ for display, and add an __SVG__ image type.  This work was branched from EARL and named __cairo__ for the graphics library.  The __cairo__ branch was still in the earliest stages, and is included here because it may depend on some of the changes in __main__.
