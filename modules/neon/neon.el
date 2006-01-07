;;; neon.el --- utilities for use with the neon_api module

;; Copyright (C) 2005  Stephen J. Turnbull  <stephen@xemacs.org>

;; All rights reserved, except as expressly indicated below.

;; This program is not considered part of XEmacs.

;; You may use, copy, modify, and distribute this software under the terms
;; of the GNU General Public License, version 2 or later at your option.

;; Author:		Stephen J. Turnbull <stephen@xemacs.org>
;; Creation-Date:	2005-11-24

(require 'neon-api "neon/neon_api")

;; Version information from the libneon I built the first version from.
;; #### This should be fixed to get the information at build time.
(put 'neon-api 'libneon-version "0.25.4")
(put 'neon-api 'libneon_version_major 0)
(put 'neon-api 'libneon_version_minor 25)
(put 'neon-api 'libneon_version_patch 4)

;; #### pull generally useful stuff into here from neon-test.el.

(provide 'neon)

;;; neon.el ends here
