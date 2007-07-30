;; this file expects to be run from $blddir
(require 'curl (expand-file-name "modules/curl/curl"))
(setq handle
      (curl-make-url-handle "http://turnbull.sk.tsukuba.ac.jp/index.html"))
(curl-easy-setopt "URL" "http://curl.haxx.se/doc/copyright.html" handle)
(curl-easy-perform handle)
