#lang racket/base

; sample launch file for geeklog
(require "geeklog.rkt")

(geeklog-merge-settings
 (make-hash `([base-path . "/www/geekmx"]
              [data-path . "data"]
              [template  . "geekmx.html"]))
 "geekmx.rkt")

(geeklog-merge-settings 
 (make-hash `([base-path . "/www/96grados"]
              [data-path . "docs"]
              [suffixes  . (".txt" ".link" "")]
              [template  . "96grados.s-html"]))
 "96g.rkt")

(geeklog #:path "/www"
         #:data-path "omg")
