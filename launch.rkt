#lang racket/base

; sample launch file for geeklog
(require "geeklog.rkt")

(let ([doc (make-hash '([title . placeholder]
                        [body . placeholder]))])
  (geeklog-merge-settings
   (make-hash `([base-path . "geekmx"]
                [data-path . "geekmx"]))
   "geekmx.rkt")

  (geeklog-merge-settings 
   (make-hash `([base-path . "/www/96grados"]
                [data-path . "docs"]
                [suffixes  . (".txt" ".link" "")]
                [template  . "96grados.s-html"]))
   "96g.rkt"))

(geeklog #:path "/www"
         #:data-path "omg")
