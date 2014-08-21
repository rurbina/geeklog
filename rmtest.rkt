#lang racket

(require "geeklog.rkt")
(require "ratamarkup.rkt")

(geeklog-settings (make-hash (list
                       '(data-path . "/home/rat/Dropbox/GeekMX/")
                       '(suffixes . (".txt")))))

(define mydoc (geeklog-load-doc "sandbox"))

(display (format "~a\n" (ratamarkup (gldoc-body mydoc))))
