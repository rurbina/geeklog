#lang racket

(require "geeklog.rkt")
(require racket/format)

(geeklog-merge-settings (make-hash (list
                                    '(data-path . "/home/rat/Dropbox/GeekMX/"))))

(map 
 (lambda (item) (printf "[31m== item [m\n~a\n" item))
 (let ([files (vector->list (current-command-line-arguments))])
   (when (empty? files) (set! files '("index")))
   (map (lambda (file) (gldoc-body (geeklog-load-doc file))) files)))

