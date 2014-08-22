#lang racket

(require "geeklog.rkt")
(require "ratamarkup.rkt")
(require racket/format)

(geeklog-settings (make-hash (list
                       '(data-path . "/home/rat/Dropbox/GeekMX/")
                       '(suffixes . (".txt")))))

(map 
 (lambda (item) (printf "[31m==> item [m\n~a\n" item))
 (let ([files (vector->list (current-command-line-arguments))])
   (when (empty? files) (set! files '("index")))
   (map
    (lambda (file)
      (ratamarkup
       (gldoc-body
        (geeklog-load-doc file))))
    files)))

