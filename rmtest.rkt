#lang racket

(require geeklog/geeklog
         racket/format)

(geeklog-merge-settings #hash([base-path . "/home/rat/src/geeklog"]
                              [data-path . "geekmx"]))

(geeklog-merge-settings #hash([base-path . "/home/rat/src/geeklog"]
                              [data-path . "96grados"]))

(map 
 (lambda (item) (printf "[31m== item [m\n~a\n" item))
 (let ([files (vector->list (current-command-line-arguments))])
   (when (empty? files) (set! files '("index")))
   (map (lambda (file) (gldoc-body (geeklog-load-doc file))) files)))

