#lang racket

#| geeklog default handlers |#

(provide default-handlers)

(struct gldoc (headers body))

;; print out some server info
(define (handler-cache req path headers settings)
  (format "<html><head><title>Cached</title></head><body><h1>Server info</h1><h2>Cache dump</h2>~a</body></html>\n"
          (string-join
           (for/list ([key (hash-keys (hash-ref settings 'cache null))])
             (format "<p><b>~a</b></p>\n" key)))))

(define (handler-categories req path headers settings)
  (let ([cats (make-hash)]
        [headers (make-hash '((title "Categories")))]
        [body ""])
    (for ([item (hash-ref settings 'cache)])
      (for ([cat (hash-ref 'categories (vector-ref (struct->vector item) 0))])
        (hash-set! cats cat (+ 1 (hash-ref cats cat 0)))))
    (set! body
          (string-join
           (for/list ([sorted (sort (hash-keys cats) string<?)])
             (format "- [[category/~a][~a]] (~a)\n" sorted sorted (hash-ref cats sorted)))))
    (cons "templatify" (cons headers body))))
     
(define default-handlers `(["cached" . ,handler-cache]
                           ["categories" . ,handler-categories]))
