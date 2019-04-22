#lang racket

#| geeklog default handlers |#

(provide (rename-out [geeklog/handlers handlers]))

(require geeklog)

(define handlers '(["info" . ,info]))

;; print out some server info
(define (info req path headers settings)
  (format "<html><head><title>~a</title></head><body><h1>Server info</h1><h2>settings</h2>~a</body></html>"
          (cdr (first (filter (lambda (h) (symbol=? (car h) 'host)) headers)))
          (string-join
           (for/list ([h (hash-keys site-settings)])
             (format "<h3>~a</h3><dl>~a</dl>" h
                     (string-join
                      (for/list ([k (hash-keys (hash-ref site-settings h))])
                        (format "<dt>~a</dt><dd>~v</dd>" k (hash-ref (hash-ref site-settings h) k))))))
           "\n")))
