#lang racket

#| geeklog default handlers |#

(provide default-handlers)

(struct gldoc (headers body))

(define (render-cell cell) (format "  <td>~a</td>\n" cell))

(define (render-row cells)
  (string-join 
   (list " <tr>\n"
         (string-join (for/list ([cell cells]) (render-cell cell)))
         " </tr>\n")))

(define (render-table rows)
  (string-join
   (list "<table>\n"
         (string-join (for/list ([row rows]) (render-row row)))
         "</table>\n")))


;; print out cached doc table
(define (handler-cache req path headers settings)
  (format
   "<html><head><title>Cached</title></head><body><h1>Server info</h1><h2>Cache dump</h2>~a</body></html>\n"
   (render-table
    (append (list)
            (list (list "Documento" "Fecha" "Tags" "Categorias"))
            (for/list ([key (hash-keys (hash-ref settings 'cache null))])
              (let ([headers (vector-ref (struct->vector (hash-ref (hash-ref settings 'cache) key)) 0)])
                (list (format "~a" key)
                      (format "~a" (hash-ref headers 'mtime))
                      null
                      null)))))))

;; print out categories
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
