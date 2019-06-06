#lang racket

#| geeklog default handlers |#

(provide default-handlers)

(require geeklog/search
         web-server/http/request-structs
         net/url-structs)

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

;; print out a feed
(define (handler-feed req path headers settings)
  (let ([result (make-hash)]
        [hostname (hash-ref settings 'hostname "localhost")]
        [scheme   (hash-ref settings 'scheme   "http")]
        [title    (hash-ref settings 'title (hash-ref settings 'hostname "localhost"))])
    (printf "\n\nFEED settings: ~a\n\n" settings)
    (hash-set! result 'mime-type #"application/atom+xml")
    (hash-set! result 'mime-type #"text/plain")
    (hash-set! result 'output
              (string-join
               (list
                "<?xml version=\"1.0\"?>"
                "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
                (format "<link href=\"~a://~a~a\" rel=\"self\"/>" scheme hostname path)
                (format "<title>~a</title>" title)
                ;; FIXME: required
                "<updated>2003-12-13T18:30:02Z</updated>"
                ;; "<author><name>John Doe</name></author>"
                ;;"<id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>"
                "<entry>"
                "<title>Atom-Powered Robots Run Amok</title>"
                "<link href=\"http://example.org/2003/12/13/atom03\"/>"
                "<id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>"
                "<updated>2003-12-13T18:30:02Z</updated>"
                "<summary>Some text.</summary>"
                "</entry>"
                "</feed>")
               "\n"))
    result))

(define default-handlers `(["cached" . ,handler-cache]
                           ["categories" . ,handler-categories]
                           ["feed" . ,handler-feed]))
