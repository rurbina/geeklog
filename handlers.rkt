#lang racket

#| geeklog default handlers |#

(provide default-handlers)

(require (only-in geeklog/search search-docs)
         geeklog/structs
         web-server/http/request-structs
         net/url-structs
         sha)

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
        [title    (hash-ref settings 'title (hash-ref settings 'hostname "localhost"))]
        [get-name (lambda (doc) (hash-ref (gldoc-headers doc) 'name))]
        [base-uri null]
        [docs (search-docs #:tags       '(blog) ;(hash-ref blog-options 'tags '(blog))
                           #:no-tags    '(draft) ;(hash-ref blog-options 'no-tags '(draft))
                           #:sort       'timestamp ;(hash-ref blog-options 'sort 'timestamp)
                           #:reverse    #t ;(if (hash-has-key? blog-options 'reverse) #f #t)
                           #:no-future  #t ;(if (hash-has-key? blog-options 'future) #f #t)
                           #:newer-than 0 ;(if (hash-has-key? blog-options 'no-past) (current-seconds) 0)
                           #:settings   settings)])
    (set! base-uri (format "~a://~a" scheme hostname))
    (hash-set! result 'mime-type #"application/atom+xml")
    ;(hash-set! result 'mime-type #"text/plain; charset=utf-8")
    (hash-set! result 'output
              (string-join
               (list
                "<?xml version=\"1.0\"?>"
                "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
                (format "<link href=\"~a~a\" rel=\"self\"/>" base-uri path)
                (format "<title>~a</title>" title)
                ;; FIXME: required
                "<updated>2003-12-13T18:30:02Z</updated>"
                ""
                ;; "<author><name>John Doe</name></author>"
                (format "<id>~a~a</id>" base-uri path)
                (string-join
                 (for/list ([doc docs])
                   (let ([uri (string-join (list base-uri (get-name doc)) "/")]
                         [summary (lambda (doc)
                                    (let ([body (gldoc-body doc)])
                                      (if (regexp-match #px"(?m:^§more)" body)
                                          (regexp-replace #px"(?s:\n§more.*)" body "")
                                          (substring body 0 50))))])
                     (string-join
                      (list
                       "<entry>"
                       (format "<title>~a</title>" (hash-ref (gldoc-headers doc) 'title))
                       (format "<link href=\"~a\"/>" uri)
                       (format "<id>~a</id>" uri);(bytes->hex-string (sha1 (string->bytes/utf-8 uri))))
                       (format "<author><name>~a</name></author>" (hash-ref (gldoc-headers doc) 'author))
                       (format "<updated>~a</updated>" (hash-ref (gldoc-headers doc) 'timestamp-iso))
                       (format "<summary>~a</summary>\n</entry>" (summary doc)))
                      "\n\t")))
                 "\n\n")
                "</feed>")
               "\n"))
    result))

(define default-handlers `(["cached" . ,handler-cache]
                           ["categories" . ,handler-categories]
                           ["feed" . ,handler-feed]))
