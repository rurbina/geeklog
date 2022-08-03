#lang racket

#| geeklog default handlers |#

(provide default-handlers)

(require (only-in geeklog/search search-docs)
         (only-in geeklog/markup
                  markup
                  render-at)
         (rename-in scribble/reader [read read-at])
         geeklog/structs
         geeklog/load
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

(define (handler-rendertest #:request r #:path p #:settings s)
  (let ([headers (make-hash '([name "rendertest"]
                              [title "Render test"]))]
        [body ""]
        [template #<<eot
* Render test

This is a test for the minirenderer.

eot
                  ])
    (set! body (render-at template))
    (eprintf "\t\t\t\e[36mrendertst: ~v\e[0m\n" body)
    ;(set! body template)
    (set! body (markup body 'ratamarkup #:settings s))
    (cons "templatify" (cons headers body))))

;; print out cached doc table
(define (handler-cache #:request req
                       #:path path
                       #:settings settings)
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
(define (handler-categories #:request req
                            #:path path
                            #:settings settings)
  (let ([cats (make-hash)]
        [headers (make-hash '((title "Categories")))]
        [body ""]
        [doc-cats (lambda (doc) (hash-ref (gldoc-headers doc) 'categories))]
        [docs (search-docs #:settings settings #:headers-only #t)])
    (for ([item docs])
      (for ([cat (doc-cats item)])
        (hash-set! cats cat (+ 1 (hash-ref cats cat 0)))))
    ;; gotta cache 'em all
    ;; TBD
    (set! body
          (string-join
           (for/list ([cat (sort (hash-keys cats) string<?)])
             (format "- [[category/~a][~a]] (~a)" cat cat (hash-ref cats cat)))
           "\n"))
    (eprintf "\t\t\thandler-categories:\n\e[1m~a\e[0m\n" body)
    (set! body (markup body 'ratamarkup #:settings settings))
    (cons "templatify" (cons headers body))))

;; print out single category
(define (handler-category #:request req #:path path #:settings settings)
  (let ([category (second (string-split (first (string-split path "?")) "/"))])
    (let ([docs (search-docs #:or-tags (list (string->symbol category)) #:settings settings)]
          [doc-name (lambda (doc) (hash-ref (gldoc-headers doc) 'name))]
          [doc-title (lambda (doc) (hash-ref (gldoc-headers doc) 'title (hash-ref (gldoc-headers doc) 'name)))]
          [tt (hash-ref (hash-ref settings 'templates) 'category)]
          [tt-empty (hash-ref (hash-ref settings 'templates) 'category-empty)])
      (let ([headers (make-hash '((title "Categoría ~a" category)))]
            [body (render-at tt #:data `((items . ,docs)
                                         (category . ,category)
                                         (tt-empty . ,tt-empty)))])
        (set! body (markup body 'ratamarkup #:settings settings))
        (cons "templatify" (cons headers body))))))

(define (handler-tags #:request r #:path p #:settings s)
  (let ([headers (make-hash `([name . "tags"]
                              [title . "Tags"]))]
        [body "Not implemented :)"])
    (cons "templatify" (cons headers body))))

;; print out cache dump
(define (handler-cache-dump #:request r #:path p #:settings s)
  (let ([headers (make-hash `([name . "cachedump"]
                              [title . "Cache Dump"]))]
        [body ""]
        [cache (cache-dump)])
    (set! body (string-join (for/list ([i cache])
                              (format "- ~a:~a(~a)\n" (hash-ref i 'source) (hash-ref i 'path) (hash-ref i 'mtime))) ""))
    (set! body (string-join (list "* Cache Dump\n\n" body "\n\n") ""))
    (cons "templatify" (cons headers body))))
  

;; print out a feed
(define (handler-feed #:request req #:path path #:settings settings)
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

(define default-handlers `(["category/" . ,handler-category]
                           ["cached" . ,handler-cache]
                           ["categories!" . ,handler-categories]
                           ["tags!" . ,handler-tags]
                           ["feed" . ,handler-feed]
                           ["rendertest" . ,handler-rendertest]
                           ["cachedump" . ,handler-cache-dump]))
