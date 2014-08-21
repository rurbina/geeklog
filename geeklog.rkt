#lang racket

; geeklog implementation in lisp

(provide geeklog
         (rename-out [load-doc geeklog-load-doc]
                     [merge-settings geeklog-settings])
         (struct-out gldoc))
  
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         xml
         racket/match
         racket/list
         "ratamarkup.rkt")

(define settings (make-hash))

(struct gldoc (headers body))

; load a geeklog document, which is basically headers\n\nbody
(define (load-doc name #:headers-only (nobody #f))
  (let ([filename "non-existing-file"] [whole ""] [doc null] [tmp null] [body null])
    (for ([suffix (hash-ref settings 'suffixes)])
      #:break (file-exists? filename)
      (set! filename (string-append (hash-ref settings 'data-path) name suffix)))
    (if (file-exists? filename)
      (set! whole (file->string filename))
      (error (format "No file: ~v ~v" (hash-ref settings 'data-path) name)))
    (set! tmp (flatten (regexp-match* #px"^(?s:^(.*?)\n\n(.*))$" whole #:match-select cdr)))
    (if nobody
        (set! body (last tmp))
        (set! body (last tmp)))
    (gldoc (parse-headers (first tmp)) body)))
                                 
; parse a geeklog document headers
(define (parse-headers text)
  (let ([lines (regexp-split #px"\n" text)] [headers (make-hash)])
    (map (lambda (line)
           (let ([matches null])
             (set!-values (matches) (flatten 
                                       (regexp-match* 
                                        #px"(?m:^(.*?)\\s*:\\s*(.*)\\s*$)" 
                                        line
                                        #:match-select cdr)))
             (hash-set! headers (string->symbol (first matches)) (last matches))))
         lines)
    headers))
            

(define (geeklog-servlet req)
  (let ([params (make-hash (url-query (request-uri req)))]
        [out-template null]
        [doc (make-hash)]
        [geekdoc null])
    (unless (hash-has-key? params 'doc) (hash-set! params 'doc (hash-ref settings 'default-doc)))
    (set! geekdoc (load-doc (hash-ref params 'doc)))
    (hash-set! doc 'title (hash-ref! (gldoc-headers geekdoc) 'title (hash-ref params 'doc)))
    (hash-set! doc 'body (gldoc-body geekdoc))
    (hash-set! doc 'body (ratamarkup (gldoc-body geekdoc)))
    (for ([key (hash-keys settings)])
      (hash-set! doc 'body (string-append (hash-ref doc 'body)
                                          (format "<p>~a : ~v</p>\n"
                                                  key (hash-ref settings key)))))
    (response/full
     200 #"OK"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (include-template "template.html"))))))

(define (merge-settings new-hash)
  (for ([key (hash-keys new-hash)])
    (hash-set! settings key (hash-ref new-hash key))))

(define (geeklog
         #:template [template "template.html"]
         #:path [base-path "."]
         #:data-path [data-path "data"]
         #:suffixes [suffixes (list ".txt" "")]
         #:404-doc [404-doc "error-404"]
         #:default-doc [default-doc "index"])
  (map (lambda (kv-pair) (hash-set! settings (first kv-pair) (last kv-pair)))
       `((base-path ,base-path)
         (data-path ,data-path)
         (suffixes ,suffixes)
         (template ,template)
         (404-doc ,404-doc)
         (default-doc ,default-doc)))
  (serve/servlet geeklog-servlet
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port 8099
                 #:stateless? #t
                 #:server-root-path base-path
                 #:servlets-root base-path
                 #:servlet-regexp #rx""))
