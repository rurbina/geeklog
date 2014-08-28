#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; geeklog implementation in lisp

(provide geeklog
         (rename-out [load-doc       geeklog-load-doc]
                     [merge-settings geeklog-merge-settings]
                     [settings       geeklog-settings])
         (struct-out gldoc))
  
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         xml
         racket/match
         racket/list
         (rename-in "ratamarkup.rkt" [ratamarkup ratamarkup-process]))

(define settings (make-hash '([base-path         . "."]
                              [data-path         . "data"]
                              [suffixes          . (".txt" "")]
                              [template          . ("template.html")]
                              [404-doc           . "error-404"]
                              [default-transform . ratamarkup]
                              [default-doc       . "index"])))

(define (transform-ratamarkup text) (ratamarkup-process text))

(define transforms (make-hash `([ratamarkup . ,transform-ratamarkup])))

(struct gldoc (headers body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratamarkup expansion stuff

(struct html-table (rows options))
(struct html-table-row (cells))
(struct html-table-cell (content))

(define (html-table-render [table html-table?]
                      #:widths [widths '()]
                      #:class [class "orgtbl"])
  (printf "got table: ~v\n" (struct->vector table))
  (let ([row-num 0])
    (string-join
     (map (lambda (row)
            (set! row-num (add1 row-num))
            (string-join
             (map (lambda (cell) (format "<td>~a</td>" 
                                    (regexp-replace* #px"^\\s+|\\|\\s*|\\s*$|\\s*\\|\\s*$"
                                                     (if (html-table-cell? cell)
                                                         (html-table-cell-content cell)
                                                         cell)
                                                     "")))
                  (if (html-table-row? row) (html-table-row-cells row) row))
             "\n\t\t"))
          (html-table-rows table))
     "\n\t</tr>\n\t<tr>\n\t\t"
     #:before-first "\n<table>\n\t<tr>\n\t\t"
     #:after-last "\n\t</tr>\n</table>\n\n")))

; missing: options, rowspan, widths
(define (rm-orgtbl text
                   #:tokens [tokens '()])
  (let ([table (html-table
                (map (lambda (line)
                       (html-table-row
                        (map (lambda (cell)
                               (html-table-cell cell))
                             (regexp-split #px"(?<!^|\\\\)\\|(?!\\s*$)" line))))
                     (string-split text "\n"))
                '())])
        (html-table-render table)))

(define (rm-blog text #:tokens [tokens '()])
  (for/fold ([output ""])
            ([doc (search-docs #:tags         '(blog)
                               #:no-tags      '(draft)
                               #:sort         'timestamp-parsed
                               #:reverse      #t)])
    (let ([body (parse-body (gldoc-body doc) 'ratamarkup)]
          [top ""]
          [has-break #f]
          [footer ""]
          [break ""])
      (if (regexp-match #px"<!-- break -->" body)
          (begin
            (set! top (regexp-replace #px"(?s:<!-- break -->.*$)" body ""))
            (set! has-break #t))
          (set! top body))
      (set! top (regexp-replace* #px"<(/?)h1>" top "<\\1h2>"))
      (when has-break (set! break (format "<p class=\"blog_break\">~a</p>\n\n"
                                          ((hash-ref (gldoc-headers doc) 'link-format) "Leer el resto"))))
      (set! footer (format "<p class=\"blog_footer\"><i>~a por ~a</i> [~a]</p>\n"
                           (hash-ref (gldoc-headers doc) 'timestamp-format-date)
                           (hash-ref (gldoc-headers doc) 'author)
                           ((hash-ref (gldoc-headers doc) 'link-format) "Permalink")))
      (string-append output top break footer))))

(define (rm-doclist-table text #:tokens [tokens '()])
  (let ([docs '()] [options (make-hash tokens)])
    (hash-set*! options
                'tags (for/list ([tag (regexp-split #px"\\s*,\\s*" (hash-ref options 'tags ""))])
                        (string->symbol tag))
                'fields (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref options 'fields "name")))
    (set! docs (search-docs #:tags (hash-ref options 'tags)))
    (if (empty? docs)
        (ratamarkup-process "No se encontraron documentos.")
        (html-table-render
         (html-table 
          (for/list ([d docs])
            (html-table-row (for/list ([field (hash-ref options 'fields)])
                              (format "~a"
                                      (hash-ref (gldoc-headers d) (string->symbol field) field)))))
          options)))))

(ratamarkup-add-section-processor 'orgtbl        rm-orgtbl)
(ratamarkup-add-section-processor 'blog          rm-blog)
(ratamarkup-add-section-processor 'doclist_table rm-doclist-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a hash and merges it key-by-key with settings
(define (merge-settings new-hash)
  (for ([key (hash-keys new-hash)])
    (hash-set! settings key (hash-ref new-hash key))))

; search for documents
(define (search-docs #:tags            [tags         '()]
                     #:or-tags         [or-tags      '()]
                     #:no-tags         [no-tags      '()]
                     #:headers-only    [headers-only #t]
                     #:sort            [sort-key     'name]
                     #:reverse         [sort-reverse #f])
  (let ([results '()])
    (set! results
          (for/list ([doc (for/list ([file (directory-list (hash-ref settings 'data-path))])
                            (with-handlers ([exn:fail? (lambda (e) void)])
                              (load-doc (path-element->string file) #:headers-only headers-only)))]
                     #:when (and [gldoc? doc]
                                 [or (empty? tags)
                                     (subset? tags (hash-ref (gldoc-headers doc) 'tags '()))]
                                 [or (empty? or-tags)
                                     (not (empty? (set-intersect or-tags (hash-ref (gldoc-headers doc) 'tags '()))))]
                                 [or (empty? no-tags)
                                     (not (subset? no-tags (hash-ref (gldoc-headers doc) 'tags '())))]
                                 #t))
            (printf "search result: ~v\n" (hash-ref (gldoc-headers doc) 'name))
            doc))
    (if sort-reverse (reverse results) results)))

(define (parse-body text transform-type)
  ((hash-ref transforms transform-type) text))
   

; load a geeklog document, which is basically headers\n\nbody
(define (load-doc name #:headers-only (nobody #f))
  (let ([filename "non-existing-file"] [whole ""] [doc null] [tmp null] [body null] [headers null])
    (for ([suffix (hash-ref settings 'suffixes)]
          #:break (file-exists? filename))
      (set! filename (build-path (hash-ref settings 'data-path) (string-append name suffix))))
    (set! whole (file->string filename))
    (set! tmp (flatten (regexp-match* #px"^(?s:^(.*?)\n\n(.*))$" whole #:match-select cdr)))
    (set! headers (parse-headers (first tmp) #:filename filename))
    (if nobody
        (set! body (last tmp))
        (set! body (parse-body (last tmp) (hash-ref headers 'transform (hash-ref settings 'default-transform)))))
    (gldoc headers body)))
                                
(define (make-link href [text ""]
                   #:title [title ""]
                   #:extra [extra ""])
  (when (not (string=? title "")) (set! extra (string-append extra (format " title=\"~a\"" (xexpr->string title)))))
  (when (not (string=? extra "")) (set! extra (xexpr->string (regexp-replace #px"^\\s*" extra " "))))
  (format "<a href=\"~a\"~a>~a</a>"
          href
          extra
          (if (string=? text "") href text)))
          

; parse a geeklog document headers
(define (parse-headers text #:filename [filename ""])
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
    ; some defaults
    (for ([key '(title author tags keywords description comment)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ; some transforms
    (hash-set*! headers
                'tags (for/list ([tag (regexp-split #px"\\s+" (hash-ref headers 'tags ""))]
                                        #:when (> (string-length tag) 0))
                               (string->symbol tag))
                'transform (string->symbol (hash-ref headers 'transform
                                                     (symbol->string (hash-ref settings 'default-transform)))))
    (when (path? filename)
      (hash-set! headers 'name (regexp-replace #px"\\..*$" (path->string (file-name-from-path filename)) "")))
    (for ([key '(author timestamp description keywords)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ; pretty-print items
    (hash-set*! headers
                'link-format (lambda (text) (make-link (hash-ref headers 'name) text
                                                  #:title (hash-ref headers 'title "")))
                'title-link (make-link (hash-ref headers 'name) (hash-ref headers 'title ""))
                'timestamp-format-date (hash-ref headers 'timestamp 123456789))
    headers))

; servlet processing thread
(define (geeklog-servlet req)
  (let ([params (make-hash (url-query (request-uri req)))]
        [out-template null]
        [doc (make-hash)]
        [geekdoc null])
    (unless (hash-has-key? params 'doc) (hash-set! params 'doc (hash-ref settings 'default-doc)))
    (set! geekdoc (load-doc (hash-ref params 'doc)))
    (hash-set! doc 'title (hash-ref! (gldoc-headers geekdoc) 'title (hash-ref params 'doc)))
    (hash-set! doc 'body (gldoc-body geekdoc))
    (response/full
     200 #"OK"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 (include-template "template.html"))))))

; launch the servlet
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
