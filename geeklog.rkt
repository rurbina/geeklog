#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; geeklog implementation in lisp

(provide geeklog
         (rename-out [load-doc          geeklog-load-doc]
                     [merge-settings    geeklog-merge-settings]
                     [default-settings  geeklog-default-settings]
                     [site-settings     geeklog-site-settings])
         (struct-out gldoc))

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         xml
         racket/match
         racket/list
         racket/date
         scribble/decode
         (rename-in ratamarkup/ratamarkup [ratamarkup ratamarkup-process]))

(define (default-format-date epoch #:timezone [tz "UTC"])
  (let ([d (seconds->date epoch)]
        [dow    #hash([0 . "Domingo"]
                      [1 . "Lunes"]
                      [2 . "Martes"]
                      [3 . "Miércoles"]
                      [4 . "Jueves"]
                      [5 . "Viernes"]
                      [6 . "Sábado"])]
        [months #hash([1 . "Enero"]
                      [2 . "Febrero"]
                      [3 . "Marzo"]
                      [4 . "Abril"]
                      [5 . "Mayo"]
                      [6 . "Junio"]
                      [7 . "Julio"]
                      [8 . "Agosto"]
                      [9 . "Septiembre"]
                      [10 . "Octubre"]
                      [11 . "Noviembre"]
                      [12 . "Diciembre"])]
        [now (seconds->date (current-seconds))])
    (if (= (date-year now) (date-year d))
        (format "~a ~a de ~a"
                (hash-ref dow (date-week-day d))
                (date-day d)
                (hash-ref months (date-month d)))
        (format "~a ~a de ~a de ~a"
                (hash-ref dow (date-week-day d))
                (date-day d)
                (hash-ref months (date-month d))
                (date-year d)))))

(define (default-format-time epoch #:timezone [tz "UTC"])
  (let ([d (seconds->date epoch)])
    (format "~a:~a ~a"
            (cond [(= (date-hour d) 0) 12]
                  [(> (date-hour d) 12) (modulo (date-hour d) 12)]
                  [else (date-hour d)])
            (substring (format "~v" (+ 100 (date-minute d))) 1)
            (cond [(< (date-hour d) 12) "am"]
                  [else "pm"]))))

(define (default-format-date-time epoch #:timezone [tz "UTC"])
  (string-replace (format "~a a las ~a"
                          (default-format-date epoch #:timezone tz)
                          (default-format-time epoch #:timezone tz))
                  "a las 1:"
                  "a la 1:"))

(define default-settings
  (make-hash `([base-path         . "."]
               [data-path         . "data"]
               [suffixes          . (".txt" "")]
               [template          . "template.html"]
               [404-doc           . "error-404"]
               [default-transform . ratamarkup]
               [default-doc       . "index"]
               [format-date       . ,default-format-date]
               [format-time       . ,default-format-time]
               [format-date-time  . ,default-format-date-time])))

(define site-settings
  (make-hash '([default settings])))

(ratamarkup-set!-link-callback
 (lambda (link #:options [options #hash()])
   (let ([m (flatten (regexp-match* "^\\[\\[(.*)?(?<!\\\\)\\|(.*?)(?<!\\\\)\\|(.*?)]]$"
                                    link
                                    #:match-select cdr
                                    ))])
     (let ([href (first m)]
           [params (if (= (string-length (second m)) 0) ""
                       (string-append " " (regexp-replace* #px"&quot;" (second m) "\"")))]
           [text (third m)]
           [docname (regexp-replaces (string-downcase (first m))
                                     '([#px" " "_"]
                                       [#px"[á]" "a"]
                                       [#px"[ó]" "o"]
                                       [#px"[ñ]" "n"]))]
           [doc null])
       (unless (regexp-match? #px"^(http:|https:|)//" (first m))
         (set! doc (load-doc docname #:headers-only #t #:settings (hash-ref options 'geeklog-settings default-settings))))
       (when (gldoc? doc)
         (let ([headers (gldoc-headers doc)])
           (set! href docname)
           (when (string=? params "")
             (set! params (format " title=\"~a\""
                                  (hash-ref headers 'title (hash-ref headers 'name docname)))))))
       (if (and (not (gldoc? doc)) (not (regexp-match? #px"^(http:|https:|)//" href)))
         (format "<span class=\"notfound\" data-href=\"~a\">~a</span>"
                 docname
                 text)
         (format "<a href=\"~a\"~a>~a</a>"
                 href
                 params
                 text))))))

(define (transform-ratamarkup text
                              #:settings [settings default-settings]
                              #:options [options (make-hash `((geeklog-settings . ,default-settings)))])
  (unless (eq? settings default-settings)
    (hash-set! options 'geeklog-settings settings))
  (ratamarkup-process text #:options options))

(define transforms (make-hash `([ratamarkup . ,transform-ratamarkup])))

(struct gldoc (headers body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ratamarkup expansion stuff

;; structures for the table renderer
(struct html-table (rows options))
(struct html-table-row (cells))
(struct html-table-row-header html-table-row (header))
(struct html-table-cell (content))
(struct html-table-cell-style html-table-cell (style))

;; the table renderer
(define (html-table-render [table html-table?]
                      #:widths [widths '()]
                      #:class [class "orgtbl"]
                      #:process [procfn (lambda (t) t)]
                      #:first-row-header [frh #t])
  (let ([row-num 0] [cell-num 0])
    (string-join
     (for/list ([row (html-table-rows table)])
       (set! row-num (add1 row-num))
       (set! cell-num 0)
       (string-join
        (for/list ([cell (if (html-table-row? row) (html-table-row-cells row) row)])
          (set! cell-num (add1 cell-num))
          (format
           (if (or [and frh (= row-num 1)]
                   [and (html-table-row-header? row) (html-table-row-header-header row)])
               "<th~a>~a</th>"
               "<td~a>~a</td>")
           (cond [(html-table-cell-style? cell)
                  (format " style=\"~a\"" (html-table-cell-style cell))]
                 [(>= (length widths) cell-num)
                  (format " style=\"width:~a\""
                          (regexp-replace
                           #px"^\\s*\\*\\s*$|^\\s*$"
                           (car (drop widths (sub1 cell-num)))
                           "auto"))]
                 [else " data-nostylegiven=\"1\""])
           (procfn (regexp-replace* #px"^\\s+|\\|\\s*|\\s*$|\\s*\\|\\s*$"
                                    (if (html-table-cell? cell)
                                        (html-table-cell-content cell)
                                        cell)
                                    ""))))
        "\n\t\t"))
     "\n\t</tr>\n\t<tr>\n\t\t"
     #:before-first "\n<table>\n\t<tr>\n\t\t"
     #:after-last "\n\t</tr>\n</table>\n\n")))

;; TODO: some options?
(define (rm-orgtbl text
                   #:options [options #hash()]
                   #:tokens [tokens '()])
  (let ([topts (make-hash tokens)] [table null])
    (set! table
          (html-table
           (if (hash-has-key? topts 'rowspan)
               ;; this is for row-spanning tables
               (let ([rows (string-split text #px"(?sm:^\\|(-+\\+)?-+\\|$)")]
                     [row-parts '()] [cell-acc '()])
                 (set! rows (for/list ([row rows] #:unless (regexp-match? #px"^\\s*$" row))
                              (regexp-replace* #px"^\n" row "")))
                 (for/list
                     ([row-cells-list
                       (for/list ([row rows])
                         (let
                             ([composite (for/list
                                             ([part (regexp-split #px"(?s:\n)" row)]
                                              #:unless (string=? part ""))
                                           (set! part (regexp-replace* #px"^\\| |\\s+\\|$" part ""))
                                           (for/list
                                               ([cell (regexp-split #px"\\s+\\|\\s" part)])
                                             (regexp-replace* #px"^\\| |\\s+\\|" cell "")))])
                           (if (> (length composite) 1)
                               (for/fold ([folded '()])
                                         ([row composite])
                                 (build-list (max (length folded) (length row))
                                             (lambda (n) (append
                                                     (if (> (length folded) n)
                                                         (first (drop folded n))
                                                         '())
                                                     (if (> (length row) n)
                                                         (list (first (drop row n)))
                                                         '())))))
                               (for/list ([i (first composite)]) (list i)))))])
                   (html-table-row (for/list ([cell-list row-cells-list])
                                     (html-table-cell
                                      (ratamarkup-process
                                       (string-join cell-list "\n")
                                       #:options options))))))
               ;; and this one for normal row-per-line tables
               (for/list ([line (string-split text "\n")]
                          #:unless (regexp-match? #px"^[|+]-{2,}" line))
                 (html-table-row
                  (for/list ([cell (regexp-split #px"(?<!^|\\\\)\\|(?!\\s*$)" line)])
                    (html-table-cell
                     (ratamarkup-inline
                      (regexp-replace* #px"^\\s*\\|\\s*|\\s*$|\\s*\\|\\s*$" cell "")
                      #:options options))))))
           '()))
    (html-table-render table #:widths (regexp-split #px"\\s*,\\s*" (hash-ref topts 'widths "")))))

(define (rm-blog text
                 #:options [options (make-hash '((null . null)))]
                 #:tokens  [tokens '()])
  (let ([settings (hash-ref options 'geeklog-settings default-settings)])
    (for/fold ([output ""])
              ([doc (search-docs #:tags      '(blog)
                                 #:no-tags   '(draft)
                                 #:sort      'timestamp
                                 #:reverse   #t
                                 #:no-future #t
                                 #:settings settings)])
      (let ([body (parse-body (gldoc-body doc)
                              'ratamarkup
                              #:settings settings)]
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
                             (hash-ref (gldoc-headers doc) 'timestamp-format-date-time)
                             (hash-ref (gldoc-headers doc) 'author)
                             ((hash-ref (gldoc-headers doc) 'link-format) "Permalink")))
        (string-append output top break footer)))))

(define (rm-doclist-table text
                          #:options [options (make-hash '((null . null)))]
                          #:tokens [tokens '()])
  (let ([docs '()] [search-options (make-hash tokens)]
        [fixlist (lambda (txt) (for/list ([tag (regexp-split #px"\\s*,\\s*" txt)]
                                     #:unless (string=? "" tag))
                            (string->symbol tag)))]
        [fixlists null])
    (set! fixlists (lambda (hash list) (flatten (for/list ([key list]
                                                      #:unless (string=? "" (hash-ref hash key "")))
                                             (fixlist (hash-ref hash key ""))))))
    (hash-set*! search-options
                'tags    (fixlist (hash-ref search-options 'tags ""))
                'or-tags (fixlist (hash-ref search-options 'or-tags ""))
                'no-tags (fixlists search-options '(not-tag not-tags no-tag no-tags
                                                    not_tag not_tags no_tag no_tags))
                'sort    (string->symbol (hash-ref search-options 'sort "name"))
                'fields  (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'fields "name"))
                'headers (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'headers "Item"))
                'widths  (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'widths "auto")))
    (set! docs (search-docs #:tags      (hash-ref search-options 'tags)
                            #:no-tags   (hash-ref search-options 'no-tags)
                            #:or-tags   (hash-ref search-options 'or-tags)
                            #:sort      (hash-ref search-options 'sort)
                            #:settings  (hash-ref options 'geeklog-settings default-settings)))
    (if (empty? docs)
        (ratamarkup-process "No se encontraron documentos." #:options options)
        (html-table-render
         (html-table
          (append
           (list (html-table-row-header (hash-ref search-options 'headers (hash-ref search-options 'fields)) #t))
           (for/list ([d docs])
             (html-table-row (for/list ([field (hash-ref search-options 'fields '(name))])
                               (format "~a"
                                       (hash-ref (gldoc-headers d) (string->symbol field) field))))))
          options)
         #:widths (hash-ref search-options 'widths)))))

(define (rm-soundcloud text
                          #:options [options (make-hash '((null . null)))]
                          #:tokens [tokens '()])
  "<!-- soundcloud -->\n\n")


(ratamarkup-add-section-processor 'orgtbl            rm-orgtbl)
(ratamarkup-add-section-processor 'blog              rm-blog)
(ratamarkup-add-section-processor 'doclist_table     rm-doclist-table)
(ratamarkup-add-section-processor 'soundcloud_player rm-soundcloud)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; takes a hash and merges it key-by-key with settings
(define (merge-settings new-hash (site null))
  (let ([settings null])
    (if (eq? site null)
        (set! settings default-settings)
        (begin
          (unless (hash-has-key? site-settings site)
            (hash-set! site-settings site (make-hash (for/list ([key (hash-keys default-settings)])
                                                       (cons key (hash-ref default-settings key))))))
          (set! settings (hash-ref site-settings site))))
    (for ([key (hash-keys new-hash)])
      (hash-set! settings key (hash-ref new-hash key)))))

; search for documents
(define (search-docs
         #:tags            [tags         '()]
         #:or-tags         [or-tags      '()]
         #:no-tags         [no-tags      '()]
         #:headers-only    [headers-only #t]
         #:sort            [sort-key     'name]
         #:reverse         [sort-reverse #f]
         #:no-future       [no-future    #f]
         #:settings        [settings     default-settings]
         #:data-path       [data-path    null])
  (when (null? data-path)
    (set! data-path (string->path (string-append (hash-ref settings 'base-path) "/" (hash-ref settings 'data-path)))))
  (let ([results '()] [file-path null] [now (current-seconds)])
    (set! results
          (for/list ([doc (for/list ([file (directory-list data-path)])
                            (set! file-path (build-path data-path file))
                            ;;(load-doc (path->string file-path) #:headers-only headers-only))]
                            (with-handlers ([exn:fail? (lambda (e)
                                                         (printf "\t\terror is ~v\n" e)
                                                         void)])
                              (load-doc (path->string file-path)
                                        #:path file-path
                                        #:headers-only headers-only)))]
                     #:when (and [gldoc? doc]
                                 [or (empty? tags)
                                     (subset? tags (hash-ref (gldoc-headers doc) 'tags '()))]
                                 [or (empty? or-tags)
                                     (not (empty? (set-intersect or-tags (hash-ref (gldoc-headers doc) 'tags '()))))]
                                 [or (empty? no-tags)
                                     (not (subset? no-tags (hash-ref (gldoc-headers doc) 'tags '())))]
                                 [or (< (hash-ref (gldoc-headers doc) 'timestamp (+ now 1)) now)
                                     (not no-future)]
                                 #t))
            doc))
    ;; sorting
    (set! results (sort results (lambda (a b) (gldoc-sort a b sort-key))))
    (if sort-reverse (reverse results) results)))

(define (gldoc-sort a b key)
  (let ([dk (lambda (doc key) (hash-ref (gldoc-headers doc) key
                                       (hash-ref (gldoc-headers doc) 'name)))])
    (let ([ka (dk a key)]
          [kb (dk b key)])
      (cond [(and (number? ka) (number? kb)) (< ka kb)]
            [else (string<? ka kb)]))))

(define (parse-body text
                    transform-type
                    #:settings [settings default-settings]
                    #:options [options #hash()])
  ((hash-ref transforms transform-type) text #:settings settings #:options (make-hash `((geeklog-settings . ,settings)))))


; load a geeklog document, which is basically headers\n\nbody
(define (load-doc name
                  #:path         [path null]
                  #:headers-only [nobody #f]
                  #:settings     [settings default-settings])
  (let ([filename path] [whole ""] [doc null] [tmp null] [body null] [headers null] [search-path null])
    (set! search-path (build-path (hash-ref settings 'base-path) (hash-ref settings 'data-path)))
    (when (path? path) (set! name (last (explode-path path))))
    (when (null? filename) (set! filename "non-existing-file"))
    (for ([suffix (hash-ref settings 'suffixes)]
          #:break (file-exists? filename))
      (set! filename (build-path (hash-ref settings 'base-path) (hash-ref settings 'data-path) (string-append name suffix))))
    (unless (file-exists? filename) (error (format "document ~v not found in ~v" name search-path)))
    (set! whole (file->string filename))
    (set! tmp (flatten (regexp-match* #px"^(?s:^(.*?)\n\n(.*))$" whole #:match-select cdr)))
    (set! headers (parse-headers (first tmp) #:filename filename #:settings settings))
    (if nobody
        (set! body (last tmp))
        (set! body (parse-body (last tmp)
                               (hash-ref headers 'transform (hash-ref settings 'default-transform))
                               #:settings settings)))
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
(define (parse-headers text
                       #:filename [filename ""]
                       #:settings [settings default-settings])
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
    ;; some defaults
    (for ([key '(title author tags keywords description comment)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ;; some transforms
    (hash-set*! headers
                'tags (for/list ([tag (regexp-split #px"\\s+" (hash-ref headers 'tags ""))]
                                 #:when (> (string-length tag) 0))
                        (string->symbol tag))
                'transform (string->symbol
                            (hash-ref headers 'transform
                                      (symbol->string (hash-ref settings 'default-transform)))))
    (when (path? filename)
      (hash-set! headers
                 'mtime
                 (file-or-directory-modify-seconds filename))
      (hash-set! headers
                 'name
                 (regexp-replace #px"\\..*$" (path->string (file-name-from-path filename)) "")))
    (for ([key '(author timestamp description keywords)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ;; parse the timestamp
    (hash-set!
     headers
     'timestamp
     (let ([ts (hash-ref headers 'timestamp)])
       (cond [(regexp-match? #px"^\\s*(\\d+)\\s*$" ts) (string->number ts)]
             [(regexp-match? #px"\\d{4}.\\d{2}.\\d{2}" ts)
              (let ([m (regexp-match
                        #px"(\\d{4}).(\\d{2}).(\\d{2})\\s*T?((\\d+):?(\\d+)?:?(\\d+)?(\\s*([AaPp][Mm]))?)?"
                        ts)]
                    [convert (lambda (i) (if (string? i) (or (string->number i) 0) 0))]
                    [d null])
                (find-seconds (convert (eighth m)) ;second
                              (convert (seventh m)) ;minute
                              (convert (sixth m)) ;hour
                              (string->number (fourth m)) ;day
                              (string->number (third m)) ;month
                              (string->number (second m)) ;year
                              #t ;localtime
                              ))]
             [else (hash-ref headers 'mtime 0)])))
    ;; these are some pretty-print items
    (hash-set*!
     headers
     'link-format (lambda (text) (make-link (hash-ref headers 'name) text
                                       #:title (hash-ref headers 'title "")))
     'title-link (make-link (hash-ref headers 'name)
                            (hash-ref headers 'title "")
                            #:title (hash-ref headers 'title ""))
     'mtime-format-date ((hash-ref settings 'format-date) (hash-ref headers 'mtime))
     'timestamp-format-date ((hash-ref settings 'format-date) (hash-ref headers 'timestamp))
     'timestamp-format-date-time ((hash-ref settings 'format-date-time) (hash-ref headers 'timestamp)))
    headers))

;; servlet processing thread
(define (geeklog-servlet req)
  (let ([params (make-hash (url-query (request-uri req)))]
        [out-template null]
        [doc (make-hash)]
        [tns (make-base-namespace)]
        [geekdoc null]
        [script (path/param-path (first (url-path (request-uri req))))]
        [settings null]
        [template-path null])
    (set! settings
          (cond [(hash-has-key? site-settings script) (hash-ref site-settings script)]
                [(and (hash-has-key? params 'config)
                      (hash-has-key? site-settings (string->symbol (hash-ref params 'config))))
                 (hash-ref site-settings (string->symbol (hash-ref params 'config)))]
                 [else default-settings]))
    (unless (hash-has-key? params 'doc) (hash-set! params 'doc (hash-ref settings 'default-doc)))
    (set! geekdoc (load-doc (hash-ref params 'doc (hash-ref settings 'default-doc)) #:settings settings))
    (hash-set! doc 'title (hash-ref! (gldoc-headers geekdoc) 'title (hash-ref params 'doc)))
    (hash-set! doc 'body (gldoc-body geekdoc))
    (eval `(require web-server/templates) tns)
    (namespace-set-variable-value! 'doc doc #f tns)
    (namespace-set-variable-value! 'gldoc gldoc #f tns)
    (namespace-set-variable-value! 'gldoc-body gldoc-body #f tns)
    (namespace-set-variable-value! 'load-doc (lambda (name) (load-doc name #:settings settings)) #f tns)
    (namespace-set-variable-value! 'default-settings settings #f tns)
    ;; template path must be relative, don't ask me why
    (set! template-path (build-path (hash-ref settings 'base-path ".") (hash-ref settings 'template)))
    (set! template-path (find-relative-path (current-directory) template-path))
    (set! out-template (eval `(include-template ,(path->string template-path)) tns))
    (response/full
     200 #"OK"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8 out-template)))))
     ;(list (string->bytes/utf-8 (include-template (hash-ref settings 'template)))))))
     ;(list (string->bytes/utf-8 (include-template "template.html"))))))

;; launch the servlet
(define (geeklog
         #:template    [template "template.html"]
         #:path        [base-path "."]
         #:data-path   [data-path "data"]
         #:suffixes    [suffixes (list ".txt" "")]
         #:404-doc     [404-doc "error-404"]
         #:default-doc [default-doc "index"]
         #:settings    [settings default-settings])
  (map (lambda (kv-pair) (hash-set! settings (first kv-pair) (last kv-pair)))
       `([base-path ,base-path]
         [data-path ,data-path]
         [suffixes ,suffixes]
         [404-doc ,404-doc]
         [default-doc ,default-doc]))
  (serve/servlet geeklog-servlet
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port 8099
                 #:stateless? #t
                 #:server-root-path base-path
                 #:servlets-root base-path
                 #:servlet-regexp #rx""))
