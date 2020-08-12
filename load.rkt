#lang racket/base

(require geeklog/structs
         racket/date
         racket/list
         racket/string
         racket/file
         racket/path
         racket/fasl
         xml
         tzinfo
         db
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup ratamarkup-process]
                    [ratamarkup-inline ratamarkup-process-inline]))

(define geeklog/load #t)

;; metadata cache database
(define metadb (sqlite3-connect #:database "meta.db" #:mode 'create))

;; regenerate database if needed
;; TBD: proper handlers for mismatched databases and such
(with-handlers
  ([exn:fail? (lambda (e) (eprintf "reusing existing metadb\n"))])
  (query-exec
   metadb
   "	create table metadata (
		path string,
		mtime string,
		headers text,
		summary text,
		body text,
		source text not null default \"file\"
	);"))

;; dump cache database
(define (metadb-dump)
  (for/list ([row (query-rows metadb "select path,mtime,headers,summary,body,source from metadata")])
    ;; newbie
    (make-hash (list
                (cons 'path    (vector-ref row 0))
                (cons 'mtime   (vector-ref row 1))
                (cons 'headers (vector-ref row 2))
                (cons 'summary (vector-ref row 3))
                (cons 'body    (vector-ref row 4))
                (cons 'source  (vector-ref row 5))))))

;; delete document from cache
(define (metadb-delete path)
  (when (path? path) (set! path (path->string path)))
  (query-exec metadb "delete from metadata where path = $1" path))

;; insert a document in the cache
(define (metadb-push #:path path
                     #:mtime mtime
                     #:headers all-headers
                     #:summary [summary sql-null]
                     #:body [body sql-null]
                     #:source [source "file"])
  (let ([headers (make-hash)])
    ;; some headers cannot be fast-serialized, remove them
    (for ([key (hash-keys all-headers)])
      (if (member key (list 'link-format))
          (hash-set! headers key null)
          (hash-set! headers key (hash-ref all-headers key))))
    ;; purgue previous row, if it exists
    (metadb-delete path)
    (eprintf "\e[35m<metadb-push:~a>\e[0m " path)
    (query-exec metadb "insert into metadata (path, mtime, headers, summary, body, source) values ($1,$2,$3,$4,$5,$6)"
                (if (path? path) (path->string path) path)
                mtime
                (s-exp->fasl headers)
                sql-null
                (if (null? body) sql-null body)
                source)))

;; read a document from cache
(define (metadb-get path)
  (let ([doc null]
        [row
         (query-maybe-row
          metadb
          "	select mtime, path, headers, body, summary, source
		from metadata
		where path = $1"
          (if (path? path) (path->string path) path))])
    (when (vector? row)
      (set! doc (make-hash (map cons
                                '(mtime path headers body summary source)
                                (vector->list row))))
      (hash-set! doc 'headers (fasl->s-exp (hash-ref doc 'headers)))
      ;; inject back non-fast-serializable headers
      (let ([headers (hash-ref doc 'headers)])
        (hash-set* headers
                   'link-format (lambda (text) (make-link (hash-ref headers 'name) text
                                                          #:title (hash-ref headers 'title "")))))
      (when (eq? sql-null (hash-ref doc 'body)) (hash-set! doc 'body ""))
      (when (eq? sql-null (hash-ref doc 'summary)) (hash-set! doc 'summary "")))
    doc))

;; check if document is in cache - returns #f or mtime in cache
(define (metadb-check #:path filename
                      #:headers-only [headers-only #f]
                      #:summary-only [summary-only #f])
  (query-maybe-value
   metadb
   "select mtime from metadata where path = $1
	and case $2
		when 'headers' then headers is not null
		when 'body' then body is not null
		when 'summary' then 0
		else 1 end"
   (path->string filename)
   (cond [(not (or headers-only summary-only)) "body"]
         [summary-only "summary"]
         [headers-only "headers"]
         [else "any"])))

;; remove accents and stuff
(define (unaccent-string text)
  (regexp-replaces (string-downcase text)
                   '([#px" " "_"]
                     [#px"[á]" "a"]
                     [#px"[é]" "e"]
                     [#px"[í]" "i"]
                     [#px"[ó]" "o"]
                     [#px"[ú]" "u"]
                     [#px"[ü]" "u"]
                     [#px"[ñ]" "n"])))

;; load a file from disk -- or from database if cached
(define (load-doc name
                  #:path            [path null]
                  #:path-prefix     [path-prefix null]
                  #:headers-only    [headers-only #f] ; do not process body, only headers
                  #:unparsed        [unparsed #f] ; load unparsed body
                  #:summary-only    [summary-only #f] ; do not parse whole body, only summary
                  #:summary-stop    [summary-stop #px"(?m:^§more)"] ; regexp to determine summary's end
                  #:summary-chars   [summary-chars 200] ; or how many chars to load as summary if regexp fails
                  #:no-cache        [no-cache #f] ; disable caching
                  #:settings        settings)
  (let ([filename (if (path? path) path #f)]
        [loaded   null]
        [whole-file null]
        [split-file null]
        [headers    null]
        [body       null]
        [summary    null]
        [is-cached #f]
        [stime (current-milliseconds)]
        [eprintf (lambda x x)]
        [reqnames (list (if (path? path) (path->string (last (explode-path path))) name))]
        [path     (build-path (hash-ref settings 'base-path) (hash-ref settings 'data-path))])
    (set! reqnames (list (first reqnames) (unaccent-string (first reqnames))))
    (unless filename
      ;; try with all possible names and suffixes
      (for ([fname  reqnames]
            [suffix (hash-ref settings 'suffixes)]
            #:break (and (path? filename) (file-exists? filename)))
        (set! filename (build-path path (string-append fname suffix)))))
    ;; not found? croak!
    (when (or (null? filename) (not (file-exists? filename)))
      (eprintf "\t\t\e[1mnew-loader:\e[0m not loaded ~a (~a) in ~a ms\n" path filename (- (current-milliseconds) stime))
      (raise (make-exn:fail:filesystem
              (format "document ~a not found as ~a in base ~a data ~a, suffixes ~a"
                      name filename
                      (hash-ref settings 'base-path)
                      (hash-ref settings 'data-path)
                      (hash-ref settings 'suffixes))
              (current-continuation-marks))))
    ;; read and parse
    (eprintf "\t\t\tload-doc: parsing file ~a [cache: ~a, headers: ~a, summary: ~a, unparsed: ~a]... " filename (not no-cache) headers-only summary-only unparsed)
    ;; check if cached -- cache is tiered with headers, summary and parsed body
    (set! is-cached (metadb-check #:path filename #:headers-only headers-only #:summary-only summary-only))
    ;; check if cache is stale and kill it if it is
    (when (and is-cached (< is-cached (file-or-directory-modify-seconds filename)))
      (eprintf " <stale cache deleted ~a < ~a> ")
      (metadb-delete filename)
      (set! is-cached #f))
    (eprintf "\e[36m(cache is ~a file is ~a)\e[0m " is-cached (file-or-directory-modify-seconds filename))
    (eprintf "~a" (if is-cached " \e[33mcached\e[0m " " \e[31mnot cached\e[0m "))
    (if is-cached
        ;; read from cache
        (let ([dbread '()])
          (eprintf "\t\t\tload-doc: loading from cache...")
          (set! dbread (metadb-get filename))
          (let ([headers (hash-ref dbread 'headers)]
                [body (hash-ref dbread 'body)])
            (set! loaded (gldoc headers body)))
          (eprintf "done"))
        ;; read from disk
        (begin
          (if headers-only
              ;; headers only
              (begin
                (let ([header-lines '()]
                      [headers-text ""]
                      [summary-body ""]
                      [infile (open-input-file filename #:mode 'text)])
                  (set! headers-text
                        (do ([line "-"])
                            ((or (not (string? line)) (string=? line ""))
                             (string-join header-lines "\n"))
                          (set! line (read-line infile))
                          (set! header-lines (append header-lines (list line)))))
                  (close-input-port infile)
                  (set! split-file (list headers-text summary-body))))
              ;; do the whole shebang
              (begin
                (set! whole-file (file->string filename))
                (set! split-file (flatten (regexp-match* #px"^(?s:^(.*?)\n\n(.*))$" whole-file #:match-select cdr)))))
          (set! headers (parse-headers (first split-file) #:filename filename #:path-prefix path-prefix #:settings settings))
          (set! body (cond (headers-only "")
                           (unparsed (last split-file))
                           (else (last split-file))))
          (hash-set! headers 'path filename)
          (set! loaded (gldoc headers body))
          (when summary-only
            (if (regexp-match #px"<!--more[^\n]*-->" body)
                (begin (set! summary (regexp-replace #px"(?s:<!--more[^\n]*-->.*$)" body ""))
                       (hash-set! headers 'has-break #t))
                (set! summary body)))
          (eprintf "done\n")
          (when (hash-ref headers 'no-cache #f) (set! no-cache #t))
          ;; cache
          (unless no-cache
            (eprintf "\t\t\tcaching...")
            (metadb-push #:path (path->string filename)
                         #:mtime (file-or-directory-modify-seconds filename)
                         #:headers headers
                         #:body (if (or headers-only summary-only unparsed) null body))
            (eprintf "done\n"))))
    loaded))

;; parse a geeklog document headers
(define (parse-headers text
                       #:filename [filename ""]
                       #:path-prefix [path-prefix null]
                       #:settings settings)
  (let ([lines (regexp-split #px"\n" text)]
        [headers (make-hash)]
        [timestamp-date (seconds->date (current-seconds))]
        [tzoffset null])
    ;; set site presets if applyable
    (when (hash-has-key? settings 'default-headers)
      (for ([pair (hash-ref settings 'default-headers)])
        (hash-set! headers (car pair) (cdr pair))))
    ;; load headers from file
    (map (lambda (line)
           (let ([matches null])
             (set!-values (matches) (flatten
                                       (regexp-match*
                                        #px"(?m:^(.*?)\\s*:\\s*(.*)\\s*$)"
                                        line
                                        #:match-select cdr)))
             (unless (empty? matches)
                 (hash-set! headers (string->symbol (first matches)) (last matches)))))
         lines)
    ;; some magic for booleans
    (for ([key (hash-keys headers)])
      (let ([value (hash-ref headers key)])
        (when (string? value)
          (when (string=? value "#t") (hash-set! headers key #t))
          (when (string=? value "#f") (hash-set! headers key #f)))))
    ;; these should always exist
    (for ([key '(title author tags categories keywords description comment)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ;; (eprintf "\theaders so far: ~v\n" headers)
    ;; some transforms
    (hash-set*! headers
                ;; categories should be tokenized, not just split on whitespace
                'categories (for/list ([cat (regexp-split #px"\\s+" (hash-ref headers 'categories ""))]
                                       #:when (> (string-length cat) 0))
                              cat)
                'tags (for/list ([tag (regexp-split #px"\\s+" (hash-ref headers 'tags ""))]
                                 #:when (> (string-length tag) 0))
                        (string->symbol tag))
                'transform (string->symbol
                            (hash-ref headers 'transform
                                      (symbol->string (hash-ref settings 'default-transform)))))
    (when (path? filename)
      (hash-set! headers 'mtime (file-or-directory-modify-seconds filename))
      (hash-set! headers 'name  (regexp-replace #px"\\..*$" (path->string (file-name-from-path filename)) "")))
    (when (not (null? path-prefix))
      (hash-set! headers 'name (string-join (list (if (path? path-prefix) (string->path path-prefix) path-prefix)
                                                  (hash-ref headers 'name)) "/")))
    (for ([key '(author timestamp description keywords)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ;; parse the timestamp
    (let ([ts (hash-ref headers 'timestamp)])
      (cond [(regexp-match? #px"^\\s*(\\d+)\\s*$" ts) (string->number ts)]
            [(regexp-match? #px"\\d{4}.\\d{2}.\\d{2}" ts)
             (let ([m (regexp-match
                       #px"(\\d{4}).(\\d{2}).(\\d{2})\\s*T?((\\d+):?(\\d+)?:?(\\d+)?(\\s*([AaPp][Mm]))?)?"
                       ts)]
                   [convert (lambda (i)
                              (cond [(string? i) (string->number i)]
                                    [(number? i) i]
                                    [(boolean? i) 0]))]
                   [matches null]
                   [converted (list)]
                   [d null])
               (set! converted (map (lambda (x) (convert x)) (rest m)))
               (set! matches (append (take converted 3) (take (drop converted 4) 3)))
               (set! matches (append (reverse matches) (list 0 0 #f 0)))
               (set! timestamp-date (apply make-date matches)))]
            [else (make-date 0 0 0 1 1 1970 0 0 #f 0)]))
    (hash-set! headers 'timestamp
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
    (date-display-format 'iso-8601)
    (set! tzoffset
          (if (tzid-exists? (hash-ref settings 'timezone))
              (regexp-replace #px"^([-+])(\\d:\\d\\d)$"
                              (regexp-replace #px"^(\\d)"
                                              (format "~a:00"
                                                      (/ (tzoffset-utc-seconds (utc-seconds->tzoffset (hash-ref settings 'timezone) 0)) 3600))
                                              "+\\1")
                              "\\1\\$0\\2")
              "Z"))
    ;; these are some pretty-print items
    (hash-set*! headers
                'link-format (lambda (text) (make-link (hash-ref headers 'name) text #:title (hash-ref headers 'title "")))
                'title-link (make-link (hash-ref headers 'name) (hash-ref headers 'title "") #:title (hash-ref headers 'title ""))
                'mtime-format-date ((hash-ref settings 'format-date) (hash-ref headers 'mtime))
                'timestamp-format-date ((hash-ref settings 'format-date) (hash-ref headers 'timestamp))
                'timestamp-format-date-time ((hash-ref settings 'format-date-time) (hash-ref headers 'timestamp))
                'timestamp-iso (string-join (list (date->string timestamp-date #t) tzoffset) ""))
    headers))

(define (make-link href [text ""]
                   #:title [title ""]
                   #:extra [extra ""])
  (when (not (string=? title "")) (set! extra (string-append extra (format " title=\"~a\"" (xexpr->string title)))))
  (when (not (string=? extra "")) (set! extra (xexpr->string (regexp-replace #px"^\\s*" extra " "))))
  (format "<a href=\"~a\"~a>~a</a>"
          href
          extra
          (if (string=? text "") href text)))

(define (gheader doc key . default)
  (hash-ref (gldoc-headers doc) key default))

;; gldoc sorting function
(define (gldoc-sort a b key)
  (eprintf "\t\t\e[35mgldoc-sort<~a>: ~a(~a) <=> ~a(~a)\n" key (gheader a 'name) (gheader a key) (gheader b 'name) (gheader b key))
  (let ([dk (lambda (doc key) (hash-ref (gldoc-headers doc) key
                                       (hash-ref (gldoc-headers doc) 'name)))])
    (let ([ka (dk a key)]
          [kb (dk b key)])
      (cond [(and (number? ka) (number? kb)) (< ka kb)]
            [else (string<? ka kb)]))))

(provide load-doc
         gldoc-sort
         (rename-out [metadb-push cache-push]
                     [metadb-dump cache-dump]))
