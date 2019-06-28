#lang racket

(require geeklog/structs
         racket/date
         xml
         ;gregor
         tzinfo
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup ratamarkup-process]
                    [ratamarkup-inline ratamarkup-process-inline]))

(define geeklog/load #t)

;;; transform modes

(define (transform-ratamarkup text
                              #:settings settings
                              #:options  options)
  (ratamarkup-process text #:options options))

(define (transform-passthrough text
                              #:settings settings
                              #:options  options)
  text)

(define transforms (make-hash `([ratamarkup  . ,transform-ratamarkup]
                                [passthrough . ,transform-passthrough]
                                [passthru    . ,transform-passthrough])))


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

;; load a geeklog document, which is basically headers\n\nbody
(define (load-doc name
                  #:path            [path null]
                  #:headers-only    [nobody #f] ; do not process body, only headers
                  #:unparsed        [no-parse #f] ; load unparsed contents
                  #:summary-only    [summary-only #f] ; do not parse whole body, only summary
                  #:summary-stop    [summary-stop #px"(?m:^§more)"] ; regexp to determine summary's end
                  #:summary-chars   [summary-chars 100] ; or how many chars to load as summary if regexp fails
                  #:settings        settings)
  (let ([reqname name]
        [filename path]
        [whole ""]
        [doc null]
        [tmp null]
        [body null]
        [headers null]
        [search-path null]
        [start-time (current-milliseconds)]
        [result null]
        [cache (hash-ref settings 'cache)])
    ;; if it's a headers-only request, check if cached
    (when (and nobody (hash-has-key? cache reqname))
      (eprintf "	cache hit: ~a\n" reqname)
      (set! result (hash-ref cache reqname)))
    ;; otherwise, load it
    (when (null? result)
      (when nobody (eprintf "	cache miss: ~a\n" reqname))
      ;; find the file
      (set! search-path (build-path (hash-ref settings 'base-path) (hash-ref settings 'data-path)))
      (when (path? path) (set! name (last (explode-path path))))
      (when (null? filename) (set! filename "non-existing-file"))
      ;; try all suffixes
      (for ([suffix (hash-ref settings 'suffixes)]
            #:break (file-exists? filename))
        (set! filename (build-path (hash-ref settings 'base-path) (hash-ref settings 'data-path) (string-append name suffix))))
      ;; try again but removing accents
      (when (and (not (file-exists? filename)) (not (string=? name (unaccent-string name))))
        (set! name (unaccent-string name))
        (for ([suffix (hash-ref settings 'suffixes)]
              #:break (file-exists? filename))
          (set! filename (build-path (hash-ref settings 'base-path) (hash-ref settings 'data-path) (string-append name suffix)))))
      (unless (file-exists? filename)
        ;; given up, croak
        (raise (make-exn:fail:filesystem
                (format "document ~a not found as ~a in base ~a data ~a, suffixes ~a"
                        name filename
                        (hash-ref settings 'base-path)
                        (hash-ref settings 'data-path)
                        (hash-ref settings 'suffixes))
                (current-continuation-marks))))
      ;; read and parse
      (set! whole (file->string filename))
      (set! tmp (flatten (regexp-match* #px"^(?s:^(.*?)\n\n(.*))$" whole #:match-select cdr)))
      (set! headers (parse-headers (first tmp) #:filename filename #:settings settings))
      (set! body
            (if nobody
                (last tmp)
                (parse-body (last tmp)
                            (hash-ref headers 'transform (hash-ref settings 'default-transform))
                            #:settings settings)))
      (set! result (gldoc headers body))
      (eprintf "	cached (not really) ~a\n" reqname)
      ; disable for now
      ;(hash-set! cache reqname result)
      )
    result))

;; parse a geeklog document headers
(define (parse-headers text
                       #:filename [filename ""]
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
      (hash-set! headers
                 'mtime
                 (file-or-directory-modify-seconds filename))
      (hash-set! headers
                 'name
                 (regexp-replace #px"\\..*$" (path->string (file-name-from-path filename)) "")))
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
               (printf "\n\nSEARCH date items: ~v\n\n" matches)
               (set! timestamp-date (apply make-date matches)))]
            [else (make-date 0 0 0 1 1 1970 0 0 #f 0)]))
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
    (hash-set*!
     headers
     'link-format (lambda (text) (make-link (hash-ref headers 'name) text
                                       #:title (hash-ref headers 'title "")))
     'title-link (make-link (hash-ref headers 'name)
                            (hash-ref headers 'title "")
                            #:title (hash-ref headers 'title ""))
     'mtime-format-date ((hash-ref settings 'format-date) (hash-ref headers 'mtime))
     'timestamp-format-date ((hash-ref settings 'format-date) (hash-ref headers 'timestamp))
     'timestamp-format-date-time ((hash-ref settings 'format-date-time) (hash-ref headers 'timestamp))
     'timestamp-iso (string-join (list (date->string timestamp-date #t)
                                       tzoffset) ""))
    headers))

;; parse a body of text (dispatches transforms)
(define (parse-body text
                    transform-type
                    #:settings settings
                    #:options [options null])
  (when (null? options)
    (set! options (make-hash `((geeklog-settings . ,settings)))))
  ((hash-ref transforms transform-type) text #:settings settings #:options options))

(define (make-link href [text ""]
                   #:title [title ""]
                   #:extra [extra ""])
  (when (not (string=? title "")) (set! extra (string-append extra (format " title=\"~a\"" (xexpr->string title)))))
  (when (not (string=? extra "")) (set! extra (xexpr->string (regexp-replace #px"^\\s*" extra " "))))
  (format "<a href=\"~a\"~a>~a</a>"
          href
          extra
          (if (string=? text "") href text)))

;; gldoc sorting function
(define (gldoc-sort a b key)
  (let ([dk (lambda (doc key) (hash-ref (gldoc-headers doc) key
                                       (hash-ref (gldoc-headers doc) 'name)))])
    (let ([ka (dk a key)]
          [kb (dk b key)])
      (cond [(and (number? ka) (number? kb)) (< ka kb)]
            [else (string<? ka kb)]))))

(provide load-doc
         gldoc-sort
         parse-body)

