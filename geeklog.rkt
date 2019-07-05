#lang racket/base

#| geeklog.rkt https://github.com/rurbina/geeklog |#

(provide geeklog
         geeklog-uri
         (rename-out [load-doc          geeklog-load-doc]
                     [merge-settings    geeklog-merge-settings]
                     [default-settings  geeklog-default-settings]
                     [site-settings     geeklog-site-settings])
         (struct-out gldoc))

(require geeklog/structs
         geeklog/load
         geeklog/markup
         web-server/servlet
         web-server/servlet-env
         web-server/templates
         xml
         racket/match
         racket/list
         racket/date
         racket/string
         racket/bool
         racket/path
         scribble/decode
         "handlers.rkt")

;;; import new site settings

;; takes a hash and merges it key-by-key with settings
(define (merge-settings new-values [site 'default])
  (when (hash? new-values) (set! new-values (hash->list new-values)))
  (when (not (hash-has-key? site-settings site))
    ;; preload all defaults
    (hash-set! site-settings site (make-hash new-values))
    (merge-settings default-settings site)
    ;; and make a new cache hash
    (hash-set! (hash-ref site-settings site) 'cache (make-hash)))
  (let ([target (hash-ref site-settings site)]
        [default (hash-ref site-settings 'default)])
    (for ([pair new-values])
      (hash-set! target (car pair) (cdr pair)))))

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
  (make-hash `([name              . "default settings"]
               [base-path         . "."]
               [hostname          . "*"]
               [data-path         . "docs"]
               [suffixes          . (".txt" ".link" "")]
               [template          . "template.html"]
               [404-doc           . "error-404"]
               [default-transform . ratamarkup]
               [default-doc       . "index"]
               [format-date       . ,default-format-date]
               [format-time       . ,default-format-time]
               [format-date-time  . ,default-format-date-time]
               [cache             . ,(make-hash)])))

(define site-settings
  (make-hash `([default . ,default-settings])))

;;; documents

(define (load-parse-doc docname #:settings s)
  (let ([doc (load-doc docname #:settings s)])
    (eprintf "  loadparsing ~a as ~v\n" docname doc)
    (if (gldoc? doc)
        (markup-doc doc #:settings s)
        null)))

(define (do-load req path headers settings)
  (let ([doc null]
        [errmsg ""]
        [docname (path/param-path (last (url-path (request-uri req))))]
        [effective-mtime (box 0)])
    (hash-set! settings 'recursion-depth (add1 (hash-ref settings 'recursion-depth 1)))
    (hash-set! settings 'effective-mtime effective-mtime)
    (when (string=? docname "") (set! docname (hash-ref settings 'default-doc)))
    (with-handlers ([exn:fail:filesystem? (lambda (x) (set! errmsg (format "document not found ~a" x)))])
      (set! doc (load-parse-doc docname #:settings settings)))
    (if (null? doc)
        (cons 404 #"Not found")
        (templatify doc #:settings settings))))

(define (do-info req path headers settings)
  (format "<html><head><title>~a</title></head><body><h1>Server info</h1><h2>settings</h2>~a</body></html>"
          (cdr (first (filter (lambda (h) (symbol=? (car h) 'host)) headers)))
          (string-join
           (for/list ([h (hash-keys site-settings)])
             (format "<h3>~a</h3><dl>~a</dl>" h
                     (string-join
                      (for/list ([k (hash-keys (hash-ref site-settings h))])
                        (format "<dt>~a</dt><dd>~v</dd>" k (hash-ref (hash-ref site-settings h) k))))))
           "\n")))

;; special handlers
(define uri-handlers
  (make-hash `([default . ,do-load]
               ["info"  . ,do-info])))

;; url-based (not query-string) servlet
(define (geeklog-servlet-uri req)
  (let ([start-time (current-milliseconds)]
        [hostname (cdr (first (filter (lambda (h) (symbol=? (car h) 'host)) (request-headers req))))]
        [path (url->string (request-uri req))]
        [item (path/param-path (last (url-path (request-uri req))))]
        [headers (list (make-header (string->bytes/utf-8 "omg") (string->bytes/utf-8 "wtf")))]
        [doc "index"]
        [output ""]
        [handler (hash-ref uri-handlers 'default)]
        [settings default-settings]
        [response-code 200]
        [mime-type TEXT/HTML-MIME-TYPE]
        [response-bytes #"OK"])
    (printf "REQUEST: ~a ~a " hostname path)
    (set! settings (hash-ref site-settings hostname default-settings))
    (printf "SITE: ~a " (hash-ref settings 'name))
    (when (string=? item "") (set! item (hash-ref settings 'default-doc)))
    (printf "ITEM: ~a " item)
    (when (hash-has-key? uri-handlers item)
      (set! handler (hash-ref uri-handlers item)))
    (printf "HANDLER: ~a " handler)
    (set! output (handler req path (request-headers req) settings))
    (printf "TIME: ~a ms "  (- (current-milliseconds) start-time))
    (cond
      [(and (pair? output) (number? (car output)))
       (set! response-code (car output))
       (set! response-bytes (cdr output))
       (set! output (format "<html><head><title>404 Not Found</title></head><body><p>Not found: ~a</p></body></html>\n" path))]
      [(and (pair? output) (string? (car output)) (string=? (car output) "templatify"))
       (set! output (templatify (gldoc (cadr output) (cddr output)) #:settings settings))]
      [(hash? output)
       (set! response-code  (hash-ref output 'response-code 200))
       (set! mime-type      (hash-ref output 'mime-type mime-type))
       (set! headers        (hash-ref output 'headers headers))
       (set! output (hash-ref output 'output ""))])
    (printf "CODE: ~v\n" response-code)
    (response/full response-code
                   response-bytes
                   (current-seconds)
                   mime-type
                   headers
                   (list (string->bytes/utf-8 output)))))

;; include cache
(define include-cache (make-hash))

;; apply template
(define (templatify geekdoc
         #:settings [settings default-settings])
  (let ([tns (make-base-namespace)] [template-path null])
    (eval `(require web-server/templates) tns)
    (for ([var (list (cons 'body     (gldoc-body geekdoc))
                     (cons 'title    (hash-ref (gldoc-headers geekdoc) 'title ""))
                     (cons 'gldoc    geekdoc)
                     (cons 'headers  (gldoc-headers geekdoc))
                     (cons 'timing 0)
                     (cons 'default-settings  settings)
                     (cons 'include-nocache  (lambda (name) (gldoc-body (load-parse-doc name #:settings settings))))
                     (cons 'include  (lambda (name (cacheable #f))
                                       (let ([load (lambda (name) (gldoc-body (load-parse-doc name #:settings settings)))])
                                         (when (and cacheable (not (hash-has-key? include-cache name)))
                                           (eprintf "INCLUDE CACHE ~a " name)
                                           (hash-set! include-cache name (load name)))
                                         (if cacheable
                                             (hash-ref include-cache name)
                                             (load name))))))])
      (namespace-set-variable-value! (car var) (cdr var) #f tns))
    ;; template path must be relative, don't ask me why
    (set! template-path (build-path (hash-ref settings 'base-path ".") (hash-ref settings 'template)))
    (set! template-path (find-relative-path (current-directory) template-path))
    (eval `(include-template ,(path->string template-path)) tns)))


;; launch the uri-based servlet
(define (geeklog-uri #:port [port 8099] #:path [path "."])
  ;; load handlers
  (for ([pair default-handlers]) (hash-set! uri-handlers (car pair) (cdr pair)))
  (serve/servlet geeklog-servlet-uri
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port port
                 #:stateless? #t
                 #:server-root-path path
                 #:servlets-root path
                 #:servlet-regexp #rx""))


;; servlet processing thread
(define (geeklog-servlet req)
  (let ([params (make-hash (url-query (request-uri req)))]
        [out-template null]
        [doc (make-hash)]
        [tns (make-base-namespace)]
        [geekdoc null]
        [script (path/param-path (first (url-path (request-uri req))))]
        [settings null]
        [template-path null]
        [cpu null] [real null] [gc null]
        [response-headers '()]
        [start-time (current-milliseconds)]
        [effective-mtime (box 0)])
    (printf "REQUEST: ~a~n" (hash-ref params 'doc))
    (set! settings
          (cond [(hash-has-key? site-settings script) (hash-ref site-settings script)]
                [(hash-has-key? site-settings (string->symbol (string-replace script ".rkt" "")))
                 (hash-ref site-settings (string->symbol (string-replace script ".rkt" "")))]
                [(and (hash-has-key? params 'config)
                      (hash-has-key? site-settings (string->symbol (hash-ref params 'config))))
                 (hash-ref site-settings (string->symbol (hash-ref params 'config)))]
                 [else
                  (eprintf "no settings for script ~v or for config ~v\n" script (hash-ref params 'config "(none)"))
                  default-settings]))
    (hash-set! settings 'recursion-depth (add1 (hash-ref settings 'recursion-depth 1)))
    (hash-set! settings 'effective-mtime effective-mtime)
    (unless (hash-has-key? params 'doc) (hash-set! params 'doc (hash-ref settings 'default-doc)))
    (set!-values (geekdoc cpu real gc)
          (time-apply
           (lambda (n) (load-parse-doc (hash-ref params 'doc (hash-ref settings 'default-doc))
                                       #:settings settings))
           '(1)))
    (set! geekdoc (first geekdoc))
    (hash-set! doc 'title (hash-ref! (gldoc-headers geekdoc) 'title (hash-ref params 'doc)))
    (hash-set! doc 'body (gldoc-body geekdoc))
    (date-display-format 'rfc2822)
    (set!
     response-headers
     (append
      response-headers
      (list (make-header
             #"Last-Modified"
             (string->bytes/utf-8 (date->string
                                   (seconds->date
                                    (if (> (unbox effective-mtime) 0)
                                        (unbox effective-mtime)
                                        (hash-ref (gldoc-headers geekdoc) 'mtime)))
                                   #t))))))
    (eval `(require web-server/templates) tns)
    (for ([var (list
                (cons 'body (hash-ref doc 'body ""))
                (cons 'title (hash-ref (gldoc-headers geekdoc) 'title ""))
                (cons 'timing real);(list cpu real gc))
                (cons 'doc doc)
                (cons 'gldoc gldoc)
                (cons 'headers (gldoc-headers geekdoc))
                (cons 'default-settings settings)
                (cons 'include (lambda (name) (gldoc-body (load-doc name #:settings settings)))))])
      (namespace-set-variable-value! (car var) (cdr var) #f tns))
    ;; template path must be relative, don't ask me why
    (set! template-path (build-path (hash-ref settings 'base-path ".") (hash-ref settings 'template)))
    (set! template-path (find-relative-path (current-directory) template-path))
    (set! out-template (eval `(include-template ,(path->string template-path)) tns))
    (printf "~a ms ~n"  (- (current-milliseconds) start-time))
    (response/full
     200 #"OK"
     (current-seconds) TEXT/HTML-MIME-TYPE
     response-headers
     (list (string->bytes/utf-8 out-template)))))

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
