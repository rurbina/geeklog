#lang racket/base

(provide markup
         markup-doc
         render-at)

(require racket/list
         racket/string
         racket/set
         racket/path
         racket/date
         racket/port
         geeklog/structs
         geeklog/load
         geeklog/search
         geeklog/log
         geeklog/markdown
         (rename-in scribble/reader [read read-at])
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup ratamarkup-process])
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup-inline ratamarkup-process-inline]))

;;; utility functions

;; turn a list of tokens into a hash
(define (hashify-tokens tokens
                        #:defaults      [default-tokens '()]
                        #:symbolic-keys [symkeys '()]
                        #:scalar-keys   [scalar '()]
                        #:all-scalar    [all-scalar #f])
  (let ([hashed (make-hash)]
        [defaults (if (> (length default-tokens) 1)
                    (hashify-tokens default-tokens #:symbolic-keys symkeys #:scalar-keys scalar #:all-scalar all-scalar)
                    (make-hash))])
    (for ([token tokens])
      (let ([key (car token)] [val (cdr token)])
        (hash-set!
         hashed
         key
         (cond [(or (set-member? scalar key) all-scalar)
                (if (set-member? symkeys key)
                    (string->symbol val)
                    val)]
               [(set-member? symkeys key)
                (cond [(boolean? val) val]
                      [{and (string? val) (string=? val "")} null]
                      [else (for/list ([item (regexp-split #px"\\s*,\\s*" val)]
                                       #:unless (string=? item ""))
                              (string->symbol item))])]
               [else val]))))
    (for ([key (hash-keys defaults)])
      (unless (hash-has-key? hashed key)
        (hash-set! hashed key (hash-ref defaults key))))
    hashed))

(define (parse-timestamp ts (default 0))
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
        [else default]))

(define (option->boolean n) (and (string? n) (string->number n) (> (string->number n) 0)))

;;; transform modes

(define (transform-ratamarkup text
                              #:settings settings
                              #:options  options)
  (hash-set! options 'geeklog-settings settings)
  (ratamarkup-process text #:options options))

(define (transform-passthrough text
                              #:settings settings
                              #:options  options)
  text)

(define (transform-markdown text
                              #:settings settings
                              #:options  options)
  (let ([processed '()])
    (define-values (sp out in err) (subprocess #f #f #f "/usr/bin/perl" "/usr/bin/markdown"))
    (write-string text in)
    (close-output-port in)
    (subprocess-wait sp)
    (set! processed (port->string out))
    (close-input-port out)
    (close-input-port err)
    processed))

(define transforms (make-hash `([ratamarkup  . ,transform-ratamarkup]
                                [passthrough . ,transform-passthrough]
                                [passthru    . ,transform-passthrough]
                                [markdown    . ,transform-markdown])))

;; for small templates, snippets and such, parse and exec a string, should be quick and easy
(define (render-at document #:data (data '()))
  (let ([ns (make-base-namespace)])
    (eval '(require racket/base
                    racket/string) ns)
    (for ([i data])
      (namespace-set-variable-value! (car i) (cdr i) #f ns))
    (string-join
     (for/list ([i (read-at (open-input-string (string-join (list "@{" document "}") "")))])
       (format "~a" (eval i ns)))
     "")))

(define (markup-doc doc #:settings settings #:options [options (make-hash)])
  (let ([body (gldoc-body doc)])
    (if (string? body)
        (let [(transform (hash-ref (gldoc-headers doc) 'transform))]
          (gldoc (gldoc-headers doc)
                 ((hash-ref transforms transform) (gldoc-body doc) #:settings settings #:options options)))
        doc)))

(define (markup text transform-type
                #:settings settings
                #:options [options null])
  (when (null? options)
    (set! options (make-hash `((geeklog-settings . ,settings)))))
  ((hash-ref transforms transform-type) text #:settings settings #:options options))

(define (html-escape text)
  (string-replace
   (string-replace
    (string-replace text "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

;;; ratamarkup customizations
(ratamarkup-set!-link-callback
 (lambda (link #:options [options (make-hash)])
   (let ([settings (hash-ref options 'geeklog-settings (make-hash))]
         [m (flatten (regexp-match* "^\\[\\[(.*)?(?<!\\\\)\\|(.*?)(?<!\\\\)\\|(.*?)]]$"
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
                                       [#px"[é]" "e"]
                                       [#px"[í]" "i"]
                                       [#px"[ó]" "o"]
                                       [#px"[ú]" "u"]
                                       [#px"[ñ]" "n"]))]
           [doc null])
       (unless (regexp-match? #px"^(http:|https:|)//" (first m))
         (with-handlers ([exn:fail? (lambda (e) void)])
           (set! doc (load-doc docname #:headers-only #t #:settings settings))
           (gldoc (gldoc-headers doc) (markup (gldoc-body doc) 'ratamarkup #:settings settings #:options options))))
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
		      (regexp-replace* #px"(?<!\\\\)\\\\vert"
				       (regexp-replace* #px"^\\s*\\|\\s*|\\s*$|\\s*\\|\\s*$" cell "")
				       "|")
                      #:options options))))))
           '()))
    (html-table-render table
                       #:class (hash-ref topts 'class "")
                       #:style (hash-ref topts 'style "")
                       #:widths (regexp-split #px"\\s*,\\s*" (hash-ref topts 'widths "")))))

(define (rm-blog text
                 #:options [options (make-hash '((null . null)))]
                 #:tokens  [tokens '()])
  (define (log x #:indent (i 2)) (logp x #:level 1 #:indent i #:tag 'markup #:color 123 #:nl #t))
  (let ([settings (hash-ref options 'geeklog-settings (make-hash))]
        [output ""]
        [header (lambda (doc key) (hash-ref (gldoc-headers doc) key))]
        [blog-options (hashify-tokens tokens
                                      #:symbolic-keys '(tags no-tags sort)
                                      #:scalar-keys   '(sort future reverse level no-past))])
    (log "rm-blog: folding ")
    (set! output
          (for/fold ([output ""])
                    ([meta (search-docs #:tags       (hash-ref blog-options 'tags '(blog))
                                        #:no-tags    (hash-ref blog-options 'no-tags '(draft))
                                        #:sort       (hash-ref blog-options 'sort 'timestamp)
                                        #:reverse    (if (hash-has-key? blog-options 'reverse) #f #t)
                                        #:no-future  (if (hash-has-key? blog-options 'future) #f #t)
                                        #:newer-than (if (hash-has-key? blog-options 'no-past) (current-seconds) 0)
                                        #:headers-only #t
                                        #:settings   settings)])
            (log `(headers . ,(gldoc-headers meta)))
            (let ([doc (load-doc (hash-ref (gldoc-headers meta) 'name)
                                 #:path (hash-ref (gldoc-headers meta) 'path)
                                 #:settings settings)])
              (let ([body (markup (gldoc-body doc)
                                  (hash-ref (gldoc-headers doc) 'transform 'ratamarkup)
                                  #:options options
                                  #:settings settings)]
                    [top ""]
                    [has-break #f]
                    [footer ""]
                    [level (hash-ref blog-options 'level "2")]
                    [break ""])
                (if (regexp-match #px"<!-- break -->" body)
                    (begin
                      (set! top (regexp-replace #px"(?s:<!-- break -->.*$)" body ""))
                      (set! has-break #t))
                    (set! top body))
                (set! top (regexp-replace* #px"<(/?)h1>" top (format "<\\1h~a>" level)))
                (when has-break (set! break (format "<p class=\"blog_break\">~a</p>\n\n"
                                                    (make-link (header doc 'name) "Leer el resto"))))
                (set! footer (format "<p class=\"blog_footer\"><i>~a por ~a</i> [~a]</p>\n"
                                     (hash-ref (gldoc-headers doc) 'timestamp-format-date-time)
                                     (hash-ref (gldoc-headers doc) 'author)
                                     (make-link (header doc 'name) "Permalink")))
                (when (> (hash-ref (gldoc-headers doc) 'mtime) (unbox (hash-ref (hash-ref options 'geeklog-settings) 'effective-mtime)))
                  (set-box! (hash-ref (hash-ref options 'geeklog-settings) 'effective-mtime) (hash-ref (gldoc-headers doc) 'mtime)))
                (log (list (hash-ref (gldoc-headers doc) 'name)))
                (string-append output top break footer)))))
    (log "done\n")
    output))

(define rm-wpblog-cache-hash (make-hash))

(define rm-wpblog-post-cache (make-hash))

(define (rm-wpblog text
                   #:options [options (make-hash '((null . null)))]
                   #:tokens  [tokens '()])
  (let ([settings (hash-ref options 'geeklog-settings (make-hash))]
        [items 'null]
        [output ""]
        [blog-options (hashify-tokens tokens
                                      #:symbolic-keys '(tags no-tags sort)
                                      #:scalar-keys   '(sort future reverse level no-past))])
    ;; get items
    (set! items
          (if (and (hash-has-key? blog-options 'cache_key)
                   (hash-has-key? rm-wpblog-cache-hash (hash-ref blog-options 'cache_key)))
              (hash-ref rm-wpblog-cache-hash (hash-ref blog-options 'cache_key))
              (search-docs #:tags         (hash-ref blog-options 'tags '(blog))
                           #:no-tags      (hash-ref blog-options 'no-tags '(draft))
                           #:sort         (hash-ref blog-options 'sort 'timestamp)
                           #:reverse      (not (option->boolean (hash-ref blog-options 'reverse "0")))
                           #:no-future    (not (option->boolean (hash-ref blog-options 'future "0")))
                           #:newer-than   (if (hash-has-key? blog-options 'no-past) (current-seconds) 0)
                           #:headers-only #f
                           #:range-count  ((lambda (n) (if (string->number n) (string->number n) 10)) (hash-ref blog-options 'count "10"))
                           #:unparsed     #t
                           #:settings     settings)))
    ;; save whole thing cached (not recommended)
    (when (and (hash-has-key? blog-options 'cache_key)
               (not (hash-has-key? rm-wpblog-cache-hash (hash-ref blog-options 'cache_key))))
      (hash-set! rm-wpblog-cache-hash (hash-ref blog-options 'cache_key) items))
    (set! output
          (for/fold ([output ""])
                    ([doc items])
            ;; check if this post has been previously cached
            (if (hash-has-key? rm-wpblog-post-cache (hash-ref (gldoc-headers doc) 'name))
                ;; it's cached
                (string-append output (hash-ref rm-wpblog-post-cache (hash-ref (gldoc-headers doc) 'name)))
                ;; it is not cached, read it and parse it and stuff
                (let ([body (markup (gldoc-body doc) (hash-ref (gldoc-headers doc) 'transform 'ratamarkup) #:options options #:settings settings)]
                      [tns (make-base-namespace)]
                      [top ""]
                      [has-break #f]
                      [level (hash-ref blog-options 'level "2")]
                      [break ""]
                      [template-path ""]
                      [out-template ""]
                      [header (lambda (h) (hash-ref (gldoc-headers doc) h))])
                  ;; take away any h1 headers, they will be replaced
                  (set! body (regexp-replace #px"<h1>.*?</h1>" body ""))
                  (if (regexp-match #px"<!--more[^\n]*-->" body)
                      (begin (set! top (regexp-replace #px"(?s:<!--more[^\n]*-->.*$)" body ""))
                             (set! has-break #t))
                      (set! top body))
                  (eval `(require web-server/templates) tns)
                  (set! template-path (build-path (hash-ref settings 'base-path ".") "post.html"))
                  (set! template-path (find-relative-path (current-directory) template-path))
                  (map (lambda (pair) (namespace-set-variable-value! (car pair) (cdr pair) #f tns))
                       (list (cons 'top top)
                             (cons 'body body)
                             (cons 'title (header 'title))
                             (cons 'author (header 'author))
                             (cons 'has-break has-break)
                             (cons 'link (header 'link-format))
                             (cons 'datetime (header 'timestamp-format-date-time))
                             (cons 'header header)))
                  (set! out-template (eval `(include-template ,(path->string template-path)) tns))
                  (hash-set! rm-wpblog-post-cache (hash-ref (gldoc-headers doc) 'name) out-template)
                  (string-append output out-template)))))
    output))

(define (rm-doclist-table text
                          #:options [options (make-hash '((null . null)))]
                          #:tokens [tokens '()])
  (let ([settings (hash-ref options 'geeklog-settings (make-hash))]
        [docs '()] [search-options (make-hash tokens)]
        [fixlist (lambda (txt) (for/list ([tag (regexp-split #px"\\s*,\\s*" txt)]
                                     #:unless (string=? "" tag))
                            (string->symbol tag)))]
        [fixlists null])
    (set! fixlists (lambda (hash list) (flatten (for/list ([key list]
                                                      #:unless (string=? "" (hash-ref hash key "")))
                                             (fixlist (hash-ref hash key ""))))))
    (hash-set*! search-options
                'reverse    ((lambda (n) (or (and (number? n) (> n 0))
                                             (and (string? n) (string->number n) (> (string->number n) 0))))
                             (hash-ref search-options 'reverse #f))
                'no-future  (option->boolean (hash-ref search-options 'no-future "0"))
                'all-future ((lambda (n) (if (option->boolean n) (current-seconds) 0))
                             (hash-ref search-options 'future-only "0"))
                'path       (hash-ref search-options 'path "")
                'tags       (fixlist (hash-ref search-options 'tags ""))
                'or-tags    (fixlist (hash-ref search-options 'or-tags ""))
                'no-tags    (fixlists search-options '(not-tag not-tags no-tag no-tags
                                                               not_tag not_tags no_tag no_tags))
                'and-tags   (fixlist (hash-ref search-options 'and-tags ""))
                'sort       (string->symbol (hash-ref search-options 'sort "name"))
                'items      ((lambda (n) (if (string->number n) (string->number n) 10))
                             (hash-ref search-options 'items "10"))
                'fields     (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'fields "name"))
                'headers    (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'headers "Item"))
                'widths     (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'widths "auto")))
    (set! docs (search-docs #:tags        (hash-ref search-options 'tags)
                            #:no-tags     (hash-ref search-options 'no-tags)
                            #:or-tags     (hash-ref search-options 'or-tags)
                            #:and-tags    (hash-ref search-options 'and-tags)
                            #:sort        (hash-ref search-options 'sort)
                            #:path        (hash-ref search-options 'path)
                            #:range-count (hash-ref search-options 'items)
                            #:reverse     (hash-ref search-options 'reverse)
                            #:no-future   (hash-ref search-options 'no-future)
                            #:newer-than  (hash-ref search-options 'all-future)
                            #:headers-only #t
                            #:settings    settings))
    (if (empty? docs)
        (ratamarkup-process "No se encontraron documentos." #:options options)
        (html-table-render
         (html-table
          (append
           (list (html-table-row-header (hash-ref search-options 'headers (hash-ref search-options 'fields)) #t))
           (for/list ([d docs])
             (when (> (hash-ref (gldoc-headers d) 'mtime) (unbox (hash-ref (hash-ref options 'geeklog-settings) 'effective-mtime)))
               (set-box! (hash-ref (hash-ref options 'geeklog-settings) 'effective-mtime) (hash-ref (gldoc-headers d) 'mtime)))
             (html-table-row (for/list ([field (hash-ref search-options 'fields '(name))])
                               (format "~a"
                                       (hash-ref (gldoc-headers d) (string->symbol field) (format "<!-- field empty: ~a -->" field)))))))
          options)
         #:widths (hash-ref search-options 'widths)))))

(define (rm-soundcloud text
                          #:options [options (make-hash '((null . null)))]
                          #:tokens [tokens '()])
  (let ([opts (hashify-tokens tokens
                              #:defaults '([width  "100%"]
                                           [height "450"]
                                           [style  "float:right;width:45%"]))])
    (format (string-append "<iframe class=\"soundcloud_player ~a\" "
                           "width=\"~a\" "
                           "height=\"~a\" "
                           "scrolling=\"no\" frameborder=\"no\" "
                           "src=\"~a\" "
                           "style=\"~a\"></iframe>\n")
            (hash-ref opts 'class "")
            (hash-ref opts 'width "100%")
            (hash-ref opts 'height "450")
            (cond [(hash-has-key? opts 'playlist)
                   (format (string-append
                            "https://w.soundcloud.com/player/?"
                            "url=https%3A//api.soundcloud.com/playlists/~a"
                            "&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true"
                            "&amp;show_user=true&amp;show_reposts=false&amp;visual=false")
                           (hash-ref opts 'playlist))])
            (hash-ref opts 'style ""))))

(define (rm-bandcamp text
                          #:options [options (make-hash '((null . null)))]
                          #:tokens [tokens '()])
  (let ([opts (hashify-tokens tokens
                              #:all-scalar #t
                              #:defaults '([width       . "200"]
                                           [height      . "350"]
                                           [type        . "album"]
                                           [id          . "1"]
                                           [bgcol       . "333333"]
                                           [linkcol     . "0f91ff"]
                                           [tracklist   . "false"]
                                           [transparent . "true"]
                                           [url         . ""]
                                           [title       . "(title)"]
                                           [size        . "large"]
                                           [style       . "float:right; clear:right; margin:4px"]
                                           ))])
    (format (string-append "<iframe style=\"~a; border: 0; width: ~apx; height: ~apx;\" src=\"https://bandcamp.com/EmbeddedPlayer/~a=~a/size=~a/bgcol=~a/linkcol=~a/tracklist=~a/transparent=~a/\" seamless>"
                             "<a href=\"~a\">~a</a>"
                             "</iframe>")
            (hash-ref opts 'style)
            (hash-ref opts 'width)
            (hash-ref opts 'height)
            (hash-ref opts 'type)
            (hash-ref opts 'id)
            (hash-ref opts 'size)
            (hash-ref opts 'bgcol)
            (hash-ref opts 'linkcol)
            (hash-ref opts 'tracklist)
            (hash-ref opts 'transparent)
            (hash-ref opts 'url)
            (hash-ref opts 'title)
            )))

(define (rm-sm2 text
                #:options [options (make-hash '((null . null)))]
                #:tokens  [tokens '(playlist rows base-uri style)])
  (let ([opts (hashify-tokens tokens
                              #:scalar-keys '(base-uri style)
                              #:defaults '([playlist . #f]
                                           [rows     .  5]
                                           [base-uri . ""]
                                           [bgcolor  . "#333"]
                                           [style    . ""]
                                           [texture  . #f]))]
        [playlist ""]
        [css ""]
        [classes ""])
    ; playlist making
    (for ([line (string-split text "\n")])
      (let ([tokens (string-split line "|")])
        (when (>= (length tokens) 2)
          (set! tokens (map (lambda (t) (string-trim t)) tokens))
          (set! playlist
                (string-append
                 playlist
                 (format
                  "<li><div class=\"sm2-row\"><div class=\"sm2-col sm2-wide\"><a href=\"~a\">~a</a></div>~a</div></li>\n"
                  (string-append (hash-ref opts 'base-uri) (first tokens))
                  (second tokens)
                  (if (> (length tokens) 2)
                      (apply string-append
                             (map (lambda (t) (format "<div class=\"sm2-col\">~a</div>" t))
                                  (map (lambda (t)
                                         (if (string=? t "@DL")
                                             (format "<a href=\"~a~a\" target=\"_blank\" class=\"sm2-icon sm2-music sm2-exclude\">Download</a>"
                                                     (hash-ref opts 'base-uri)
                                                     (first tokens))
                                             t))
                                  (rest (rest tokens)))))
                      "")
                  ))))))
    ; css making
    (set! css (string-append
               (format ".sm2-bar-ui .sm2-main-controls, .sm2-bar-ui .sm2-playlist-drawer { background-color: ~a; }\n" (hash-ref opts 'bgcolor))))
    ; class making
    (set! classes (string-join
                   (list (if (hash-ref opts 'playlist) "playlist-open" "")
                         (if (string? (hash-ref opts 'texture)) "textured" ""))
                   " "))
    ; html making
    (format (string-append
             "<div style=\"~a\">"
             "<div class=\"sm2-bar-ui full-width ~a\">
 <div class=\"bd sm2-main-controls\">"
             "

  <div class=\"sm2-inline-texture\"></div>
  <div class=\"sm2-inline-gradient\"></div>

  <div class=\"sm2-inline-element sm2-button-element\">
   <div class=\"sm2-button-bd\">
    <a href=\"#play\" class=\"sm2-inline-button play-pause\">Play / pause</a>
   </div>
  </div>

  <div class=\"sm2-inline-element sm2-inline-status\">

   <div class=\"sm2-playlist\">
    <div class=\"sm2-playlist-target\">
     <noscript><p>JavaScript is required.</p></noscript>
    </div>
   </div>

   <div class=\"sm2-progress\">
    <div class=\"sm2-row\">
    <div class=\"sm2-inline-time\">0:00</div>
     <div class=\"sm2-progress-bd\">
      <div class=\"sm2-progress-track\">
       <div class=\"sm2-progress-bar\"></div>
       <div class=\"sm2-progress-ball\"><div class=\"icon-overlay\"></div></div>
      </div>
     </div>
     <div class=\"sm2-inline-duration\">0:00</div>
    </div>
   </div>

  </div>

  <div class=\"sm2-inline-element sm2-button-element sm2-volume\">
   <div class=\"sm2-button-bd\">
    <span class=\"sm2-inline-button sm2-volume-control volume-shade\"></span>
    <a href=\"#volume\" class=\"sm2-inline-button sm2-volume-control\">volume</a>
   </div>
  </div>

  <div class=\"sm2-inline-element sm2-button-element\">
   <div class=\"sm2-button-bd\">
    <a href=\"#prev\" title=\"Previous\" class=\"sm2-inline-button previous\">&lt; previous</a>
   </div>
  </div>

  <div class=\"sm2-inline-element sm2-button-element\">
   <div class=\"sm2-button-bd\">
    <a href=\"#next\" title=\"Next\" class=\"sm2-inline-button next\">&gt; next</a>
   </div>
  </div>

  <div class=\"sm2-inline-element sm2-button-element sm2-menu\">
   <div class=\"sm2-button-bd\">
     <a href=\"#menu\" class=\"sm2-inline-button menu\">menu</a>
   </div>
  </div>

 </div>"
             "
 <div class=\"bd sm2-playlist-drawer sm2-element\">

  <div class=\"sm2-inline-texture\">
   <div class=\"sm2-box-shadow\"></div>
  </div>

  <!-- playlist content is mirrored here -->

  <div class=\"sm2-playlist-wrapper\">

    <ul class=\"sm2-playlist-bd\">
     ~a
    </ul>

  </div>

  <div class=\"sm2-extra-controls\">

   <div class=\"bd\">

    <div class=\"sm2-inline-element sm2-button-element\">
     <a href=\"#prev\" title=\"Previous\" class=\"sm2-inline-button previous\">&lt; previous</a>
    </div>

    <div class=\"sm2-inline-element sm2-button-element\">
     <a href=\"#next\" title=\"Next\" class=\"sm2-inline-button next\">&gt; next</a>
    </div>

   </div>

  </div>

 </div>

</div>
</div>
"
             "
<style>~a</style>
"
             )
            (hash-ref opts 'style)
            classes
            playlist
            css
            )))

(define (rm-div text
                 #:options [options (make-hash '((null . null)))]
                 #:tokens  [tokens '()])
  (format "<div class=\"~a\"~a>\n~a\n</div>\n\n"
          (string-join (for/list ([token tokens]
                                  #:when (eq? (cdr token) #t))
                         (symbol->string (car token)))
                       " ")
          (for/fold ([items ""])
                    ([token tokens]
                     #:unless (eq? (cdr token) #t))
            (string-append items (format " ~a=\"~a\"" (car token) (cdr token))))
          (ratamarkup-process text #:options options)))

(define (rm-tag text
                 #:options [options (make-hash '((null . null)))]
                 #:tokens  [tokens '()])
  (format "<~a~a>\n~a\n</~a>\n\n"
          (car (first tokens))
          (for/fold ([items ""])
                    ([token (rest tokens)]
                     #:unless (eq? (cdr token) #t))
            (string-append items (format " ~a=\"~a\"" (car token) (cdr token))))
          (ratamarkup-process text #:options options)
          (car (first tokens))))

(define (rm-entry text
                  #:options [options (make-hash '((null . null)))]
                  #:tokens  [tokens '()])
  (let ([settings (hash-ref options 'geeklog-settings (make-hash))]
        [hdr (hashify-tokens tokens)])
    (if (subset? (hash-keys hdr) '(author timestamp title))
        (format "<article class=\"entry\">\n<header>\n<h1>~a</h1>\n</header>\n~a\n</article>\n\n"
                ((hash-ref (hash-ref options 'geeklog-settings settings)
                          'format-date)
                 (parse-timestamp (hash-ref hdr 'timestamp) 0))
                (ratamarkup-process text #:options options))
        (format "<!-- §entry error: all these headers are required: author, timestamp -->\n\n"))))

(define (rm-more text #:options options #:tokens tokens) "<!--more-->\n\n")

(define (rm-include text
                    #:options [options (make-hash '((null . null)))]
                    #:tokens  [tokens '()])
  (let ([settings (hash-ref options 'geeklog-settings (make-hash))]
        [opts (hashify-tokens tokens)])
    (cond [(> (hash-ref options 'geeklog-recursion 0) 1)
           (format "<!-- §include error: recursion level exceeded -->\n\n")]
          [(not (hash-has-key? opts 'name))
           (format "<!-- §include usage: § include name=docname § -->\n\n")]
          [else
           (hash-set! options 'geeklog-recursion
                      (add1 (hash-ref options 'geeklog-recursion 0)))
           (let ([doc (load-doc (hash-ref opts 'name)
                                #:headers-only #t
                                #:settings (hash-ref options 'geeklog-settings settings))])
             (let ([body (markup (gldoc-body doc)
                                 (hash-ref (gldoc-headers doc) 'transform null)
                                 #:settings settings
                                 #:options options)])
               (string-join (list (format "<!-- §include : ~a -->" (hash-ref (gldoc-headers doc) 'title))
                                  (when (hash-has-key? opts 'aside)
                                    (format "<aside class=\"include~a\">"
                                            (string-join (flatten (list (hash-ref opts 'class)))
                                                         " "
                                                         #:before-first " ")))
                                  (when (hash-has-key? opts 'title)
                                    (format "<header><h1>~a</h1></header>" (hash-ref opts 'title)))
                                  body
                                  (when (hash-has-key? opts 'aside) "</aside>"))
                            "\n"
                            #:after-last "\n")))])))

(define (rm-4shared-audio text
                          #:options [options (make-hash '((null . null)))]
                          #:tokens  [tokens '()])
  (let ([opts (hashify-tokens tokens
                              #:symbolic-keys '(cover)
                              #:defaults '([type . "MINI"]
                                           [kind . "playlist"]
                                           [width . "300"]
                                           [height . "200"]
                                           [plheight . "200"]
                                           [cover . #f]
                                           [artwork . #f]))])
    (cond [(hash-has-key? opts 'id)
           (format
            (string-append "<!-- 4shared-audio -->\n"
                           "<iframe src='http://www.4shared.com/web/embed/audio/~a/~a"
                           "?type=~a"
                           "&widgetWidth=~a&widgetHeight=~a&showPlaylist=true&playlistHeight=~a"
                           "&showArtwork=~a&showCover=~a"
                           "&widgetRid=252956435456' "
                           "style='overflow:hidden;width:~apx;height:~apx;border: 0;margin:0;~a'></iframe>"
                           "\n\n")
            (hash-ref opts 'kind)
            (hash-ref opts 'id)
            (hash-ref opts 'type)
            (hash-ref opts 'width)
            (hash-ref opts 'height)
            (hash-ref opts 'plheight)
            (if (hash-ref opts 'artwork) "true" "false")
            (if (hash-ref opts 'cover) "true" "false")
            (hash-ref opts 'width)
            (hash-ref opts 'height)
            (hash-ref opts 'style ""))]
          [else "<!-- 4shared-audio: required params: id width height plheight style kind=(folder|playlist) type=(NORMAL|MINI|MEGA) -->\n\n"])))

(define (rm-lyrics text
                   #:options [globals (make-hash '((null . null)))]
                   #:tokens  [tokens '()])
  (let ([options (hashify-tokens tokens
                                 #:defaults '([level "2"]))])
    (if (string=? (hash-ref options 'caption "") "")
        (format "<div class=\"lyrics~a\"~a>~a</div>\n\n"
                (if (hash-has-key? options 'class) (format " ~a" (hash-ref options 'class "")) "")
                (if (hash-has-key? options 'style) (format " style=\"~a\"" (hash-ref options 'style)) "")
                (ratamarkup-process-inline text #:options options))
        (format "<div~a~a>\n<h~a>~a</h~a>\n<div class=\"lyrics\">~a</div></div>\n\n"
                (if (hash-has-key? options 'class) (format " class=\"~a\"" (hash-ref options 'class "")) "")
                (if (hash-has-key? options 'style) (format " style=\"~a\"" (hash-ref options 'style)) "")
                (hash-ref options 'level)
                (hash-ref options 'caption)
                (hash-ref options 'level)
                (ratamarkup-process-inline text #:options options)))))

(define (rm-code text
                   #:options [globals (make-hash '((null . null)))]
                   #:tokens  [tokens '()])
  (let ([options (hashify-tokens tokens
                                 #:defaults '([level "2"]))])
    (if (string=? (hash-ref options 'caption "") "")
        (format "<pre class=\"code~a\"~a>~a</pre>\n\n"
                (if (hash-has-key? options 'class) (format " ~a" (hash-ref options 'class "")) "")
                (if (hash-has-key? options 'style) (format " style=\"~a\"" (hash-ref options 'style)) "")
                (html-escape text))
        (format "<div~a~a>\n<h~a>~a</h~a>\n<pre class=\"code\">~a</pre></div>\n\n"
                (if (hash-has-key? options 'class) (format " class=\"~a\"" (hash-ref options 'class "")) "")
                (if (hash-has-key? options 'style) (format " style=\"~a\"" (hash-ref options 'style)) "")
                (hash-ref options 'level)
                (hash-ref options 'caption)
                (hash-ref options 'level)
                (html-escape text)))))

(define (rm-quote text
                  #:options [options (make-hash '((null . null)))]
                  #:tokens  [tokens '()])
  (string-join
   (list
    "<figure class=\"quote\">\n"
    (format "<blockquote>~a</blockquote>\n"
            (ratamarkup-process text #:options options))
    (format "<figcaption>~a</figcaption>\n"
            (ratamarkup-process-inline (cdr (first tokens)) #:options options))
    "</figure>\n\n")))

(define (rm-markdown text #:options [options (make-hash '((null . null)))] #:tokens  [tokens '()]) (markdown text #:options options))

(define (rm-raw text
                   #:options [globals (make-hash '((null . null)))]
                   #:tokens  [tokens '()])
  text)

(ratamarkup-add-section-processor 'orgtbl            rm-orgtbl)
(ratamarkup-add-section-processor 'blog              rm-blog)
(ratamarkup-add-section-processor 'wpblog            rm-wpblog)
(ratamarkup-add-section-processor 'doclist_table     rm-doclist-table)
(ratamarkup-add-section-processor 'doclist-table     rm-doclist-table)
(ratamarkup-add-section-processor 'soundcloud_player rm-soundcloud)
(ratamarkup-add-section-processor 'bandcamp_player   rm-bandcamp)
(ratamarkup-add-section-processor 'sm2_player        rm-sm2)
(ratamarkup-add-section-processor 'sm2-player        rm-sm2)
(ratamarkup-add-section-processor 'div               rm-div)
(ratamarkup-add-section-processor 'tag               rm-tag)
(ratamarkup-add-section-processor 'lyrics            rm-lyrics)
(ratamarkup-add-section-processor 'code              rm-code)
(ratamarkup-add-section-processor 'entry             rm-entry)
(ratamarkup-add-section-processor 'include           rm-include)
(ratamarkup-add-section-processor '4shared-audio     rm-4shared-audio)
(ratamarkup-add-section-processor 'more              rm-more)
(ratamarkup-add-section-processor 'raw               rm-raw)
(ratamarkup-add-section-processor 'quote             rm-quote)
(ratamarkup-add-section-processor 'markdown          rm-markdown)

;;; table rendering utility function

(struct html-table (rows options))
(struct html-table-row (cells))
(struct html-table-row-header html-table-row (header))
(struct html-table-cell (content))
(struct html-table-cell-style html-table-cell (style))

(define (html-table-render [table html-table?]
                      #:widths [widths '()]
                      #:class [class ""]
                      #:style [style ""]
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
     #:before-first (format "\n<table~a~a>\n\t<tr>\n\t\t"
                            (if (string=? class "") "" (format " class=\"~a\"" class))
                            (if (string=? style "") "" (format " style=\"~a\"" style)))
     #:after-last "\n\t</tr>\n</table>\n\n")))


