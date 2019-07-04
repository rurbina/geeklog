#lang racket

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
         geeklog/search
         web-server/servlet
         web-server/servlet-env
         web-server/templates
         xml
         racket/match
         racket/list
         racket/date
         scribble/decode
         "handlers.rkt"
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup ratamarkup-process])
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup-inline ratamarkup-process-inline]))

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

;;; ratamarkup customizations

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
                                       [#px"[é]" "e"]
                                       [#px"[í]" "i"]
                                       [#px"[ó]" "o"]
                                       [#px"[ú]" "u"]
                                       [#px"[ñ]" "n"]))]
           [doc null])
       (unless (regexp-match? #px"^(http:|https:|)//" (first m))
         (set! doc
               (with-handlers ([exn:fail? (lambda (e) void)])
                 (load-doc docname #:headers-only #t #:settings (hash-ref options 'geeklog-settings default-settings)))))
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
  (let ([settings (hash-ref options 'geeklog-settings default-settings)]
        [blog-options (hashify-tokens tokens
                                      #:symbolic-keys '(tags no-tags sort)
                                      #:scalar-keys   '(sort future reverse level no-past))])
    (for/fold ([output ""])
              ([doc (search-docs #:tags       (hash-ref blog-options 'tags '(blog))
                                 #:no-tags    (hash-ref blog-options 'no-tags '(draft))
                                 #:sort       (hash-ref blog-options 'sort 'timestamp)
                                 #:reverse    (if (hash-has-key? blog-options 'reverse) #f #t)
                                 #:no-future  (if (hash-has-key? blog-options 'future) #f #t)
                                 #:newer-than (if (hash-has-key? blog-options 'no-past) (current-seconds) 0)
                                 #:settings   settings)])
      (let ([body (parse-body (gldoc-body doc)
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
                                            ((hash-ref (gldoc-headers doc) 'link-format) "Leer el resto"))))
        (set! footer (format "<p class=\"blog_footer\"><i>~a por ~a</i> [~a]</p>\n"
                             (hash-ref (gldoc-headers doc) 'timestamp-format-date-time)
                             (hash-ref (gldoc-headers doc) 'author)
                             ((hash-ref (gldoc-headers doc) 'link-format) "Permalink")))
        (when (> (hash-ref (gldoc-headers doc) 'mtime) (unbox (hash-ref (hash-ref options 'geeklog-settings) 'effective-mtime)))
          (set-box! (hash-ref (hash-ref options 'geeklog-settings) 'effective-mtime) (hash-ref (gldoc-headers doc) 'mtime)))
        (string-append output top break footer)))))

(define rm-wpblog-cache-hash (make-hash))

(define (rm-wpblog text
                 #:options [options (make-hash '((null . null)))]
                 #:tokens  [tokens '()])
  (let ([settings (hash-ref options 'geeklog-settings default-settings)]
        [items 'null]
        [blog-options (hashify-tokens tokens
                                      #:symbolic-keys '(tags no-tags sort)
                                      #:scalar-keys   '(sort future reverse level no-past))])
    (set! items
          (if (and (hash-has-key? blog-options 'cache_key)
                   (hash-has-key? rm-wpblog-cache-hash (hash-ref blog-options 'cache_key)))
              (hash-ref rm-wpblog-cache-hash (hash-ref blog-options 'cache_key))
              (search-docs #:tags         (hash-ref blog-options 'tags '(blog))
                           #:no-tags      (hash-ref blog-options 'no-tags '(draft))
                           #:sort         (hash-ref blog-options 'sort 'timestamp)
                           #:reverse      (if (hash-has-key? blog-options 'reverse) #f #t)
                           #:no-future    (if (hash-has-key? blog-options 'future) #f #t)
                           #:newer-than   (if (hash-has-key? blog-options 'no-past) (current-seconds) 0)
                           #:headers-only #f
                           #:range-count  (string->number (hash-ref blog-options 'count "10"))
                           #:unparsed     #t
                           #:settings     settings)))
    (when (and (hash-has-key? blog-options 'cache_key)
               (not (hash-has-key? rm-wpblog-cache-hash (hash-ref blog-options 'cache_key))))
      (hash-set! rm-wpblog-cache-hash (hash-ref blog-options 'cache_key) items))
    (for/fold ([output ""])
              ([doc items])
      (let ([body (parse-body (gldoc-body doc)
                              (hash-ref (gldoc-headers doc) 'transform 'ratamarkup)
                              #:options options
                              #:settings settings)]
            [tns (make-base-namespace)]
            [top ""]
            [has-break #f]
            [level (hash-ref blog-options 'level "2")]
            [break ""]
            [template-path ""]
            [out-template ""]
            [header (lambda (h) (hash-ref (gldoc-headers doc) h))])
        (set! body (regexp-replace #px"<h1>.*?</h1>" body ""))
        (if (regexp-match #px"<!--more[^\n]*-->" body)
            (begin
              (set! top (regexp-replace #px"(?s:<!--more[^\n]*-->.*$)" body ""))
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
        (string-append output out-template)))))

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
                'and-tags (fixlist (hash-ref search-options 'and-tags ""))
                'sort    (string->symbol (hash-ref search-options 'sort "name"))
                'fields  (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'fields "name"))
                'headers (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'headers "Item"))
                'widths  (regexp-split #px"\\s*(?<!\\\\),\\s*" (hash-ref search-options 'widths "auto")))
    (set! docs (search-docs #:tags      (hash-ref search-options 'tags)
                            #:no-tags   (hash-ref search-options 'no-tags)
                            #:or-tags   (hash-ref search-options 'or-tags)
                            #:and-tags  (hash-ref search-options 'and-tags)
                            #:sort      (hash-ref search-options 'sort)
                            #:settings  (hash-ref options 'geeklog-settings default-settings)))
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
                                       (hash-ref (gldoc-headers d) (string->symbol field) field))))))
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
  (let ([hdr (hashify-tokens tokens)])
    (if (subset? (hash-keys hdr) '(author timestamp title))
        (format "<article class=\"entry\">\n<header>\n<h1>~a</h1>\n</header>\n~a\n</article>\n\n"
                ((hash-ref (hash-ref options 'geeklog-settings default-settings)
                          'format-date)
                 (parse-timestamp (hash-ref hdr 'timestamp) 0))
                (ratamarkup-process text #:options options))
        (format "<!-- §entry error: all these headers are required: author, timestamp -->\n\n"))))

(define (rm-more text #:options options #:tokens tokens) "<!--more-->\n\n")

(define (rm-include text
                    #:options [options (make-hash '((null . null)))]
                    #:tokens  [tokens '()])
  (let ([opts (hashify-tokens tokens)])
    (cond [(> (hash-ref options 'geeklog-recursion 0) 1)
           (format "<!-- §include error: recursion level exceeded -->\n\n")]
          [(not (hash-has-key? opts 'name))
           (format "<!-- §include usage: § include name=docname § -->\n\n")]
          [else
           (hash-set! options 'geeklog-recursion
                      (add1 (hash-ref options 'geeklog-recursion 0)))
           (let ([doc (load-doc (hash-ref opts 'name)
                                #:headers-only #t
                                #:settings (hash-ref options 'geeklog-settings default-settings))])
             (let ([body (parse-body (gldoc-body doc)
                                             (hash-ref (gldoc-headers doc) 'transform null)
                                             #:settings (hash-ref options 'geeklog-settings default-settings)
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
(ratamarkup-add-section-processor 'entry             rm-entry)
(ratamarkup-add-section-processor 'include           rm-include)
(ratamarkup-add-section-processor '4shared-audio     rm-4shared-audio)
(ratamarkup-add-section-processor 'more              rm-more)
(ratamarkup-add-section-processor 'raw               rm-raw)
(ratamarkup-add-section-processor 'quote             rm-quote)

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

;;; documents

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

(define (do-load req path headers settings)
  (let ([doc null]
        [errmsg ""]
        [docname (path/param-path (last (url-path (request-uri req))))]
        [effective-mtime (box 0)])
    (hash-set! settings 'recursion-depth (add1 (hash-ref settings 'recursion-depth 1)))
    (hash-set! settings 'effective-mtime effective-mtime)
    (when (string=? docname "") (set! docname (hash-ref settings 'default-doc)))
    (with-handlers ([exn:fail:filesystem? (lambda (x) (set! errmsg (format "document not found ~a" x)))])
      (set! doc (load-doc docname #:settings settings)))
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
                     (cons 'include-nocache  (lambda (name) (gldoc-body (load-doc name #:settings settings))))
                     (cons 'include  (lambda (name (cacheable #f))
                                       (let ([load (lambda (name) (gldoc-body (load-doc name #:settings settings)))])
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
           (lambda (n) (load-doc (hash-ref params 'doc (hash-ref settings 'default-doc))
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
