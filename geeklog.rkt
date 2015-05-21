#lang racket

#| geeklog.rkt https://github.com/rurbina/geeklog |#

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
         (rename-in ratamarkup/ratamarkup
                    [ratamarkup ratamarkup-process]))

;;; settings

;; takes a hash and merges it key-by-key with settings
(define (merge-settings new-hash [site 'default])
  (unless (hash-has-key? site-settings site)
    (hash-set! site-settings site (make-hash (for/list ([key (hash-keys default-settings)])
                                               (cons key (hash-ref default-settings key))))))
  (for ([key (hash-keys new-hash)])
    (hash-set! (hash-ref site-settings site)
               key
               (hash-ref new-hash key))))

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
               [suffixes          . (".txt" ".link" "")]
               [template          . "template.html"]
               [404-doc           . "error-404"]
               [default-transform . ratamarkup]
               [default-doc       . "index"]
               [format-date       . ,default-format-date]
               [format-time       . ,default-format-time]
               [format-date-time  . ,default-format-date-time])))

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
                                       [#px"[ó]" "o"]
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
    (html-table-render table #:widths (regexp-split #px"\\s*,\\s*" (hash-ref topts 'widths "")))))

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
                              #:defaults '([width  "200"]
                                           [height "350"]
                                           [album "1"]
                                           [bgcol "333333"]
                                           [url ""]
                                           [title ""]
                                           [style  "float:right; margin:4px"]))])
    (format (string-append "<iframe style=\"~a; border: 0; width: ~apx; height: ~apx;\" src=\"https://bandcamp.com/EmbeddedPlayer/album=~a/size=large/bgcol=~a/linkcol=0f91ff/tracklist=false/transparent=true/\" seamless>"
                           "<a href=\"~a\">~a</a>"
                           "</iframe>")
            (hash-ref opts 'style "")
            (hash-ref opts 'width  "200")
            (hash-ref opts 'height "350")
            (hash-ref opts 'album  "1")
            (hash-ref opts 'bgcol  "333333")
            (hash-ref opts 'url    "bandcamp album url")
            (hash-ref opts 'title  "link title"))))

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
  (let ([opts (hashify-tokens tokens)])
    (cond [(hash-has-key? opts 'folder)
           (format 
            (string-append "<!-- 4shared-audio -->\n"
                           "<iframe src='http://www.4shared.com/web/embed/audio/folder/~a"
                           "?type=~a"
                           "&widgetWidth=300&widgetHeight=210&showPlaylist=true&playlistHeight=150"
                           "&widgetRid=252956435456' "
                           "style='overflow:hidden;height:210px;width:300px;border: 0;margin:0;~a'></iframe>"
                           "\n\n")
            (hash-ref opts 'folder)
            "MINI"
            (hash-ref opts 'style ""))]
          [else "<!-- 4shared-audio: required folder param -->\n\n"])))
                 

(ratamarkup-add-section-processor 'orgtbl            rm-orgtbl)
(ratamarkup-add-section-processor 'blog              rm-blog)
(ratamarkup-add-section-processor 'doclist_table     rm-doclist-table)
(ratamarkup-add-section-processor 'soundcloud_player rm-soundcloud)
(ratamarkup-add-section-processor 'bandcamp_player   rm-bandcamp)
(ratamarkup-add-section-processor 'div               rm-div)
(ratamarkup-add-section-processor 'entry             rm-entry)
(ratamarkup-add-section-processor 'include           rm-include)
(ratamarkup-add-section-processor '4shared-audio     rm-4shared-audio)

;;; transform modes

(define (transform-ratamarkup text
                              #:settings [settings default-settings]
                              #:options [options (make-hash `((geeklog-settings . ,default-settings)))])
  (unless (eq? settings default-settings)
    (hash-set! options 'geeklog-settings settings))
  (ratamarkup-process text #:options options))

(define transforms (make-hash `([ratamarkup . ,transform-ratamarkup])))

;;; table rendering utility function

(struct html-table (rows options))
(struct html-table-row (cells))
(struct html-table-row-header html-table-row (header))
(struct html-table-cell (content))
(struct html-table-cell-style html-table-cell (style))

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

;;; utility functions

;; turn a list of tokens into a hash
(define (hashify-tokens tokens
                        #:defaults      [defaults '()]
                        #:symbolic-keys [symkeys '()]
                        #:scalar-keys   [scalar '()])
  (for/hash ([token tokens])
    (let ([key (car token)] [val (cdr token)])
      (values 
       key
       (cond [(set-member? scalar key)
              (if (set-member? symkeys key)
                  (string->symbol val)
                  val)]
             [(set-member? symkeys key)
              (cond [(boolean? val) val]
                    [{and (string? val) (string=? val "")} null]
                    [else (for/list ([item (regexp-split #px"\\s*,\\s*" val)]
                                     #:unless (string=? item ""))
                            (string->symbol item))])]
             [else val])))))

;;; documents

(struct gldoc (headers body))

;; search for documents
(define (search-docs
         #:tags            [tags         '()]
         #:or-tags         [or-tags      '()]
         #:no-tags         [no-tags      '()]
         #:headers-only    [headers-only #t]
         #:sort            [sort-key     'name]
         #:reverse         [sort-reverse #f]
         #:no-future       [no-future    #f]
         #:older-than      [before-secs  0]
         #:newer-than      [after-secs   0]
         #:settings        [settings     default-settings]
         #:data-path       [data-path    null])
  (when (null? data-path)
    (set! data-path (string->path (string-append (hash-ref settings 'base-path) "/" (hash-ref settings 'data-path)))))
  (let ([results '()] [file-path null] [now (current-seconds)])
    (set! results
          (for/list ([doc (for/list ([file (directory-list data-path)]
                                     #:when (file-exists? (build-path data-path file)))
                            (set! file-path (build-path data-path file))
                            ;;(load-doc (path->string file-path) #:headers-only headers-only))]
                            (with-handlers ([exn:fail? (lambda (e)
                                                         (eprintf "\t\terror is ~v\n" e)
                                                         void)])
                              (load-doc (path->string file-path)
                                        #:path file-path
                                        #:settings settings
                                        #:headers-only headers-only)))]
                     #:when (and [gldoc? doc]
                                 [or (empty? tags)
                                     (subset? tags (hash-ref (gldoc-headers doc) 'tags '()))]
                                 [or (empty? or-tags)
                                     (not (empty? (set-intersect or-tags (hash-ref (gldoc-headers doc) 'tags '()))))]
                                 [or (empty? no-tags)
                                     (not (subset? no-tags (hash-ref (gldoc-headers doc) 'tags '())))]
                                 [or (< (hash-ref (gldoc-headers doc) 'timestamp (add1 now)) now)
                                     (not no-future)]
                                 [or (= before-secs 0)
                                     (< (hash-ref (gldoc-headers doc) 'timestamp before-secs) before-secs)]
                                 [or (= after-secs 0)
                                     (> (hash-ref (gldoc-headers doc) 'timestamp after-secs) after-secs)]
                                 #t))
            doc))
    ;; sorting
    (set! results (sort results (lambda (a b) (gldoc-sort a b sort-key))))
    (if sort-reverse (reverse results) results)))

;; gldoc sorting function
(define (gldoc-sort a b key)
  (let ([dk (lambda (doc key) (hash-ref (gldoc-headers doc) key
                                       (hash-ref (gldoc-headers doc) 'name)))])
    (let ([ka (dk a key)]
          [kb (dk b key)])
      (cond [(and (number? ka) (number? kb)) (< ka kb)]
            [else (string<? ka kb)]))))

;; parse a body of text (dispatches transforms)
(define (parse-body text
                    transform-type
                    #:settings [settings default-settings]
                    #:options [options null])
  (when (null? options)
    (set! options (make-hash `((geeklog-settings . ,settings)))))
  ((hash-ref transforms transform-type) text #:settings settings #:options options))

;; load a geeklog document, which is basically headers\n\nbody
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

;; parse a geeklog document headers
(define (parse-headers text
                       #:filename [filename ""]
                       #:settings [settings default-settings])
  (let ([lines (regexp-split #px"\n" text)] [headers (make-hash)])
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
    (for ([key '(title author tags keywords description comment)])
      (unless (hash-has-key? headers key) (hash-set! headers key "")))
    ;; (eprintf "\theaders so far: ~v\n" headers)
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
        [cpu null] [real null] [gc null])
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
    (unless (hash-has-key? params 'doc) (hash-set! params 'doc (hash-ref settings 'default-doc)))
    (set!-values (geekdoc cpu real gc)
          (time-apply
           (lambda (n) (load-doc (hash-ref params 'doc (hash-ref settings 'default-doc))
                            #:settings settings))
           '(1)))
    (set! geekdoc (first geekdoc))
    (hash-set! doc 'title (hash-ref! (gldoc-headers geekdoc) 'title (hash-ref params 'doc)))
    (hash-set! doc 'body (gldoc-body geekdoc))
    (eval `(require web-server/templates) tns)
    (namespace-set-variable-value! 'timing (list cpu real gc) #f tns)
    (namespace-set-variable-value! 'doc doc #f tns)
    (namespace-set-variable-value! 'gldoc gldoc #f tns)
    (namespace-set-variable-value! 'gldoc-body gldoc-body #f tns)
    (namespace-set-variable-value! 'headers (gldoc-headers geekdoc) #f tns)
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
