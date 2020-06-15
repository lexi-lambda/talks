#lang at-exp curly-fn slideshow

(require (for-syntax racket/list)

         pict/conditional
         ppict
         racket/draw
         racket/runtime-path
         rsvg
         slideshow/code
         syntax/parse/define
         threading

         (only-in slideshow [current-font-size current-text-size] [slide slideshow:slide])
         (rename-in (except-in slideshow/staged-slide slide/staged) [staged slideshow:staged]))

;; ---------------------------------------------------------------------------------------------------

(define (make-lazy-parameter val)
  (make-derived-parameter (make-parameter val)
                          identity
                          (λ (v) (if (procedure? v) (v) v))))

(define keyword-apply/defaults
  (make-keyword-procedure
   (λ (def-kws def-kw-args proc given-kws given-kw-args pos-args)
     (define kws+args (for/fold ([kws+args (map cons given-kws given-kw-args)]
                                 #:result (sort kws+args keyword<? #:key car))
                                ([def-kw (in-list def-kws)]
                                 [def-kw-arg (in-list def-kw-args)])
                        (if (assq def-kw kws+args)
                            kws+args
                            (cons (cons def-kw def-kw-arg) kws+args))))
     (keyword-apply proc (map car kws+args) (map cdr kws+args) pos-args))))

(define (align->superimpose align)
  (case align
    [(lt) lt-superimpose]
    [(ct) ct-superimpose]
    [(rt) rt-superimpose]
    [(lc) lc-superimpose]
    [(cc) cc-superimpose]
    [(rc) rc-superimpose]
    [(lb) lb-superimpose]
    [(cb) cb-superimpose]
    [(rb) rb-superimpose]))

(define (pict-when test then)
  (pict-if test then (blank)))
(define (pict-unless test then)
  (pict-if test (blank) then))

;; ---------------------------------------------------------------------------------------------------

(define (set-brush p color)
  (define draw-p (make-pict-drawer p))
  (define brush (new brush% [color color]))
  (dc (λ (dc x y)
        (define brush0 (send dc get-brush))
        (send dc set-brush brush)
        (draw-p dc x y)
        (send dc set-brush brush0))
      (pict-width p)
      (pict-height p)
      (pict-ascent p)
      (pict-descent p)))

(define (set-pen p #:color [color "black"] #:width [width 0] #:scale? [scale? #f])
  (define draw-p (make-pict-drawer p))
  (dc (λ (dc x y)
        (define w
          (cond
            [scale? width]
            [else
             ; not really accurate at the moment
             (match-define (vector (vector sx0 _ _ sy0 _ _) _ _ sx1 sy1 _) (send dc get-transformation))
             (define sx (* sx0 sx1))
             (define sy (* sy0 sy1))
             (define s (/ (+ sx sy) 2))
             (/ width s)]))
        (define pen (new pen% [color color] [width w]))
        (define pen0 (send dc get-pen))
        (send dc set-pen pen)
        (draw-p dc x y)
        (send dc set-pen pen0))
      (pict-width p)
      (pict-height p)
      (pict-ascent p)
      (pict-descent p)))

(define (isolate p)
  (define draw-p (make-pict-drawer p))
  (dc (λ (dc x y)
        ; for reasons I cannot fathom, this prevents rsvg from screwing up the color of subsequent
        ; draw operations
        (send dc draw-point -inf.0 -inf.0)
        (draw-p dc x y))
      (pict-width p)
      (pict-height p)
      (pict-ascent p)
      (pict-descent p)))

(define-runtime-path hasura-icon-black.svg "../lib/assets/hasura_icon_black.svg")
(define-runtime-path postgres-logo.svg "../lib/assets/postgres_logo.svg")
(define-runtime-path timed.1.png "assets/timed.1.png")
(define-runtime-path timed.2.png "assets/timed.2.png")
(define-runtime-path incremental-pr.png "assets/incremental-pr.png")
(define-runtime-path incremental-pr-build.png "assets/incremental-pr-build.png")
(define hasura-logo (isolate (svg-file->pict hasura-icon-black.svg)))
(define postgres-logo (isolate (svg-file->pict postgres-logo.svg)))
(define timings-1 (bitmap timed.1.png))
(define timings-2 (bitmap timed.2.png))
(define incremental-pr (bitmap incremental-pr.png))
(define incremental-pr-build (bitmap incremental-pr-build.png))

;; ---------------------------------------------------------------------------------------------------

(define current-slide-name (make-parameter #f))
(define current-slide-title (make-parameter #f))
(define current-slide-condense? (make-parameter #f))

(define slide
  (make-keyword-procedure
   (λ (kws kw-args . args)
     (keyword-apply/defaults slideshow:slide kws kw-args args
                             #:layout 'top
                             #:name (or (current-slide-name)
                                        (and (string? (current-slide-title))
                                             (current-slide-title)))
                             #:title (current-slide-title)
                             #:condense? (current-slide-condense?)))))

(begin-for-syntax
  (define-splicing-syntax-class (stage-spec #:condense-by-default? condense-by-default?)
    #:description "stage spec"
    #:attributes [id condense?]
    #:commit
    [pattern {~seq #:! ~! id:id}
     #:attr condense? #f]
    [pattern {~seq #:~ ~! id:id}
     #:attr condense? #t]
    [pattern id:id
     #:attr condense? condense-by-default?])

  (define-syntax-class stages-spec
    #:description "stages spec"
    #:attributes [[id 1] [condensed-id 1]]
    #:commit
    [pattern [{~var stage (stage-spec #:condense-by-default? #t)} ...
              {~var final-stage (stage-spec #:condense-by-default? #f)}]
     #:with [id ...] #'[stage.id ... final-stage.id]
     #:do [(define condense?s (append (attribute stage.condense?)
                                      (list (attribute final-stage.condense?))))]
     #:attr [condensed-id 1] (filter-map #{and %2 %1} (attribute id) condense?s)]))

(define-syntax-parser staged
  [(_ stages:stages-spec body ...)
   (syntax/loc this-syntax
     (slideshow:staged [stages.id ...]
       (parameterize ([current-slide-condense? (member stage (list stages.condensed-id ...))])
         body ...)))])
(define-syntax-parser slide/staged
  [(_ stages:stages-spec body ...)
   (quasisyntax/loc this-syntax
     (staged stages
       #,(syntax/loc this-syntax
           (slide body ...))))])

;; ---------------------------------------------------------------------------------------------------

(define background-color (make-color #xFF #xFF #xFF))
(define code-background-color (make-color #xFA #xE9 #xE6))
(define code-border-color (make-color #xE8 #xCC #xC8))
(define text-plain-color (make-color #x00 #x19 #x34))
(define text-secondary-color (make-color #x5C #x2E #x2E))
(define highlight-color (make-color #x1c #xd3 #xc6))
(define shadow-color (make-color #x70 #x30 #x30))

(define current-text-color (make-parameter text-plain-color))
(define current-title-text-color (make-lazy-parameter current-text-color))

(current-base-color (make-color #x9E #x55 #x55))
(current-keyword-color (make-color #x96 #x00 #x37))
(current-id-color text-secondary-color)
(current-literal-color (make-color #xBA #x21 #x21))
(current-comment-color (make-color #x9E #x55 #x55))
(let ([super (current-token-class->color)])
  (current-token-class->color
   (λ (c) (case c
            [(text) (current-id-color)]
            [else (super c)]))))

(current-main-font "Poppins")
(current-text-size 40)

(current-code-font '((weight . semibold) . "Fira Mono"))
(current-code-line-sep 5)
(define (tt s) (text s (current-code-font) (current-text-size)))
(define (itt s) (text s (cons 'italic (current-code-font)) (current-text-size)))
(define (btt s) (text s (cons 'bold (current-code-font)) (current-text-size)))
(define (emoji s) (text s "Apple Color Emoji" (current-text-size)))

(define (ct p) (colorize (if (string? p) (t p) p) (current-text-color)))

(define current-title-text-size (make-parameter 60))
(current-titlet (λ (s) (parameterize ([current-text-size (current-title-text-size)]
                                      [current-text-color (current-title-text-color)])
                         (ct (bt s)))))
(set-title-h! (pict-height (titlet "X")))

(set-margin! 40)

(let ([old-slide-assembler (current-slide-assembler)])
  (current-slide-assembler
   (λ (title-str gap content)
     (define background
       (inset (filled-rectangle (+ client-w (* margin 2))
                                (+ client-h (* margin 2))
                                #:draw-border? #f
                                #:color background-color)
              (- margin)))
     (define background+extras
       (~> background
           (pin-over/align 0 0 'l 'b (filled-rectangle 60 8 #:draw-border? #f #:color highlight-color))
           (rt-superimpose (scale-to-fit hasura-logo 50 50))))
     (define title (and title-str ((current-titlet) title-str)))
     (define content-area
       (if title
           (blank (pict-width background)
                  (- (pict-height background)
                     (pict-height title)
                     gap))
           background))
     (define bounded-content (scale-to-fit content content-area #:mode 'inset))
     (define title+content (if title (vl-append gap title bounded-content) bounded-content))
     (cc-superimpose background+extras title+content))))

;; ---------------------------------------------------------------------------------------------------

(define (pygmentize str #:language language)
  (define pygments-output
    (with-output-to-string
      (λ ()
        (unless (parameterize ([current-input-port (open-input-string str)])
                  (system* (find-executable-path "pygmentize") "-l" language "-f" "raw"))
          (raise-arguments-error 'pygmentize "unclean exit")))))

  (define tt (current-code-tt))
  (define base-color (current-base-color))
  (define comment-color (current-comment-color))
  (define id-color (current-id-color))
  (define keyword-color (current-keyword-color))
  (define literal-color (current-literal-color))
  (define (finish-line ps) (apply hbl-append (reverse ps)))

  (apply
   vl-append (current-code-line-sep)
   (for/fold ([lines '()]
              [line '()]
              #:result (reverse lines))
             ([directive (in-list (string-split pygments-output "\n"))])
     (match-define (regexp #px"^([a-zA-Z.]+)\t('|\")(.*)\\2$"
                           (list _ type _ contents))
       directive)
     (define parsed-contents (~> contents
                                 (string-replace "\\\\" "\\")
                                 (string-replace "\\n" "\n")))
     (define all-line-contents (if (zero? (string-length parsed-contents))
                                   (list "")
                                   (string-split parsed-contents "\n" #:trim? #f)))
     (define color
       (match type
         [(regexp #px"^Token\\.Comment")
          comment-color]
         [(regexp #px"^Token\\.(Keyword|Name\\.Builtin|Operator)")
          keyword-color]
         [(regexp #px"^Token\\.Literal")
          literal-color]
         ["Token.Punctuation"
          base-color]
         [(regexp #px"^Token\\.(Name|Text)")
          id-color]))
     (match-define (list finished-lines ... last-line)
       (map #{colorize (tt %) color} all-line-contents))
     (define-values [lines* line*]
       (for/fold ([lines lines] [line line])
                 ([finished-line (in-list finished-lines)])
         (values (cons (finish-line (cons finished-line line)) lines) '())))
     (values lines* (cons last-line line*)))))

(define (codeblock #:keep-lang-line? [keep-lang-line? #f] . strs)
  (codeblock-pict (string-append* strs) #:keep-lang-line? keep-lang-line?))
(define (codeblock/pygments #:language language . strs)
  (pygmentize (string-append* strs) #:language language))

;; ---------------------------------------------------------------------------------------------------

(define (arrow-line #:arrow-size [arrow-size 10]
                    #:line-length [line-length 70]
                    #:line-width [line-width 2])
  (panorama (pin-over/align (linewidth line-width (hline line-length line-width))
                            line-length (/ line-width 2) 'c 'c
                            (arrowhead arrow-size 0))))

(define (database w h)
  (define sh (/ h 8))
  (define dy (/ (- h sh) 3))
  (dc (λ (dc x y)
        (define tr (send dc get-transformation))
        (send dc translate x y)
        (define path (new dc-path%))
        (send path arc 0 0 w sh pi 0 #f)
        (send path line-to w (- h sh))
        (send path arc w (- h sh) (- w) sh 0 pi #f)
        (send path close)
        (send dc draw-path path 0 0 'winding)
        (send dc draw-arc 0 0 w sh pi 0)
        (send dc draw-arc 0 (+ dy) w sh pi 0)
        (send dc draw-arc 0 (+ dy dy) w sh pi 0)
        (send dc set-transformation tr))
      w
      h))

(define (postgres size)
  (define base (~> (database (* size 17/21) size)
                   (set-brush "gray")
                   (set-pen #:width 4)))
  (~> (scale-to-fit postgres-logo (scale base 3/4))
      (inset 0 0 0 (* size 1/10))
      (cb-superimpose base _)))

(define (hasura size)
  (define base (~> (filled-rounded-rectangle 100 100 -1/8 #:color "white")
                   (set-pen #:width 3 #:scale? #t)))
  (~> (scale-to-fit hasura-logo 75 75)
      (cc-superimpose base _)
      (scale-to-fit size size)))

(define (frame/bg p c #:border-width [width 1])
  (cc-superimpose (filled-rectangle (pict-width p) (pict-height p) #:color c #:border-width width) p))

;; ---------------------------------------------------------------------------------------------------

(parameterize ([current-title-text-size 80])
  (slide #:title "The Hasura Schema Cache"))

(parameterize ([current-slide-title "Metadata & the catalog"])
  (slide/staged [s0 s1 s2 s3 s4 s5]
    (hc-append
     10
     (pict-when (at/after s4)
       (vc-append 5 (vc-append (scale (vc-append (ct "GraphQL") (ct "introspection query")) 1/4)
                                (arrow-line))
                  (pict-when (at/after s5)
                    (vc-append (scale (vc-append (ct "response") (ct "using cache")) 1/4)
                               (rotate (arrow-line) pi)))))
     (rb-superimpose (hasura 85)
                     (pict-when (at/after s3)
                       (postgres 30)))
     (pict-when (at/after s2)
       (vc-append (scale (vc-append (ct "load from") (ct (tt "hdb_catalog"))) 1/4) (rotate (arrow-line) pi)))
     (pict-when (at/after s1)
       (postgres 100))))

  (slide/staged [s1 s3 s4 s5 s6 s7]
    (hc-append
     10
     (pict-when (at/after s3)
       (vc-append (scale (vc-append (ct "metadata change") (ct "request")) 1/4)
                  (arrow-line)))
     (rb-superimpose
      (hasura 85)
      (let ([pg (postgres 30)])
        (~> (pict-when (at/after s7)
              (~> (scale (t "apply patch") 1/5)
                  (inset 3)
                  (frame/bg "honeydew")
                  (colorize "limegreen")))
            (pin-over/align pg (+ (pict-width pg) 10) 10 'r 'b _)
            (refocus pg))))
     (vc-append 5 (pict-when (at/after s4)
                    (vc-append (scale (vc-append (ct "modify") (ct (tt "hdb_catalog"))) 1/4)
                               (arrow-line)))
                (pict-when (at/after s5)
                  (~> (vc-append (scale (vc-append (t "reload /") (t "rebuild cache")) 1/4)
                                 (rotate (arrow-line) pi))
                      (colorize (if (at/after s6) "crimson" (current-text-color))))))
     (postgres 100))))

(slide/staged [s0 s1 s2 s3]
  #:title "Problems"
  (let ([2.p (ct "2. ")])
    (define (evenize p)
      (rbl-superimpose (ghost 2.p) p))
    (vl-append
     20
     (pict-when (at/after s1)
       (htl-append (evenize (ct "1. ")) (ct "Logic to generate patches is very complex")))
     (pict-when (at/after s2)
       (htl-append 2.p (vl-append (ct "Generating minimal patches is difficult") (ct "or infeasible in general"))))
     (pict-when (at/after s3)
       (htl-append (evenize (ct "3. ")) (vl-append (ct "Full schema reload is still necessary") (ct "for other Hasura instances")))))))

(define (dependency-node label #:color [color "lightgray"])
  (~> (filled-rounded-rectangle (+ 100 (pict-width label)) (+ (pict-height label) 30) #:color color)
      (set-pen #:color "gray" #:width 2)
      (cc-superimpose label)))

(define (dependency-graph #:stage [stage 0])
  (define red "LightCoral")
  (define green "Aquamarine")
  (define (color-at stage* color) (if (>= stage stage*) color "lightgray"))
  (define sc (dependency-node (ct "schema cache") #:color (color-at 2 red)))
  (define tabs (dependency-node (ct "tables") #:color (color-at 2 green)))
  (define perms (dependency-node (ct "permissions") #:color (color-at 2 green)))
  (define rels (dependency-node (ct "relationships") #:color (color-at 2 green)))
  (define ets (dependency-node (ct "event triggers") #:color (color-at 2 green)))
  (define compfs (dependency-node (ct "computed fields") #:color (color-at 2 green)))
  (define funcs (dependency-node (ct "functions") #:color (color-at 1 red)))
  (define schemas (dependency-node (ct "remote schemas") #:color (color-at 2 red)))
  (define (add-dep p from to)
    (pin-arrow-line 10 p from ct-find to cb-find #:under? #t #:line-width 2))
  (define (add-deps p from . tos)
    (for/fold ([p p]) ([to (in-list tos)]) (add-dep p from to)))
  (~> (vc-append 75 tabs (hc-append 125 rels compfs) (hc-append 300 perms ets)
                 (inset funcs 0 0 300 0) (inset schemas 300 0 0 0) sc)
      (add-deps sc tabs perms rels ets compfs funcs schemas)
      (add-deps funcs tabs)
      (add-deps schemas tabs funcs)
      (add-deps perms tabs rels)
      (add-deps ets tabs)
      (add-deps schemas rels tabs)
      (add-deps compfs tabs)))

(slide/staged [s1 s2]
  (hc-append
   10
   (vc-append (scale (vc-append (ct "metadata change") (ct "request")) 1/4)
              (arrow-line))
   (rb-superimpose (hasura 85) (postgres 30))
   (vc-append 5 (vc-append (scale (vc-append (ct "modify") (ct (tt "hdb_catalog"))) 1/4)
                           (arrow-line))
              (~> (vc-append (scale (vc-append (t "reload /") (t "rebuild cache")) 1/4)
                             (rotate (arrow-line) pi))
                  (colorize (if (at/after s2) "limegreen" (current-text-color)))))
   (postgres 100)))

(slide/staged [s0 s1 s1.1 s2 s3]
  #:title "Idea: Dependency tracking"
  (hc-append
   50
   (pict-when (at/after s1)
     (~> (dependency-graph)
         (rb-superimpose (pict-when (at/after s1.1)
                           (~> (scale (ct "V1") 3)
                               (inset 30 0)
                               (frame/bg "white" #:border-width 5))))))
   (pict-when (at/after s2)
     (hc-append
      50
      (scale (vc-append (scale (ct "track function") 1/4) (arrow-line)) 5)
      (~> (dependency-graph #:stage (if (before s3) 1 2))
          (rb-superimpose (pict-when (at/after s1.1)
                            (~> (scale (ct "V2") 3)
                                (inset 20 0)
                                (frame/bg "white" #:border-width 5)))))))))

(slide (titlet "New challenge: make it go fast"))

(parameterize ([current-slide-title "Slowness"])
  (slide timings-1)

  (slide/staged [s1 s2 s3 s4]
    (~> timings-1
        (lt-superimpose (pict-when (at s2)
                          (~> (filled-rectangle 800 (pict-height timings-1)
                                                #:color (make-object color% #xed #x78 #xff 0.1))
                              (set-pen #:color "VioletRed" #:width 4))))
        (rt-superimpose (pict-when (at s3)
                          (~> (filled-rectangle (- (pict-width timings-1) 800) (pict-height timings-1)
                                                #:color (make-object color% #xed #x78 #xff 0.1))
                              (set-pen #:color "VioletRed" #:width 4))))
        (lt-superimpose (pict-when (at s4)
                          (~> (filled-rectangle (pict-width timings-1) 200
                                                #:color (make-object color% #xed #x78 #xff 0.1))
                              (set-pen #:color "VioletRed" #:width 4)
                              (inset 0 435 0 0))))
        (inset 0 0 (* (pict-width timings-1) -2/3) 0)))

  (slide/staged [s1 s2 s3]
    (vc-append
     50
     (~> timings-1
         (cc-superimpose (pict-when (at/after s2)
                           (~> (filled-rectangle (pict-width timings-1) 250
                                                 #:color (make-object color% #xed #x78 #xff 0.1))
                               (set-pen #:color "VioletRed" #:width 2)
                               (inset 0 330 0 0)))))
     (pict-when (at/after s3)
       (~> (ct (t "Why are we doing so much work?"))
           (scale 10))))))

(slide/staged [s0 s1 s2 s2.1 s3]
  #:title "Course-grained caching"
  (vc-append
   150
   (pict-when (at/after s1) @codeblock/pygments[#:language "json"]|{{"id":{"_eq":"X-Hasura-User-Id"}}}|)
   (pict-when (at/after s2)
     @codeblock/pygments[#:language "json"]|{{"_exists":{"_table":"users","_where":{"id":{"_eq":"X-Hasura-User-Id"}}}}}|)
   (pict-when (at/after s3)
     (~> (ct (hbl-append (t "All permissions get rebuilt when ") (it "any") (t " table changes! D:")))
         (scale 3/2)))))

#;(parameterize ([current-slide-title "Course-Grained Caching"])
  (slide/staged [s1 s2 s3 s4 s5]
    (vc-append
     100
     (pict-when (at/after s2)
      @codeblock/pygments[#:language "haskell"]{
        buildPermission ::
          Rule (TableCache, TableName, [CatalogPermission])
               (Maybe PermissionInfo)})
     (~> (pict-when (at/after s3)
      @codeblock/pygments[#:language "haskell"]{
        buildPermission ::
             (TableCache, TableName, [CatalogPermission])
          -> Maybe PermissionInfo})
         (cc-superimpose (pict-when (at s4)
                           (~> (filled-rectangle 1100 100
                                                 #:color (make-object color% #xed #x78 #xff 0.1))
                               (set-pen #:color "VioletRed" #:width 4)
                               (inset 100 0 0 0))))
         (cc-superimpose (pict-when (at s5)
                           (~> (filled-rectangle 320 90
                                                 #:color (make-object color% #xed #x78 #xff 0.1))
                               (set-pen #:color "VioletRed" #:width 4)
                               (inset 0 0 660 0))))))))

(parameterize ([current-slide-title "Solution: fine-grained caching"])
  (slide timings-2))

(slide/staged [s1 s2 s3]
  (vc-append
   30
   (scale incremental-pr 2/3)
   (pict-when (at/after s2) incremental-pr-build)
   (pict-when (at/after s3) (scale (ct (bt "Feedback welcome!")) 5))))

#;(start-at-recent-slide)
