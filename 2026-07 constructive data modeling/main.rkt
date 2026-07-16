#lang at-exp slideshow

(require (for-syntax racket/match
                     syntax/parse/experimental/template)
         pict/conditional
         pict/shadow
         ppict/align
         ppict/tag
         racket/draw
         racket/runtime-path
         racket/sandbox
         slideshow/code
         slideshow/text
         scribble/example
         syntax/parse/define
         threading

         (prefix-in racket: racket/base)
         (prefix-in slideshow: slideshow/base)
         (only-in slideshow [current-font-size current-text-size])

         "lib/color.rkt"
         "lib/pict.rkt"
         "lib/slideshow.rkt"
         "lib/unicode.rkt"
         "lib/util.rkt")

(begin
  (define-runtime-path august-logo.png "assets/august-logo.png")
  (define-runtime-path c++-logo.png "assets/cpp-logo.png")
  (define-runtime-path java-logo.png "assets/java-logo.png")
  (define-runtime-path haskell-logo.png "assets/haskell-logo.png")
  (define-runtime-path rust-logo.png "assets/rust-logo.png")
  (define-runtime-path scala-logo.png "assets/scala-logo.png")
  (define-runtime-path ts-logo.png "assets/ts-logo.png")
  (define-runtime-path pdv.png "assets/pdv.png")
  (define-runtime-path emar.png "assets/emar.png")
  (define-runtime-path sowing-reaping.png "assets/sowing-reaping.png")

  (define august-logo (scale-to-fit (bitmap august-logo.png) 400 400))
  (define c++-logo (scale-to-fit (bitmap c++-logo.png) 400 400))
  (define java-logo (scale-to-fit (bitmap java-logo.png) 400 400))
  (define haskell-logo (scale-to-fit (bitmap haskell-logo.png) 400 400))
  (define rust-logo (scale-to-fit (bitmap rust-logo.png) 400 400))
  (define scala-logo (scale-to-fit (bitmap scala-logo.png) 400 400))
  (define ts-logo (scale-to-fit (bitmap ts-logo.png) 400 400))
  (define pdv (scale-to-fit (bitmap pdv.png) 400 800))
  (define emar (scale-to-fit (bitmap emar.png) 800 800))
  (define sowing-reaping (scale-to-fit (bitmap sowing-reaping.png) 800 800)))

(define CONFERENCE-NAME "Software Should Work")

;; ---------------------------------------------------------------------------------------------------

(define section (make-parameter #f))

(current-main-font "Concourse T3")
(current-code-font '((weight . 500) no-combine . "Fira Code"))
(current-text-size 40)
(current-line-sep -5)
(get-current-code-font-size
 (thunk (round (* (current-text-size) 9/10))))

(define transparent (make-color 0 0 0 0))
(define background-color (make-color #xf9 #xf9 #xf9))
(define code-background-color (make-color #xFA #xE9 #xE6))
(define code-border-color (make-color #xE8 #xCC #xC8))
(define text-plain-color (make-color #x40 #x40 #x40))
(define text-secondary-color (make-color #x60 #x60 #x60))
(define text-tertiary-color (make-color #xa0 #xa0 #xa0))
(define tertiary-color (make-color #xc0 #xc0 #xc0))
(define text-secondary-highlight-color (make-color #xb5 #xd0 #xff))
(define text-tertiary-highlight-color (make-color #xc7 #xff #xcd))
(define text-error-color (make-color #x8a #x16 #x16))
(define interaction-output-color (make-color #x96 #x00 #x96))
(define interaction-result-color (->color% (hsv 23/36 0.77 0.74)))
(define shadow-color (make-color #x70 #x30 #x30))

(define shape-bg-color (make-color #xf7 #xf7 #xf7))
(define shape-border-color (make-color #xd8 #xd8 #xd8))

(current-highlight-color (make-color #xff #xd4 #xd1))

(current-text-color text-plain-color)
(current-base-color text-plain-color)
(current-keyword-color text-plain-color)
(current-id-color (make-color #x37 #x50 #x73))
(current-literal-color (make-color #x87 #x4f #x37))
(current-comment-color (make-color #x9E #x55 #x55))
(define current-constructor-color (make-parameter (make-color #x59 #x37 #x73)))
(let ([super (current-token-class->color)])
  (current-token-class->color
   (λ (c) (case c
            [(constructor) (current-constructor-color)]
            [else (super c)]))))

(define (c:plain p) (colorize p text-plain-color))

(define (flavor-highlight-color flavor)
  (->color% (match flavor
              [0 (hsv 0.01 0.18 1.0)]
              [1 (hsv 0.35 0.22 1.0)]
              [2 (hsv 0.60 0.29 1.0)])))

(define (flavor-text-color flavor)
  (->color% (match flavor
              [0 (hsv 0.01 0.80 0.85)]
              [1 (hsv 0.35 0.60 0.60)]
              [2 (hsv 0.60 0.80 0.80)])))

;; ---------------------------------------------------------------------------------------------------

(define-syntax-parser #%top
  [(_ . id:id)
   #:do [(define tag-name
           (match (symbol->string (syntax-e #'id))
             [(regexp #px"^t:(.+)$" (list _ tag-name)) tag-name]
             [_ #f]))]
   #:when tag-name
   (quasisyntax/loc #'id
     (tag* '#,(string->symbol tag-name)))]
  [(_ . id) (quasisyntax/loc this-syntax (racket:#%top . id))])

(define c:highlights (make-parameter '()))
(define c:flavors (make-parameter (hash '|1| 1 '|2| 2)))

(define (tag-highlight-color tag)
  (flavor-highlight-color (hash-ref (c:flavors) tag 0)))
(define (tag-text-color tag)
  (flavor-text-color (hash-ref (c:flavors) tag 0)))

(struct tagged (tag values) #:transparent)
(define (tag t . vs)
  (match vs
    [(list (? pict? p))
     (~> (tag-pict p t)
         (when~> (memq t (c:highlights))
           (highlight #:color (tag-highlight-color t))))]
    [_
     (tagged t vs)]))
(define ((tag* t) . vs)
  (apply tag t vs))

;; ---------------------------------------------------------------------------------------------------

(set-margin! 0)
(set-title-h! 80)

(current-slide-margin 30)
(current-slide-assembler (make-scaling-slide-assembler #:background-color background-color))

(current-titlet
 (λ (s) (parameterize ([current-main-font "Concourse C3"])
          (colorize (t (string-downcase s)) text-plain-color))))

;; ---------------------------------------------------------------------------------------------------

(define slides-prompt-tag (make-continuation-prompt-tag 'slides))
(define current-slide-render (make-parameter (thunk (error 'current-slide-render "no slide renderer"))))

(struct slide-info (title body skippable?) #:transparent
  #:guard (struct-guard/c (or/c string? pict? #f) pict? any/c))

(define (call-with-slide-renderer render-thunk body-thunk #:title [title #f])
  (define skipped 0)
  (parameterize ([current-slide-render render-thunk])
    (let loop ([continue-thunk body-thunk])
      (call-with-continuation-prompt
       continue-thunk slides-prompt-tag
       (λ (info continue)
         (cond
           [(and condense? (slide-info-skippable? info))
            (set! skipped (add1 skipped))
            (loop continue)]
           [else
            ; add a bunch of 'nexts to the slide to tell slideshow that some slides were dropped,
            ; which will cause it to display a range for the slide number
            (apply slide (append (make-list skipped 'next) (list (slide-info-body info)))
                   #:title (slide-info-title info)
                   #:name (section)
                   #:layout 'top)
            (set! skipped 0)
            (loop continue)])))))
  (skip-slides skipped))

(define (add-slide! info)
  (call-with-composable-continuation
   (λ (continue)
     (abort-current-continuation slides-prompt-tag
                                 info
                                 (thunk (continue (void)))))
   slides-prompt-tag))

(define (render-slide! #:skippable? skippable?)
  (define-values [title body] ((current-slide-render)))
  (add-slide! (slide-info title body (and skippable? #t))))
(define (next)  (render-slide! #:skippable? #t))
(define (next!) (render-slide! #:skippable? #f))

(define-syntax-parser slides
  [(_ ({~describe "binding pair" [x:id e:expr]} ...)
      {~alt {~seq #:with param:expr param-val:expr}
            {~optional {~seq #:title title-e:expr}}
            {~once {~var render-e (expr/c #'pict? #:name "render expression")}}
            {~optional {~seq #:timeline timeline-body:expr ...+}}
            {~optional {~and #:condense-last {~bind [condense-last? #t]}}}}
      ...
      {~optional {~seq #:where ~! where-body:expr ...}})

   (define stx this-syntax)
   (define-template-metafunction maybe-tl:last!
     (syntax-parser
       [(_ body ...) (if (attribute condense-last?)
                         (syntax/loc stx (let () body ...))
                         (syntax/loc stx (tl:last! body ...)))]))

   (quasisyntax/loc this-syntax
     (let ([x (make-parameter e)] ...)
       (parameterize ([c:highlights (c:highlights)]
                      [param param-val] ...)
         (call-with-slide-renderer
          #,(syntax/loc #'render-e (thunk {~? {~@ where-body ...}} (values {~? title-e #f} render-e)))
          #,(syntax/loc this-syntax (thunk {~? (maybe-tl:last! {~@ timeline-body ...}) (next!)} (void)))))))])

(define (blank-slide)
  (slide #:name (section) #:condense? #t))

;; ---------------------------------------------------------------------------------------------------

(define (tl:last!/proc continue)
  (define prev #f)
  (begin0
    (let loop ([continue continue])
      (call-with-continuation-prompt
       continue slides-prompt-tag
       (λ (info continue)
         (when prev (add-slide! prev))
         (set! prev info)
         (loop continue))))
    (if prev
        (add-slide! (struct-copy slide-info prev [skippable? #f]))
        (error 'tl:last! "timeline did not yield"))))

(define-simple-macro (tl:last! body ...+)
  (tl:last!/proc (thunk body ...)))

(define (tl:sequence param seq)
  (for ([v seq])
    (param v)
    (next)))

(define (tl:range param start end)
  (tl:sequence param (in-inclusive-range start end)))

(define (tl:flags . params)
  (for ([param (in-list params)])
    (param (not (param)))
    (next)))

(define (c:highlight+ which)
  (if (list? which)
      (c:highlights (append which (c:highlights)))
      (c:highlights (cons which (c:highlights)))))

(define (c:highlight- which)
  (if (list? which)
      (c:highlights (remove* which (c:highlights)))
      (c:highlights (remove which (c:highlights)))))

(define (tl:highlight+ . whichs)
  (for ([which (in-list whichs)])
    (c:highlight+ which)
    (next)))

(define (tl:highlight1 . whichs)
  (for ([which (in-list whichs)])
    (c:highlight+ which)
    (next)
    (c:highlight- which)))

;; ---------------------------------------------------------------------------------------------------

(define current-code-tt (make-parameter tt))

(define (haskell-code-tt str)
  (parameterize ([current-keyword-list '("let" "if" "then" "else" "try" "catch" "def" "void" "match" "->" ".")])
    (codeblock-pict
     #:keep-lang-line? #f
     (string-append "#lang haskell-lexer/distinguish-constructors\n" str))))

(define (code . elems)
  (define (decode elem)
    (match elem
      ["\n" 'newline]
      [(tagged t elems)
       (tag t (apply code elems))]
      [(? string?)
       ((current-code-tt) elem)]
      [_ elem]))

  (define H (tt "H"))
  (define blank-line (blank 0 (pict-height H) (pict-ascent H) (pict-descent H)))
  (define lines
    (for/fold ([lines (list blank-line)])
              ([elem (in-list elems)])
      (match (decode elem)
        ['newline (cons blank-line lines)]
        [p        (cons (line-append (first lines) p) (rest lines))])))
  (apply vl-append (current-code-line-sep) (reverse lines)))

(define (haskell . elems)
  (parameterize ([current-code-tt haskell-code-tt])
    (apply code elems)))

(define (matht str)
  (text str "Blackboard Modern Math" (current-text-size)))

(define (mathv str)
  (matht (apply string (for/list ([c (in-string str)])
                         (math-char c #:italic? #t)))))

(define (mathit str)
  (text str (cons 'italic "Blackboard Modern Roman") (current-text-size)))

(define (basic-step #:ip ip #:leaf? [leaf? #t] . lines)
  (~> (for/list ([(line i) (in-indexed lines)])
        (define redex? (and leaf? (= i ip)))
        (hc-append
         (pict-when redex?
           (~> (arrow-line #:arrow-size 20 #:line-length 0)
               (colorize text-plain-color)))
         (blank 5 0)
         (if (< i ip)
             (colorize (apply code line) text-tertiary-color)
             (tag (if redex? 'redex 'continuation) (apply haskell line)))))
      (apply vl-append (current-code-line-sep) _)))

(define (ol #:sep [sep (em 4/5)]
            #:spacing [spacing (current-para-spacing)]
            #:stage [stage #f]
            . elems)
  (define num-picts (parameterize ([current-main-font "Concourse Index"])
                      (for/list ([i (in-range (length elems))])
                        (c:plain (t (~a (add1 i)))))))
  (define max-num-width (apply max (map pict-width num-picts)))
  (~>> (for/list ([elem (in-list elems)]
                  [num (in-list num-picts)])
         (htl-append sep (indent #:by (- max-num-width (pict-width num)) num) elem))
       (apply paras #:spacing spacing #:stage stage)))

(define current-shape-border-width (make-parameter 2))

(define (spike size #:border-width [border-width (current-shape-border-width)])
  (define s/2 (/ size 2))
  (define path (new dc-path%))
  (send path move-to 0 0)
  (send path line-to s/2 s/2)
  (send path line-to size 0)
  (dc (λ (dc x y)
        (define old-pen (send dc get-pen))
        (define old-brush (send dc get-brush))
        (send dc set-pen (make-pen #:width border-width
                                   #:color shape-border-color))
        (send dc set-brush (make-brush #:color shape-bg-color))
        (send dc draw-path path x y)
        (send dc set-pen old-pen)
        (send dc set-brush old-brush))
      size
      s/2))

(define (box w h
             #:highlight [highlight-tag #f]
             #:border-width [border-width (current-shape-border-width)])
  (define highlight? (member highlight-tag (c:highlights)))
  (filled-rectangle w h
                    #:color (if highlight? (tag-highlight-color highlight-tag) shape-bg-color)
                    #:border-width border-width
                    #:border-color (if highlight? (tag-text-color highlight-tag) shape-border-color)))

(define current-box-padding (make-parameter 15))
(define (wrap-box p
                  #:padding [padding (current-box-padding)]
                  #:highlight [highlight-tag #f])
  (cc-superimpose (box (+ (pict-width p) (* padding 2))
                       (+ (pict-height p) (* padding 2))
                       #:highlight highlight-tag)
                  p))

(define (balloon w h
                 #:corner-radius [r 25]
                 #:spike-size [spike-s 25]
                 #:spike-position [spike-posn 0]
                 #:spike-side [spike-side 'top]
                 #:color [color shape-bg-color]
                 #:border-color [border-color shape-border-color]
                 #:border-width [bw (current-shape-border-width)]
                 #:highlight [highlight-tag #f])
  (define 2spike-s (* 2 spike-s))
  (define spike-side-type (match spike-side
                            [(or 'top 'bottom) 'horizontal]
                            [(or 'left 'right) 'vertical]))
  (define spike-side-len (match spike-side-type
                           ['horizontal w]
                           ['vertical   h]))
  (define spike-offset (* spike-posn (- spike-side-len 2spike-s)))
  (define posn-after-spike (+ spike-offset 2spike-s))
  (define len-after-spike (- spike-side-len (+ spike-offset 2spike-s)))

  (define tl-r (cond
                 [(eq? spike-side 'left)   (min r spike-offset)]
                 [(eq? spike-side 'top)    (min r spike-offset)]
                 [else                     r]))
  (define tr-r (cond
                 [(eq? spike-side 'right)  (min r spike-offset)]
                 [(eq? spike-side 'top)    (min r len-after-spike)]
                 [else                     r]))
  (define bl-r (cond
                 [(eq? spike-side 'left)   (min r len-after-spike)]
                 [(eq? spike-side 'bottom) (min r spike-offset)]
                 [else                     r]))
  (define br-r (cond
                 [(eq? spike-side 'right)  (min r len-after-spike)]
                 [(eq? spike-side 'bottom) (min r len-after-spike)]
                 [else                     r]))
  
  (define path (new dc-path%))
  ; top left corner
  (send path arc 0 0 tl-r tl-r (turns 1/2) (turns 1/4) #f)
  ; top side
  (when (eq? spike-side 'top)
    (send path line-to spike-offset 0)
    (send path line-to (+ spike-offset spike-s) (- spike-s))
    (send path line-to (+ spike-offset 2spike-s) 0))
  (send path line-to tr-r 0)
  ; top right corner
  (send path arc (- w tr-r) 0 tr-r tr-r (turns 1/4) 0 #f)
  ; right side
  (when (eq? spike-side 'right)
    (send path line-to w spike-offset)
    (send path line-to (+ w spike-s) (+ spike-offset spike-s))
    (send path line-to w (+ spike-offset 2spike-s)))
  (send path line-to w br-r)
  ; bottom right corner
  (send path arc (- w br-r) (- h br-r) br-r br-r 0 (turns -1/4) #f)
  ; bottom side
  (when (eq? spike-side 'bottom)
    (send path line-to posn-after-spike h)
    (send path line-to (- posn-after-spike spike-s) (+ h spike-s))
    (send path line-to (- posn-after-spike 2spike-s) h))
  (send path line-to bl-r h)
  ; bottom left corner
  (send path arc 0 (- h bl-r) bl-r bl-r (turns 3/4) (turns 1/2) #f)
  ; left side
  (when (eq? spike-side 'left)
    (send path line-to 0 posn-after-spike)
    (send path line-to spike-s (- posn-after-spike spike-s))
    (send path line-to 0 (- posn-after-spike 2spike-s)))
  (send path close)

  (define highlight? (member highlight-tag (c:highlights)))
  (define color* (if highlight? (tag-highlight-color highlight-tag) color))
  (define border-color* (if highlight? (tag-text-color highlight-tag) border-color))

  (~> (dc (λ (dc dx dy) (send dc draw-path path dx (+ dy spike-s))) w (+ h spike-s))
      (inset 0 (- spike-s) 0 0)
      (adjust-pen #:color border-color* #:width bw)
      (maybe-colorize color*)))

(define (encircle p
                  #:padding [padding 15]
                  #:highlight? [highlight? #f]
                  #:color [color (if highlight? (current-highlight-color) shape-bg-color)]
                  #:border-color [border-color (if highlight? (current-highlight-border-color) shape-border-color)]
                  #:border-width [border-width 2])
  (~> (disk (+ (max (pict-width p) (pict-height p))
               (* (+ padding (/ border-width 2)) 2))
            #:color color
            #:border-width border-width
            #:border-color border-color)
      (cc-superimpose p)))

(define (p:file p
                #:color [color shape-bg-color]
                #:border-color [border-color shape-border-color])
  (cc-superimpose (~> (file-icon 40 50)
                      (adjust-pen #:color border-color
                                  #:width 1.25
                                  #:cap 'projecting
                                  #:join 'miter)
                      (colorize color))
                  (~> (scale-to-fit p 25 35)
                      (colorize text-secondary-color))))

(define terminal-text-color (->color% (hsv 0 0 0.93)))
(define terminal-bg-color (->color% (hsv 0 0 0.2)))
(define terminal-highlight-color (->color% (hsv 0 0.3 0.4)))
(define (wrap-terminal-frame p #:padding [padding 20])
  (cc-superimpose
   (filled-rounded-rectangle (+ (pict-width p) (* padding 2))
                             (+ (pict-height p) (* padding 2))
                             15
                             #:draw-border? #f
                             #:color terminal-bg-color)
   (colorize p terminal-text-color)))

(define (strikethrough p
                       #:line-width [line-width 2]
                       #:color [color #f])
  (pin-over p 0 (- (* (pict-ascent p) 3/4) (/ line-width 2))
            (filled-rectangle (pict-width p) line-width #:draw-border? #f #:color color)))

(define (cross-out p
                   #:line-width [line-width 2]
                   #:color [color #f])
  (define w (pict-width p))
  (define h (pict-height p))
  (~> p
      (pin-over p cc-find #:hole cc-find
                (adjust-pen #:width line-width #:color color (line w h)))
      (pin-over p cc-find #:hole cc-find
                (adjust-pen #:width line-width #:color color (line w (- h))))))

(define (steps #:append p-append
               #:stage stage
               . ps)
  (~> (for/list ([(p i) (in-indexed (in-list ps))])
        (pict-when (> stage i) p))
      (apply p-append _)))

(define (pin-arrow-tail base-p src-path find-src dest-path find-dest
                        #:start-angle [start-angle #f]
                        #:end-angle [end-angle #f]
                        #:start-pull [start-pull 1/4]
                        #:end-pull [end-pull 1/4]
                        #:color [color text-secondary-color])
  (pin-line base-p
            (find-child base-p src-path) find-src
            (find-child base-p dest-path) find-dest
            #:line-width 2.5
            #:color color
            #:start-angle start-angle
            #:end-angle end-angle
            #:start-pull start-pull
            #:end-pull end-pull))

(define (pin-arrow base-p src-path find-src dest-path find-dest
                   #:start-angle [start-angle #f]
                   #:end-angle [end-angle #f]
                   #:start-pull [start-pull 1/4]
                   #:end-pull [end-pull 1/4]
                   #:color [color text-secondary-color])
  (pin-arrow-line 15 base-p
                  (find-child base-p src-path) find-src
                  (find-child base-p dest-path) find-dest
                  #:line-width 2.5
                  #:color color
                  #:start-angle start-angle
                  #:end-angle end-angle
                  #:start-pull start-pull
                  #:end-pull end-pull))

(define (table c-aligns
               #:stage [stage #f]
               #:r-align [r-align htl-append]
               #:c-sep [c-sep 10]
               #:r-sep [r-sep 0]
               . rows)
  (define cols (apply map list rows))
  (define aligned-cols
    (for/list ([h-align (in-list c-aligns)]
               [cells (in-list cols)])
      (define max-w (apply max (map pict-width cells)))
      (define size-p (blank max-w 0))
      (map (λ~> (h-align size-p _)) cells)))

  (define aligned-rows
    (apply map (λ rs (apply r-align c-sep rs)) aligned-cols))
  (define staged-rows
    (if stage
        (for/list ([row (in-list aligned-rows)]
                   [i (in-naturals 1)])
          (if (< stage i) (ghost row) row))
        aligned-rows))

  (apply vl-append r-sep staged-rows))

;; ---------------------------------------------------------------------------------------------------

(section "Title")

(slides ()
  (~> (vc-append title
                 (~> (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                     (inset 0 -5 0 5))
                 (with-size 20
                   (hflex (+ (pict-width title) 20)
                          (t "Alexis King") (spring 1) (t CONFERENCE-NAME)))
                 (blank 0 50))
      (colorize text-secondary-color)
      (inset 15))
  #:where
  (define demystified (with-size 80
                        (with-font "Concourse C3"
                          @t{data modeling})))
  (define delcont (with-font "Concourse C3"
                    (htl-append @t{constructive})))
  (define unreasonable (with-font "Equity Text B"
                         (htl-append 25 @it{The} @it{unreasonable} @it{effectiveness} @it{of})))
  (define title (~> (vc-append (scale-to-fit unreasonable (- (pict-width demystified) 100) +inf.0)
                               (blank 0 -25)
                               (scale-to-fit delcont (- (pict-width demystified) -10) +inf.0)
                               (blank 0 -35)
                               demystified
                               (blank 0 10))
                    (colorize text-plain-color))))

(begin
  (section "Introduction")

  (slides ([s:pdv? #f] [s:haskell? #f] [s:august? #f])
    #:timeline (next) (tl:flags s:pdv? s:haskell? s:august?)
    #:title "About Me"
    (hc-append
     (pict-when (s:pdv?)
       (~> pdv
           (shadow-frame #:margin 0
                         #:shadow-descent 10)
           (rotate (turns 0.001))))
     (blank 30)
     (vc-append
      (pict-when (s:haskell?)
        (vc-append
         @titlet{Previously}
         @elem{@(vc-append (scale haskell-logo 0.5) (blank 0 -5))/GHC}))
      (blank 0 50)
      (pict-when (s:august?)
        (vc-append
         @titlet{Now}
         @elem{@(vc-append (scale scala-logo 0.3) (blank 0 -12)) @"@"@(vc-append (scale august-logo 0.5) (blank 0 -15))}))
      (blank 0 50))))

  (slides ([s:caption? #f])
    #:timeline (next) (tl:flags s:caption?)
    (vc-append
     (~> emar
         (shadow-frame #:margin 0
                       #:shadow-descent 10
                       #:shadow-alpha-factor 1/4)
         (scale 0.8))
     (pict-when (s:caption?)
       (scale @elem{Software should work, indeed!} 1.4))))

  (slides ()
    (inset (titlet "Static  Typing") 50))

  (section "Static typing")

  (slides ([s:java? #f] [s:cpp? #f] [s:java 0] [s:cpp 0] [s:old? #f])
    #:with current-para-spacing 0
    #:timeline (next) (tl:flags s:java? s:cpp?) (tl:range s:java 1 2) (tl:range s:cpp 1 2) (tl:flags s:old?)
    (vc-append
     @elem{@it{15 years ago: the static typing dark ages.}}
     (blank 0 50)
     (~> (hc-append
          (pict-when (s:java?)
            (vc-append
             (scale-to-fit java-logo 100 100)
             (scale @elem{Java} 1.2)
             (~> (paras
                  #:stage (s:java)
                  @item{infamously conservative}
                  @item{deeply entangled with OOP})
                 (scale 0.7))))
          (blank 80 0)
          (pict-when (s:cpp?)
            (vc-append
             (scale-to-fit c++-logo 100 100)
             (scale @elem{C++} 1.2)
             (~> (paras
                  #:stage (s:cpp)
                  @item{extremely complicated}
                  @item{very unsound})
                 (scale 0.7)))))
         (inset 30 0))
     (blank 0 50)
     (pict-when (s:old?)
       @elem{Static typing: old and crusty!})))

  (slides ([s:rust? #f] [s:ts? #f] [s:rust 0] [s:ts 0] [s:cool? #f])
    #:with current-para-spacing 0
    #:timeline (next) (tl:flags s:rust? s:ts?)
    (s:rust 1) (next) (s:rust 3) (next) (s:ts 3) (next)
    (tl:flags s:cool?)
    #:title "The Renaissance"
    (vc-append
     (hc-append
      (pict-when (s:rust?)
        (vc-append
         (scale-to-fit rust-logo 100 100)
         (scale @elem{Rust} 1.2)
         (~> (paras
              #:stage (s:rust)
              @item{focus on safety/soundness}
              @item{sum types, pattern-matching}
              @item{traits and associated types})
             (scale 0.7))))
      (blank 80 0)
      (pict-when (s:ts?)
        (vc-append
         (scale-to-fit ts-logo 100 100)
         (scale @elem{TypeScript} 1.2)
         (~> (paras
              #:stage (s:ts)
              @item{structural typing}
              @item{fancy type inference}
              @item{type-level programming})
             (scale 0.7)))))
     (blank 0 60)
     (pict-when (s:cool?)
       @elem{Static typing is cool again!})))

  (slides ([s:complex? #f] [s:bullet 0] [s:accept? #f])
    #:timeline (next) (tl:flags s:complex?) (tl:range s:bullet 1 4) (tl:flags s:accept?)
    #:title "But at what cost?"
    (vc-append
     (pict-when (s:complex?)
       (scale @elem{Extreme increase in type system complexity!} 1.2))
     (blank 0 40)
     (paras
      #:stage (s:bullet)
      @item{Programmers don’t know the whole type system}
      @item{Typechecker treated as a black box}
      @item{Time spent “fighting with the typechecker”}
      @item{Ever-growing set of type system features})
     (blank 0 40)
     (pict-when (s:accept?)
       (scale @elem{What made programmers willing to accept this?} 1.2))
     (blank 0 60)))

  (slides ([s:bullet 0])
    #:timeline (tl:range s:bullet 0 4)
    #:title "Gradual Typing"
    (vc-append
     (paras
      #:stage (s:bullet)
      @item{TypeScript is supposed to be “JavaScript, but typed”}
      (vc-append
       @item{Static semantics must be more complex to accommodate dynamic typing idioms}
       (blank 0 20)
       (~> @haskell|{if (typeof x === "string") {
                       useString(x)
                     }}|
           (scale 0.9)))
      @item{Programmers have mental model of @it{dynamic} semantics}
      @item{Type checker is perceived as a magic oracle})
     (blank 0 50)))

  (slides ([s:timeline? #f] [s:java? #f] [s:ts? #f] [s:middle? #f])
    #:timeline (next) (tl:flags s:timeline? s:java? s:ts? s:middle?)
    (vc-append
     @elem{This makes me sad. :(}
     (blank 0 20)
     (pict-when (s:timeline?)
       (vc-append
        (vc-append
         (scale @elem{@it{increasing type system complexity}} 0.45)
         (colorize (arrow-line #:line-length 300
                               #:line-width 2)
                   text-plain-color))
        (blank 0 -10)
        (~> (hc-append
             tick
             (line 400 0)
             tick)
            (adjust-pen #:width 3
                        #:color text-secondary-color))
        (hc-append
         (~> (vc-append
              (blank 0 10)
              (scale-to-fit java-logo 80 80)
              @elem{Java}
              (pict-when (s:java?)
                (vc-append
                 up-arrow
                 (scale @elem{frustrating!} 0.9)
                 (scale @elem{(too limiting)} 0.8))))
             (scale 0.5))
         (blank 100 0)
         (~> (pict-when (s:middle?)
               (vc-append
                (blank 0 -15)
                @elem{?}
                (blank 0 15)
                (vc-append
                 up-arrow
                 (vc-append -10
                            @elem{the mythical}
                            @elem{sweet spot?}))))
             (scale 0.5))
         (blank 100 0)
         (~> (vc-append
              (blank 0 10)
              (scale-to-fit ts-logo 80 80)
              @elem{TypeScript}
              (pict-when (s:ts?)
                (vc-append
                 up-arrow
                 (scale @elem{frustrating!} 0.9)
                 (scale @elem{(too complex)} 0.8))))
             (scale 0.5))))))
    #:where
    (define tick (line 0 30))
    (define up-arrow (colorize (arrowhead 20 (turns 1/4)) text-secondary-color)))

  (slides ([s:bullet 0])
    #:timeline (tl:range s:bullet 0 4)
    (vc-append
     (scale @elem{What if I told you…} 1.3)
     (blank 0 30)
     (paras
      #:stage (s:bullet)
      #:spacing '(lines 0.5)
      @item{…I don’t ever feel like I’m fighting the typechecker.}
      @item{…I feel equipped to use my type system to capture invariants without built-in language support.}
      @item{…it involves @it{less} sophisticated type system machinery.}
      @item{…you can do it, too, in the language you already use!})))

  (slides ([s:bullet 0] [s:thats-it? #f])
    #:timeline (tl:range s:bullet 0 3) (tl:flags s:thats-it?)
    #:title "Ingredients"
    (vc-append
     (paras
      #:stage (s:bullet)
      @item{Some form of product types (e.g. tuples, structs, records).}
      @item{Some form of sum types (e.g. ADTs/Rust-style enums, union types, sealed interfaces).}
      @item{Exhaustive pattern-matching on sum type variants.})
     (blank 0 60)
     (pict-when (s:thats-it?)
       (scale @elem{That’s it!} 1.6))
     (blank 0 40))))

(begin
  (slides ()
    (inset (titlet "A Shift in Perspective") 50))

  (section "Positive space")

  (slides ([s:answer? #f] [s:values? #f] [s:unknown? #f] [s:circles1? #f] [s:circles2? #f])
    #:timeline (next) (tl:flags s:answer? s:values? s:unknown? s:circles1? s:circles2?)
    (vc-append
     (htl-append
      @elem{What are types?}
      (blank 35 0)
      (pict-when (s:answer?)
        @elem{Possible answer: @it{restrictions}.}))
     (blank 0 20)
     (pict-when (s:values?)
       (~> (region 700 #:label (pict-when (s:unknown?) @haskell{unknown}) #:color -1 #:when #t)
           (pin-over 100 100
                     (~> (region 200 #:label (scale @haskell{number} 0.8) #:color 0 #:when (s:circles1?))
                         (pin-over 30 60 @haskell{8})
                         (pin-over 80 80 @haskell{-103})
                         (pin-over 60 130 @haskell{59})))
           (pin-over 350 140
                     (~> (region 250 #:label (scale @haskell{string} 0.8) #:color 1 #:when (s:circles1?))
                         (pin-over 30 60 @haskell{"hello"})
                         (pin-over 60 110 @haskell{"spring"})
                         (pin-over 25 160 @haskell{"abc123"})))
           (pin-over 80 320
                     (~> (region 320 #:label (scale @haskell{unknown[]} 0.8) #:color 2 #:when (s:circles1?))
                         (pin-over 40 70 (scale @haskell{[1, "a"]} 0.8))
                         (pin-over 60 110
                                   (~> (region 200 #:label (scale @haskell{number[]} 0.7) #:color 1 #:when (s:circles2?))
                                       (pin-over 20 75 (scale @haskell{[2, 9]} 0.8))
                                       (pin-over 50 120 (scale @haskell{[-3, 4]} 0.8))))))
           (pin-over 420 400
                     (~> (region 200 #:label (scale @haskell{boolean} 0.8) #:color 0 #:when (s:circles1?))
                         (pin-over 30 70 @haskell{true})
                         (pin-over 60 120 @haskell{false})))
           (scale 0.5))))

    #:where
    (define (region w [h w] #:label label #:color color #:when [show? #t])
      (pict-when show?
        (ct-superimpose
         (filled-ellipse w h
                         #:color (match color
                                   [-1 shape-bg-color]
                                   [_ (flavor-highlight-color color)])
                         #:border-color (match color
                                          [-1 shape-border-color]
                                          [_ (flavor-text-color color)])
                         #:border-width 3)
         (vc-append (blank 0 20) label)))))

  (slides ([s:bullet 0] [s:extend? #f])
    #:timeline (next) (s:bullet 1) (next) (s:bullet 4) (next) (tl:flags s:extend?)
    (vc-append
     @elem{But other languages work differently!}
     (blank 0 20)
     (table
      #:stage (s:bullet)
      (list rtl-superimpose ltl-superimpose)
      (list @elem{Java: } (scale @haskell{record Point(double x, double y)} 0.8))
      (list @elem{Rust: } (scale @haskell{struct Point { x: f64, y: f64 }} 0.8))
      (list @elem{Scala: } (scale @haskell{case class Point(x: Double, y: Double)} 0.8))
      (list @elem{Haskell: } (scale @haskell{data Point = Point
                                               { x :: Double, y :: Double }} 0.8)))
     (blank 0 20)
     (pict-when (s:extend?)
       @elem{We can extend the set of possible values!})))

  (slides ([s:pos-space? #f] [s:nat? #f] [s:int? #f] [s:numbers? #f] [s:cross-neg? #f] [s:uint? #f] [s:int-defn? #f] [s:add-not-sub? #f])
    #:timeline (next)
    (tl:last! (tl:flags s:pos-space? s:nat? s:int? s:numbers? s:cross-neg?))
    (tl:flags s:uint? s:int-defn? s:add-not-sub?)
    #:title "Key Idea 1"
    (vc-append
     (pict-when (s:pos-space?)
       @elem{@it{Think positive space, not negative space.}})
     (blank 0 40)
     (pict-when (s:nat?)
       (htl-append
        @haskell{type Natural = }
        (pict-cond
         [(s:uint?) @haskell{UInt}]
         [(s:int?) (htl-append
                    @haskell{Int }
                    (pict-cond
                     [(s:cross-neg?) @elem{where @haskell{>= 0}}]
                     [else @elem{?}]))]
         [else @elem{?}])))
     (blank 0 10)
     (pict-when (s:numbers?)
       (htl-append
        (~> @elem{..., @num[-3], @num[-2], @num[-1], }
            (when~> (s:uint?)
              (cellophane 0.1)))
        @elem{@num[0], @num[1], @num[2], @num[3], ...}))
     (blank 0 30)
     (pict-when (s:int-defn?)
       @haskell{type Integer = (Boolean, Natural)})
     (blank 0 40)
     (pict-when (s:add-not-sub?)
       (scale @elem{It’s easier to add than to subtract!} 1.2)))
    #:where
    (define (num n)
      (~> (haskell (~a n))
          (when~> (and (s:cross-neg?) (not (s:uint?)) (< n 0))
            cross-out))))

  (slides ([s:list? #f]
           [s:ne-list? #f] [s:ne-list-ts? #f] [s:ne-list-ts-cross? #f] [s:ne-list-restrict? #f] [s:ne-list-restrict-cross? #f]
           [s:ne-list-thought-1? #f] [s:ne-list-thought-2? #f] [s:ne-list-defn? #f]
           [s:even-list-thought? #f] [s:even-list? #f] [s:even-list-defn? #f]
           [s:cheating? #f])
    #:with c:highlights '(|0| |1| |2|)
    #:timeline (next)
    (tl:flags s:list?
              s:ne-list? s:ne-list-restrict? s:ne-list-restrict-cross?
              s:ne-list-thought-1? s:ne-list-thought-2? s:ne-list-defn?
              s:even-list? s:even-list-thought? s:even-list-defn?
              s:cheating?)
    #:title "More Examples"
    (vc-append
     (vl-append
      (pict-when (s:list?)
        @haskell{type List<T> = @elem{<@it{from library}>}})
      (blank 0 30)
      (vc-append
       (pict-when (s:ne-list?)
         (htl-append
          @haskell{type NonEmptyList<T> = }
          (pict-cond
           [(s:ne-list-defn?) @haskell{(@t:0{T}, @haskell{@t:1{List<T>}})}]
           [(s:ne-list-restrict?)
            (~> @elem{@haskell{List<T>}  where @haskell{length > 0}}
                (when~> (s:ne-list-restrict-cross?)
                  (cellophane 0.5)
                  cross-out))]
           [(s:ne-list-ts?)
            (~> @haskell{[T, ...T[]]}
                (when~> (s:ne-list-ts-cross?)
                  (cellophane 0.5)
                  cross-out))]
           [else @elem{?}])))
       (blank 0 10)
       (pict-when (s:ne-list-thought-1?)
         @elem{Thought process: a nonempty list always has @t:0{@elem{one element}}…})
       (pict-when (s:ne-list-thought-2?)
         @elem{…plus @t:1{@elem{zero or more additional elements}}.}))
      (blank 0 40)
      (pict-when (s:even-list?)
        (htl-append
         @haskell{type EvenList<T> = }
         (pict-cond
          [(s:even-list-defn?) @haskell{List<(T, T)>}]
          [else @elem{?}]))))
     (blank 0 10)
     (pict-when (s:even-list-thought?)
       @elem{Thought process: in an @haskell{EvenList}, elements always come in pairs.})
     (blank 0 10)
     (pict-when (s:cheating?)
       (scale @elem{Is this “cheating”?} 1.4))
     (blank 0 10)))

  (slides ([s:decouple? #f] [s:wrong? #f] [s:strike? #f] [s:idea-1? #f] [s:idea-2? #f] [s:privileged? #f])
    #:timeline (next) (tl:flags s:decouple? s:wrong?) (s:strike? #t) (tl:flags s:idea-1? s:idea-2? s:privileged?)
    #:title "Key Idea 2"
    (vc-append
     (pict-when (s:decouple?)
       (scale @elem{@it{Decouple representation from interpretation.}} 1.3))
     (blank 0 60)
     (pict-when (s:wrong?)
       (~> @elem{“My data @it{is} an array, so I must ascribe it an array type.”}
           (when~> (s:strike?)
             (cellophane 0.5)
             (strikethrough #:color text-plain-color))))
     (blank 0 10)
     (pict-when (s:idea-1?)
       (vc-append
        @elem{“My data is a sequence of zero or more elements,}
        (blank 0 -10)
        @elem{which can be represented by an array.”}))
     (blank 0 10)
     (pict-when (s:idea-2?)
       (vc-append
        @elem{“My data is a sequence of one or more elements, which}
        (blank 0 -10)
        @elem{can be represented by a single element, plus an array.”}))
     (blank 0 60)
     (pict-when (s:privileged?)
       (scale @elem{Data does not have a single “privileged” representation!} 1.2))
     (blank 0 20)))

  (slides ([s:restriction? #f] [s:contact? #f] [s:no-comment? #f] [s:ior? #f] [s:system? #f] [s:system-def? #f])
    #:timeline (next!)
    (c:highlights '(dep-t)) (tl:last! (tl:flags s:restriction?))
    (c:highlights '(contact)) (tl:flags s:contact?)
    (c:highlights '()) (tl:last! (tl:flags s:no-comment?)) (tl:last! (tl:flags s:ior?)) (tl:flags s:ior?)
    (c:highlights '(system)) (tl:last! (tl:flags s:system?)) (tl:last! (tl:flags s:system-def?))
    (vl-append
     (pict-cond
      [(s:system?)
       (~> @elem{// @haskell{contact} is @haskell{None} iff @haskell{isSystemUser} is @haskell{true}}
           (when~> (s:system-def?)
             (cellophane 0.5)
             strikethrough))]
      [else
       (~> @elem{// @it{at least one of} @haskell{email} @it{or} @haskell{phone} @it{must be set}}
           (when~> (s:no-comment?)
             (cellophane 0.5)
             strikethrough))])
     (pict-cond
      [(and (s:system?) (not (s:system-def?)))
       @haskell{
 struct User {
   id: Int,
   contact: @t:system{Option}<UserContact>,
   @t:system{isSystemUser: Boolean}
 }}]
      [(s:contact?)
       @haskell{
 struct User {
   id: Int,
   contact: @(pict-cond
              [(s:ior?) @haskell{Ior<EmailAddress, PhoneNumber>}]
              [else @haskell{@t:contact{UserContact}}])
 }}]
      [(s:restriction?)
       @haskell{
 struct User {
   id: Int,
   email: Option<EmailAddress>,
   phone: @t:dep-t{if (email == None) PhoneNumber
                   else Option<PhoneNumber>}
 }}]
      [else @haskell{
 struct User {
   id: Int,
   email: Option<EmailAddress>,
   phone: Option<PhoneNumber>
 }}])
     (pict-cond
      [(s:system-def?) @haskell{
 enum UserContact {
   @t:system{System},
   Email(EmailAddress),
   Phone(PhoneNumber),
   Both(EmailAddress, PhoneNumber)
 }}]
      [(s:ior?)
       @haskell{
 enum Ior<A, B> {
   Left(A),
   Right(A),
   Both(A, B)
 }}]
      [(s:contact?) @haskell{
 enum @t:contact{UserContact} {
   Email(EmailAddress),
   Phone(PhoneNumber),
   Both(EmailAddress, PhoneNumber)
 }}])))

  (slides ([s:defn-1? #f] [s:defn-2? #f])
    #:timeline (next) (tl:flags s:defn-1? s:defn-2?)
    (vc-append
     (scale @elem{Constructive modeling can be tricky!} 1.3)
     (blank 0 40)
     (pict-when (s:defn-1?)
       @haskell{struct TimeRange
                  { start: Instant, end: Instant }
                  @elem{where} start <= end})
     (blank 0 15)
     (pict-when (s:defn-2?)
       (vc-append
        (colorize (arrowhead 30 (turns 3/4)) text-plain-color)
        (blank 0 15)
        @haskell{struct TimeRange
                   { start: Instant, duration: Duration }}))
     (blank 0 15)))

  (slides ()
    @elem{“How do I know which representation to pick?”})

  (section "Obligation propagation")

  (slides ([s:correctness? #f] [s:bullet 0] [s:answer? #f])
    #:timeline (next) (tl:flags s:correctness?) (tl:range s:bullet 1 4) (tl:flags s:answer?)
    (vc-append
     (scale @elem{What is the type system @it{for}?} 1.2)
     (pict-when (s:correctness?)
       (scale @elem{(At least as far as correctness goes.)} 0.8))
     (blank 0 30)
     (paras
      #:stage (s:bullet)
      @item{Preventing you from writing @haskell{1 + stringVariable}?}
      @item{Formally proving your program correct?}
      @item{Playing taxonomist in your product domain?}
      @item{Writing maximally-precise specifications of the shape of your data at runtime?})
     (blank 0 30)
     (pict-when (s:answer?)
       (scale @elem{My answer: keeping track of the cases to handle!} 1.2))
     (blank 0 20)))

  (slides ([s:producers? #f] [s:consumers? #f] [s:far-apart? #f] [s:api? #f] [s:err? #f])
    #:with c:highlights '(occ)
    #:timeline (next) (tl:last! (tl:flags s:producers? s:consumers? s:far-apart?))
    (c:highlights '(new)) (tl:flags s:api? s:err?)
    (vc-append
     (~> (pict-cond
          [(s:api?) @haskell{enum UserType { Standard, Admin, System, @t:new{Api} }}]
          [else @haskell{enum UserType { Standard, Admin, System }}])
         (scale 1.3))
     (blank 0 10)
     (ht-append
      (pict-when (s:producers?)
        (vc-append
       arr
       (blank 0 20)
       @haskell{def authenticateUser(request) =
                  @elem{…}
                  if (hasSuperuserPrivileges)
                    User(type = @t:occ{Admin})
                  else
                    User(type = @t:occ{Standard})}
       (blank 0 40)
       @haskell{def processJob(job) =
                  let user = User(type = @t:occ{System})
                  @elem{…}}))
      (blank 150 0)
      (pict-when (s:consumers?)
        (vc-append
         (scale arr -1 1)
         (blank 0 20)
         @haskell{def authorizeAction(user) =
                    match user {
                      case @t:occ{Standard} => @elem{…}
                      case @t:occ{Admin}    => @elem{…}
                      case @t:occ{System}   => @elem{…}
                    }}
         (blank 0 -10)
         (pict-when (s:err?)
           (vc-append
            (~> (arrow-line #:arrow-size 20 #:line-length 20 #:line-width 5)
                (rotate (turns 1/4))
                (colorize text-secondary-color))
            (blank 0 10)
            @elem{@it{error: non-exhaustive pattern match}})))))
     (blank 0 40)
     (pict-when (s:far-apart?)
       (scale @elem{Might be very far apart!} 1.8)))
    #:where
    (define arr
      (~> (arrow-line #:arrow-size 20
                      #:line-length 80
                      #:line-width 5)
          (colorize text-secondary-color)
          (rotate (turns 0.1)))))

  (slides ([s:use-sites? #f])
    #:timeline (next) (tl:flags s:use-sites?)
    (vc-append
     @elem{A type system is an @it{obligation propagation machine}.}
     (blank 0 10)
     (pict-when (s:use-sites?)
       (scale @elem{Use sites inform what the obligations must be!} 1.1))
     (blank 0 60)))

  (slides ([s:fancy? #f] [s:changed? #f] [s:panic? #f] [s:cross? #f] [s:changed-2? #f])
    #:timeline (next)
    (tl:highlight1 'entries-1 'change 'sum) (tl:highlight1 'could-be-ne)
    (c:highlight+ 'sum) (tl:flags s:fancy?) (c:highlight- 'sum) (tl:flags s:changed?)
    (tl:highlight1 'head '?) (c:highlight+ 'panic)
    (tl:flags s:panic?) (s:cross? #t) (c:highlight+ 'nonempty) (tl:flags s:changed-2?)
    (c:highlight- 'nonempty) (tl:highlight1 'head-ne)
    (vc-append
     @haskell{def calculateTotal(@t:entries-1{entries: @t:could-be-ne{List<InventoryLogEntry>}}) =
                entries.map(entry => @t:change{entry.change}).@t:sum{sum()}}
     (blank 0 20)
     (pict-when (s:fancy?)
       (scale @elem{No need to get fancy: @haskell{List} is fine!} 1.2))
     (blank 0 60)
     (vl-append
      (~> @haskell{def getLastChanged(entries: List<InventoryLogEntry>) =
                    match @t:head{entries.head} {
                      Some(latestEntry) => latestEntry.timestamp
                      None              => @(pict-cond
                                             [(s:panic?) @t:panic{@haskell{panic("shouldn't happen")}}]
                                             [else @t:?{@elem{???}}])
                    }}
          (when~> (s:cross?)
            (cellophane 0.5)
            cross-out)
          (pict-when (s:changed?) _))
      (blank 0 40)
      (pict-when (s:changed-2?)
        @haskell{def getLastChanged(
                       entries: @t:nonempty{NonEmptyList}<InventoryLogEntry>) =
                   entries.@t:head-ne{head}.timestamp}))))

  (slides ([s:simplest? #f] [s:total? #f])
    #:timeline (next) (tl:flags s:simplest? s:total?)
    #:title "Key Idea 3"
    (vc-append
     (pict-when (s:simplest?)
       (vc-append
        @elem{@it{Pick the simplest representation that allows writing}}
        (blank 0 10)
        @(scale @haskell{panic("this shouldn't happen")} 0.9)
        @elem{@it{as little as possible.}}))
     (blank 0 50)
     (pict-when (s:total?)
       (scale @elem{In other words: write total functions!} 1.3))
     (blank 0 80)))

  (slides ([s:defn? #t] [s:restrictive? #f])
    #:timeline (next) (tl:flags s:restrictive?)
    #:title "Option Types"
    (vc-append
     (pict-when (s:defn?)
       (table
        (list rtl-superimpose ltl-superimpose)
        (list @elem{Rust: } (scale @haskell{enum Option<T> { None, Some(T) }} 0.8))
        (list @elem{Haskell: } (scale @haskell{data Maybe a = Nothing | Just a} 0.8))
        (list @elem{TypeScript: } (vl-append
                                   (scale @haskell{type Option<T> = T | null} 0.8)
                                   (scale @haskell{type Option<T> = { some: T } | null} 0.8)))))
     (blank 0 40)
     (pict-when (s:restrictive?)
       (vc-append
        (scale @elem{Which is more restrictive?} 1.2)
        @elem{@haskell{T} @it{or} @haskell{Option<T>}@elem{?}}))
     (blank 0 40)))

  (slides ([s:decl? #f] [s:endpoint? #f] [s:defn? #f])
    #:timeline (next) (tl:flags s:decl?) (tl:highlight1 'option)
    (tl:flags s:endpoint?) (tl:highlight1 'none)
    (tl:flags s:defn?) (tl:highlight1 '???)
    (vc-append
     (scale @elem{It depends!} 1.3)
     (blank 0 40)
     (pict-when (s:decl?)
       (scale @haskell{notifyAboutFailure(user: @t:option{Option<User>}): void} 1.1))
     (blank 0 40)
     (pict-when (s:endpoint?)
       @haskell{def myCoolApiEndpoint() =
                  try { @elem{...} }
                  catch { notifyAboutFailure(@t:none{None}) }})
     (blank 0 30)
     (pict-when (s:defn?)
       @haskell{def notifyAboutFailure(user: Option<User>) =
                  match user {
                    case Some(user) => sendFailureEmail(user.email)
                    case None       => @t:???{@elem{???}}
                  }})
     (blank 0 40)))

  (slides ()
    (~> sowing-reaping
        (shadow-frame #:margin 0
                      #:shadow-descent 10
                      #:shadow-alpha-factor 1/4)))

  (slides ([s:defn? #f] [s:endpoint? #f] [s:moving? #f])
    #:timeline (next) (tl:flags s:defn?) (tl:highlight1 'option)
    (tl:flags s:endpoint?) (tl:highlight1 '???) (tl:flags s:moving?)
    (vc-append
     (scale @elem{Let’s try again.} 1.3)
     (blank 0 30)
     (pict-when (s:defn?)
       @haskell{def notifyAboutFailure(user: @t:option{User}) =
                  sendFailureEmail(user.email)})
     (blank 0 30)
     (pict-when (s:endpoint?)
       @haskell{def myCoolApiEndpoint() =
                  try { @elem{...} }
                  catch { notifyAboutFailure(@t:???{@elem{???}}) }})
     (blank 0 30)
     (pict-when (s:moving?)
       (scale @elem{Types allow@it{moving obligations around.}} 1.2))
     (blank 0 40)))

  (section "Conclusion")

  (slides ([s:bullet 0] [s:simple? #f])
    #:with current-para-spacing '(lines 0.4)
    #:timeline (tl:range s:bullet 0 4) (tl:flags s:simple?)
    #:title "Recap"
    (vc-append
     (paras
      #:stage (s:bullet)
      @item{Define your data’s positive space, not its negative space.}
      @item{Decouple data representation from interpretation.}
      @item{Use type definitions to encode the obligations necessary to write total functions.}
      @item{Push obligations to the places best equipped to handle them.})
     (blank 0 40)
     (pict-when (s:simple?)
       (scale @elem{@it{“As simple as possible, but no simpler.”}} 1.4))
     (blank 0 40)))

  (slides ([s:fancy? #f] [s:ts-ne? #f] [s:advantage? #f] [s:tricked? #f])
    #:timeline (next) (tl:flags s:fancy? s:ts-ne? s:advantage? s:tricked?)
    #:title "Convenience"
    (vc-append
     (pict-when (s:fancy?)
       (scale @elem{Fancy type system features are not bad!} 1.2))
     (blank 0 30)
     (pict-when (s:ts-ne?)
       @haskell{type NonEmptyList<T> = [T, ...T[]]})
     (blank 0 30)
     (pict-when (s:advantage?)
       @elem{Take advantage of what your language has to offer!})
     (blank 0 15)
     (pict-when (s:tricked?)
       @elem{Just don’t get tricked into thinking you @it{need} them.})
     (blank 0 20)))

  (slides ()
    #:title "Thanks!"
    (vc-append
     (paras
      @item{Define your data’s positive space, not its negative space.}
      @item{Decouple data representation from interpretation.}
      @item{Use type definitions to encode the obligations necessary to write total functions.}
      @item{Push obligations to the places best equipped to handle them.})
     (blank 0 40)
     (table
      (list rtl-superimpose ltl-superimpose)
      (list @elem{blog: } @elem{https://lexi-lambda.github.io/})
      (list @elem{twitter: } @elem{@"@"lexi_lambda})
      (list @elem{bluesky: } @elem{@"@"lexi-lambda.bsky.social}))
     (blank 0 40)))

  #;(slides ()
    (vc-append
     (scale @elem{Works the other way, too!} 1.4)
     (blank 0 40)
     @haskell{def getCriticalInfo(): Option<CriticalInfo> =
                if (haveCriticalData)
                  Some(computeCriticalInfo())
                else
                  None @elem{// @it{haha oopsies let’s hope no one notices}}
                }
     (blank 0 40)
     @haskell{def handleCriticalSituation() =
                match getCriticalInfo() {
                  case Some(info) => makeInformedChoice(info)
                  case None       => hailMary()
                }}
     (blank 0 40)))

  #;(slides ([s:answer? #f])
    #:timeline (next) (tl:flags s:answer?)
    (vc-append
     (scale @elem{What is the type system for?} 0.8)
     (blank 0 15)
     (pict-when (s:answer?)
       @elem{My answer: keeping track of the cases to handle!}))))
