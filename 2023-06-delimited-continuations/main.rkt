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
         rsvg
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
  (define-runtime-path haskell-logo.svg "assets/haskell-logo.svg")
  (define-runtime-path tweag.svg "assets/tweag.svg")

  (define haskell-logo
    (~> (rsvg-isolate (svg-file->pict haskell-logo.svg))
        (scale-to-fit 100 +inf.0)))

  (define tweag-logo
    (~> (rsvg-isolate (svg-file->pict tweag.svg))
        (scale-to-fit 100 +inf.0))))

(define CONFERENCE-NAME "Lambda Days 2023")

;; ---------------------------------------------------------------------------------------------------

(define section (make-parameter #f))

(current-main-font "Concourse T3")
(current-code-font '((weight . 500) . "Fira Code"))
(current-text-size 40)
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
(current-base-color text-tertiary-color)
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
(define c:flavors (make-parameter (hash)))

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

(set-margin! 20)
(set-title-h! 80)

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

(define (tl:flags #:set [val #t] . params)
  (for ([param (in-list params)])
    (param val)
    (next)))

(define (tl:show . params)
  (apply tl:flags #:set show params))

(define (c:highlight+ which)
  (if (list? which)
      (c:highlights (append which (c:highlights)))
      (c:highlights (cons which (c:highlights)))))

(define (tl:highlight+ . whichs)
  (for ([which (in-list whichs)])
    (c:highlight+ which)
    (next)))

;; ---------------------------------------------------------------------------------------------------

(define current-code-tt (make-parameter tt))

(define (haskell-code-tt str)
  (parameterize ([current-keyword-list '("let" "if" "then" "else" "catch" "prompt" "delimit" "->")])
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

(define (reduction-arrow)
  (~> (arrow-line #:arrow-size 15
                  #:line-length 25
                  #:line-width 2.5)
      (colorize text-secondary-color)))

(define (reduction-steps
         #:stage [stage +inf.0]
         #:arrow [arrow-p (~> (reduction-arrow)
                              (rotate (turns 3/4)))]
         #:margin [margin 5]
         . ps)
  (match ps
    ['() (blank)]
    [(cons p ps)
     (~> (for/list ([p (in-list ps)])
           (vc-append (inset arrow-p 0 margin) p))
         (apply steps p _
                #:append vc-append
                #:stage stage))]))

(define (e_1)
  (define base-p (mathv "e"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "1") 0.6) -10))
      (refocus base-p)
      (inset 0 0 15 0)))
(define (e_2)
  (define base-p (mathv "e"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "2") 0.6) -10))
      (refocus base-p)
      (inset 0 0 15 0)))

(define (E #:base [base-p (mathv "E")]
           #:tag [t #f]
           . elems)
  (line-append
   (hbl-append
    (~> base-p (when~> t (tag t _)))
    (matht "["))
   (apply elem #:color #f elems)
   (matht "]")))

(define (E_1 #:tag [tag #f] . elems)
  (define base-p (mathv "E"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "1") 0.6) -10))
      (refocus base-p)
      (inset 0 0 10 0)
      (apply E elems #:base _ #:tag tag)))

(define (E_2 #:tag [tag #f] . elems)
  (define base-p (mathv "E"))
  (~> (hbl-append base-p (lift-bottom-relative-to-baseline (scale (matht "2") 0.6) -10))
      (refocus base-p)
      (inset 0 0 10 0)
      (apply E elems #:base _ #:tag tag)))

(define (delay-highlight-proc thunk tag)
  (if (member tag (c:highlights))
      (parameterize ([c:highlights (remove tag (c:highlights))])
        (highlight (thunk)
                   #:path tag
                   #:color (tag-highlight-color tag)))
      (thunk)))

(define-syntax-parse-rule (delay-highlight pict-e:expr tag-e:expr)
  (delay-highlight-proc (λ () pict-e) tag-e))

(define (hole #:color [color text-plain-color])
  (~> (disk 0.8 #:draw-border? #f #:color color)
      (lift-above-baseline -0.1)
      (scale (current-text-size))
      (tag 'hole _)))

;; ---------------------------------------------------------------------------------------------------

(section "Title")

(slides ()
  (~> (vc-append title
                 (~> (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                     (inset 0 -5 0 5))
                 (with-size 15
                   (hflex (+ (pict-width title) 20)
                          (t "Alexis King, Tweag") (spring 1) (t CONFERENCE-NAME)))
                 (blank 0 50))
      (colorize text-secondary-color))
  #:where
  (define demystified (with-size 100
                        (with-font "Concourse C2"
                          @t{demystified})))
  (define delcont (with-font "Concourse C3"
                    (htl-append @t{delimited} (blank 30 0) @t{continuations})))
  (define title (~> (vc-append (scale-to-fit delcont (- (pict-width demystified) 15) +inf.0)
                               (blank 0 -40)
                               demystified)
                    (colorize text-plain-color))))

(define (progress-list #:completed [completed 0]
                       #:focus [focus #f])
  (define (focus-color n v)
    (if (and focus (not (= focus n)))
        text-secondary-color
        v))
  (define (check n)
    (pict-when (> completed n)
      @elem[#:color (focus-color n (->color% (hsv 0.38 0.79 0.76)))]{✓}))
  (~> (for/list ([label (in-list '("continuations" "delimited" "first-class" "native"))]
                 [color (in-list '(#f 0 1 2))]
                 [index (in-naturals)])
        (~> (elem #:color (focus-color index (and~> color flavor-text-color)) label)
            (when~> (and focus (= focus index))
                    highlight)
            (elem " " (check index))))
      (apply ol _)))

(begin
  (section "Introduction")

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:with current-para-spacing '(lines 0.5)
    #:with current-para-fill? #f
    #:with current-para-width 800
    #:title "History"
    (paras
     #:stage (s:bullet)
     @item{Delimited continuations introduced by @(blank 50 0) Matthias Felleisen 35 years ago.}
     @item{Flurry of initial publications, mostly in Scheme.}
     @item{Not much mainstream adoption.}
     @item{Recently: some renewed interest.}))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:with current-para-spacing '(lines 0.4)
    #:with current-para-fill? #f
    #:with current-para-width 900
    (vc-append
     (scale haskell-logo 5)
     (blank 0 50)
     (paras
      #:stage (s:bullet)
      @item{Initial proposal in early 2020; revised version accepted in late 2020.}
      @item{Implementation in limbo for several years.}
      @item{Started at Tweag last year; patch landed last fall.}
      @item{Finally released this past March in GHC 9.6!})))

  (slides ()
    @elem{Problem: nobody knows what they are.})

  (slides ()
    (inset @titlet{Demystification} 50))

  (slides ([s:stage 0] [s:color? #f] [s:focus-noun? #f])
    #:timeline (tl:last! (tl:sequence s:stage 5) (tl:flags s:color?))
    ; (c:highlights '(noun)) (tl:flags s:focus-noun?) #:condense-last
    #:title "Terminology"
    (~> (vc-append
         (pict-when (> (s:stage) 0)
           (~> (htl-append (t "“") (when~> (t "continuations") strike? strikethrough) (t "”"))
               (when~> strike? (colorize text-tertiary-color))
               elem))
         (blank 0 25)
         (pict-when (> (s:stage) 1)
           (vc-append
            (elem native first-class)
            (elem delimited
                  (t:noun (elem "continuations"))
                  (non-noun (t "”"))))))
        (inset 25))
    #:where
    (define strike? (> (s:stage) 1))

    (define (non-noun p)
      (if (s:focus-noun?)
          (colorize p text-secondary-color)
          p))

    (define (adj flavor name)
      (pict-when (>= (s:stage) (+ flavor 2))
        (non-noun
         (htl-append (if (= (s:stage) (+ flavor 2)) (pip (t "“") rbl-find) (blank))
                     (~> (t name)
                         (when~> (and (s:color?) (not (s:focus-noun?)))
                           (colorize (flavor-text-color flavor))))
                     (if (> flavor 0) (t ",") (blank))))))
    (define delimited (adj 0 "delimited"))
    (define first-class (adj 1 "first-class"))
    (define native (adj 2 "native")))

  (slides ([s:focus #f])
    #:timeline (next) (s:focus 0) (next)
    (~> (progress-list #:focus (s:focus))
        (inset 50))))

(begin
  (section "Continuations")

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 5)
    (~> (paras #:align 'center
               #:spacing '(lines 0.5)
               #:stage (s:stage)
               (vc-append @elem{A “continuation” is a@it{concept},}
                          @elem{not a language feature.})
               (scale @elem{(Like “scope” or “value”.)} 0.75)
               (vc-append (blank 0 35) @elem{Applies to most programming languages!})
               (vc-append (blank 0 20) @elem{Useful for talking about@it{evaluation}.}))
        (inset 20)))

  (slides ([s:reduction-stage 0])
    #:with c:flavors (hash 'cont1 1 'cont2 1)
    #:timeline
    (s:reduction-stage 1) (next)
    (c:highlights '(redex1)) (next) (s:reduction-stage 2) (c:highlights '(redex1 value1)) (next)
    (c:highlights '(redex2)) (next) (s:reduction-stage 3) (c:highlights '(redex2 value2)) (next)
    (c:highlights '(redex3)) (next) (s:reduction-stage 4) (c:highlights '(redex3 value3)) (next)
    (c:highlights '()) (next)
    (c:highlights '(redex1 redex2 redex3)) (next)

    (reduction-steps
     #:stage (s:reduction-stage)
     @haskell{@t:redex1[redex1] @t:cont1{* @redex2}}
     @haskell{@(rtl-superimpose (ghost @haskell{@redex1 *}) (t:cont2 @haskell{3 *})) @t:redex2[redex2]}
     @haskell{@t:redex3{3 * @t:value2{7}}}
     @haskell{@t:value3{21}})
    #:where
    (define redex1 @haskell{(1 + 2)})
    (define redex2 @haskell{(3 + 4)}))

  (slides ([s:split? #f] [s:reduce? #f] [s:recombine? #f] [s:label-redex? #f] [s:cont-label #f])
    #:with c:flavors (hash 'cont1 1 'cont2 1 'cont3 1 'redex3 2 'redex4 2)
    #:timeline
    (next)
    (tl:highlight+ '(redex1 redex2 redex3 redex4)) (tl:highlight+ '(cont1 cont2 cont3))
    (tl:flags s:split? s:reduce? s:recombine? s:label-redex?)
    (s:cont-label '?) (next)
    (s:cont-label 'cont) (next)

    (~> (vc-append
         @haskell{@t:redex1{(1 + 2)} @t:cont1{* (3 + 4)}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append (reduction-steps
                        #:stage (if (s:reduce?) 2 1)
                        @haskell{@t:redex2{1 + 2}}
                        @haskell{@t:redex3{3}})
                       (blank 50 0)
                       @haskell{@t:cont2{@hole[] * (3 + 4)}}))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:redex4{3} @t:cont3{* (3 + 4)}}))
        (when~> (s:split?)
          (pin-arrow 'redex1 cb-find #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont1 cb-find 'cont2 ct-find))
        (when~> (s:recombine?)
          (pin-arrow 'redex3 cb-find #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont2 cb-find #:start-angle (turns 3/4)
                     'cont3 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:label-redex?)
          (pin-over 'redex2 (adjust-find cb-find -30 5) (scale @elem{“redex”} 0.8) #:hole rt-find))
        (when~> (s:cont-label)
          (pin-over 'cont2 (adjust-find cb-find 30 5)
                    (scale (pict-if (eq? (s:cont-label) '?)
                                    (htl-append (blank 30 0) @elem{???})
                                    @elem{“continuation”})
                           0.8)))))

  (slides ([s:split? #f] [s:reduce? #f] [s:recombine? #f] [s:label-redex? #f] [s:cont-label #f])
    #:with c:flavors (hash 'cont1 1 'cont2 1 'cont3 1 'redex3 2 'redex4 2)
    #:timeline
    (next)
    (tl:highlight+ '(redex1 redex2 redex3 redex4 cont1 cont2 cont3))
    (s:split? #t) (s:label-redex? #t) (s:cont-label 'cont) (next)
    (tl:flags s:reduce? s:recombine?)

    (~> (vc-append
         @haskell{@t:cont1{3 *} @t:redex1{(3 + 4)}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append @haskell{@t:cont2{3 * @hole[]}}
                       (blank 50 0)
                       (reduction-steps
                        #:stage (if (s:reduce?) 2 1)
                        @haskell{@t:redex2{3 + 4}}
                        @haskell{@t:redex3{7}})))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:cont3{3 *} @t:redex4{7}}))
        (when~> (s:split?)
          (pin-arrow 'redex1 cb-find #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont1 cb-find 'cont2 ct-find))
        (when~> (s:recombine?)
          (pin-arrow 'redex3 cb-find #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont2 cb-find #:start-angle (turns 3/4)
                     'cont3 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:label-redex?)
          (pin-over 'redex2 (adjust-find cb-find 30 5)
                    (scale @elem{redex} 0.8)))
        (when~> (s:cont-label)
          (pin-over 'cont2 (adjust-find cb-find -30 5)
                    (scale (pict-if (eq? (s:cont-label) '?)
                                    (htl-append (blank 30 0) @elem{???})
                                    @elem{continuation})
                           0.8)
                    #:hole rt-find))))

  (slides ([s:split? #f] [s:reduce? #f] [s:recombine? #f] [s:label-redex? #f] [s:cont-label #f])
    #:with c:flavors (hash 'cont1 1 'cont2 1 'cont3 1 'redex3 2 'redex4 2)
    #:timeline
    (next)
    (tl:highlight+ '(redex1 redex2 redex3 redex4 cont1 cont2 cont3))
    (s:split? #t) (s:label-redex? #t) (s:cont-label 'cont) (next)
    (tl:flags s:reduce? s:recombine?)

    (~> (vc-append
         @haskell{@t:redex1{3 * 7}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append @haskell{@t:cont2{@hole[]}}
                       (blank 50 0)
                       (reduction-steps
                        #:stage (if (s:reduce?) 2 1)
                        @haskell{@t:redex2{3 * 7}}
                        @haskell{@t:redex3{21}})))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:redex4{21}}))
        (when~> (s:split?)
          (pin-arrow 'redex1 cb-find #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:recombine?)
          (pin-arrow 'redex3 cb-find #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont2 (adjust-find cb-find 0 5) #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:label-redex?)
          (pin-over 'redex2 (adjust-find cb-find 30 5)
                    (scale @elem{redex} 0.8)))
        (when~> (s:cont-label)
          (pin-over 'cont2 (adjust-find cb-find -20 5)
                    (~> (vc-append @elem{continuation}
                                   (scale @elem[#:color text-secondary-color]{(empty)} 0.8))
                        (scale 0.8))
                    #:hole rt-find))))

  (slides ([s:stage 0])
    #:with current-para-fill? #f
    #:timeline (tl:sequence s:stage 5)
    (vc-append
     (scale @elem{What is the continuation?} 1.4)
     (blank 0 50)
     (paras
      #:stage (s:stage)
      #:spacing '(lines 0.35)
      @item{The “context” in which the redex is evaluated.}
      @item{An expression with a hole.}
      @item{The place the redex’s value is “returned to”.}
      @item{“The rest of the program.”})))

  (slides ([s:split? #f] [s:reduce? #f] [s:recombine? #f])
    #:with c:flavors (hash 'cont 1 'redex3 2 'redex4 2)
    #:with current-highlight-bleed 7
    #:timeline
    (next)
    (tl:highlight+ '(redex1 redex2 redex3 redex4 cont))
    (tl:flags s:split? s:reduce? s:recombine?)

    (~> (vc-append
         @haskell{@t:cont{let x =} @t:redex1{1 + 2}
                  @t:cont{let y = 3 + 4}
                  @t:cont{@t:cont1{x * y}}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append
            @haskell{@t:cont{@t:cont2{let x = @hole[]}}
                     @t:cont{let y = 3 + 4}
                     @t:cont{@t:cont3{x * y}}}
            (blank 70 0)
            (reduction-steps
             #:stage (if (s:reduce?) 2 1)
             @haskell{@t:redex2{1 + 2}}
             @haskell{@t:redex3{3}})))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:cont{@t:cont4{let x =}} @t:redex4{3}
                    @t:cont{let y = 3 + 4}
                    @t:cont{x * y}}))
        (when~> (s:split?)
          (pin-arrow 'redex1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont1 cb-find #:start-angle (turns 3/4)
                     'cont2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:recombine?)
          (pin-arrow 'redex3 cb-find #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont3 rb-find #:start-angle (turns 3/4)
                     'cont4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))))

  (slides ([s:stage 1])
    #:timeline (next) (tl:highlight+ 'defn 'use) (s:stage 2) (next)
    (reduction-steps
     #:stage (s:stage)
     @haskell{@t:defn{let x = 3}
              let y = 3 + 4
              @t:use{x} * y}
     @haskell{let y = 3 + 4
              @t:use{3} * y}))

  (slides ([s:split? #f] [s:reduce? #f] [s:recombine? #f])
    #:with c:flavors (hash 'cont 1 'redex3 2 'redex4 2)
    #:with current-highlight-bleed 7
    #:timeline
    (next)
    (tl:highlight+ '(redex1 redex2 redex3 redex4 cont))
    (tl:flags s:split?)
    (s:reduce? #t) (s:recombine? #t) (next)

    (~> (vc-append
         @haskell{@t:cont{let y =} @t:redex1{3 + 4}
                  @t:cont{@t:cont1{3 * y}}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append
            @haskell{@t:cont{@t:cont2{let y = @hole[]}}
                     @t:cont{@t:cont3{3 * y}}}
            (blank 70 0)
            (reduction-steps
             #:stage (if (s:reduce?) 2 1)
             @haskell{@t:redex2{3 + 4}}
             @haskell{@t:redex3{7}})))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:cont{@t:cont4{let y =}} @t:redex4{7}
                    @t:cont{3 * y}}))
        (when~> (s:split?)
          (pin-arrow 'redex1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont1 cb-find #:start-angle (turns 3/4)
                     'cont2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:recombine?)
          (pin-arrow 'redex3 cb-find #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont3 rb-find #:start-angle (turns 3/4)
                     'cont4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))))

  (when #f
    (slides ([s:stage 0])
      #:with current-para-fill? #f
      #:timeline (tl:sequence s:stage 5)
      (vc-append
       (scale @elem{This style of reasoning is very nice!} 1.4)
       (blank 0 50)
       (paras
        #:stage (s:stage)
        #:spacing '(lines 0.35)
        @item{“Reducing” is like “simplifying” in grade school algebra.}
        @item{No need to introduce a stateful “abstract machine”.}
        @item{Each intermediate step is a valid program.}
        @item{Can be made very formal (small-step operational semantics, reduction semantics).}))))

  (blank-slide)

  (slides ([s:regular? #f] [s:bullet 0] [s:interesting? #f])
    #:with current-para-fill? #f
    #:timeline (next) (tl:flags s:regular?) (tl:sequence s:bullet (in-range 1 5)) (tl:flags s:interesting?)
    (vc-append
     (scale @para{Why care about continuations?} 1.4)
     (blank 0 50)
     (pict-when (s:regular?) @para{Evaluation is@it{extremely} regular:})
     (blank 0 10)
     (~> (ol #:stage (s:bullet)
             @elem{Split the redex and continuation.}
             @elem{Reduce the redex.}
             @elem{Substitute the result into the continuation.}
             @elem{Repeat.})
         (scale 0.9))
     (blank 0 30)
     (pict-when (s:interesting?) @para{Why is the continuation itself interesting?})))

  (slides ([s:most-programmers? #f] [s:other-operators? #f])
    #:timeline (next) (tl:flags s:most-programmers? s:other-operators?)
    (vc-append
     @elem{Compiler writers care about the continuation!}
     (blank 0 60)
     (pict-when (s:most-programmers?)
       (vc-append @elem{Most programmers don’t have much}
                  @elem{reason to, most of the time.}))
     (blank 0 60)
     (pict-when (s:other-operators?)
       @elem{…but what about operators that use different rules?})))

  (slides ([s:split? #f] [s:cross-out? #f] [s:recombine? #f] [s:thrown-away? #f])
    #:with c:flavors (hash 'cont 1)
    #:timeline
    (next)
    (tl:highlight+ '(redex0 redex1 redex2 redex3 cont))
    (tl:flags s:split?)
    (s:cross-out? #t) (s:recombine? #t) (next)
    (tl:flags s:thrown-away?)

    (~> (vc-append
         @haskell{@t:cont{@t:cont1{1 +}} @t:redex1{exit(-1)}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append
            (~> @haskell{@t:cont{@t:cont2{1 + @hole[]}}}
                (when~> (s:cross-out?)
                  (cellophane 0.5)
                  (cross-out #:color text-error-color)))
            (blank 70 0)
            @haskell{@t:redex2{exit(-1)}}))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:redex3{exit(-1)}})
         (blank 0 50)
         (pict-when (s:thrown-away?)
           @elem{Continuation is thrown away!}))
        (when~> (s:split?)
          (pin-arrow 'redex1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont1 cb-find #:start-angle (turns 3/4)
                     'cont2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:recombine?)
          (pin-arrow 'redex2 cb-find #:start-angle (turns 3/4)
                     'redex3 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))))

  (slides ()
    (~> @elem{@haskell{exit} is still not terribly interesting.}
        (inset 50 0)))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage (in-range 1 6))
    (~> (steps
         #:stage (s:stage)
         #:append vc-append
         @elem{What about @haskell{throw}/@haskell{catch}?}
         (vc-append (blank 0 70) @elem{@haskell{throw(exn)}})
         (vc-append (blank 0 5) (~> @elem{Raises@haskell{exn} as an exception.} (scale 0.8)))
         (vc-append (blank 0 70) @elem{@haskell{catch{body, handler}}})
         (vc-append
          (blank 0 10)
          (~> (vc-append @elem{Evaluates @haskell{body}, and if an exception}
                         @elem{is raised, evaluates @haskell{handler(exn)}.})
              (scale 0.8))))
        (inset 50 0)))

  (slides ([s:reduction-stage 0])
    #:with c:flavors (hash 'catch 1)
    #:timeline
    (s:reduction-stage 1) (next)
    (tl:highlight+ 'throw 'catch)
    (c:highlights '(catch handler)) (next)
    (s:reduction-stage 2) (next)
    (c:highlights '()) (tl:sequence s:reduction-stage (in-range 3 5))

    (reduction-steps
     #:stage (s:reduction-stage)
     @haskell{1 + @t:catch{catch{2 * @t:throw{throw(5)},
                                 @t:handler{(n) -> 3 * n}}}}
     @haskell{1 + @t:handler{(3 * 5)}}
     @haskell{1 + 15}
     @haskell{16}))

  (slides ([s:split? #f] [s:recombine? #f] [s:cross-out? #f])
    #:with c:flavors (hash 'cont 1 'outer-cont 1 'inner-cont 1)
    #:timeline
    (next)
    (tl:highlight+ '(redex1 redex2) 'cont)
    (tl:flags s:split? s:recombine?)
    (c:highlights '()) (next) (tl:highlight+ 'outer-cont)
    (c:highlights '(inner-cont)) (next) (tl:flags s:cross-out?)
    (c:highlights '()) (s:cross-out? #f) (next)

    (~> (vc-append
         @haskell{@t:cont{1 + catch{2 * @t:redex1{throw(5)},
                          @t:cont1{          }(n) -> 3 * n}}}
         (blank 0 50)
         (pict-when (s:split?)
           (hc-append
            (~> (let ()
                  (define inner-cont
                    (~> @haskell{2 *}
                        (when~> (s:cross-out?)
                          (cellophane 0.5)
                          (cross-out #:color text-error-color))
                        (tag 'inner-cont _)))
                  @haskell{@t:cont{@t:cont2{
                    @t:outer-cont{1 +} catch{@inner-cont @hole[],
                              (n) -> 3 * n}}}})
                (delay-highlight 'inner-cont))
            (blank 70 0)
            @haskell{@t:redex2{throw(5)}}))
         (blank 0 50)
         (pict-when (s:recombine?)
           (vc-append
            (tag '??? @elem{???})
            (blank 0 50)
            (tag 'result @haskell{@t:outer-cont{1 +} (3 * 5)}))))
        (when~> (s:split?)
          (pin-arrow 'redex1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4) #:start-pull 1/2
                     'redex2 (adjust-find lt-find 10 0) #:end-angle (turns 3/4))
          (pin-arrow 'cont1 cb-find #:start-angle (turns 3/4)
                     'cont2 (adjust-find ct-find -25 0) #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:recombine?)
          (pin-arrow-tail 'redex2 (adjust-find lb-find 10 0) #:start-angle (turns 3/4) #:start-pull 1/3
                          '??? ct-find #:end-angle (turns 3/4))
          (pin-arrow-tail 'cont2 cb-find #:start-angle (turns 3/4)
                          '??? ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow '??? cb-find 'result ct-find))))

  (slides ([s:stage 0] [s:cross-out? #f])
    #:with c:flavors (hash 'outer-cont 1)
    #:with current-box-padding 8
    #:with current-slide-margin 0
    #:timeline
    (s:stage 1) (next)
    (tl:highlight+ 'inner-cont 'outer-cont)
    (s:stage 2) (next) (tl:flags s:cross-out?)
    (s:stage 3) (next)

    (~> (steps
         #:stage (s:stage)
         #:append vc-append
         (vc-append (blank 0 50) @haskell{@t:outer-cont{1 +} catch{@t:inner-cont{2 * @hole[]}, (n) -> 3 * n}})
         (~> (vc-append
              10
              (blank 0 20)
              (~> (tag 'inner-frame (wrap-box #:highlight 'inner-cont @haskell{2 * @hole[]}))
                  (when~> (s:cross-out?)
                    (cellophane 0.5)
                    (cross-out #:color text-error-color)))
              (tag 'catch-frame (wrap-box @haskell{catch{@hole[], (n) -> 3 * n}}))
              (tag 'outer-frame (wrap-box #:highlight 'outer-cont @haskell{1 + @hole[]})))
             (unless~> (s:cross-out?)
               (pin-arrow-tail 'inner-frame (adjust-find lb-find 20 0) #:start-angle (turns 3/4)
                               '(catch-frame hole) ct-find #:end-angle (turns 3/4)
                               #:color text-plain-color))
             (pin-arrow-tail 'catch-frame cb-find #:start-angle (turns 3/4)
                             '(outer-frame hole) ct-find #:end-angle (turns 3/4)
                             #:color text-plain-color))
         (vc-append
          (blank 0 30)
          @elem{@haskell{catch}@it{delimits} the discarded continuation.}
          (blank 0 30))))))

(begin
  (blank-slide)
  (section "Notation")

  (slides ()
    (inset @titlet{Interlude: Notation} 50))

  (slides ([s:stage 0])
    #:with current-slide-margin 0
    #:timeline (tl:sequence s:stage (in-range 1 8))
    (steps
     #:stage (s:stage)
     #:append vc-append
     @elem{@mathv{A} @matht{⟶} @mathv{B}}
     (scale @elem{“@(blank -25 0)@mathv{A} reduces to@mathv{B}.”} 0.9)
     (vc-append
      (blank 0 30)
      @elem{@haskell{not(false)} @matht{⟶} @haskell{true }})
     @elem{@haskell{not(true) } @matht{⟶} @haskell{false}}
     (vc-append
      (blank 0 30)
      @elem{@haskell{if true  then @e_1[] else @e_2[]} @matht{⟶} @e_1[]})
     @elem{@haskell{if false then @e_1[] else @e_2[]} @matht{⟶} @e_2[]}
     (vc-append
      (blank 0 50)
      @elem{@haskell{if not(false) then 1 else 2}?})))

  (slides ([s:split? #f] [s:rule? #f] [s:reduce? #f] [s:recombine? #f])
    #:with c:flavors (hash 'cont 1 'value 2 'redex3 2 'redex4 2)
    #:with current-highlight-bleed 7
    #:timeline
    (next)
    (tl:highlight+ '(redex redex1 redex2))
    (tl:highlight+ '(cont))
    (tl:flags s:split? s:rule?)
    (c:highlight+ '(value redex3 redex4))
    (tl:flags s:reduce? s:recombine?)

    (~> (vc-append
         @haskell{@t:cont{@t:cont1{if}} @t:redex1{not(false)} @t:cont{then 1 else 2}}
         (blank 0 70)
         (pict-when (s:split?)
           (htl-append
            @haskell{@t:cont{@t:cont2{if @hole[]} then 1 else 2}}
            (blank 70 0)
            (reduction-steps
             #:stage (if (s:reduce?) 2 1)
             @haskell{@t:redex2{not(false)}}
             @haskell{@t:redex3{true}})))
         (blank 0 70)
         (pict-when (s:recombine?)
           @haskell{@t:cont{@t:cont3{if}} @t:redex4{true} @t:cont{then 1 else 2}}))
        (pin-over 'redex2 (adjust-find lb-find 40 20) #:hole rt-find
                  (pict-when (s:rule?)
                    (~> @elem{@haskell{@t:redex{not(false)}} @matht{⟶} @haskell{@t:value{true}}}
                        wrap-box
                        (scale 0.8))))
        (when~> (s:split?)
          (pin-arrow 'redex1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4)
                     'redex2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont1 cb-find #:start-angle (turns 3/4)
                     'cont2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:recombine?)
          (pin-arrow 'redex3 cb-find #:start-angle (turns 3/4)
                     'redex4 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont2 cb-find #:start-angle (turns 3/4)
                     'cont3 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))))

  (slides ([s:stage 0] [s:strike? #f] [s:bullet 0] [s:example 0])
    #:timeline
    (s:stage 1) (next) (s:strike? #t) (s:stage 2) (next)
    (s:stage 3) (tl:sequence s:bullet (in-range 1 3))
    (s:stage 4) (tl:sequence s:example (in-range 1 4))

    #:with current-para-fill? #f
    #:with current-para-width 1000
    (steps
     #:stage (s:stage)
     #:append vc-append
     (~> (let ()
           (define term (if (s:strike?) tt haskell))
           @elem[#:color #f]{@term{not(false)} @matht{⟶} @term{true}})
         (when~> (s:strike?)
           strikethrough
           (colorize text-tertiary-color))
         elem)
     @elem{@E[@haskell{not(false)}] @matht{⟶} @E[@haskell{true}]}
     (vc-append
      (blank 0 40)
      (paras
       #:stage (s:bullet)
       #:spacing '(lines 0.5)
       @item{@mathv{E} stands for “some arbitrary continuation”.}
       @item{@E[@mathv{x}] denotes “plugging the hole” in@mathv{E} with @mathv{x}.}))
     (vc-append
      (blank 0 40)
      (paras
       #:stage (s:example)
       @elem{@aligned[@mathv{E}] @matht{=} @haskell{if @hole[] then 1 else 2}}
       @elem{@aligned[@mathv{x}] @matht{=} @haskell{not(false)}}
       @elem{@E:x @matht{=} @haskell{if not(false) then 1 else 2}})))

    #:where
    (define E:x @E[@mathv{x}])
    (define (aligned p)
      (rbl-superimpose (ghost E:x) p)))

  (slides ([s:stage 0])
    #:with c:flavors (hash 'catch-cont1 1 'catch-resume 2)
    #:timeline
    (s:stage 1) (next)
    (s:stage 2) (next) (tl:highlight+ 'exit-cont) (c:highlights '())
    (s:stage 3) (next) (tl:highlight+ 'catch-cont1 'catch-cont2 'catch-resume)
    (s:stage 4) (next)

    (steps
     #:stage (s:stage)
     #:append vc-append
     (scale @elem{Why bother with all of this?} 1.1)
     (vc-append
      (blank 0 50)
      @elem{@E[#:tag 'exit-cont @haskell{exit(@mathv{v})}] @matht{⟶} @haskell{exit(@mathv{v})}})
     (vc-append
      (blank 0 30)
      (~> @elem{@E_1[#:tag 'catch-cont1 @haskell{catch{@E_2[#:tag 'catch-cont2 @haskell{throw(@t:catch-resume[@mathv{v}])}], @t:catch-resume[@mathv{f}]}}]
                @matht{⟶} @E_1[#:tag 'catch-cont1 @haskell{@t:catch-resume{@mathv{f}(@mathv{v})}}]}
          (delay-highlight 'catch-resume)))
     (vc-append
      (blank 0 50)
      @elem{Lots of operations can be described this way!}))))

(begin
  (slides ([s:completed 0] [s:focus #f])
    #:timeline (tl:sequence s:completed 3) (s:focus 2) (next)
    (~> (progress-list #:completed (s:completed) #:focus (s:focus))
        (inset 50)))

  (section "First-class continuations")

  (slides ()
    (inset @elem{What makes something “first class”?} 50))

  (slides ()
    (inset @elem{How could a@it{continuation} be a@it{value}?} 50))

  (slides ([s:plug? #f] [s:quantify? #f])
    #:timeline (next) (tl:highlight+ '(hole* lambda)) (tl:flags s:plug? s:quantify?) (c:highlights '()) (next)
    (vc-append
     40
     (wrap-lam @haskell{1 + (@hole* * 2)})
     (wrap-lam @haskell{if @hole* > 0 then 1 else -1})
     (wrap-lam @haskell{f(catch{throw(@hole*), handle})}))
    #:where
    (define hole*
      (tag 'hole* (if (s:plug?) @haskell{x} (hole))))
    (define (wrap-lam p)
      (define lam-p @haskell{(x) ->})
      @haskell{@(pict-when (s:quantify?) (t:lambda lam-p)) @p @(ghost lam-p)}))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (steps
     #:stage (s:stage)
     #:append vc-append
     @elem{What is a “first-class continuation”?}
     (vc-append (blank 0 20) @elem{Answer: a continuation reified as a function.})))

  (slides ([s:stage 0])
    #:with c:flavors (hash 'arg 1 'cont 1)
    #:timeline (tl:sequence s:stage (in-range 1 4)) (tl:highlight+ 'fn 'arg) (c:highlights '(fn cont)) (next) (s:stage 4) (next)
    (steps
     #:stage (s:stage)
     #:append vc-append
     @haskell{call_cc}
     (vc-append (blank 0 20) @elem{“call with current continuation”})
     (vc-append
      (blank 0 60)
      (~> @elem{@E[#:tag 'cont @haskell{call_cc(@t:fn{@mathv{f}})}] @matht{⟶}
                @E[#:tag 'cont @haskell{@t:fn{@mathv{f}}(@t:arg{(x) -> @E[#:tag 'cont @haskell{x}]})}]}
          (delay-highlight 'fn)
          (delay-highlight 'arg)))
     (vc-append
      (blank 0 50)
      @elem{This has some problems!}
      (blank 0 30))))

  (slides ([s:expand? #f])
    #:timeline (next) (tl:flags s:expand?)
    (~> (vc-append
         @simple{1 + (@hole[#:color #f] * 2)}
         (blank 40)
         (pict-when (s:expand?)
           @haskell{print(1 + (@hole[] * 2))
                    shutdown_runtime()
                    run_libc_atexit()
                    exit_process()}))
        (inset 40))
    #:where
    (define (simple . elems)
      (pict-if (s:expand?)
               (~> (apply code elems)
                   strikethrough
                   (colorize text-tertiary-color))
               (elem (apply haskell elems)))))

  (slides ([s:stage 0])
    #:with c:flavors (hash 'cont1 1 'fn 2)
    #:timeline
    (tl:sequence s:stage (in-range 1 4))
    (tl:highlight+ 'cont1 'cont2.1 'cont2.2 'fn)

    (steps
     #:stage (s:stage)
     #:append vc-append
     @elem{We need more control!}
     (vc-append (blank 0 20) @elem{@haskell{prompt}/@haskell{control}})
     (vc-append
      (blank 0 60)
      (~> (vc-append
           10
           @elem{@E_1[#:tag 'cont1 @haskell{prompt{@E_2[#:tag 'cont2.1 @haskell{control(@t:fn{@mathv{f}})}]}}]}
           @elem{@matht{⟶} @E_1[#:tag 'cont1 @haskell{@t:fn{@mathv{f}}(@t:arg{(x) -> @E_2[#:tag 'cont2.2 @haskell{x}]})}]})
          (delay-highlight 'fn)
          (delay-highlight 'arg))
      (blank 0 50))))

  (slides ([s:split? #f] [s:inside-out? #f] [s:recombine? #f])
    #:with c:flavors (hash 'cont-o1 1 'cont-o2 1 'cont-o3 1 'fn1 2 'fn2 2 'fn3 2)
    #:with current-slide-margin 0
    #:timeline
    (next)
    (tl:highlight+ '(cont-o1 cont-o2 cont-o3) '(cont-i1 cont-i2 cont-i3) '(fn1 fn2 fn3))
    (tl:flags s:split? s:inside-out? s:recombine?)

    (~> (vc-append
         @haskell{@t:cont-o1{1 +} prompt{@t:cont-i1{2 *} control(@t:fn1{(k) -> k(3) + k(5)})}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append
            @haskell{@t:cont-o2{1 + @hole[]}}
            (blank 70 0)
            @haskell{@t:cont-i2{2 * @hole[]}}
            (blank 70 0)
            @haskell{@t:fn2{(k) -> k(3) + k(5)}}))
         (blank 0 50)
         (pict-when (s:inside-out?)
           (htl-append
            (blank 150 0)
            @haskell{let k = @t:cont-i3{(x) -> 2 * x}
                     @t:fn3{k(3) @t:inside-out1{+} k(5)}}))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:cont-o3{1 +} (let k @t:inside-out2{=} (x) -> 2 * x
                         k(3) + k(5))})
         (blank 0 50))
        (when~> (s:split?)
          (pin-arrow 'cont-o1 cb-find #:start-angle (turns 3/4)
                     'cont-o2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont-i1 cb-find #:start-angle (turns 3/4)
                     'cont-i2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'fn1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4)
                     'fn2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:inside-out?)
          (pin-arrow 'cont-i2 rb-find #:start-angle (turns 3/4)
                     'cont-i3 lt-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'fn2 rb-find #:start-angle (turns 3/4) #:start-pull 1/2
                     'fn3 rc-find #:end-angle (turns 2/4) #:end-pull 1/2))
        (when~> (s:recombine?)
          (pin-arrow 'cont-o2 cb-find #:start-angle (turns 3/4)
                     'cont-o3 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'inside-out1 (adjust-find cb-find 0 10)
                     'inside-out2 ct-find))))

  (slides ([s:stage 0])
    #:with c:flavors (hash 'fn 1 'cont 1)
    #:with current-slide-margin 0
    #:timeline
    (s:stage 2) (next) (tl:highlight+ 'redex1 'fn 'cont)
    (s:stage 3) (next) (c:highlights '(redex2))
    (s:stage 4) (next) (c:highlights '())
    (s:stage 5) (next)

    (reduction-steps
     #:stage (s:stage)
     @haskell{1 + prompt{@t:cont{2 *} control((k) -> k(3) + k(5))}}
     @haskell{1 + (let k = @t:fn{(x) -> 2 * x}
                   @t:redex1{k(3)} + @t:redex1{k(5)})}
     @haskell{1 + @t:redex2{(@t:redex1{6} + @t:redex1{10})}}
     @haskell{@(ghost @haskell{1})1 + @t:redex2{16}}
     @haskell{17}))

  (slides ([s:stage 0] [s:delimit? #f])
    #:with c:flavors (hash 'cont 1 'exn 2)
    #:timeline
    (s:stage 1) (next)
    (s:stage 2) (next)
    (c:highlights '(delimiter)) (next) (c:highlights '(yield)) (next) (c:highlights '()) (next)
    (tl:highlight+ 'rfn 'lfn1 'lfn2)
    (tl:flags s:delimit?)
    (tl:highlight+ 'exn 'cont)
    (s:stage 3) (next)

    #:with current-slide-margin 0
    (steps
     #:stage (s:stage)
     #:append vc-append
     (scale @elem{Why is this so confusing?} 1.3)
     (~> (vl-append
          10
          (blank 0 50)
          @elem{@(rtl-superimpose (launder (ghost (handle-lhs))) (catch-lhs)) @matht{⟶} @E_1[@haskell{@t:rfn{@mathv{f}}(@t:exn{@mathv{v}})}]}
          @elem{@(rtl-superimpose (launder (ghost (handle-lhs))) (prompt-lhs)) @matht{⟶} @E_1[@haskell{@t:rfn{@mathv{f}}(@t:cont{(x) -> @E_2[@haskell{x}]})}]}
          (pict-when (s:delimit?) #:launder? #t
            @elem{@(handle-lhs) @matht{⟶} @E_1[@haskell{@t:rfn{@mathv{f}}(@t:exn{@mathv{v}}, @t:cont{(x) -> @E_2[@haskell{x}]})}]})
          (blank 0 50))
         (delay-highlight 'delimiter)
         (delay-highlight 'yield)
         (delay-highlight 'rfn)
         (delay-highlight 'lfn1)
         (delay-highlight 'lfn2)
         (delay-highlight 'exn)
         (delay-highlight 'cont))
     (scale @elem{@haskell{delimit}/@haskell{yield} provide@it{resumable exceptions}.} 1.15))

    #:where
    (define (catch-lhs)
      @E_1[@haskell{@t:delimiter{catch}{@E_2[@haskell{@t:yield{throw}(@t:exn{@mathv{v}})}],@t:lfn1{@mathv{f}}}}])
    (define (prompt-lhs)
      @E_1[@haskell{@t:delimiter{prompt}{@E_2[@haskell{@t:yield{control}(@t:lfn2{@mathv{f}})}]}}])
    (define (handle-lhs)
      @E_1[@haskell{@t:delimiter{delimit}{@E_2[@haskell{@t:yield{yield}(@t:exn{@mathv{v}})}],@t:lfn2{@mathv{f}}}}]))

  (slides ([s:split? #f] [s:inside-out? #f] [s:recombine? #f])
    #:with c:flavors (hash 'cont-o1 1 'cont-o2 1 'cont-o3 1 'fn1 2 'fn2 2 'fn3 2 'resume 2)
    #:with current-slide-margin 0
    #:timeline
    (next)
    (c:highlights '(throw)) (next) (tl:highlight+ 'fn1) (c:highlights '(throw resume)) (next)
    (c:highlights '()) (next) (tl:highlight+ '(cont-o1 cont-o2 cont-o3) '(cont-i1 cont-i2 cont-i3) '(fn1 fn2 fn3))
    (tl:flags s:split? s:inside-out? s:recombine?)

    (~> (vc-append
         @haskell{@t:cont-o1{1 +} delimit{@t:body{@t:cont-i1{2 *} @t:throw{yield(())}},
                              @t:fn1{((), @t:resume{k}) -> k(3) + k(5)}}}
         (blank 0 50)
         (pict-when (s:split?)
           (htl-append
            @haskell{@t:cont-o2{1 + @hole[]}}
            (blank 70 0)
            @haskell{@t:cont-i2{2 * @hole[]}}
            (blank 70 0)
            @haskell{@t:fn2{((), k) -> k(3) + k(5)}}))
         (blank 0 50)
         (pict-when (s:inside-out?)
           (htl-append
            (blank 150 0)
            @haskell{let k = @t:cont-i3{(x) -> 2 * x}
                     @t:fn3{k(3) @t:inside-out1{+} k(5)}}))
         (blank 0 50)
         (pict-when (s:recombine?)
           @haskell{@t:cont-o3{1 +} (let k @t:inside-out2{=} (x) -> 2 * x
                         k(3) + k(5))})
         (blank 0 50))
        (when~> (s:split?)
          (pin-arrow 'cont-o1 cb-find #:start-angle (turns 3/4)
                     'cont-o2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'cont-i1 cb-find #:start-angle (turns 3/4)
                     'cont-i2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'fn1 (adjust-find cb-find 10 0) #:start-angle (turns 3/4)
                     'fn2 ct-find #:end-angle (turns 3/4) #:end-pull 1/3))
        (when~> (s:inside-out?)
          (pin-arrow 'cont-i2 rb-find #:start-angle (turns 3/4)
                     'cont-i3 lt-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'fn2 rb-find #:start-angle (turns 3/4) #:start-pull 1/2
                     'fn3 rc-find #:end-angle (turns 2/4) #:end-pull 1/2))
        (when~> (s:recombine?)
          (pin-arrow 'cont-o2 cb-find #:start-angle (turns 3/4)
                     'cont-o3 ct-find #:end-angle (turns 3/4) #:end-pull 1/3)
          (pin-arrow 'inside-out1 (adjust-find cb-find 0 10)
                     'inside-out2 ct-find))))

  (slides ([s:stage 0])
    #:with current-para-fill? #f
    #:timeline (tl:sequence s:stage 4)
    (vc-append
     (scale @elem{Why@haskell{prompt}/@haskell{control}?} 1.1)
     (blank 0 50)
     (paras
      #:stage (s:stage)
      @item{In some sense “simpler”.}
      @item{Historical relationship to@haskell{call_cc}.}
      @item{Easier to statically type.})
     (blank 0 30))))

(begin
  (section "Types")

  (slides ()
    (inset @titlet{Types} 50))

  (slides ([s:stage 0])
    #:timeline
    (s:stage 1) (next) (s:stage 2) (next) (tl:highlight+ 'var-a) (c:highlights '(Exn1)) (next) (c:highlights '())
    (c:highlights '()) (s:stage 3) (next) (s:stage 4) (next) (c:highlights '(Exn2)) (next) (c:highlights '(var-b)) (next)
    (c:highlights '()) (next)

    (steps
     #:stage (s:stage)
     #:append vc-append
     (scale @elem{Even typing exceptions is hard!} 1.3)
     (vc-append (blank 0 80) @haskell{throw : @t:Exn1{Exception} -> @t:var-a{a}})
     (vc-append (blank 0 40) @haskell{catch{body, handler} : @t:var-b{b}})
     (vc-append
      (blank 0 10)
      (vc-append
       @haskell{body : @t:var-b{b}}
       (blank 0 10)
       @haskell{handler : @t:Exn2{Exception} -> @t:var-b{b}})
      (blank 50))))

  (slides ([s:stage 0])
    #:timeline (s:stage 1) (next) (s:stage 2) (next) (s:stage 3) (next) (c:highlights '(resume)) (next) (s:stage 4) (next) (tl:highlight+ 'cont)
    (c:highlights '(var-a yield)) (next) (c:highlights '(var-b delimit)) (next)
    (c:highlights '(var-a yield)) (next)
    (steps
     #:stage (s:stage)
     #:append vc-append
     @haskell{yield : DelimiterTag -> @t:var-a{a}}
     (vc-append (blank 0 50) @haskell{delimit{body, handler} : @t:var-b{b}})
     (vc-append
      (blank 0 10)
      (vc-append
       @haskell{body : @t:var-b{b}}
       (blank 0 10)
       @haskell{handler : DelimiterTag -> @t:resume{(@t:var-a{a} -> @t:var-b{b})} -> @t:var-b{b}})
      (blank 50))
     (~> (vc-append
          @elem{@E_1[@haskell{@t:delimit{delimit{@E_2[#:tag 'cont @haskell{@t:yield{yield(@t:exn{@mathv{v}})}}], @mathv{f}}}}]}
          @elem{@matht{⟶} @E_1[@haskell{@t:rfn{@mathv{f}}(@t:exn{@mathv{v}}, (x) -> @E_2[@haskell{x}])}]})
         (delay-highlight 'delimit)
         (delay-highlight 'yield))))

  (slides ([s:stage 0])
    #:timeline (s:stage 1) (next) (s:stage 2) (next) (s:stage 3) (next) (s:stage 4) (next) (tl:highlight+ 'cont)
    (c:highlights '(var-a yield)) (next) (c:highlights '(var-b delimit)) (next)
    (steps
     #:stage (s:stage)
     #:append vc-append
     @haskell{prompt{body} : @t:var-b{b}}
     (vc-append (blank 0 10) @haskell{body : @t:var-b{b}})
     (vc-append
      (blank 0 40)
      @haskell{control : (@t:cont{(@t:var-a{a} -> @t:var-b{b})} -> @t:var-b{b}) -> @t:var-a{a}}
      (blank 0 50))
     (~> (vc-append
           10
           @elem{@E_1[@haskell{@t:delimit{prompt{@E_2[#:tag 'cont @haskell{@t:yield{control(@mathv{f})}}]}}}]}
           @elem{@matht{⟶} @E_1[@haskell{@t:fn{@mathv{f}}(@t:arg{(x) -> @E_2[#:tag 'cont2.2 @haskell{x}]})}]})
         (delay-highlight 'delimit)
         (delay-highlight 'yield))))

  (slides ()
    @elem{Solution: tagged prompts.})
  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage (in-range 1 6)) (tl:highlight+ 'cont)
    (c:highlights '(var-b delimit)) (next)
    (steps
     #:stage (s:stage)
     #:append vc-append
     (vc-append (blank 0 50) @haskell{new_prompt_tag : () -> @PromptTag<b>[]})
     (vc-append (blank 0 40) @haskell{prompt{tag, body} : @t:var-b{b}})
     (vc-append (blank 0 10) @haskell{tag : @PromptTag<b>[]} (blank 0 10) @haskell{body : @t:var-b{b}})
     (vc-append
      (blank 0 40)
      @haskell{control : (@PromptTag<b>[], (@t:cont{(@t:var-a{a} -> @t:var-b{b})} -> @t:var-b{b})) -> @t:var-a{a}}
      (blank 0 50))
     (~> (vc-append
           10
           @elem{@E_1[@haskell{@t:delimit{prompt{@mathit{tag}, @E_2[#:tag 'cont @haskell{@t:yield{control(@mathit{tag}, @mathv{f})}}]}}}]}
           @elem{@matht{⟶} @E_1[@haskell{@t:fn{@mathv{f}}(@t:arg{(x) -> @E_2[#:tag 'cont2.2 @haskell{x}]})}]})
         (delay-highlight 'delimit)
         (delay-highlight 'yield)))
    #:where
    (define (bracket str)
      (colorize (tt str) (current-base-color)))
    (define (PromptTag<b>)
      (~> @haskell{PromptTag@bracket{<}@t:var-b{b}@bracket{>}}
          (delay-highlight 'var-b)))))

(begin
  (slides ([s:completed 2] [s:focus #f])
    #:timeline (next) (s:completed 3) (next) (s:focus 3) (next)
    (~> (progress-list #:completed (s:completed) #:focus (s:focus))
        (inset 50)))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage (in-range 1 5))
    (steps
     #:stage (s:stage)
     #:append vc-append
     (scale @elem{How do we implement this?} 1.3)
     (vc-append (blank 0 50) (scale @elem{Option one: continuation-passing style.} 1.1))
     (vc-append (blank 0 10) @elem{Problem: slow! (See my talk from ZuriHac 2020.)})
     (vc-append (blank 0 50) (scale @elem{Option two: bake them into the runtime.} 1.1))))

  (define (cont-stack #:padding [padding 5] . frames)
    (define (frame-pict frame)
      (match frame
        [(tagged _ p) p]
        [p            p]))
    (define (frame-tag frame)
      (match frame
        [(tagged t _) t]
        [_            #f]))

    (define max-width (apply max (map (λ~> frame-pict pict-width) frames)))
    (define box-width (+ max-width (* padding 3)))
    (define spike-size 9)

    (for/foldr ([stack-p (blank box-width 0)]
                [spike-posn #f]
                #:result stack-p)
               ([frame (in-list frames)])
      (define content-p (frame-pict frame))
      (define box-height (+ (pict-height content-p) (* padding 2)))
      (define frame-p (~> (if spike-posn
                              (balloon box-width
                                       box-height
                                       #:corner-radius 0
                                       #:spike-size spike-size
                                       #:spike-side 'bottom
                                       #:spike-position spike-posn
                                       #:highlight (frame-tag frame))
                              (box box-width box-height
                                   #:highlight (frame-tag frame)))
                          (cc-superimpose (inset content-p 0 padding 0 0))))
      (define-values [hole-x hole-y] (cc-find frame-p (find-child frame-p 'hole)))
      (values (~> (inset stack-p 0 box-height 0 0)
                  (pin-over 0 0 frame-p)
                  (set-ascent ltl-find frame-p))
              (/ (- hole-x spike-size) (- box-width (* spike-size 2))))))

  (slides ([s:stack? #f] [s:label? #f])
    #:timeline (next) (tl:flags s:stack? s:label?)
    (vc-append
     (reduction-steps
      #:stage (if (s:stack?) 2 1)
      @haskell{1 + prompt{tag, f(true, @hole[]) * 5}}
      (cont-stack
       @haskell{f(true, @hole[])}
       @haskell{@hole[] * 5}
       @haskell{prompt{tag, @hole[]}}
       @haskell{1 + @hole[]}))
     (blank 0 10)
     (pict-when (s:label?)
       (scale @elem{This is a call stack!} 1.1))))

  (slides ([s:stage 'init] [s:memcpy? #f])
    #:timeline
    (next) (tl:highlight+ 'frame1 'frame2 'frame3)
    (s:stage 'copy) (next!) (s:stage 'copy+pop) (next) (s:stage 'popping-prompt) (next) (s:stage 'popped-prompt) (next) (s:stage 'replaced-redex) (next!)
    (s:stage 'apply-cont) (next) (s:stage 'copy-back) (next!) (s:stage 'copied-back) (c:highlights '()) (next)
    (tl:flags s:memcpy?)

    #:with c:flavors (hash 'frame3 1)
    (~> (vc-append
         (vl-append
          (htl-append
           @elem{redex: }
           (pict-cond
            [(eq? (s:stage) 'replaced-redex)
             (htl-append @haskell{g(}
                         (if (eq? (s:stage) 'replaced-redex)
                             cont-closure
                             (launder cont-closure))
                         @haskell{)})]
            [(eq? (s:stage) 'apply-cont)
             (htl-append (if (memq (s:stage) '(apply-cont copy-back))
                             cont-closure
                             (launder cont-closure))
                         @haskell{("hello")})]
            [(memq (s:stage) '(copy-back copied-back))
             @haskell{"hello"}]
            [else
             @haskell{control(tag, g)}]))
          (blank 0 10)
          (htl-append
           (rtl-superimpose (ghost @elem{redex: }) @elem{stack: })
           (pict-cond
            [(eq? (s:stage) 'init)
             (cont-stack
              (tagged 'frame1 frame1)
              (tagged 'frame2 frame2)
              (tagged 'frame3 frame3)
              frame4)]
            [(eq? (s:stage) 'copy)
             (vc-append
              (~> (cont-stack
                   (cc-superimpose frame-sizer frame1)
                   frame2)
                  (cellophane 0.5))
              (cont-stack
               (tagged 'frame3 frame3)
               frame4))]
            [(eq? (s:stage) 'copy+pop)
             (cont-stack
              (tagged 'frame3 frame3)
              frame4)]
            [(eq? (s:stage) 'popping-prompt)
             (vc-append
              (cellophane (cont-stack frame3) 0.5)
              (cont-stack (cc-superimpose frame-sizer frame4)))]
            [(memq (s:stage) '(popped-prompt replaced-redex apply-cont))
             (cont-stack (cc-superimpose frame-sizer frame4))]
            [(memq (s:stage) '(copy-back copied-back))
             (cont-stack
              (tagged 'frame1 frame1)
              (tagged 'frame2 frame2)
              (cc-superimpose frame-sizer frame4))])
           (pict-unless (eq? (s:stage) 'init)
             (hc-append
              (blank 10 0)
              (cc-superimpose
               (pict-when (eq? (s:stage) 'copy)
                 (reduction-arrow))
               (pict-when (eq? (s:stage) 'copy-back)
                 (rotate (reduction-arrow) (turns 1/2))))
              (blank 10 0)
              (t:heap-cont
               (cont-stack
                (tagged (and (eq? (s:stage) 'copy) 'frame1) @haskell{f(true, @hole[])})
                (tagged (and (eq? (s:stage) 'copy) 'frame2) @haskell{@hole[] * 5})))))))
         (pict-when (s:memcpy?)
           @elem{Capture/restore are just @haskell{memcpy}!}))
        (when~> (memq (s:stage) '(replaced-redex apply-cont))
          (pin-arrow 'cont-ptr cb-find #:start-angle (turns 3/4)
                     'heap-cont lt-find #:end-angle (turns 7/8) #:end-pull 1/5)))
    #:where
    (define frame1 @haskell{f(true, @hole[])})
    (define frame2 @haskell{@hole[] * 5})
    (define frame3 @haskell{prompt{tag, @hole[]}})
    (define frame4 @haskell{1 + @hole[]})
    (define frame-sizer (ghost (launder frame3)))

    (define cont-closure
      (~> (htl-append
           @haskell{CONT}
           (blank 15 0)
           (~> (circle 20
                       #:border-color text-secondary-color
                       #:border-width 2.5)
               t:cont-ptr
               (translate 0 -3)))
          (inset 10 0 10 -3)
          (wrap-box #:padding 3))))

  (slides ([s:completed 3])
    #:timeline (next) (s:completed 4) (next)
    (~> (progress-list #:completed (s:completed))
        (inset 50))))

(begin
  (section "Conclusion")

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet (in-range 5))
    #:with current-para-fill? #f
    #:with current-para-width 1000
    #:title "Miscellany"
    (vc-append
     (paras
      #:stage (s:bullet)
      #:spacing '(lines 0.5)
      @item{Can further optimize implementation for specific use cases.}
      @item{Strict monads permit embedding into a lazy language.}
      @item{Reality is always at least a little more complicated (e.g. stack overflow, async exceptions).}
      @item{We sorely lack non-synthetic continuation benchmarks!})
     (blank 0 50)))

  (slides ([s:hero? #f])
    #:timeline (next) (tl:flags s:hero?)
    (~> (vc-append
         @elem{The unsung hero of this talk:}
         (blank 0 -5)
         (pict-when (s:hero?)
           (scale (htl-append (ghost @elem{.}) @elem{reduction semantics.}) 1.5)))
        (inset 10)))

  (slides ([s:completed 0] [s:label? #f])
    #:timeline (next) (s:completed 2) (next) (tl:flags s:label?)
    (~> (vc-append
         (progress-list #:completed (s:completed))
         (blank 0 50)
         (pict-when (s:label?)
           (scale @elem{Still extremely useful!} 1.3)))
        (inset 25)))

  (slides ([s:bullet 0] [s:thanks? #f])
    #:timeline (tl:sequence s:bullet 5) (tl:flags s:thanks?)
    #:with current-para-fill? #f
    #:with current-para-width 1300
    (vc-append
     (paras
      #:stage (s:bullet)
      #:spacing '(lines 0.4)
      @item{Continuations are a concept that arises naturally in evaluation.}
      @item{Special operators like@haskell{catch} delimit portions of the continuation.}
      @item{First-class continuations allow reifying the continuation as a function.}
      @item{Remarkably, this corresponds to manipulation of the call stack.})
     (blank 0 50)
     (pict-when (s:thanks?)
       (vc-append
        (scale @elem{Thanks!} 1.3)
        (blank 0 30)
        (vl-append
         (htl-append (align @elem{me: }) (vl-append @elem{https://lexi-lambda.github.io/}
                                                    @elem{https://twitter.com/lexi_lambda}))
         (htl-append @elem{Tweag: } @elem{https://www.tweag.io/})))))
    #:where
    (define (align p)
      (rtl-superimpose (ghost @elem{Tweag: }) p))))

#;(start-at-recent-slide)
