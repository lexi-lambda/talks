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
         "lib/util.rkt")

(begin
  (define-runtime-path tweag.svg "assets/tweag.svg")
  (define-runtime-path mercury-icon.svg "assets/mercury-icon.svg")
  (define-runtime-path mercury-text.svg "assets/mercury-text.svg")
  (define-runtime-path screenshot-issue-21700-th-linking.png "assets/screenshot-issue-21700-th-linking.png")
  (define-runtime-path screenshot-mr-7502-fat-interface-files.png "assets/screenshot-mr-7502-fat-interface-files.png")
  (define-runtime-path screenshot-issue-bytecode-1.png "assets/screenshot-issue-bytecode-1.png")
  (define-runtime-path screenshot-issue-bytecode-2.png "assets/screenshot-issue-bytecode-2.png")
  (define-runtime-path screenshot-issue-bytecode-3.png "assets/screenshot-issue-bytecode-3.png")
  (define-runtime-path screenshot-issue-21853-codegen-printer.png "assets/screenshot-issue-21853-codegen-printer.png")

  (define tweag-logo
    (~> (rsvg-isolate (svg-file->pict tweag.svg))
        (scale-to-fit 100 +inf.0)))
  (define mercury-logo
    (~> (hc-append (rsvg-isolate (svg-file->pict mercury-icon.svg))
                   (blank 10 0)
                   (rsvg-isolate (svg-file->pict mercury-text.svg)))
        (scale-to-fit 95 +inf.0))))

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

(struct tagged (tag values) #:transparent)
(define (tag t . vs)
  (match vs
    [(list (? pict? p)) (highlight-if (memq t (c:highlights)) (tag-pict p t))]
    [_                  (tagged t vs)]))
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
   (λ (continue) (abort-current-continuation slides-prompt-tag
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

(define (tl:highlight+ . whichs)
  (for ([which (in-list whichs)])
    (if (list? which)
        (c:highlights (append which (c:highlights)))
        (c:highlights (cons which (c:highlights))))
    (next)))

;; ---------------------------------------------------------------------------------------------------

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

(define (box w h
             #:highlight? [highlight? #f]
             #:border-width [border-width 2])
  (filled-rectangle w h
                    #:color (if highlight? (current-highlight-color) shape-bg-color)
                    #:border-width border-width
                    #:border-color (if highlight? (current-highlight-border-color) shape-border-color)))

(define (wrap-box p #:padding [padding 15])
  (cc-superimpose (box (+ (pict-width p) (* padding 2))
                       (+ (pict-height p) (* padding 2)))
                  p))

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

;; ---------------------------------------------------------------------------------------------------

(section "Title")

(slides ()
  (~> (vc-append title
                 (~> (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                     (inset 0 -5 0 15))
                 (with-size 30
                   (hflex (+ (pict-width title) 20)
                          (t "Alexis King") (spring 1) (t "Tweag")))
                 (blank 0 50))
      (colorize text-secondary-color))
  #:where
  (define title (~> (with-size 100
                      (with-font "Concourse C2"
                        @t{Towards a Faster GHC}))
                    (colorize text-plain-color))))

(begin
  (slides ([s:header? #f] [s:bullet 0])
    #:timeline (next) (tl:flags s:header?) (tl:sequence s:bullet (in-range 1 4))
    #:with current-para-spacing '(lines 0.5)
    #:with current-para-width 700
    (vc-append
     (pict-when (s:header?)
       (scale @elem{“GHC is slow.”} 2))
     (blank 0 40)
     (paras #:stage (s:bullet)
            @item{GHC does a lot of stuff!}
            @item{GHC@it{has} been optimized.}
            @item{Finding new improvements is hard.})
     (blank 0 60)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 4)
    #:with current-para-spacing '(lines 0.5)
    (vc-append
     (inset (scale mercury-logo 7) 0 0 20 0)
     (blank 0 70)
     (paras #:stage (s:bullet)
            @item{Almost 4,000 Haskell source modules.}
            @item{Easily 5+ minute cold build times, even with@tt{-O0} and @tt{-j}.}
            @item{Even longer deploy times with@tt{-O2}.})))

  (slides ()
    (~> (vc-append mercury-logo
                   (blank 0 -2)
                   (scale (elem "&") 0.3)
                   (blank 0 5)
                   tweag-logo
                   (blank 0 15))
        (inset 10 0)))

  (slides ()
    (inset @titlet{Progress Report} 30))

  (slides ()
    (~> (bitmap screenshot-issue-21700-th-linking.png)
        (shadow-frame #:margin 0 #:shadow-descent 20)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:with current-para-spacing '(lines 0.8)
    #:title "Some Context"
    (vc-append (paras #:stage (s:bullet)
                      @item{Mercury’s code uses lots of Template Haskell for deriving typeclass instances.}
                      @item{Folk wisdom is that TH is slow, but why?}
                      @item{Evaluating TH splices themselves is relatively cheap.}
                      @item{Turns out: most the time is spent in the linker, not GHC!})
               (blank 0 100)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:with current-para-spacing '(lines 0.6)
    #:with current-para-width 1100
    (vc-append
     (scale @elem{Linking is a headache.} 1.7)
     (blank 0 60)
     (paras #:stage (s:bullet)
            @item{TH needs to load code from other modules.}
            @item{Loading code uses the system dynamic linker.}
            @item{The system dynamic linker can only load@it{libraries}, not objects.}
            @item{Linking libraries on-demand has@it{seconds-long} linking times!})
     (blank 0 100)))

  (slides ([s:solution? #f])
    #:timeline (next) (tl:flags s:solution?)
    (inset
     (vc-append
      @elem{“The only winning move is not to play.”}
      (blank 40)
      (pict-when (s:solution?)
        (scale @elem{Solution: the bytecode compiler.} 1.3))
      (blank 60))
     20))

  (slides ()
    (~> (bitmap screenshot-mr-7502-fat-interface-files.png)
        (shadow-frame #:margin 0 #:shadow-descent 20)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 6)
    #:title "Quick Summary"
    #:with current-para-spacing '(lines 0.8)
    #:with current-para-width 1400
    (vc-append
     (paras #:stage (s:bullet)
            @item{A set of new code generation options in GHC 9.6, implemented by Matthew Pickering.}
            @item{Pass@tt{-fwrite-if-simplfied-core},@tt{-fbyte-code-and-object-code}, and@tt{-fprefer-byte-code} for the full benefit.}
            @item{GHC will generate bytecode from Core as-needed for Template Haskell.}
            @item{For Mercury’s codebase, a nearly 60% build time improvement with@tt{-O0}!}
            @item{…does not yet work properly with@tt{-O1}.})
     (blank 0 100)))

  (slides ()
    (~> (list (bitmap screenshot-issue-bytecode-1.png)
              (bitmap screenshot-issue-bytecode-2.png)
              (bitmap screenshot-issue-bytecode-3.png))
        (map (λ~> (shadow-frame #:shadow-descent 20)) _)
        (apply vc-append _)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:title "Modernizing the Bytecode Compiler"
    #:with current-para-spacing '(lines 0.6)
    (vc-append
     (paras #:stage (s:bullet)
            @item{The bytecode compiler was designed for GHCi.}
            @item{Now used in many other places, including TH.}
            @item{Not many experts on the bytecode compiler.}
            @item{Goal: bytecode should be usable with@tt{-O} by GHC 9.8.})
     (blank 0 100)))

  (slides ()
    (inset (vc-append @titlet{Codegen Printer}
                      (blank 0 -8)
                      @titlet{Optimizations})
           50))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:with current-para-spacing '(lines 0.6)
    (vc-append
     (paras #:stage (s:bullet)
            @item{In most build configurations, GHC uses its own native code generator (NCG).}
            @item{GHC uses the system assembler to assemble native code.}
            @item{In practice, GHC spends a surprising amount of time printing assembly.}
            @item{In some extreme cases, ~15% of time and ~17% of allocations could be spent printing!})
     (blank 0 100)))

  (slides ()
    (~> (bitmap screenshot-issue-21853-codegen-printer.png)
        (shadow-frame #:margin 0 #:shadow-descent 20)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:title "New Codegen Printer"
    #:with current-para-spacing '(lines 0.7)
    #:with current-para-width 1300
    (vc-append
     (paras #:stage (s:bullet)
            @item{New, targeted implementation of the codegen printer (by Krzysztof Gogolewski and myself) in GHC 9.6.}
            @item{On average, seems to provide a 2–3% reduction in compile times and a 5–10% reduction in allocations, when compiled with@tt{-O0}.}
            @item{Benefits essentially all build configurations, but gains are proportionally smaller for optimized builds.}
            @item{In-depth blog post will be published on the Tweag blog in coming weeks.})
     (blank 0 130)))

  (slides ([s:bullet 0] [s:thanks? #f])
    #:timeline (tl:sequence s:bullet 6) (tl:flags s:thanks?)
    #:title "Closing Thoughts"
    #:with current-para-spacing '(lines 0.6)
    #:with current-para-width 1300
    (vc-append
     (paras #:stage (s:bullet)
            @item{Recap: TH and codegen performance improvements coming to GHC 9.6.}
            (indent #:by (em 2) @item{Further bytecode compiler improvements coming to GHC 9.8.})
            @item{Many thanks again to Mercury for funding this work!}
            @item{Next steps: the typechecker and optimizer?}
            @item{Please come talk to us while you’re here if you’d like to learn more!})
     (blank 0 40)
     (pict-when (s:thanks?)
       (scale @elem{Thanks!} 2))
     (blank 0 80))))

(start-at-recent-slide)
