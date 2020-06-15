#lang at-exp curly-fn slideshow

(require (for-syntax racket/match
                     syntax/parse/experimental/template)
         csv-reading
         match-plus
         pict/conditional
         ppict/tag
         (except-in plot/pict line)
         plot/utils
         ppict/align
         racket/draw
         racket/runtime-path
         rsvg
         slideshow/code
         slideshow/text
         syntax/parse/define
         threading

         (prefix-in racket: racket/base)
         (prefix-in slideshow: slideshow/base)
         (only-in slideshow [current-font-size current-text-size])

         "lib/pict.rkt"
         "lib/slideshow.rkt"
         "lib/util.rkt")

(define-runtime-path eff.png "assets/eff.png")
(define-runtime-path proposal.png "assets/proposal.png")

(define-runtime-path hasura-logo-vertical-black.svg "assets/hasura_logo_vertical_black.svg")
(define-runtime-path graphql-logo.svg "assets/graphql-logo.svg")
(define-runtime-path postgres-logo.svg "assets/postgres-logo.svg")

(define-runtime-path countdown/big-ints.csv "assets/countdown/big-ints.csv")
(define-runtime-path countdown/single-module.csv "assets/countdown/single-module.csv")
(define-runtime-path countdown/multi-module.csv "assets/countdown/multi-module.csv")

(define-runtime-path BoolExp.hs "assets/BoolExp.hs")

;; ---------------------------------------------------------------------------------------------------

(define section (make-parameter #f))

(current-main-font "Concourse T3")
(current-code-font '((weight . semibold) . "Fira Code"))
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
(define shadow-color (make-color #x70 #x30 #x30))
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

(define (haskell . elems)
  (define (decode elem)
    (match elem
      ["\n" 'newline]
      [(tagged t elems) (tag t (apply haskell elems))]
      [(? string?)
       (codeblock-pict
        #:keep-lang-line? #f
        (string-append "#lang haskell-lexer/distinguish-constructors\n" elem))]
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

(define (ol #:make-num [make-num (λ (n) (t (~a n ".")))]
            #:sep [sep (em 3/4)]
            #:spacing [spacing (current-para-spacing)]
            #:stage [stage #f]
            . elems)
  (define num-picts (for/list ([i (in-range (length elems))])
                      (c:plain (make-num (add1 i)))))
  (define max-num-width (apply max (map pict-width num-picts)))
  (~>> (for/list ([elem (in-list elems)]
                  [num (in-list num-picts)])
         (htl-append sep (indent #:by (- max-num-width (pict-width num)) num) elem))
       (apply paras #:spacing spacing #:stage stage)))

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

(define (tl:bullets params tree)
  (match-define (list s:a s:b) params)
  (define init-a (s:a))
  (define init-b (s:b))
  (for ([i (in-naturals)]
        [n (in-list tree)])
    (s:a (+ init-a i))
    (for ([j (in-range n)])
      (s:b (+ init-b j))
      (next))))

;; ---------------------------------------------------------------------------------------------------

;                                                                                 
;                                                                                 
;                                      ;;                      ;                  
;  ;;;;;                                ;                      ;                  
;    ;           ;                      ;                 ;                       
;    ;           ;                      ;                 ;                       
;    ;  ;; ;;   ;;;; ;; ;;   ;;;     ;;;;  ;;  ;;    ;;; ;;;;  ;    ;;;   ;; ;;   
;    ;   ;;  ;   ;    ;; ;  ;   ;   ;   ;   ;   ;   ;   ; ;   ;;   ;   ;   ;;  ;  
;    ;   ;   ;   ;    ;    ;     ; ;    ;   ;   ;  ;    ; ;    ;  ;     ;  ;   ;  
;    ;   ;   ;   ;    ;    ;     ; ;    ;   ;   ;  ;      ;    ;  ;     ;  ;   ;  
;    ;   ;   ;   ;    ;    ;     ; ;    ;   ;   ;  ;      ;    ;  ;     ;  ;   ;  
;    ;   ;   ;   ;    ;     ;   ;   ;   ;   ;  ;;   ;   ; ;    ;   ;   ;   ;   ;  
;  ;;;;;;;; ;;;   ;; ;;;     ;;;     ;;;;;   ;; ;;   ;;;   ;; ;;;   ;;;   ;;; ;;; 
;                                                                                 
;                                                                                 
;                                                                                 

(section "Title")

(slides ()
  (~> (vc-append title
                 (~> (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                     (inset 0 -5 0 15))
                 (with-size 30
                   (hflex (+ (pict-width title) 20)
                          (t "Alexis King") (spring 1) (t "ZuriHac 2020"))))
      (colorize text-secondary-color))

  #:where
  (define title (~> (with-size 150
                      (with-font "Concourse C2"
                        (t "Effects for Less")))
                    (colorize text-plain-color))))

(begin
  (section "History")

  (define (timeline-pict events
                         #:stage [stage (length events)]
                         #:gap [gap (* (resolve-para-spacing) 5)]
                         #:bullet-size [bullet-size 14]
                         #:bullet-border-size [bullet-border-size 2]
                         #:bullet-margin [bullet-margin 10]
                         #:line-width [line-width 2]
                         #:line-segment-size [segment-size 5])
    (define bullet (cc-superimpose (disk bullet-size
                                         #:color text-secondary-color
                                         #:draw-border? #f)
                                   (disk (- bullet-size bullet-border-size)
                                         #:color "white"
                                         #:draw-border? #f)))
    (define max-year-width (~>> events
                                (map (λ~> car pict-width))
                                (apply max)))

    ; assemble each timeline entry
    (match-define (list (list event-picts bullets) ...)
      (for/list ([event (in-list events)]
                 [i (in-naturals 1)])
        (match-define (cons year description) event)
        (define bullet* (launder bullet))
        (define event-pict
          (htl-append
           ; make all dates the same width, right-aligned
           (inset year (- max-year-width (pict-width year)) 0 0 0)
           ; align bullet to center of first line of text
           (inset bullet*
                  bullet-margin
                  (/ (- (pict-ascent year) bullet-size) 2))
           description))
        (list (pict-when (>= stage i) event-pict) bullet*)))

    ; add connecting lines under bullets
    (for/fold ([events-pict (apply vl-append gap event-picts)])
              ([i (in-range 1 stage)])
      (define prev-bullet (list-ref bullets (sub1 i)))
      (define next-bullet (list-ref bullets i))
      (define-values [_ y1] (cc-find events-pict prev-bullet))
      (match-define-values [_ y2] (cc-find events-pict next-bullet))
      (define line-pict (~> (vline 0 (- y2 y1) #:segment segment-size)
                            (adjust-pen #:width 1.5
                                        #:color tertiary-color
                                        #:cap 'butt)))
      (pin-under events-pict prev-bullet cc-find line-pict)))

  (slides ([s:event 0] [s:bullet 1])
    #:with current-text-size 16
    #:with current-line-sep 0
    (timeline-pict events #:stage (s:event))
    #:timeline (tl:bullets (list s:event s:bullet) '[1 3 3 1 1])
    #:where
    (define (date s) (with-font "Concourse C3"
                       (colorize (t s) text-plain-color)))
    (define-simple-macro (sub p:expr) (with-size 14 p))

    (define all-events
      (parameterize ([current-para-width 300])
        (list (cons (date "Feb 2016") (list @para{Alexis starts writing Haskell.}
                                            (sub @item{Mostly a CRUD app.})
                                            (sub @item{Performance not that important.})))
              (cons (date "Dec 2017") (list @para{Effect management is getting complicated.}
                                            (sub @item{Start exploring effect systems.})
                                            (sub @item{Clean up@tt{freer}, release as@tt{freer-simple}.})))
              (cons (date "Feb 2018") (list @para{Break from writing Haskell to write Racket.}))
              (cons (date "Jul 2019") (list @para{Back to writing Haskell.})))))
    (define events
      (for/list ([event (in-list all-events)]
                 [i (in-naturals 1)])
        (match-define (cons date bullets) event)
        (define shown-bullets
          (if (= i (s:event))
              (picts-take bullets (s:bullet))
              bullets))
        (cons date (apply paras shown-bullets)))))

  (slides ([s:description? #f] [s:gql-to-sql? #f] [hi:realtime? #f] [s:perf? #f])
    (vc-append 80 (hc-append 50 (scale-to-fit hasura-logo 300 300)
                             (pict-when (s:description?)
                               (c:plain (vc-append 50 p:realtime-gql p:gql-to-sql))))
               (pict-when (s:perf?)
                 (with-size 60 @elem{Performance is@it{really} important!})))

    #:timeline (next)
    (s:description? #t) (next) (s:gql-to-sql? #t) (next)
    (hi:realtime? #t) (next) (s:perf? #t) (next)
    #:where
    (define (realtime p) (highlight-if (hi:realtime?) (t p)))

    (define hasura-logo (rsvg-isolate (svg-file->pict hasura-logo-vertical-black.svg)))
    (define graphql-logo (rsvg-isolate (svg-file->pict graphql-logo.svg)))
    (define postgres-logo (rsvg-isolate (svg-file->pict postgres-logo.svg)))
  
    (define p:realtime-gql
      @elem{“@realtime{Realtime} GraphQL on PostgreSQL”})
    (define p:gql-to-sql
      (pict-when (s:gql-to-sql?)
        (vc-append 30 @elem{Secretly: a GraphQL to SQL@realtime{JIT} compiler}
                   (hc-append 30 (scale-to-fit graphql-logo 100 100)
                              (arrow-line #:line-length 150)
                              (scale-to-fit postgres-logo 100 100))))))

  (define p:can-we-afford? @elem{Can we afford to use an effect system?})
  (slides () p:can-we-afford?))

;                                                                              
;                                                                              
;                                ;;                               ;;           
;  ;;;;;;                         ;                                ;           
;   ;    ;                        ;                                ;           
;   ;    ;                        ;                                ;           
;   ;    ;    ;;;  ;; ;;     ;;;  ; ;;   ;; ;;  ;;    ;;;   ;; ;;  ;  ;;; ;;;; 
;   ;;;;;    ;   ;  ;;  ;   ;   ; ;;  ;   ;;  ;;  ;      ;   ;; ;  ;  ;  ;   ; 
;   ;    ;  ;    ;  ;   ;  ;    ; ;   ;   ;   ;   ;      ;   ;     ; ;   ;     
;   ;     ; ;;;;;;  ;   ;  ;      ;   ;   ;   ;   ;   ;;;;   ;     ;;;    ;;;  
;   ;     ; ;       ;   ;  ;      ;   ;   ;   ;   ;  ;   ;   ;     ;  ;      ; 
;   ;     ;  ;   ;  ;   ;   ;   ; ;   ;   ;   ;   ;  ;   ;   ;     ;   ; ;   ; 
;  ;;;;;;;    ;;;  ;;; ;;;   ;;; ;;; ;;; ;;; ;;; ;;;  ;;;;; ;;;   ;;;  ;;;;;;  
;                                                                              
;                                                                              
;                                                                              

(section "Benchmarks")

(plot-background-alpha 0)
(plot-foreground text-plain-color)
(plot-font-size 30)
(plot-font-face (current-main-font))
(plot-y-ticks no-ticks)
(error-bar-width 20)

(define red-brush-color (apply make-color (->brush-color 1)))
(define red-pen-color (apply make-color (->pen-color 1)))
(define blue-brush-color (apply make-color (->brush-color 3)))
(define blue-pen-color (apply make-color (->pen-color 3)))

(define/match (brush-color name)
  [('red) red-brush-color]
  [('blue) blue-brush-color])
(define/match (pen-color name)
  [('red) red-pen-color]
  [('blue) blue-pen-color])

(define (µs n) (* n 10e6))

(define (countdown-mtl:main)
  @haskell{
 @t:main{countDown} :: Int -> (Int, Int)
 @t:main{countDown} @t:init{initial} = @t:state/f{runState} @t:call-prog{program} @t:init{initial}})

(define (countdown-mtl:prog #:signature? [signature? #t] #:use-do? [use-do? #t])
  (define signature
    @haskell{@t:prog{program} :: @t:constraint{@t:state/c{MonadState} Int m} => m Int})
  (define definition
    @haskell{
 @t:prog{program} = @(pict-if use-do?
                              @haskell{do @t:get{n <- get}}
                              @haskell{@t:get{get} @t:bind{>>=} \n ->})
              if @t:cmp{n <= 0}
                then @t:ret{return n}
                else @t:put{put (n - 1)} @t:bind{>>} @t:loop{program}})
  (if signature?
      @haskell{@signature
               @definition}
      definition))

(define (countdown-mtl #:use-do? [use-do? #t])
  @haskell{
 @(countdown-mtl:main)

 @(countdown-mtl:prog #:use-do? use-do?)})

(define (countdown-baseline:prog #:int-type [int-type "Int"])
  @haskell{
 program :: @int-type -> (@int-type, @int-type)
 program n = if n <= 0 then (n, n)
             else program (n - 1)})

(define (countdown-baseline)
  @haskell{
 countDown :: Int -> (Int, Int)
 countDown = @t:call-prog{program}

 @(countdown-baseline:prog)})

(begin
  (section "Benchmarks")
  
  (struct bench-row (name mean error) #:transparent)
  (define/match* (parse-bench-row (list name mean lower upper _ _ _))
    (bench-row name (string->number mean) (- (string->number upper) (string->number lower))))

  (define (rows-max rows)
    (~>> (for/list ([row (in-list rows)])
           (+ (bench-row-mean row) (bench-row-error row)))
         (apply max)))

  (define (load-benchmark-results csv)
    (let ()
      (define raw-data (call-with-input-file #:mode 'text csv csv->list))
      (define data (map parse-bench-row (rest raw-data)))
      (λ (#:polysemy? polysemy? #:eff? [eff? #f] #:mtl? [mtl? #t] #:fused-effects? [fused-effects? #t])
        (define to-remove (append (if polysemy? '() '("polysemy"))
                                  (if eff? '() '("eff"))
                                  (if mtl? '() '("mtl"))
                                  (if fused-effects? '() '("fused-effects"))))
        (filter-not (λ~> bench-row-name (member to-remove)) data))))

  (define countdown/single-module (load-benchmark-results countdown/single-module.csv))
  (define countdown/multi-module (load-benchmark-results countdown/multi-module.csv))
  (define countdown/big-ints (load-benchmark-results countdown/big-ints.csv))

  (define (chart rows
                 #:highlight [highlight? (const #f)]
                 #:width [width 900]
                 #:x-max [x-max (rows-max rows)]
                 #:x-label? [x-label? #t]
                 #:color [color 3]
                 #:label-bars? [label-bars? #f])
    (chart* (map list rows)
            #:highlight highlight?
            #:width width
            #:x-max x-max
            #:x-label? x-label?
            #:colors (list color)
            #:label-bars? label-bars?))

  (define (chart* rowss
                  #:highlight [highlight? (const #f)]
                  #:width [width 900]
                  #:height [height 400]
                  #:x-max [x-max (apply max (map rows-max rowss))]
                  #:x-label? [x-label? #t]
                  #:colors [colors '(3 2)]
                  #:label-bars? [label-bars? #f]
                  #:labels [labels #f]
                  #:gap [gap 0])
    (~> (for/list ([rows (in-list (reverse rowss))]
                   [i (in-naturals)]
                   #:when #t
                   [row (in-list (reverse rows))]
                   [base-color (in-cycle (reverse colors))]
                   ; only add labels once, since plot will duplicate them otherwise
                   [label (if (and (zero? i) labels)
                              (reverse labels)
                              (in-cycle (in-value #f)))]
                   [j (in-naturals)])
          (define n (+ (* i (+ (length rows) gap)) j))
          (match-define (bench-row name mean error) row)
          (define color (if (highlight? name) 1 base-color))
          (list (discrete-histogram (list (vector name (µs mean)))
                                    #:invert? #t
                                    #:x-min n
                                    #:color color
                                    #:line-color color
                                    #:label label)
                (error-bars (list (vector (+ n 1/2) (µs mean) (µs error))) #:invert? #t)
                (if label-bars?
                    (point-label (vector (µs mean) (+ n 1/2))
                                 (~a (~r (µs mean) #:precision 0) " µs")
                                 #:point-sym 'none
                                 #:size (round (* (plot-font-size) 2/3)))
                    '())))
        reverse ; to get the legend in the right order
        (plot #:width width #:x-max (ceiling (* (µs x-max)
                                                (if label-bars? 1.1 1)))
              #:x-label (and x-label? "mean time (µs)") #:y-label #f
              #:legend-anchor 'top-right)))

  (slides () (inset @titlet{Benchmarks} 50))

  (slides ([s:chart? #f] [s:lower-is-better? #f] [s:highlight? (const #f)] [s:polysemy? #t])
    (~> (pict-when (s:chart?)
          (chart (countdown/single-module #:polysemy? (s:polysemy?)) #:highlight (s:highlight?)))
        (vl-append 40 (hflex 900 @titlet{Benchmark: countdown} (spring 1) p:lower-is-better) _))
    #:timeline (next)
    (s:chart? #t) (next) (s:lower-is-better? #t) (next!)
    (s:highlight? (λ~> (equal? "polysemy"))) (next)
    (s:polysemy? #f) (next)
    #:where
    (define p:lower-is-better
      (pict-when (s:lower-is-better?)
        (with-size 36 @elem[#:color text-secondary-color]{(lower is better)}))))

  (slides ([s:answer? #f])
    (vc-append 30 p:can-we-afford?
               (pict-when (s:answer?)
                 (vc-append (with-size 60 @elem{Answer: yes.})
                            @elem{Just don’t pick polysemy.})))
    #:timeline (next) (s:answer? #t) (next))

  (slides ()
    (inset @elem{It’s never that simple!} 10))

  #;(slides ([s:never? #f])
      (vc-append 10 (with-size 30 @elem{(Just kidding.)})
                 (pict-when (s:never?) @elem{It’s never that simple!}))
      #:timeline (next) (s:never? #t) (next))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 5)
    #:with current-para-width 650
    (vl-append 50 (with-size 50 @titlet{Questions})
               (indent (ol #:stage (s:bullet)
                           @para{What@it{is} the countdown benchmark?}
                           @para{What is the “baseline” implementation?}
                           @para{Are these differences even meaningful?}
                           @para{Why does polysemy do@it{that} much worse?}))))

  (slides ()
    #:timeline (tl:sequence c:highlights '([] [main] [prog] [state/c state/f] [main] [state/f init]
                                              [prog call-prog] [get] [cmp] [ret] [put] [loop] []))
    (vc-append 50 @titlet{The Countdown Microbenchmark}
               (with-size 36 (countdown-mtl))))

  (slides ()
    #:timeline (tl:sequence c:highlights '([] [call-prog] []))
    (~> (vc-append 65 @titlet{Countdown Microbenchmark: Baseline}
                   (with-size 36 (countdown-baseline)))
        (inset 0 0 0 45)))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (paras #:align 'center #:spacing '(lines .75) #:stage (s:stage)
           (with-size 50 @elem{This is incredibly synthetic!})
           @elem{Is countdown a bad benchmark?}))

  (define big-haskell
    (let ([contents (file->lines BoolExp.hs)])
      (λ ()
        (define center-point (blank))
        (~> (map haskell contents)
            (apply vl-append (current-code-line-sep) _)
            (cc-superimpose center-point)
            (refocus center-point)))))

  (slides ([s:rw? #f] [s:micro? #f] [s:pros? #f] [s:cons? #f] [s:big-haskell? #f]
                      [s:rw/pros 0] [s:rw/cons 0] [s:micro/pros 0] [s:micro/cons 0])
    #:with current-slide-margin 0
    #:with current-para-width 530
    #:with current-item-indent 0

    (~> (paras #:align 'center #:spacing '(lines 1)
               (with-size 50 @titlet{In Defense of Microbenchmarks})
               (with-size 30
                 (hc-append area-margin
                            side-labels
                            (vc-append area-margin title:rw
                                       (area #:stage (s:rw/pros)
                                             @item{Probably representative of@it{something}.}
                                             @item{Unlikely to be a fluke/flaw of the benchmarking process.})
                                       (area #:stage (s:rw/cons)
                                             @item{Really big!}
                                             @item{Difficult to isolate costs.}
                                             @item{May be challenging to extrapolate results.}))
                            (vc-append area-margin title:micro
                                       (area #:stage (s:micro/pros)
                                             @item{Easy to isolate costs!}
                                             @item{Small enough to@it{completely} understand.}
                                             @item{If thoroughly understood, can provide a useful cost model.})
                                       (area #:stage (s:micro/cons)
                                             @item{Easy to measure the wrong thing!}
                                             @item{@it{Crucial} to understand the scope of results.}
                                             @item{Costs are not considered in context.})))))
        (cc-superimpose (pict-when (s:big-haskell?)
                          (~> (big-haskell) (scale 3/4) (rotate (* pi 1/8))))))

    #:timeline (next) (s:rw? #t) (s:pros? #t) (next)
    (tl:sequence s:rw/pros (in-range 1 3)) (s:cons? #t) (next)
    (s:rw/cons 1) (next) (tl:sequence s:rw/cons (in-range 2 4))
    (s:micro? #t) (next) (tl:sequence s:micro/pros (in-range 1 4))
    (tl:sequence s:micro/cons (in-range 1 4))
  
    #:where
    (define area-h 175)
    (define area-margin 25)
    (define area-bg (blank (current-para-width) area-h))
    (define (area #:stage stage . ps) (lc-superimpose area-bg (apply paras #:stage stage ps)))

    (define title:rw (pict-when (s:rw?) @titlet{Real-World Benchmarks}))
    (define title:micro (pict-when (s:micro?) @titlet{Microbenchmarks}))

    (define (rotate-label p) (rotate p (* pi -1/8)))
    (define label:pros (pict-when (s:pros?) (rotate-label @titlet{pros})))
    (define label:cons (pict-when (s:cons?) (rotate-label @titlet{cons})))
    (define side-labels (~> (hflex (+ (* 2 area-h) area-margin) (spring 1)
                                   label:cons (spring 2) label:pros (spring 1))
                            (rotate (* pi 1/2))
                            (inset -10 (+ (pict-height title:rw) area-margin) 0 0))))

  (slides ([s:code hide] [s:parse? #f] [s:get? #f] [s:put? #f] [s:ask? #f] [s:throw? #f])
    #:timeline
    (next) (s:code show) (next) (c:highlights '[parse]) (s:parse? #t) (next!)
    (c:highlights '[eff]) (tl:flags s:get? s:put? s:ask? s:throw?) (c:highlights '[])

    (vc-append 60 p:title ((s:code) p:code))

    #:with current-code-font (cons 'bold (current-code-font))
    #:where
    (define p:title
      (with-size 50
        @elem{Effects are@it{particularly} hard to measure with real-world benchmarks!}))

    (define ((fake-haskell class) chars)
      (define H (ghost (tt (make-string chars #\H))))
      (~> (filled-rectangle (pict-width H) (pict-ascent H)
                            #:color ((current-token-class->color) class) #:draw-border? #f)
          (cc-superimpose H)
          (refocus H)))

    (define fake:id (fake-haskell 'symbol))
    (define fake:ctor (fake-haskell 'constructor))
    (define fake:kw (fake-haskell 'keyword))
    (define (fake:space n) (make-string n #\space))

    (define p:parse
      (if (s:parse?)
          @haskell{@t:parse{res <- parseQuery q}}
          @haskell{@fake:id[3] @fake:kw[2] @fake:id[12]}))

    (define p:get (if (s:get?) @haskell{ @t:eff{get}} @fake:id[4]))
    (define p:put (if (s:put?) @haskell{@t:eff{put} } @fake:id[4]))
    (define p:ask (if (s:ask?) @haskell{ @t:eff{ask}} @fake:id[4]))
    (define p:throw (if (s:throw?) @haskell{@t:eff{throw}} @fake:id[5]))
  
    (define p:code
      @haskell{
 @fake:id[6] @fake:kw[2] @fake:ctor[5] @fake:kw[2] @fake:ctor[7]
 @fake:id[6] @fake:kw[4] @fake:id[3] @fake:kw[2] @fake:id[7]@p:get
 @fake:space[11] @|p:put|@fake:id[4]
 @fake:space[2] @fake:kw[5]
 @fake:space[4] @fake:id[5] @fake:kw[1] @fake:id[8]
 @fake:space[4] @fake:id[2] @fake:kw[1] @p:throw @fake:ctor[3] @fake:id[7]

 @fake:id[4] @fake:kw[2] @fake:ctor[7] @fake:kw[2] @fake:ctor[4] @fake:id[2]
 @fake:id[4] @fake:kw[4] @p:parse
 @fake:space[9] @fake:id[3] @fake:ctor[5] @fake:id[9]
 @fake:space[9] @fake:id[6] @fake:kw[2] @fake:id[8]
 @fake:space[9] @fake:kw[3] @fake:id[2] @fake:kw[1] @fake:id[2]@p:ask
 @fake:space[13] @fake:id[3] @fake:kw[1] @fake:ctor[4] @fake:id[2]
 @fake:space[9] @fake:id[6] @fake:ctor[5] @fake:id[6]}))

  (slides ([s:code hide])
    #:timeline (tl:show s:code) (c:highlights '[get put]) (next)
    (paras #:align 'center #:spacing 50
           (with-size 60 @elem{What does countdown measure?})
           ((s:code) (countdown-mtl))))

  (define (p:call fn) @elem{call @fn})
  (define p:ret @elem{return to caller})

  (define (draw-call call body ret
                     #:width [width 475]
                     #:padding [padding 30]
                     #:edge-color [edge-color 'red]
                     #:body-color [body-color 'blue]
                     #:stage [stage #f])
    (vc-append (pict-when (or (not stage) (>= stage 1))
                 (~> (filled-rectangle width (+ (pict-height call) (* padding 2))
                                       #:color (brush-color edge-color)
                                       #:border-color (pen-color edge-color)
                                       #:border-width 1)
                     (cc-superimpose call)))
               (pict-when (or (not stage) (>= stage 2))
                 (~> (filled-rectangle width (+ (pict-height body) (* padding 2))
                                       #:color (brush-color body-color)
                                       #:border-color (pen-color body-color)
                                       #:border-width 1)
                     (cc-superimpose body)))
               (pict-when (or (not stage) (>= stage 3))
                 (~> (filled-rectangle width (+ (pict-height ret) (* padding 2))
                                       #:color (brush-color edge-color)
                                       #:border-color (pen-color edge-color)
                                       #:border-width 1)
                     (cc-superimpose ret)))))

  (slides ([s:call 0] [s:call-color 'blue] [s:realcall? #f] [s:irrelevant? #f])
    (paras
     #:align 'center #:spacing 75
     expensive-defn
     (~> (hc-append (draw-call #:stage (s:call) #:edge-color (s:call-color)
                               (p:call @haskell{reallyExpensive})
                               (paras #:spacing '(lines 0.5)
                                      @haskell{colorGraph}
                                      @haskell{mineBitcoin}
                                      @haskell{compileHaskell})
                               p:ret)
                    (blank 100 0)
                    p:realcall
                    (blank 40 0)
                    p:irrelevant)
         (when~> (s:irrelevant?)
                 (pin-arrow-line 20 _ p:irrelevant ct-find
                                 p:realcall (adjust-find rt-find 10 (/ irrelevant-height 2))
                                 #:line-width 2 #:color text-plain-color
                                 #:start-angle (* pi 1/2) #:end-angle pi #:start-pull 2/3)
                 (pin-arrow-line 20 _ p:irrelevant cb-find
                                 p:realcall (adjust-find rb-find 10 (/ irrelevant-height -2))
                                 #:line-width 2 #:color text-plain-color
                                 #:start-angle (* pi 3/2) #:end-angle pi #:start-pull 2/3))
         (inset 0 0 -50 0)))
    #:timeline
    (tl:sequence s:call 4) (s:call-color 'red) (next) (s:realcall? #t) (next) (s:irrelevant? #t) (next)
    #:where
    (define expensive-defn
      @haskell{
 reallyExpensive :: MonadExpensive m => m Blah
 reallyExpensive = colorGraph >> mineBitcoin >> compileHaskell})

    (define p:irrelevant (pict-when (s:irrelevant?)
                           (~> @para[#:align 'center #:width 200]{basically irrelevant}
                               (inset 0 10))))
    (define irrelevant-height 15)
    (define p:realcall (pict-when (s:realcall?)
                         (draw-call #:padding 0
                                    (blank irrelevant-height)
                                    (inset (paras #:spacing '(lines 2.1)
                                                  @haskell{colorGraph}
                                                  @haskell{mineBitcoin}
                                                  @haskell{compileHaskell})
                                           0 60)
                                    (blank irrelevant-height)))))

  (slides ([s:cheap? #f] [s:appreciable? #f] [s:takeaway? #f])
    #:timeline (next) (s:cheap? #t) (next) (s:appreciable? #t) (next) (s:takeaway? #t) (next)
    (~> (vc-append 50
                   p:appreciable
                   (hc-append 10
                              call:get
                              (pict-when (s:cheap?)
                                (hc-append 10 (rotate cheap-arrow pi)
                                           (vc-append -5 @elem{really} @elem{cheap!})
                                           cheap-arrow))
                              call:put)
                   (pict-when (s:takeaway?)
                     (with-size 50 @elem{Countdown benchmarks@it{effect dispatch}.})))
        (when~> (s:appreciable?)
                (pin-arrow-line 20 _ p:appreciable (adjust-find lc-find 0 5)
                                call:get (adjust-find ct-find 0 -10)
                                #:line-width 2 #:color text-plain-color
                                #:start-angle pi #:end-angle (* pi 3/2) #:end-pull 2/3)
                (pin-arrow-line 20 _ p:appreciable (adjust-find rc-find 0 5)
                                call:put (adjust-find ct-find 0 -10)
                                #:line-width 2 #:color text-plain-color
                                #:start-angle 0 #:end-angle (* pi 3/2) #:end-pull 2/3)))
    #:where
    (define cheap-arrow (c:plain (arrow-line #:arrow-size 20 #:line-length 40)))
    (define call:get (draw-call #:width 400 (p:call @haskell{get}) @elem{retrieve state} p:ret))
    (define call:put (draw-call #:width 400 (p:call @haskell{put}) @elem{update state} p:ret))
    (define p:appreciable (pict-when (s:appreciable?)
                            (~> @elem{appreciable amount of time}
                                (inset 20 0)))))

  (slides ([s:use-do? #t])
    #:timeline (next) (s:use-do? #f) (next) (c:highlights '[bind]) (next)
    (countdown-mtl #:use-do? (s:use-do?)))

  (slides ([s:do hide] [s:ap hide] [s:traverse hide] [s:cond hide])
    #:timeline (next) (tl:show s:do s:ap s:traverse s:cond)
    (vc-append 50 (with-size 50 @elem{@haskell{>>=} is an exceptionally common operation.})
               (hc-append 50 ((s:do) p:do) ((s:ap) p:ap))
               (hc-append 50 ((s:traverse) p:traverse) ((s:cond) p:cond)))
    #:where
    (define tile-bg (blank 300 150))
    (define (tile p) (cc-superimpose tile-bg p))
    (define p:do
      (tile @haskell{do a <- f x
                        b <- g y
                        h a b}))
    (define p:ap
      (tile @haskell{Foo <$> getX
                         <*> getY
                         <*> getZ}))
    (define p:traverse
      (tile (paras #:align 'center @haskell{mapM} @haskell{traverse} @haskell{sequence})))
    (define p:cond
      (tile (paras #:align 'center @haskell{when} @haskell{unless} @haskell{replicateM}))))

  (slides ([s:bullet 0] [s:measurement 1])
    #:timeline (tl:sequence s:bullet 4) (tl:sequence s:measurement (in-range 2 4))
    #:with current-para-width 700
    (paras #:spacing 35
           (with-size 60 @titlet{Recap})
           (indent (ol #:stage (s:bullet)
                       @para{Countdown is a microbenchmark.}
                       @para{Theoretically, it’s a valuable benchmark.}
                       (paras #:stage (s:measurement)
                              @para{It measures two things:}
                              @item{…the cost of effect dispatch.}
                              @item{…the cost of@haskell{>>=}.})))))

  (slides () @elem{Why am I belaboring this point?})

  (slides ([s:wrong hide] [s:misleading hide])
    #:timeline (tl:show s:wrong s:misleading)
    (~> (chart (countdown/single-module #:polysemy? #t) #:x-label? #f)
        (cc-superimpose (~> (with-font "Concourse T6"
                              (with-size 200
                                @elem[#:color text-error-color]{WRONG!}))
                            (rotate (* pi 0.05))
                            (inset 0 -50 -100 0)
                            ((s:wrong))))
        (vc-append 50 _ ((s:misleading) (with-size 60 @elem{…or at least highly misleading.})))))

  (slides ([s:program 'single] [s:hope hide])
    #:timeline (next) (s:program 'multiple) (next) (tl:show s:hope)
    (~> (pict-case (s:program)
          #:combine cc-superimpose
          [(single) (paras @haskell{module CountDown where} (indent (countdown-mtl)))]
          [(multiple) (paras @haskell{module CountDown where}
                             (indent (paras @haskell{import Program}
                                            (countdown-mtl:main)))
                             @haskell{}
                             @haskell{module Program where} (indent (countdown-mtl:prog)))])
        (vc-append 40 _ ((s:hope) (with-size 50 @elem{Surely this shouldn’t change anything?})))))

  (define (countdown-chart/both #:eff? [eff? #f]
                                #:polysemy? [polysemy? #t]
                                #:label-bars? [label-bars? #f]
                                #:x-label? [x-label? #t])
    (chart* (map list (countdown/single-module #:eff? eff? #:polysemy? polysemy?)
                 (countdown/multi-module #:eff? eff? #:polysemy? polysemy?))
            #:gap 0.75
            #:labels '["single module" "multi module"]
            #:label-bars? label-bars?
            #:x-label? x-label?))

  (slides ([s:program 'single] [s:polysemy? #t] [s:label-bars? #f])
    #:timeline (next) (s:program 'multiple) (next) (s:program 'both) (next!)
    (s:polysemy? #f) (s:label-bars? #t) (next)
    (pict-case (s:program)
      [(single) (chart (countdown/single-module #:polysemy? (s:polysemy?))
                       #:label-bars? (s:label-bars?))]
      [(multiple) (chart (countdown/multi-module #:polysemy? (s:polysemy?))
                         #:label-bars? (s:label-bars?)
                         #:color 2)]
      [(both) (countdown-chart/both #:polysemy? (s:polysemy?)
                                    #:label-bars? (s:label-bars?))]))

  (slides () (inset @elem{What happened?} 50))

  (slides ([s:small hide] [s:big hide] [s:victim hide])
    #:timeline (next) (tl:show s:small s:big s:victim)
    #:with plot-height 350
    (~> (countdown-chart/both #:x-label? #f)
        (pin-over 350 85 (~> (with-size 30 (paras #:spacing 0 @elem{@it{massive}} @elem{change!}))
                             (hc-append 15
                                        (~> (vr-append 20
                                                       (~> (c:plain (arrow-line #:line-length 55))
                                                           (rotate (turns 0.5))
                                                           (inset 0 0 5 0))
                                                       (~> (c:plain (arrow-line #:line-length 45))
                                                           (rotate (turns 0.6))))
                                            (inset 0 0 0 -50))
                                        _)
                             ((s:big))))
        (pin-over 725 175 (~> (with-size 30 @elem{small change})
                              (vc-append 10 _ (~> (c:plain (arrow-line #:line-length 25))
                                                  (rotate (turns 0.7))
                                                  (inset -20 0 0 0)))
                              ((s:small))))
        (vc-append 40 _ ((s:victim) @elem{mtl and fused-effects are victims of the optimizer.}))))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 4)
    #:with current-para-width 775
    (paras #:spacing 40 (with-size 55 @titlet{Key Takeaways})
           (indent (ol #:spacing '(lines 0.75) #:stage (s:bullet)
                       @para{mtl and fused-effects@it{are} faster than polysemy…}
                       @para{…but are more reliant on compiler optimizations for best-case perf.}
                       @para{Tiny program changes can have huge perf diffs!}))))

  #;(slides ([s:stage 0])
      #:timeline (tl:sequence s:stage (in-range 1 3))
      (paras #:spacing 50 #:align 'center #:stage (s:stage)
             @elem{This is somewhat troubling.}
             @elem{Can we get that performance back?})))

;                                                                                           
;                                                                                           
;           ;;                                        ;               ;                     
;  ;;;;;;;;; ;                  ;;;;;                 ;               ;                     
;  ;   ;   ; ;                 ;     ;           ;                                          
;  ;   ;   ; ;                ;       ;          ;                                          
;      ;     ; ;;     ;;;     ;       ; ;;;;;   ;;;;  ;  ;; ;;  ;;    ;  ;;;;;   ;;;  ;; ;; 
;      ;     ;;  ;   ;   ;    ;       ;  ;   ;   ;   ;;   ;;  ;;  ;  ;;  ;   ;  ;   ;  ;; ; 
;      ;     ;   ;  ;    ;    ;       ;  ;    ;  ;    ;   ;   ;   ;   ;     ;  ;    ;  ;    
;      ;     ;   ;  ;;;;;;    ;       ;  ;    ;  ;    ;   ;   ;   ;   ;    ;   ;;;;;;  ;    
;      ;     ;   ;  ;         ;       ;  ;    ;  ;    ;   ;   ;   ;   ;   ;    ;       ;    
;      ;     ;   ;   ;   ;     ;     ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   ;  ;   ;  ;    
;     ;;;   ;;; ;;;   ;;;       ;;;;;    ;;;;     ;; ;;; ;;; ;;; ;;; ;;; ;;;;;   ;;;  ;;;   
;                                        ;                                                  
;                                        ;                                                  
;                                       ;;;                                                 

(define shape-bg-color (make-color #xf7 #xf7 #xf7))
(define shape-border-color (make-color #xd8 #xd8 #xd8))
(define (p:file p #:color [color shape-bg-color]
                  #:border-color [border-color shape-border-color])
  (cc-superimpose (~> (file-icon 40 50)
                      (adjust-pen #:color border-color
                                  #:width 1.25
                                  #:cap 'projecting
                                  #:join 'miter)
                      (colorize color))
                  (~> (scale-to-fit p 25 35)
                      (colorize text-secondary-color))))

(define (circled n #:inline? [inline? #f])
  (define s (current-font-size))
  (define w (* s 3/50))
  (define offset (match n
                   [1 -0.04]
                   [2 -0.02]
                   [3 -0.06]
                   [4  0.02]))
  (define p (~> (with-font "Concourse C4" (t (~a n)))
                (inset 0 0 (* s offset) 0)))
  (define p* (scale p 0.75))
  (define g (ghost p))
  (define d (- (pict-height p*) (* s 1/25)))
  (~> ((if inline? ctl-superimpose cc-superimpose) g p*)
      (refocus p*)
      (cc-superimpose (circle d #:border-width w))
      (refocus g)
      (inset (/ (- (pict-height p*)
                   (pict-width p*))
                2)
             0)))

(define (mem:box [w 55] [h w])
    (~> (filled-rectangle w h #:color shape-bg-color)
        (adjust-pen #:color shape-border-color #:width 2 #:join 'miter)))
  (define (mem:label p #:padding [padding 15])
    (cc-superimpose (mem:box (+ (pict-width p) (* 2 padding)) 55) p))
  (define (mem:ptr #:length [len 50]
                   #:angle [angle 0])
    (define box (mem:box))
    (define cx (/ (pict-width box) 2))
    (define cy (/ (pict-height box) 2))
    (define arrow (points-at #:length len))
    (~> box
        (pin-over (- cx 7) (- cy 7) (disk 14 #:draw-border? #f #:color text-plain-color))
        (pin-over cx cy (~> (blank)
                            (pin-over 0 (/ (pict-height arrow) -2) arrow)
                            (rotate angle)))))
  
  (define (points-at #:length [len 50])
    (~> (arrow-line #:line-length len
                    #:arrow-size 15
                    #:line-width 3)
        (inset 0 0 1 0)
        (adjust-pen #:color text-plain-color
                    #:cap 'butt)))

(define step-arrow:right (~> (arrowhead 25 0) (colorize text-secondary-color)))
(define step-arrow:down (rotate step-arrow:right (turns -1/4)))

(define (free:countdown)
  @haskell{
 program :: @t:type{Eff} (@t:type{State} Int) Int
 program = @t:op{Get} @t:op{`Then`} \n ->
             if n <= 0
               then @t:op{Return} n
               else @t:op{Put} (n - 1) @t:op{`Then`} \_ -> program})

(define (free:run-state)
  @haskell{
 runState :: s -> Eff (State s) a -> (s, a)
 runState s (Return x)       = (s, x)
 runState s (Get   `Then` k) = runState s (k s)
 runState _ (Put s `Then` k) = runState s (k ())})

(define (trans:countdown)
  @haskell{
 program :: Int -> State Int Int
 program = get >>= \n ->
             if n <= 0
               then return n
               else put (n - 1) >> program})

(define (balloon w h
                 #:corner-radius [r 25]
                 #:spike-size [s 25]
                 #:spike-position [s-posn 0]
                 #:color [c shape-bg-color]
                 #:border-color [bc shape-border-color]
                 #:border-width [bw 2])
  (define 2s (* 2 s))
  (define s-offset (* s-posn (- w 2s)))
  (define tl-r (min r s-offset))
  (define tr-r (min r (- (- w 2s) s-offset)))
  
  (define path (new dc-path%))
  ; draw the left side of the top
  (send path arc 0 0 tl-r tl-r (turns 1/2) (turns 1/4) #f)
  (send path line-to s-offset 0)

  ; draw the spike
  (send path line-to (+ s-offset s) (- s))
  (send path line-to (+ s-offset 2s) 0)

  ; draw the right side of the top
  (send path line-to (- w tr-r) 0)
  (send path arc (- w tr-r) 0 tr-r tr-r (turns 1/4) 0 #f)
  (send path line-to w (- h r))

  ; draw the rest
  (send path arc (- w r) (- h r) r r 0 (turns -1/4) #f)
  (send path line-to r h)
  (send path arc 0 (- h r) r r (turns 3/4) (turns 1/2) #f)
  (send path close)

  (~> (dc (λ (dc dx dy) (send dc draw-path path dx (+ dy s))) w (+ h s))
      (inset 0 (- s) 0 0)
      (adjust-pen #:color bc #:width bw)
      (maybe-colorize c)))

(define (wrap-balloon p
                      #:spike-size [s 25]
                      #:spike-position [s-posn 0]
                      #:padding [padding 25])
  (cc-superimpose (balloon (+ (pict-width p) (* padding 2))
                           (+ (pict-height p) (* padding 2))
                           #:spike-size s #:spike-position s-posn)
                  p))

(define (pin-balloon base path find p
                     #:spike-size [s 25]
                     #:spike-position [s-posn 0]
                     #:show? [show? #t])
  (define balloon-p (~> (wrap-balloon p #:spike-size s #:spike-position s-posn)
                        (show show?)))
  (define s-offset (* s-posn (- (pict-width balloon-p) (* 2 s))))
  (pin-over base path (adjust-find find (- (+ s-offset s)) s) balloon-p))

(begin
  (slides ([s:stage 0] [s:monad-opacity 1])
    #:with current-para-fill? #f
    #:timeline (tl:last! (tl:sequence s:stage 9)) (s:monad-opacity 1/3) (next) #:condense-last
    (ol #:make-num (λ (n) (~> (circled n)
                              (when~> (= n 1)
                                      (cellophane (s:monad-opacity)))))
        #:spacing '(lines 0.5)
        #:stage (ceiling (/ (s:stage) 4))
        (~> (paras #:stage (s:stage)
                   (with-size 50 @elem{Monad transformers + typeclasses.})
                   @item{Tried and true; believed to be performant.}
                   @item{Can require complex boilerplate.}
                   @elem{Examples:@tt{mtl},@tt{fused-effects}})
            (cellophane (s:monad-opacity)))
        (paras #:stage (- (s:stage) 4)
               (with-size 50 @elem{Free-like monads.})
               @item{Highly flexible, can be simpler to use.}
               @item{Performance is a known limitation.}
               @elem{Examples:@tt{freer-simple},@tt{polysemy}})))

  (slides ([s:mtl hide] [s:free hide])
    #:timeline (next) (tl:show s:mtl s:free)
    (tl:sequence c:highlights '([op] [op type]))
    (vc-append 50 ((s:mtl) countdown-mtl)
               ((s:free) step-arrow:down)
               ((s:free) (free:countdown)))
    #:where
    (define countdown-mtl
      (lc-superimpose (blank (pict-width (free:countdown)) 0)
                      (countdown-mtl:prog #:signature? #f #:use-do? #f))))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 4))
    (paras #:spacing '(lines 1) #:stage (s:stage)
           defn:eff defn:state (free:run-state))
    #:where
    (define defn:eff
      @haskell{
        data Eff f a where
          Return :: a -> Eff a
          Then   :: f a -> (a -> Eff b) -> Eff b})
    (define defn:state
      @haskell{
        data State s a where
          Get    :: State s s
          Put    :: s -> State ()}))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 7))
    #:with current-para-fill? #f
    (paras #:stage (s:stage)
           @titlet{Pros}
           @item{Beautifully simple.}
           @item{Extremely flexible.}
           (paras (blank 0 20) @titlet{Cons})
           @item{Reifies the entire program as a tree.}
           @item{Obscures structure to the optimizer.}))

  (slides () (free:countdown))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 4)
    (paras #:spacing '(lines 1) #:stage (s:stage)
           defn:state defn:monad defn:ops)
    #:where
    (define defn:state
      @haskell{newtype State s a = State { runState :: s -> (s, a) }})
    (define defn:monad
      @haskell{
 instance Monad (State s) where
   return x = State $ \s -> (s, x)
   m >>= f = State $ \s -> case runState m s of
     (s', a) -> runState (f a) s'})

    (define defn:ops
      @haskell{
 get :: State s s
 get = State $ \s -> (s, s)

 put :: s -> State s ()
 put s = State $ \_ -> (s, ())}))

  (define (countdown:state)
    @haskell{
      program :: @t:program-type{State Int Int}
      program = get @t:bind{>>=} \n ->
                  if n <= 0
                    then return n
                    else put (n - 1) @t:bind{>>} program})

  (slides ([inline:bind? #f] [inline:state? #f]
           [inline:return? #f] [inline:get? #f] [inline:put? #f]
           [simplify:body? #f] [simplify:else? #f])
    #:timeline
    (define (tl:inline sym flag #:skip-last? [skip-last? #f])
      (c:highlights (list sym)) (next)
      (flag #t) (next!)
      (c:highlights '[]) (unless skip-last? (next)))

    (next)
    (c:highlights '[bind]) (next!) (inline:bind? #t) (c:highlights '[]) (next)
    (c:highlights '[state]) (next!) (inline:state? #t) (c:highlights '[]) (next)
    (tl:inline 'return inline:return?)
    (tl:inline 'put inline:put?) (tl:inline 'else simplify:else?)
    (tl:inline 'get inline:get?) (tl:inline 'body simplify:body? #:skip-last? #t)

    (pict-cond
     [(inline:state?) (countdown/inlined "s1")]
     [(inline:bind?)  countdown/no-bind]
     [else            (countdown:state)])

    #:where
    (define runState @t:state{runState})
    (define countdown/no-bind
      @haskell{
        program :: State Int Int
        program = @t:state{State} $ \s1 -> @runState get s1 of
          (s2, n) -> if n <= 0
                       then @runState (return n) s2
                       else case @runState (put (n - 1)) s2 of
                         (s3, _) -> @runState program s3})

    (define (p:return x s)
      (if (inline:return?)
          @haskell{@t:return{(@s, @x)}}
          @haskell{@t:return{return} @x @s}))
    (define (p:get s)
      (if (inline:get?)
          @haskell{@t:get{(@s, @s)}}
          @haskell{@t:get{get} @s}))
    (define (p:put s1 s2)
      (if (inline:put?)
          @haskell{@t:put{(@s1, ())}}
          @haskell{@t:put{put} (@s1) @s2}))

    (define (p:else n s)
      (define n-1 @haskell{@n - 1})
      (pict-if (simplify:else?)
               @haskell{@t:else{program (@n-1)}}
               @haskell{case @t:else{@p:put[@n-1 s]} of
                          @t:else{(s3, _)} -> program s3}))
    (define (p:body s n)
      @haskell{if @n <= 0
                 then @p:return[n s]
                 else @p:else[n s]})

    (define (countdown/inlined s)
      @haskell{
        program :: Int -> (Int, Int)
        @(pict-if (simplify:body?)
                  @haskell{
                    program @s = @p:body[s s]}
                  @haskell{
                    program @s = case @t:body{@p:get[s]} of
                      @t:body{(s2, n)} -> @p:body["s2" "n"]})}))

  (slides ([s:baseline 1])
    #:timeline (tl:sequence s:baseline (in-range 1 3)) (c:highlights '[var]) (next)
    (paras #:stage (s:baseline) #:spacing '(lines 1)
           (countdown/inlined (t:var "s1"))
           (countdown/inlined (t:var "n")))
    #:where
    (define (countdown/inlined s)
      @haskell{
        program :: Int -> (Int, Int)
        program @s = if @s <= 0
                       then (@s, @s)
                       else program (@s - 1)}))

  (slides ([s:coupled? #f])
    #:timeline (next) (c:highlights '[program-type]) (tl:flags s:coupled?)
    (paras #:align 'center #:spacing '(lines 1.25)
           (with-size 50 @elem{This is great! But it’s not an effect system.})
           (~> p:countdown
               (when~> (s:coupled?)
                       (pin-over path:type (adjust-find rc-find 125 0) (pip p:coupled lc-find))
                       (pin-arrow-line 15 _ #:line-width 2 #:color text-secondary-color
                                       p:coupled (adjust-find lc-find -15 0)
                                       path:type (adjust-find rc-find  15 0)))))
    #:where
    (define p:countdown (countdown:state))
    (define path:type (find-tag p:countdown 'program-type))
    (define p:coupled
      (with-size 32
        @para[#:width 300 #:spacing -5 #:fill? #f]{coupled to implementation})))

  (slides ()
    (paras #:align 'center #:spacing 2
           @elem{Can we get the flexibility of free monads}
           @elem{with the performance of monad transformers?}))

  (slides ([s:consumer+producer hide] [s:expansion hide] [s:list hide]
           [hi:consumer? #f] [hi:producer? #f] [hi:cons? #f] [s:plus? #f] [hi:nil? #f] [s:zero? #f])
    #:timeline (next) (tl:show s:consumer+producer)
    (tl:last! (tl:flags hi:producer? hi:consumer?))
    (hi:producer? #f) (hi:consumer? #f) (tl:show s:expansion s:list)
    (tl:last! (tl:flags hi:cons? s:plus?)) (tl:flags #:set #f hi:cons?)
    (tl:last! (tl:flags hi:nil? s:zero?)) (tl:flags #:set #f hi:nil?)

    (vc-append
     25 @elem{Similar to the list fusion problem.}
     (~> (vc-append 30 p:consumer+producer
                    (hc-append (blank 50 0) p:l:consumer (blank 90 0) p:l:producer))
         (when~> (hi:consumer?)
                 (pin-arrow-line 10 _ #:line-width 2 #:color text-secondary-color
                                 p:l:consumer (adjust-find ct-find  0  5)
                                 p:consumer   (adjust-find cb-find  0 10)))
         (when~> (hi:producer?)
                 (pin-arrow-line 10 _ #:line-width 2 #:color text-secondary-color
                                 p:l:producer (adjust-find ct-find -5  0)
                                 p:producer   (adjust-find cb-find  0 10)))
         (refocus p:consumer+producer)
         ((s:consumer+producer)))
     ((s:expansion) step-arrow:down)
     (~> (vc-append 50 @haskell{@haskell{foldr @p:+ @p:0} (1:2:3:4:5:[])} ((s:list) p:list))
         (when~> (hi:cons?) (pin-arrow-lines p:+ cb-find 'cons ct-find))
         (when~> (hi:nil?) (pin-arrow-line 10 _ #:line-width 2 #:color text-secondary-color
                                           p:0 cb-find p:nil ct-find
                                           #:start-angle (turns -1/4) #:start-pull 0.1
                                           #:end-angle (turns -1/4) #:end-pull 0.2))
         ((s:expansion))))
    #:where
    (define (pin-arrow-lines base src find-src dest-tag find-dest)
      (for*/fold ([base base])
                 ([dest (in-list (find-tag* base dest-tag))])
        (pin-arrow-line 10 #:line-width 2 #:color text-secondary-color
                        base src find-src dest find-dest
                        #:start-angle (turns -1/4) #:start-pull 0.1
                        #:end-angle (turns -1/4) #:end-pull 0.2)))
    
    (define p:+ (highlight-if (hi:cons?) @haskell{(+)}))
    (define p:0 (highlight-if (hi:nil?) @haskell{0}))

    (define p:consumer (highlight-if (hi:consumer?) @haskell{foldr (+) 0}))
    (define p:producer (highlight-if (hi:producer?) @haskell{[1..5]}))
    (define p:consumer+producer @haskell{@p:consumer @p:producer})
    (define p:l:consumer (pict-when (hi:consumer?) (with-size 35 @elem{consumer})))
    (define p:l:producer (pict-when (hi:producer?) (with-size 35 @elem{producer})))

    (define (p:cons) (highlight-if (hi:cons?) @haskell{@t:cons{@(if (s:plus?) "+" ":")}}))
    (define p:nil (highlight-if (hi:nil?) (pict-if (s:zero?) @haskell{0} @haskell{[]})))
    (define p:list @haskell{1 @p:cons[] 2 @p:cons[] 3 @p:cons[] 4 @p:cons[] 5 @p:cons[] @p:nil}))

  (slides ([s:producer? #f] [s:consumer? #f])
    #:timeline (next)
    (c:highlights '[producer]) (next) (s:producer? #t) (next)
    (c:highlights '[consumer]) (next) (s:consumer? #t) (next)
    (c:highlights '[]) (next) (c:highlights '[op]) (next)
    
    (~> (vc-append 30 p:countdown (hc-append 500 p:producer p:consumer) p:run-state)
        (when~> (s:producer?)
                (pin-arrow-line 20 _ #:line-width 3 #:color text-secondary-color
                                p:producer (adjust-find rc-find 10 5) #:start-angle 0
                                p:countdown (adjust-find cb-find -70 10) #:end-angle (turns 1/4)))
        (when~> (s:consumer?)
                (pin-arrow-line 20 _ #:line-width 3 #:color text-secondary-color
                                p:consumer (adjust-find lc-find -10 5) #:start-angle (turns 1/2)
                                p:run-state (adjust-find ct-find 70 -5) #:end-angle (turns -1/4))))
    #:where
    (define p:producer (pict-when (s:producer?) @elem{producer}))
    (define p:consumer (pict-when (s:consumer?) @elem{consumer}))
    (define p:countdown (t:producer (free:countdown)))
    (define p:run-state (t:consumer (free:run-state))))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (paras #:align 'center #:stage (s:stage)
           (with-size 45 @elem{Can we get GHC to do this?})
           @elem{Might@tt{mtl} do better?}))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (tl:sequence c:highlights '([get put bind ret] [get put bind ret constraint]))
    (paras #:align 'center #:spacing 75 #:stage (s:stage)
           (parameterize ([current-code-line-sep 5])
             (countdown-mtl:prog #:use-do? #f))
           (with-size 55 @elem{How is this compiled?})))

  (slides ()
    @elem{How are typeclasses compiled?})

  (slides ([s:bullet 0])
    #:with current-para-align 'center
    #:with current-para-fill? #f
    #:with current-para-width 500
    #:timeline (tl:sequence s:bullet 4)
    (paras #:align 'center #:stage (s:bullet)
           (with-size 45 @elem{Non-Solution 1: Type Dispatch})
           (inset p:show 0 20)
           @para[#:spacing '(lines -0.1)]{Immediate problem: requires whole-program compilation.})
    #:where
    (define p:show
      @haskell{
        show x = case typeOf x of
          Bool   -> show_Bool x
          Char   -> show_Char x
          String -> show_String x
          ...}))

  (slides ([s:java 0] [s:haskell 0])
    #:timeline (tl:sequence s:java 3) (tl:sequence s:haskell (in-range 1 4))
    #:with get-current-code-font-size (const 35)
    (vc-append 50 (with-size 50 @elem{Deeper problem: full type erasure.})
               (vl-append 50
                          (pict-when (> (s:java) 0) p:java)
                          (pict-when (> (s:haskell) 0) p:haskell)))
    #:where
    (define p:obj
      (vl-append 21 (hc-append (hc-append 10 @haskell{obj} (points-at))
                               (mem:ptr #:length 40 #:angle (turns -1/4))
                               (mem:label @elem{@it{fields}}))
                 (hc-append (hc-append 10 @haskell{Foo} (points-at))
                            (mem:label @elem{class})
                            (mem:label @elem{@it{methods}}))))
    (define p:haskell: @elem{Haskell:})
    (define p:java
      (htl-append 20 (rc-superimpose (ghost p:haskell:) @elem{Java:})
                  (with-size 35
                    (paras #:align 'center #:spacing 20 #:stage (s:java)
                           @haskell{if (obj @(c:plain @tt{instanceof}) Foo) { ... }}
                           p:obj))))
    (define hdot (disk 4 #:draw-border? #f #:color text-plain-color))
    (define hdots (hc-append 8 hdot hdot hdot))
    (define p:haskell
      (htl-append 20 p:haskell:
                  (with-size 35
                    (paras #:align 'center #:spacing 20 #:stage (s:haskell)
                           @haskell{data Foo = MkFoo Int String}
                           @haskell{let val = MkFoo 42 "hello"}
                           (hc-append (hc-append 10 @haskell{val} (points-at))
                                      (mem:label @haskell{42})
                                      (mem:ptr) (blank 31)
                                      (mem:label @haskell{'h'})
                                      (mem:ptr) (blank 41) hdots))))))

  (define (p:exclaim)
    @haskell{
 exclaim :: Show a => a -> String
 exclaim x = show x ++ "!"})

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 5)
    (paras #:align 'center #:spacing 75 #:stage (s:stage)
           (with-size 45 @elem{Non-Solution 2: Monomorphization})
           (p:exclaim)
           (ol #:stage (- (s:stage) 2)
               @elem{Generate@it{no code} for@haskell{exclaim}.}
               @elem{Record@haskell{exclaim}’s definition in the interface file.})))

  (slides ([s:info hide] [s:results hide] [s:bullet 0])
    #:timeline (tl:show s:info s:results) (tl:sequence s:bullet (in-range 1 6))
    #:with current-para-fill? #f
    #:with current-para-width 600
    ((s:info)
     (hc-append 50 (scale files 1.5)
                (paras #:stage (s:bullet)
                       @elem{@tt{.hi} — “Haskell interface”}
                       @item{type/class/instance declarations}
                       @item{types of exported bindings}
                       @item{source code of small bindings}
                       @item{for monomorphization: source code of@it{all} overloaded bindings})))
    #:where
    (define (ext p) (cc-superimpose (ghost @tt{.hs}) p))
    (define p:arrow (scale step-arrow:down 3/4))
    (define files (vc-append 10 (p:file @tt{.hs})
                             ((s:results) (hc-append 25 (rotate p:arrow (turns -0.1))
                                                     (rotate p:arrow (turns 0.1))))
                             ((s:results) (hc-append 45 (p:file (ext @tt{.o})) (p:file @tt{.hi}))))))

  (define (p:exclaim/spec ty)
    @haskell{
      @~a{exclaim_@ty} :: @ty -> String
      @~a{exclaim_@ty} x = @~a{show_@ty} x ++ "!"})
  
  (slides ([s:stage 1] [s:show-defn? #f])
    #:timeline (next) (tl:flags s:show-defn?) (tl:sequence s:stage (in-range 2 5))
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           (~> @haskell{@p:call True}
               (pin-balloon p:call cb-find (scale (p:exclaim) 0.75)
                            #:spike-position 0.45
                            #:show? (s:show-defn?))
               (inset 0 0 0 150))
           (p:exclaim/spec "Bool")
           (hc-append 25 p:expr step-arrow:right @haskell{exclaim_Bool True})
           (with-size 50 @elem{Overloading has no runtime cost!}))
    #:where
    (define p:call @haskell{exclaim})
    (define p:expr @haskell{@p:call True}))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage '[1 2 4])
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           (with-size 50 @elem{…but it can create a lot of bloat.})
           (hc-append 75 @haskell{exclaim True} @haskell{exclaim 42} @haskell{exclaim "hello"})
           step-arrow:down
           (para #:spacing 30
                 (p:exclaim/spec "Bool") (p:exclaim/spec "Int") (p:exclaim/spec "String"))))

  (slides ([s:table hide])
    #:timeline (next) (tl:show s:table)
    (vc-append 40 p:big
               ((s:table) step-arrow:down)
               (~> (table 3 p:specs lc-superimpose cc-superimpose 45 15)
                   (scale-to-fit 1000 +inf.0)
                   ((s:table))))
    #:where
    (define p:big
      @haskell{
        reallyBig :: (Foo @t:var{a}, Bar @t:var{b}, Baz @t:var{c}) => ...
        reallyBig = @elem{<@it{really big RHS}>}})

    (define p:String (ghost @haskell{String}))
    (define (pad-tyapp s)
      (haskell "@" (ltl-superimpose p:String (haskell s))))
    (define p:specs
      (for*/list ([a (in-list '("Bool" "Int" "String"))]
                  [b (in-list '("Bool" "Int" "String"))]
                  [c (in-list '("Bool" "Int" "String"))])
        @haskell{reallyBig @pad-tyapp[a] @pad-tyapp[b] @pad-tyapp[c]})))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 5)
    #:with current-para-fill? #f
    #:with current-item-indent 0
    (paras #:align 'center #:spacing 50
           (with-size 50 @elem{Monomorphization})
           (paras #:stage (s:stage)
                  @item{Can be good for runtime performance.}
                  @item{Can be very bad for code size & compile times.}
                  @item{C++/Rust programmers have to worry about this!}
                  @item{Haskell programmers generally do not.})))

  (define (p:exclaim/ho)
    @haskell{
      exclaim :: @t:new-ty{(a -> String)} -> a -> String
      exclaim @t:new-arg{show_a} x = @t:new-arg{show_a} x ++ "!"})

  (slides ([s:stage 0] [s:call/Bool hide] [s:call/Int hide])
    #:timeline (c:highlights '[new-ty new-arg])
    (tl:sequence s:stage '[0 1 3 4]) (tl:show s:call/Bool)
    (tl:sequence s:stage '[5]) (tl:show s:call/Int)
    (paras #:align 'center #:spacing '(lines 0.5) #:stage (s:stage)
           (p:exclaim)
           step-arrow:down (p:exclaim/ho)
           (~> (p:call (s:call/Bool) "True" "Bool")
               (inset 0 30 0 0))
           (p:call (s:call/Int) "42" "Int"))
    #:with current-code-line-sep 4
    #:where
    (define (p:call show/hide arg ty)
      (hc-append 30 @haskell{exclaim @arg}
                 (show/hide step-arrow:right)
                 (show/hide @haskell{exclaim @t:new-arg{@~a{show_@ty}} @arg}))))

  (slides ([s:stage 1])
    #:timeline (c:highlights '[new]) (tl:sequence s:stage (in-range 1 4))
    (paras #:align 'center #:spacing 50 #:stage (s:stage)
           p:class p:dict p:call)
    #:where
    (define p:class
      @haskell{
        class Show a where
          show      :: a -> String
          showsPrec :: Int -> a -> ShowS
          showList  :: [a] -> ShowS})
    (define p:dict
      @haskell{
        data Show a = ShowDict
          { show      :: a -> String
          , showsPrec :: Int -> a -> ShowS
          , showList  :: [a] -> ShowS }})
    (define p:call
      (parameterize ([current-code-line-sep 4])
        @haskell{
          exclaim :: @t:new{Show a ->} a -> String
          exclaim @t:new{dict} x = @t:new{show dict} x ++ "!"})))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 4)
    #:with current-para-fill? #f
    #:with current-item-indent 0
    (~> (paras #:align 'center #:spacing 40
               (with-size 50 @elem{This is@it{dictionary passing}.})
               (paras #:stage (s:stage)
                      @item{Elegantly simple.}
                      @item{Cheap to compile.}
                      @item{Does it have a runtime cost?}))
        (inset 50)))

  (define (countdown-mtl/dicts)
    (define stateDict @t:state-dict{stateDict})
    (define monadDict @t:monad-dict{monadDict})
    @haskell{
      program :: @t:dict-ty{MonadState Int m ->} m Int
      program @t:state-dict-pat{stateDict@"@"(MonadStateDict @t:monad-dict-pat{monadDict} _ _)} =
        @t:op{@t:was-infix{(>>=)}} @monadDict
              (@t:op{get} @stateDict)
              (\n -> if n <= 0
                 then @t:op{return} @monadDict n
                 else @t:op{@t:was-infix{(>>)}} @monadDict
                           (@t:op{put} @stateDict (n - 1))
                           (program @stateDict)})
  
  (slides ([s:stage 1] [s:question #f])
    #:timeline (tl:sequence s:stage (in-range 1 4))
    (~>> '([dict-ty state-dict-pat] [monad-dict-pat] [monad-dict] [monad-dict was-infix] [stateDict] [])
         (tl:sequence c:highlights)
         tl:last!)
    (tl:sequence s:question (in-range 1 3))
    (tl:sequence c:highlights '([op] [state-dict-pat]))
    (paras #:align 'center #:spacing 35 #:stage (s:stage)
           (pict-if #:combine cb-superimpose
                    (s:question)
                    (paras #:align 'center #:spacing 10 #:stage (s:question)
                           (with-size 80 @elem{Is this performant?})
                           (with-size 60 @elem{Nope: we can’t inline anything.}))
                    (countdown-mtl:prog #:use-do? #f))
           (pict-unless (s:question) step-arrow:down)
           (countdown-mtl/dicts)))

  (slides ([s:stage 1] [s:defn? #f])
    (~> (paras #:align 'center #:spacing 20 #:stage (s:stage)
               (~> (with-size 50 @elem{Most calls are@it{known} calls.})
                   (inset 0 0 0 30))
               @haskell{@p:fst ("hello", "world")}
               (vc-append 20 step-arrow:down @haskell{@t:lam{(\(x, _) -> x)} ("hello", "world")})
               (vc-append 20 step-arrow:down @haskell{"hello"}))
        (when~> (s:defn?) (pin-balloon p:fst (adjust-find cbl-find 0 10) defn:fst))
        panorama)

    #:timeline (next)
    (s:stage 2) (next) (s:defn? #t) (next!)
    (s:stage 3) (s:defn? #f) (c:highlights '[lam]) (next)
    (s:stage 4) (c:highlights '[]) (next)

    #:where
    (define p:fst (highlight-if (s:defn?) @haskell{fst}))
    (define defn:fst
      @haskell{
        fst :: (a, b) -> a
        fst (x, _) = x}))

  (slides ([s:stage 1] [s:defn? #f])
    #:timeline
    (next) (s:stage 2) (next)
    (c:highlights '[f/use]) (next) (s:defn? #t) (next) (c:highlights '[f/use f/bind])
    (next) (s:stage 3) (next)

    (paras #:align 'center #:spacing 40 #:stage (s:stage)
           (with-size 50 @elem{Higher-order functions make@it{unknown} calls.})
           (~> defn:map
               (pin-balloon #:show? (s:defn?) f/use (adjust-find cbl-find 0 10) @elem{???})
               panorama)
           (with-size 45 @elem{Unknown calls are@it{hard stops} for the optimizer.}))

    #:where
    (define f/use @haskell{@t:f/use{f}})
    (define defn:map
      @haskell{
        foldr :: (a -> b -> b) -> b -> [a] -> b
        foldr _ v []     = v
        foldr @t:f/bind{f} v (x:xs) = @f/use x (foldr f v xs)}))

  (slides ([s:stage 0] [s:cost hide])
    #:with current-para-width 550
    #:with current-item-indent 0
    #:timeline (tl:sequence s:stage 11) (tl:show s:cost)
    (paras #:align 'center #:spacing '(lines 1)
           (with-size 45
             (htl-append col-sep
                         @para[#:align 'center]{@titlet{Known Call}}
                         @para[#:align 'center]{@titlet{Unknown Call}}))
           (entries @item{Compiled to direct jump.}          @item{Compiled to indirect jump.}
                    @item{Exposes strictness info.}          @item{Assumed lazy in all args.}
                    @item{Args can be unboxed.}              @item{Args must have declared types.}
                    @item{Can have rewrite@haskell{RULES}.}  @item{Opaque to@haskell{RULES}.}
                    @item{Inlined if small.}                 @item{Never inlined.})
           (blank)
           ((s:cost) (with-size 55 @elem{This is the cost of AOT compilation!})))
    #:where
    (define col-sep 0)
    (define (entries . elems)
      (~> (for/list ([row (in-slice 2 elems)]
                     [i (in-naturals)])
            (match-define (list one two) row)
            (htl-append col-sep
                        (show one (>= (s:stage) (+ (* i 2) 1)))
                        (show two (>= (s:stage) (+ (* i 2) 2)))))
          (apply paras #:spacing '(lines 0.5) #:stage (s:stage) _))))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (paras #:align 'center #:spacing 10 #:stage (s:stage)
           @elem{Typeclass overloading creates unknown calls.}
           (with-size 50 @elem{Overloading is@bt{not} free!})))

  (slides ([s:stage 1])
    #:timeline
    (tl:sequence s:stage (in-range 1 3))
    (tl:sequence c:highlights '([unknown] [unknown constraint]))
    (tl:sequence s:stage (in-range 3 4))

    (paras #:align 'center #:spacing 40 #:stage (s:stage)
           (with-size 50 @elem{Unknown calls are not a death sentence.})
           p:defn
           (with-size 50 @elem{List fusion can still happen!}))
    #:where
    (define p:defn
      @haskell{
        sumIndicies :: @t:constraint{Eq a} => a -> [a] -> [Int]
        sumIndicies v xs = zip [1..] xs
                         & filter ((@t:unknown{@haskell{=}=} v) . snd)
                         & map fst
                         & sum}))

  (slides ([s:stage 1] [s:generalize? #f])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (tl:last! (tl:sequence c:highlights '([producer] [producer consumer] [bind] [changed])))
    (tl:flags s:generalize?) (tl:sequence c:highlights '([bind]))
    (c:highlights '[]) (tl:sequence s:stage (in-range 3 4))

    (paras #:align 'center #:spacing 50 #:stage (s:stage)
           (with-size 50 @elem{Unknown calls to @haskell{>>=} are a problem.})
           (pict-if (s:generalize?) (p:code #t) (p:code #f))
           (with-size 70 @elem{@haskell{>>=} is@it{glue}.}))
    #:where
    (define (p:code generalize?)
      (define p:throw (t:changed (if generalize? @haskell{throwError} @haskell{Left})))
      (define p:return (t:changed (if generalize? @haskell{return} @haskell{Right})))
      (define p:constr (if generalize? @haskell{@t:changed{MonadError String m =>} } (blank)))
      (define p:monad (t:changed (if generalize? @haskell{m} @haskell{Either String})))
      @haskell{
        foo :: @|p:constr|k -> Map k Int -> @p:monad Int
        foo key vals = do
          nums @t:bind{<-} case Map.lookup key vals of
                    Nothing  -> @p:throw "not found"
                    Just val -> @p:return @t:producer{[1..val]}
          @p:return $ @t:consumer{sum} nums}))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (paras #:align 'center #:spacing 20 #:stage (s:stage)
           @elem{Conclusion: not surprising at all that@tt{mtl} has a cost!}
           @elem{But why is it@it{sometimes} fast?}))

  (slides () (inset @titlet{Specialization} 20))

  (define (specialization-criteria #:highlight-first? [highlight-first? #f]
                                   #:highlight-second? [highlight-second? #f]
                                   #:other-opacity [other-opacity 1])
    (list @item{@(highlight-if highlight-first? @elem{It is defined in the current module.})}
          (cellophane @item{@(highlight-if highlight-second? @elem{It was declared with an@haskell{INLINABLE} pragma.})} other-opacity)
          (cellophane @item{It is a class method@it{and} its unfolding (source code) is in the interface file.} other-opacity)))

  (slides ([s:stage 1] [s:rule-opacity 1] [s:rule-highlight? #f])
    #:timeline (tl:last! (tl:sequence s:stage (in-range 1 7)))
    (s:rule-opacity 1/4) (next) (tl:flags s:rule-highlight?)

    #:with current-para-fill? #f
    #:with current-para-width 850
    (paras #:align 'center #:spacing 40 #:stage (s:stage)
           (with-size 50 @elem{GHC monomorphizes in limited circumstances.})
           (ol #:stage (- (s:stage) 1)
               @para{GHC looks for calls to overloaded functions at known, concrete types.}
               (apply paras #:stage (- (s:stage) 2)
                      @para{The function must satisfy@it{at least one} of the following:}
                      (specialization-criteria #:highlight-first? (s:rule-highlight?)
                                               #:other-opacity (s:rule-opacity))))))
  
  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage '[1 3])
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           (countdown-mtl:prog)
           step-arrow:down
           (countdown:state)))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage '[1 3])
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           (countdown-mtl:prog)
           step-arrow:down
           (countdown-mtl/dicts)))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (paras #:align 'center #:spacing 40 #:stage (s:stage)
           @elem{Why bother explaining all of this?}
           (with-size 50 @elem{Because we’re not done.})))

  (slides ([s:stage 1] [s:highlight? #f])
    #:timeline (tl:sequence s:stage (in-range 1 3)) (tl:flags s:highlight?)
    #:with current-para-fill? #f
    #:with current-para-width 850
    #:with current-item-indent 0
    (paras #:align 'center #:spacing 40 #:stage (s:stage)
           (with-size 50 @elem{Can we avoid the performance regression?})
           (apply paras (specialization-criteria #:highlight-second? (s:highlight?)))))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 7)
    #:with current-para-fill? #f
    #:with current-para-width 850
    (paras #:align 'center #:spacing 40 #:stage (add1 (s:stage))
           (with-size 50 @elem{This approach has many problems.})
           (ol #:stage (s:stage)
               @elem{It would be annoying.}
               (paras #:stage (sub1 (s:stage))
                      @elem{It requires@it{whole-program} specialization!}
                      @item{Probably recompiles everything in@tt{Main.hs}.}
                      @item{Compilation may not terminate.}
                      @item{This cost is incurred for@it{each} specialization.}
                      @item{Can be defeated by existential quantification.}))))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (paras #:align 'center #:stage (s:stage)
           (with-size 50 @elem{Let’s take a step back.})
           @elem{Why is specialization necessary here?}))

  (slides ([s:stage 1] [s:interpretation? #f] [s:proposition 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    (c:highlights '[get put]) (next) (s:interpretation? #t) (next!)
    (c:highlights '[]) (s:interpretation? #f) (s:proposition 2) (next)
    (c:highlights '[bind]) (next)
    (~> (paras #:align 'center #:spacing 50 #:stage (s:stage)
               (with-size 50
                 (pict-case (s:proposition)
                   #:combine cb-superimpose
                   [(1) (paras #:align 'center #:spacing 0
                               @elem{Proposition 1: effect systems are}
                               @elem{@it{fundamentally} about dynamic dispatch.})]
                   [(2) (paras #:align 'center #:spacing 0
                               @elem{Proposition 2: effect dispatch can be made}
                               @elem{perfectly affordable. The real problem is@haskell{>>=}.})]))
               p:prog)
        (when~> (s:interpretation?)
                (pin-over p:prog (adjust-find rt-find -60 0) (pip p:label rt-find))
                (pin-arrow-line 15 _ p:label (adjust-find lc-find -15 -15) p:get (adjust-find rc-find 10 0)
                                #:color text-secondary-color #:line-width 3)
                (pin-arrow-line 15 _ p:label (adjust-find lb-find -5 10) p:put (adjust-find ct-find 50 -10)
                                #:color text-secondary-color #:line-width 3)))
    #:where
    (define p:prog (countdown-mtl:prog #:use-do? #f #:signature? #f))
    (define p:label (with-size 30
                      (paras #:spacing -2 @elem{interpretation not} @elem{yet decided!})))
    (define p:get (find-tag p:prog 'get))
    (define p:put (find-tag p:prog 'put)))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 4))
    #:with current-para-fill? #f
    #:with current-para-width 800
    (paras #:align 'center #:spacing 40 #:stage (s:stage)
           (with-size 50 @elem{Passing@haskell{>>=} via dictionary creates problems!})
           (ol @para{@haskell{>>=} gets called a@it{lot}.}
               @para{@haskell{>>=} is glue; it needs to be inlined to expose further optimizations.}
               @para{Unknown calls to @haskell{>>=} balloon closure allocation.})))

  (slides ([s:stage 1] [s:lam? #f] [s:cap? #f] [s:case? #f] [s:stack? #f])
    #:timeline (tl:sequence s:stage '[2 5])
    (c:highlights '[cap stack]) (next)
    (c:highlights '[lam]) (next) (s:lam? #t) (next)
    (c:highlights '[case]) (next) (s:case? #t) (next)
    (c:highlights '[cap]) (next) (s:cap? #t) (next)
    (c:highlights '[stack]) (next) (s:stack? #t) (next)
    (c:highlights '[]) (next)

    (~> (paras #:align 'center #:spacing 25 #:stage (s:stage)
               (~> (hc-append 100 (show label:cap (s:cap?)) (show label:lambda (s:lam?)))
                   (inset 0 0 100 0))
               p:defn step-arrow:down p:either
               (~> label:case (show (s:case?)) (inset 0 0 100 0)))
        (when~> (s:lam?)
                (pin-arrow-line 15 _ #:color text-secondary-color #:line-width 2
                                label:lambda (adjust-find rc-find 10 5) #:start-angle 0
                                p:y/lam (adjust-find ct-find 0 5) #:end-angle (turns -1/4) #:end-pull 1/2))
        (when~> (s:cap?)
                (pin-arrow-line 15 _ #:color text-secondary-color #:line-width 2
                                p:y/cap (adjust-find ct-find 0 -23)
                                p:y/cap (adjust-find ct-find 0 6)))
        (when~> (s:case?)
                (pin-arrow-line 15 _ #:color text-secondary-color #:line-width 2
                                label:case (adjust-find rc-find 10 0) #:start-angle 0
                                p:y/case (adjust-find cb-find 0 5) #:end-angle (turns 1/4) #:end-pull 1/2))
        (when~> (s:stack?)
                (pin-over p:y/stack (adjust-find ct-find 0 -25) (pip label:stack cb-find))
                (pin-arrow-line 15 _ #:color text-secondary-color #:line-width 2
                                label:stack (adjust-find cbl-find 0 10)
                                p:y/stack (adjust-find ct-find 0 6))))
    #:where
    (define p:y/lam (t:lam @haskell{y}))
    (define p:y/case (t:case @haskell{y}))
    (define p:y/cap (t:cap @haskell{y}))
    (define p:y/stack (t:stack @haskell{y}))
    (define p:defn @haskell{f x @p:y/cap = foo x >>= \z -> bar (+ @p:y/lam z)})
    (define p:either @haskell{
      f x @p:y/stack = case foo x of
        Left e  -> Left e
        Right z -> bar (+ @p:y/case z)})
    (define label:lambda (with-size 35 @elem{used under a lambda}))
    (define label:cap (with-size 35 @elem{captured in closure}))
    (define label:case (with-size 35 @elem{used in a@haskell{case} RHS}))
    (define label:stack (with-size 35 @elem{stored on stack})))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    #:with current-para-fill? #f
    #:with current-para-align 'center
    #:with current-para-width 700
    #:with current-line-sep 0
    (paras #:spacing 50 #:stage (s:stage)
           @para{If@haskell{>>=} is an unknown call, the caller must allocate a closure for the continuation.}
           @para{When@haskell{>>=} is passed via dictionary, the program is @it{reified as a tree of lambdas!}}))

  (slides ([s:different hide])
    #:timeline (tl:sequence c:highlights '([] [op] [op bind])) (tl:show s:different)
    (paras #:align 'center #:spacing 50
           (free:countdown)
           ((s:different) (with-size 55 @elem{Without specialization, these@it{aren’t that different!}}))
           (countdown-mtl:prog #:use-do? #f)))

  (slides () (inset @elem{How do we escape?} 25))

  (slides ([s:bullet 0] [s:suffix 0])
    #:timeline (tl:sequence s:bullet 5) (tl:sequence s:suffix (in-range 1 3))
    #:with current-para-fill? #f
    #:title "Escape Plan"
    (paras #:align 'center #:spacing 50
           (ol #:make-num circled #:stage (s:bullet)
               @elem{We need a monad with an inlinable@haskell{>>=}.}
               @elem{@haskell{>>=} itself must not allocate.}
               @elem{It must be able to handle all algebraic effects.}
               @elem{Effect dispatch can be dynamic (but must be fast).})
           (paras #:align 'center #:spacing 0 #:stage (s:suffix)
                  @elem{This is not a small order.}
                  @elem{Requirements@circled[2 #:inline? #t] and@circled[3 #:inline? #t] are especially hard.})))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 6)
    (paras #:align 'center #:spacing 5 #:stage (s:stage)
           (with-size 45 @elem{Monad transformers are out of the question.})
           (~> @elem{(Effect behavior lives in@haskell{>>=}.)}
               (inset 0 0 0 30))
           (with-size 45 @elem{Free-like monads are also disqualified.})
           (~> @elem{(@haskell{>>=} must allocate to construct the tree.)}
               (inset 0 0 0 30))
           (with-size 45 @elem{What’s left?}))))

(section "eff")
(begin
  (slides ([s:stage 0] [s:scope hide])
    #:timeline (tl:sequence s:stage '[0 1]) (tl:show s:scope) (tl:sequence s:stage '[2 3])
    #:title "Delimited Continuations"
    #:with current-para-align 'center
    (paras #:spacing '(lines 1) #:stage (s:stage)
           (paras #:spacing 1
                  @elem{There is a deep, well-known connection between}
                  @elem{delimited continuations and algebraic effects.}
                  (blank 0 10)
                  ((s:scope) (with-size 35 @elem{(Well outside the scope of this talk!)})))
           (paras #:spacing 1
                  @elem{Big idea: equip a monad with a pair of super}
                  @elem{powerful control operators,@haskell{prompt} and@haskell{control}.})
           (paras @elem{All effect handlers can be defined in terms of these operators.}
                  @elem{@haskell{>>=} does not need to know anything about effect behavior.})))

  (slides ([s:stage 1] [s:stage/cps 2])
    #:timeline (tl:sequence s:stage '[1 2]) (tl:sequence s:stage/cps '[3]) (tl:sequence s:stage '[3])
    #:with current-para-align 'center
    (paras #:spacing '(lines 1) #:stage (s:stage)
           (paras #:spacing 1
                  @elem{New problem: the only way to implement}
                  @elem{delimited continuations in Haskell is CPS.})
           (paras #:spacing '(lines 0.5) #:stage (s:stage/cps)
                  @elem{In CPS, continuation is passed via closure:}
                  (with-size 35 @haskell{m >>= f = \k -> m @highlight[@haskell{(\x -> f x k)}]})
                  (paras #:spacing 1
                         @elem{All calls to non-inlined monadic functions must allocate.}
                         @elem{(This isn’t much better than free monads.)}))
           (paras #:spacing 1
                  @elem{Cost is okay for continuation-happy code, but effects that}
                  @elem{don’t need them (very common!) still must pay for them.})))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    #:with current-para-align 'center
    (paras #:spacing 30 #:stage (s:stage)
           (with-size 50 @elem{But this is frustrating!})
           (paras #:spacing 1
                  @elem{There are well-known, efficient implementation}
                  @elem{techniques for delimited continuations!})))

  (define proposal-pict (bitmap proposal.png))

  (slides ([s:patch hide])
    #:timeline (next) (tl:show s:patch)
    #:with current-para-align 'center
    (paras #:spacing 50
           (with-size 50 @elem{I give up.@((s:patch) @elem{Let’s just patch GHC.})})
           (~> (scale-to-fit proposal-pict 900 900)
               (inset 0 0 0 -150)
               ((s:patch)))))

  (slides () (inset @titlet{eff} 75))

  (define eff-pict (bitmap eff.png))
  (slides () (inset eff-pict 0 0 0 -100))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 8)
    #:title "Eff: Key Features"
    #:with current-para-fill? #f
    #:with current-item-indent 0
    (paras #:stage (s:stage)
           @item{API is similar to free-like approaches (actually simpler!).}
           @item{Provides a single monad, named@haskell{Eff}.}
           @item{Basically@haskell{ReaderT} over@haskell{ST} under the hood.}
           (indent #:by (em 2) @item{Reader context holds current handlers.})
           (indent #:by (em 2) @item{Wraps the unsafe primops in a safe API.})
           @item{Effect dispatch takes constant time (not amortized).}
           @item{Faster for most use cases than unspecialized@tt{mtl}.}))

  (slides ([s:chart hide] [s:arrow hide] [s:text hide])
    #:timeline (next) (tl:show s:chart s:arrow s:text)
    #:title "Eff: The Numbers"
    #:with current-slide-margin 0
    (paras #:align 'center #:spacing 30
           (~> (countdown-chart/both #:eff? #t #:polysemy? #f #:label-bars? #t #:x-label? #f)
               (pin-over 310 110
                         ((s:arrow) (hc-append 15 (~> (c:plain (arrow-line #:line-length 50))
                                                      (rotate (turns 1/2))
                                                      (inset 0 10 0 0))
                                               (with-size 30 @elem{no change!}))))
               (inset 0 0 100 0)
               ((s:chart)))
           ((s:text) (with-size 35 @elem{@tt{eff}’s performance does@it{not} depend on compiler optimizations!}))))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    #:title "Eff: The Numbers"
    #:with current-slide-margin 0
    #:with plot-height 200
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           (chart (countdown/multi-module #:eff? #t #:polysemy? #f #:mtl? #f #:fused-effects? #f)
                  #:color 2 #:x-label? #f #:label-bars? #t)
           @elem{Should we be worried?}))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 3))
    #:title "Countdown: The Rest of the Story"
    (paras #:align 'center #:spacing 50 #:stage (s:stage)
           (countdown-baseline:prog)
           @elem{Why@it{so} much faster?}))

  (define countdown/unboxed @haskell{
    $wprogram :: Int# -> (# Int#, Int# #)
    $wprogram n = if n <=# 0# then (# n, n #)
                              else $wprogram (n -# 1#) })

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage '[1 3])
    (paras #:align 'center #:spacing 50 #:stage (s:stage)
           (countdown-baseline:prog)
           step-arrow:down
           countdown/unboxed))

  (slides ([s:stage 1] [s:count hide])
    #:timeline (tl:sequence s:stage '[1 3]) (tl:show s:count)
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           countdown/unboxed
           step-arrow:down
           (~> (hc-append -10 asm ((s:count) @elem{8 instructions!}))
               (refocus asm)))
    #:where
    (define asm @haskell{
 _c4ap: addq $16,%r12
	cmpq 856(%r13),%r12
	ja _c4aB
 _c4aA: testq %r14,%r14
	jle _c4aw
 _c4av: addq $-16,%r12
	decq %r14
	jmp _c4ap}))

  (slides ([s:stage 1])
    #:timeline (tl:sequence s:stage '[1 3])
    (paras #:align 'center #:spacing 50 #:stage (s:stage)
           (countdown-baseline:prog)
           step-arrow:down
           (countdown-baseline:prog #:int-type (highlight @haskell{Integer}))))

  (slides ([s:stage 1])
    #:title "Countdown: No Unboxing"
    #:timeline (tl:sequence s:stage (in-range 1 3))
    #:with plot-height 200
    (paras #:align 'center #:spacing 30 #:stage (s:stage)
           (chart (countdown/big-ints #:eff? #t #:polysemy? #f)
                  #:color 2 #:x-label? #f #:label-bars? #t)
           @elem{Not quite so bad, after all!}))

  #;(slides ([s:stage 1])
    #:timeline (tl:sequence s:stage (in-range 1 4))
    #:with current-para-align 'center
    (paras #:spacing 30 #:stage (s:stage)
           @elem{Should the benchmark use@haskell{Integer}?}
           @elem{No,@haskell{Int} is useful to measure@it{worst-case} overhead.}
           (inset (with-size 60 @elem{Understand your benchmarks!})
                  0 20 0 0)))

  (slides () (inset @elem{Phew.} 20))

  (define (p:recap #:stage [stage #f])
    (paras #:stage stage
           @item{Countdown benchmarks@haskell{>>=} and effect dispatch.}
           @item{@tt{mtl} and@tt{fused-effects} rely on compiler optimizations.}
           @item{Those optimizations are very often not viable.}
           @item{@tt{eff} closes the gap by being inherently fast:}
           (indent #:by (em 2) @item{It exposes more local transformations to the optimizer.})
           (indent #:by (em 2) @item{Delimited continuation primops avoid CPSing.})
           (indent #:by (em 2) @item{Result:@tt{eff} handily wins the benchmark shootout.})
           @item{Numbers are not enough. It is our responsibility to ask why.}))

  (define (p:closing-thoughts #:stage [stage #f])
    (paras #:stage stage
           @item{The@tt{eff} story is not over.}
           (indent #:by (em 2) @item{Need to plumb in support for@tt{IO} exceptions.})
           (indent #:by (em 2) @item{The GHC proposal needs to be accepted.})
           @item{The design of@tt{eff} itself could be its own talk!}
           @item{@tt{mtl}’s performance is misunderstood and overhyped!}
           @item{Effect systems are too foundational to ignore perf.}
           @item{We really do need good real-world benchmarks.}
           @item{Someday: effect specialization?}))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 9)
    #:title "Recap"
    #:with current-item-indent 0
    #:with current-para-fill? #f
    (p:recap #:stage (s:bullet)))

  (slides ([s:bullet 0])
    #:timeline (tl:sequence s:bullet 9)
    #:title "Closing Thoughts"
    #:with current-item-indent 0
    #:with current-para-fill? #f
    (p:closing-thoughts #:stage (s:bullet)))

  (slides ()
    #:with current-slide-margin 0
    #:with current-item-indent 0
    #:with current-para-fill? #f
    (paras #:align 'center
           (with-size 60 @titlet{The End})
           (blank 0 20)
           (~> (ht-append 50 (p:recap) (p:closing-thoughts))
               (scale 0.65))
           (blank 0 20)
           (links @tt{eff} @elem{https://github.com/hasura/eff}
                  @elem{benchmarks} @elem{https://github.com/ocharles/effect-zoo}
                  @elem{proposal} @elem{https://github.com/ghc-proposals/ghc-proposals/pull/313}
                  @elem{Hasura} @elem{https://hasura.io}
                  @elem{me} @elem{https://lexi-lambda.github.io}))
    #:where
    (define (links . elems)
      (define pairs (for/list ([ps (in-slice 2 elems)]) ps))
      (define longest-label (ghost (argmax pict-width (map first pairs))))
      (define longest-link (ghost (argmax pict-width (map second pairs))))
      (~>> (for/list ([pair (in-list pairs)])
             (match-define (list label link) pair)
             @elem{@(rc-superimpose label longest-label):@(lc-superimpose link longest-link)})
           (apply vc-append)))))
