#lang at-exp slideshow

(require (for-syntax racket/match
                     syntax/parse/experimental/template)
         pict/conditional
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

(define-runtime-path hoogle-local-search-no-results.png "assets/hoogle-local-search-no-results.png")
(define-runtime-path hoogle-search-bar.png "assets/hoogle-search-bar.png")
(define-runtime-path macro-stepper.png "assets/macro-stepper.png")
(define-runtime-path racket-logo.svg '(lib "racket-logo.svg" "icons"))

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

(define (typeset-interaction exprs)
  (define eval (call-with-trusted-sandbox-configuration
                (λ ()
                  (parameterize ([sandbox-output 'string]
                                 [sandbox-error-output 'string]
                                 [sandbox-propagate-breaks #f])
                    (define eval (make-evaluator '(begin (require racket))))
                    (call-in-sandbox-context
                     eval
                     (λ ()
                       (current-print (dynamic-require 'racket/pretty 'pretty-print-handler))))
                    eval))))

  (define (typeset-output str color)
    (map (λ~> tt (colorize color))
         (string-split (string-trim str "\n" #:repeat? #t) "\n" #:trim? #f)))

  (for/list ([expr (in-list exprs)])
    (define result (eval expr))
    (define output (get-output eval))
    (define error-output (get-output eval))
    (call-in-sandbox-context
     eval
     (λ () ((current-print) result)))
    (define result-output (get-output eval))
    (~> (list (htl-append (tt "> ") (typeset-code (datum->syntax #f expr)))
              (typeset-output output interaction-output-color)
              (typeset-output error-output "red")
              (typeset-output result-output interaction-result-color))
        flatten
        (apply vl-append (current-code-line-sep) _))))

;; ---------------------------------------------------------------------------------------------------

(section "Title")

(slides ()
  (~> (vc-append title
                 (~> (filled-rectangle (+ (pict-width title) 40) 1 #:draw-border? #f)
                     (inset 0 -5 0 15))
                 (with-size 30
                   (hflex (+ (pict-width title) 20)
                          (t "Alexis King") (spring 1) (t "Haskell Implementors’ Workshop 2022"))))
      (colorize text-secondary-color))
  #:where
  (define title (~> (with-size 150
                      (with-font "Concourse C2"
                        @t{Across the Pond}))
                    (colorize text-plain-color))))

#;(slides () (inset @titlet{Tooling} 50))

(define (around base elems
                #:stage [stage #f]
                #:distance distance
                #:turn-offset [turn-offset 0])
  (define pin (blank))
  (define count (length elems))
  (define stage* (or stage count))
  (pict-when (>= stage* 0)
    (~> (for/fold ([p (pip base cc-find)])
                  ([elem (in-list elems)]
                   [i (in-naturals)])
          (define angle (turns (+ -1/4 turn-offset (/ i count))))
          (define x (* distance (cos angle)))
          (define y (* distance (sin angle)))
          (pict-if (> stage* i)
                   (~> (pin-over/align p x y 'c 'c elem)
                       (pin-line base cc-find elem cc-find
                                 #:under? #t
                                 #:line-width 2
                                 #:color shape-border-color))
                   p))
        (refocus* (cons base elems)))))

(define step-arrow:right (~> (arrowhead 25 0) (colorize text-secondary-color)))
(define step-arrow:down (rotate step-arrow:right (turns -1/4)))

#;(begin
  (section "Haskell tooling")

  (define (ghc-ecosystem #:stage [stage #f])
    (around #:stage stage
            #:distance 225
            #:turn-offset 1/12
            (encircle (scale @elem{GHC} 1.75)
                      #:padding 20
                      #:border-width 3)
            (list (encircle @elem{Cabal} #:highlight? (memq 'cabal (c:highlights)))
                  (encircle @elem{Haddock})
                  (encircle @elem{Hoogle} #:highlight? (memq 'hoogle (c:highlights)))
                  (encircle @elem{HLS})
                  (encircle (scale (vc-append @elem{profiling} @elem{tools}) 0.9)))))
  
  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 7) (c:highlights '[hoogle]) (next)
    (ghc-ecosystem #:stage (sub1 (s:stage))))

  (slides ([s:highlight? #f] [s:what-if? #f])
    #:timeline (next) (tl:flags s:highlight? s:what-if?)
    (vc-append (~> (bitmap hoogle-search-bar.png)
                   (pin-over 2900 80 (cellophane (highlight-if (s:highlight?) (blank 1230 190)) 0.5))
                   (scale-to-fit 800 800))
               (blank 50)
               (pict-when (s:what-if?)
                 @elem{What if my local packages are different?})))

  (define (hoogle-generate-help #:s:cmd? [s:cmd? #t] #:s:output? [s:output? #t])
    (parameterize ([current-highlight-color terminal-highlight-color])
      (wrap-terminal-frame
       (vl-append (htl-append @tt{$ } (pict-when s:cmd? @tt{hoogle generate --help}))
                  (pict-when s:output?
                    (vl-append
                     (tt "")
                     (htl-append @tt{hoogle generate [OPTIONS] } (t:package @tt{[PACKAGE]}))
                     @tt{  Generate Hoogle databases}
                     (tt "")
                     @tt{Flags:}
                     @tt{     --download         Download all files from the web}
                     @tt{     --database=FILE    Name of database to use (use .hoo extension)}
                     @tt{  -i --insecure         Allow insecure HTTPS connections}
                     @tt{  -n --count=INT        Maximum number of packages to index}
                     (t:local
                      (htl-append
                       @tt{  -l --local}
                       (t:local-item @tt{[=ITEM]})
                       @tt{     Index local packages and link to local haddock}))
                     @tt{  -h --haddock=ITEM     Use local haddocks}
                     @tt{     --debug            Generate debug information}
                     (tt "...")))))))
  
  (slides ([s:cmd? #f] [s:output? #f])
    #:timeline (next) (tl:flags s:cmd? s:output?) (c:highlights '[local]) (next)
    (hoogle-generate-help #:s:cmd? (s:cmd?) #:s:output? (s:output?)))

  (slides ([s:why? #f])
    #:timeline (next) (tl:flags s:why?)
    (vc-append (scale-to-fit (bitmap hoogle-local-search-no-results.png) 800 800)
               (blank 50)
               (pict-when (s:why?)
                 @elem{Why didn’t it work?})))

  (slides ()
    #:timeline (next) (tl:highlight+ 'hoogle 'cabal)
    (ghc-ecosystem))

  (slides ()
    #:timeline (next) (tl:highlight+ 'local-item)
    (hoogle-generate-help))

  (slides ([s:output? #f])
    #:timeline (next) (tl:flags s:output?)
    (vc-append
     20
     (wrap-terminal-frame (tt "$ cabal build --write-ghc-environment-files=always"))
     (pict-when (s:output?) step-arrow:down)
     (pict-when (s:output?)
       (wrap-terminal-frame
        (~> (vl-append (tt "$ cat .ghc.environment.x86_64-linux-9.0.1")
                       (apply vl-append (map tt (string-split ghc-environment-file-contents "\n"))))
            (scale 0.7)))))
    #:where
    (define ghc-environment-file-contents
      @~a{
clear-package-db
global-package-db
package-db /home/alexis/.cabal/store/ghc-9.0.1/package.db
package-db dist-newstyle/packagedb/ghc-9.0.1
package-id dynamical-0.0.0.0-inplace
package-id base-4.15.0.0
package-id ghc-bignum-1.0
package-id ghc-prim-0.7.0
package-id rts
package-id linear-1.21.10-8fed9254c4c0df2b00a1ef48e09ddeae1c36b7ae30c1ed225e2592d9b0e3053f
package-id adjunctions-4.4.2-8bab27889bcc67b0c0713be504159796f1268c5b41e575c1640c7a7912cf09cc
package-id array-0.5.4.0
package-id comonad-5.0.8-4e0b2dcd392f8c626be3210aa366eef2b6b554cd5a8061c4db18e19e74b05210
package-id containers-0.6.4.1
package-id deepseq-1.4.5.0
package-id distributive-0.6.2.1-a5de5d1d4aed8fa2f6ff9741c8b6203bb8ee144be80e50270b646fd256e97c5c
package-id base-orphans-0.8.7-d6bcc5ddb2274dd33496d8d4f64125cd34cdcfc42480e56480582b2d4b168d74
package-id tagged-0.8.6.1-f280ae38a699b5c2ec482ff18146bacc531c4624663244196dec426227de4dae
package-id template-haskell-2.17.0.0
package-id ghc-boot-th-9.0.1
...}))

  (slides ()
    (code (define-values [env-file-path hoogle-db-path]
            (command-line #:args (environment-file hoogle-db) (values environment-file hoogle-db)))
          (code:line) (code:line)
          (define-values [pkg-db-args pkg-ids]
            (call-with-input-file* env-file-path #:mode 'text
              (λ (env-file-in)
                (for/fold ([pkg-db-args '()] [pkg-ids '()])
                          ([line (in-lines env-file-in)])
                  (match line
                    [(or "" "clear-package-db" (regexp #px"^--"))
                     (values pkg-db-args pkg-ids)]
                    ["global-package-db"
                     (values (cons "--global" pkg-db-args) pkg-ids)]
                    [(regexp #px"^package-db (.+)$" (list _ db))
                     (values (append (list "--package-db" db) pkg-db-args) pkg-ids)]
                    [(regexp #px"^package-id (.+)$" (list _ id))
                     (values pkg-db-args (cons id pkg-ids))])))))
          (code:line) (code:line)
          (match-define (list ghc-pkg-in #f _ #f _)
            (apply process*/ports #f (open-input-string "") (current-error-port)
                   (find-executable-path "ghc-pkg") "dump" pkg-db-args))
          (code:line) (code:line)
          (define doc-dirs
            (let loop ([doc-dirs '()])
              (match (regexp-match #px"(?m:^id:\\s*(\\S+))" ghc-pkg-in)
                [#f doc-dirs]
                [(list _ (app bytes->string/utf-8 pkg-id))
                 (if (member pkg-id pkg-ids)
                     (match (regexp-match #px"(?m:^haddock-html:\\s*(\\S.*)$|^---$)" ghc-pkg-in)
                       [(or #f (list #"---" #f))
                        (fprintf (current-error-port) "warning: no haddock-html entry for package ‘~a’\n" pkg-id)
                        (loop doc-dirs)]
                       [(list _ doc-dir)
                        (cond
                          [(directory-exists? (bytes->path doc-dir))
                           (loop (cons doc-dir doc-dirs))]
                          [else
                           (fprintf (current-error-port) "warning: haddock-html directory for package ‘~a’ does not exist\n" pkg-id)
                           (loop doc-dirs)])])
                     (loop doc-dirs))])))
          (code:line) (code:line)
          (close-input-port ghc-pkg-in)
          (void (apply system* (find-executable-path "hoogle") "generate" "--database" hoogle-db-path
                       (for*/list ([doc-dir (in-list doc-dirs)])
                         (bytes-append #"--local=" doc-dir))))))

  (slides ()
    (inset @elem{This sucks.} 80))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 4)
    #:with current-para-spacing 40
    (paras #:stage (s:stage)
           #:align 'center
           @elem{Why are things like this?}
           @elem{Who manages to do better?}
           @elem{What are they doing that we aren’t?})))

(begin
  (section "Racket tooling")

  (slides ()
    (vc-append
     (cc-superimpose border-disk
                     (scale-to-fit (inset (rsvg-isolate (svg-file->pict racket-logo.svg)) 7) border-disk))
     @titlet{Racket})
    #:where
    (define border-disk
      (disk 100 #:color shape-border-color #:draw-border? #f)))

  (slides ()
    (inset @elem{“They just care about it more.”} 100))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 4)
    #:with current-para-width 800
    #:with current-line-sep 0
    #:with current-para-spacing '(lines 1)
    (vc-append
     (scale @titlet{Thesis of This Talk} 1.4)
     (blank 30)
     (ol #:stage (s:stage)
         @para{Racket has produced exceptionally powerful and reliable tooling for such a small community.}
         @para{The quality of this tooling derives in large part from Racket’s @it{technical architecture}.}
         @para{Several of these architectural choices could be adopted by the Haskell ecosystem to good effect.})))

  (slides ([s:flags? #f] [s:input-file? #f] [s:outputs? #f] [s:summary? #f])
    #:timeline (next) (tl:flags s:flags? s:input-file? s:outputs? s:summary?)
    #:title "The GHC Compilation Model"
    #:with current-para-align 'center
    (vc-append
     (~> (vc-append
          10
          (~> (hc-append
               10
               (~> (wrap-terminal-frame (apply vc-append (map tt '("-c -O1" "-dynamic-too" "-ddump-simpl"))))
                   (scale 0.3)
                   (pict-when (s:flags?) _))
               (pict-when (s:input-file?) (t:plus @elem[#:color text-secondary-color]{+}))
               (pict-when (s:input-file?) (p:file @tt{.hs})))
              (recenter/tag 'plus))
          (pict-when (s:outputs?) (scale step-arrow:down 3/4))
          (pict-when (s:outputs?)
            (hc-append (p:file (ext @tt{.o}))
                       (inset (p:file @tt{.hi}) -5 10 0 0))))
         (scale 2))
     (blank 50)
     (pict-when (s:summary?)
       @para{GHC is @it{primarily} a traditional, batch-mode compiler.}))
    #:where
    (define (ext p) (cc-superimpose (ghost @tt{.hs}) p))
    (define p:arrow (scale step-arrow:down 3/4)))

  (slides ()
    #:title "The Racket Compilation Model"
    (vc-append
     (cc-superimpose (box 600 70) @elem{The Racket Machine})))

  (start-at-recent-slide)

  (define (racket-ecosystem #:stage [stage #f])
    (around #:stage stage
            #:distance 225
            #:turn-offset 1/12
            (encircle (scale @elem{Racket} 1.6)
                      #:padding 20
                      #:border-width 3)
            (list (encircle (scale @elem{raco} 1.1))
                  (encircle @elem{Scribble})
                  (encircle @elem{DrRacket})
                  (encircle (scale (vc-append @elem{debugging} @elem{tools}) 0.8))
                  (encircle (scale (vc-append @elem{profiling} @elem{tools}) 0.9)))))

  (slides ([s:stage 0])
    #:timeline (tl:sequence s:stage 6)
    (racket-ecosystem #:stage (s:stage)))

  (slides ([s:stage 0])
    (apply vl-append 10
           (typeset-interaction
            '((require pkg/lib)
              (pkg-directory "slideshow")))))

  (parameterize ([pretty-print-columns 50])
    (define steps (typeset-interaction
                   '((require setup/xref scribble/xref)
                     (define xref (load-collections-xref))
                     (define index-entries (xref-index xref))
                     (map entry-tag (take index-entries 3)))))
    (slides ([s:stage 0])
      #:timeline (tl:sequence s:stage 5)
      (apply paras #:stage (s:stage) steps)))

  (slides () (bitmap macro-stepper.png))

  (parameterize ([pretty-print-columns 60])
    (define steps (typeset-interaction
                   '((require macro-debugger/stepper-text)
                     (define stepper (stepper-text #'(or #f #t)))
                     (stepper 'next))))
    (slides ([s:stage 0])
      #:timeline (tl:sequence s:stage 4)
      (apply paras #:stage (s:stage) steps))))
