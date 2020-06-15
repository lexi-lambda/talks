#lang at-exp curly-fn slideshow

(require (for-syntax racket/list)

         pict/conditional
         pict/shadow
         ppict/align
         racket/draw
         racket/runtime-path
         slideshow/code
         syntax/parse/define
         threading

         (only-in slideshow [current-font-size current-text-size] [slide slideshow:slide])
         (rename-in (except-in slideshow/staged-slide slide/staged) [staged slideshow:staged]))

;; ---------------------------------------------------------------------------------------------------

(define-runtime-path proposal-pr.png "assets/proposal-pr.png")
(define-runtime-path typechecking-rules.png "assets/typechecking-rules.png")
(define proposal-pr (bitmap proposal-pr.png))
(define typechecking-rules (bitmap typechecking-rules.png))

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
  (if test then (ghost then)))
(define (pict-unless test then)
  (if test (ghost then) then))

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
(define text-highlight-color (make-color #xFF #xB9 #xB5))
(define text-secondary-highlight-color (make-color #xb5 #xd0 #xff))
(define text-tertiary-highlight-color (make-color #xc7 #xff #xcd))
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
(define (sbt s) (text s (cons '(weight . semibold) (current-main-font)) (current-text-size)))
(define (tt s) (text s (current-code-font) (current-text-size)))
(define (itt s) (text s (cons 'italic (current-code-font)) (current-text-size)))
(define (btt s) (text s (cons 'bold (current-code-font)) (current-text-size)))
(define (emoji s) (text s "Apple Color Emoji" (current-text-size)))

; Horizontally append two picts, placing the second pict after pict-last if it exists, effectively
; extending the last line of text.
(define (text-append/2 a b)
  (cond
    [(pict-last a)
     => (λ (subs)
          (define sub (if (list? subs) (last subs) subs))
          (define sub* (htl-append (launder (ghost sub)) b))
          (define-values [x y] (lt-find a subs))
          (define new (lt-superimpose a (inset sub* x y 0 0)))
          (use-last* new sub*))]
    [else (htl-append a b)]))
(define text-append
  (case-lambda
    [() (blank)]
    [(p . ps) (foldl (λ (b a) (text-append/2 a b)) p ps)]))

(define (ct/1 p) (colorize (if (string? p) (t p) p) (current-text-color)))
(define (ct #:sep [sep 0] . ps) (apply htl-append sep (map ct/1 ps)))

(define current-title-text-size (make-parameter 40))
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
           (pin-over/align 0 0 'l 'b (filled-rectangle 60 8 #:draw-border? #f #:color highlight-color))))
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

(match-define (list pygments-in pygments-out _ #f _)
  (process*/ports #f #f (current-error-port) (find-executable-path "pygmentize") "-s" "-l" "haskell" "-f" "raw"))
(define pygments-cache (make-hash))

(define (haskell-line str)
  (define tokens
    (hash-ref!
     pygments-cache str
     (λ ()
       (write-string str pygments-out)
       (newline pygments-out)
       (flush-output pygments-out)
       (let loop ([lines '()])
         (match-define (regexp #px"^([a-zA-Z.]+)\t('|\")(.*)\\2$"
                               (list _ type _ contents))
           (read-line pygments-in))
         (define-values [trimmed-contents last-line?]
           (match contents
             [(regexp #px"(.*)\\\\n$" (list _ trimmed-contents))
              (values trimmed-contents #t)]
             [_ (values contents #f)]))
         (define parsed-contents (string-replace trimmed-contents "\\\\" "\\"))

         (define color
           (match type
             [(regexp #px"^Token\\.Comment") 'comment]
             [(regexp #px"^Token\\.(Keyword|Name\\.Builtin|Operator)") 'keyword]
             [(regexp #px"^Token\\.Literal") 'literal]
             [(regexp #px"^Token\\.(Name|Text)") 'id]
             ["Token.Punctuation" 'base]))

         (define lines* (cons (cons parsed-contents color) lines))
         (if last-line? (reverse lines*) (loop lines*))))))

  (define tt (current-code-tt))
  (~>> (for/list ([token (in-list tokens)])
         (match-define (cons str type) token)
         (colorize (tt str) (match type
                              ['comment (current-comment-color)]
                              ['keyword (current-keyword-color)]
                              ['literal (current-literal-color)]
                              ['id (current-id-color)]
                              ['base (current-base-color)])))
      (apply htl-append)))

(define (list-split lst sep [=? equal?])
  (if (empty? lst) lst
      (for/foldr ([lsts '(())])
                 ([v (in-list lst)])
        (if (=? v sep)
            (cons '() lsts)
            (cons (cons v (car lsts)) (cdr lsts))))))

(define (haskell . ps)
  (~>> (for/list ([ps (in-list (list-split ps "\n"))])
         (~>> (for/list ([p (in-list ps)])
                (if (string? p) (haskell-line p) p))
              (apply text-append)))
       (apply vl-append (current-code-line-sep))))

;; ---------------------------------------------------------------------------------------------------

(define (highlight p #:bleed [bleed 6] #:color [color text-highlight-color])
  (define bg (filled-rectangle (+ (pict-width p) (* 2 bleed))
                               (+ (- (pict-height p) (pict-descent p)) (* 2 bleed))
                               #:draw-border? #f
                               #:color color))
  (refocus (cc-superimpose bg p) p))
(define (highlight-if c p #:color [color text-highlight-color])
  (pict-if c (highlight p #:color color) p))

(define (arrow-line #:arrow-size [arrow-size 10]
                    #:line-length [line-length 70]
                    #:line-width [line-width 2])
  (panorama (pin-over/align (linewidth line-width (hline line-length line-width))
                            line-length (/ line-width 2) 'c 'c
                            (arrowhead arrow-size 0))))

(define (frame/bg p c)
  (~> (filled-rectangle (pict-width p) (pict-height p) #:color c #:draw-border? #f)
      (cc-superimpose p)
      frame))

(define (enumerate #:sep [sep 0] #:stage [stage +inf.0] . ps)
  (define i-ps (for/list ([i (in-range 1 (add1 (length ps)))]) (ct (~a i ". "))))
  (define max-i-p (ghost (argmax pict-width i-ps)))
  (define i-ps* (for/list ([p (in-list i-ps)]) (rtl-superimpose p max-i-p)))
  (~>> (for/list ([p (in-list ps)]
                  [i-p (in-list i-ps*)]
                  [i (in-naturals)])
         (pict-when (<= i stage) (htl-append i-p p)))
       (apply vl-append sep)))

;; ---------------------------------------------------------------------------------------------------

(define ((adjust-find find dx dy) p1 p2)
  (define-values [x y] (find p1 p2))
  (values (+ x dx) (+ y dy)))

(define arrow:in (blank))
(define arrow:out (blank))

(define (launder/preserve p0 . paths)
  (for/fold ([p p0])
            ([path (in-list paths)])
    (define-values [x y] (lt-find p0 path))
    (define sub (if (list? path) (last path) path))
    (pin-over p x y (ghost sub))))

(define (arrow:close p)
  (define p* (inset p 50 0))
  (~> (pin-line p* p* lc-find arrow:in lc-find)
      (pin-arrow-line 10 _ arrow:out rc-find p* rc-find)
      launder
      (set-pen #:width 3)))

(define (arrow:expr p)
  (~> (frame/bg (inset p 20 10) "white")
      (lc-superimpose arrow:in)
      (rc-superimpose arrow:out)))

(define (arrow:compose/2 p1 p2)
  (~> (hc-append 50 p1 p2)
      (pin-line (list p1 arrow:out) rc-find (list p2 arrow:in) lc-find)
      (launder/preserve (list p1 arrow:in) (list p2 arrow:out))))
(define (arrow:compose p . ps) (foldl (λ (a b) (arrow:compose/2 b a)) p ps))

(define (pad-arrow p w)
  (define d (/ (- w (pict-width p)) 2))
  (cond
    [(<= d 0) p]
    [else
     (define p* (inset p d 0))
     (~> (pin-line p* p* lc-find arrow:in lc-find)
         (pin-line arrow:out rc-find p* rc-find)
         launder
         (lc-superimpose arrow:in)
         (rc-superimpose arrow:out))]))

(define (arrow:joiner p)
  (define base (disk 30 #:color "white" #:border-color "black"))
  (define icon (scale-to-fit p base))
  (cc-superimpose base icon))

(define ((arrow:fork/choice joiner) p1 p2)
  (define w (+ (max (pict-width p1) (pict-width p2)) 30))
  (define p1* (pad-arrow p1 w))
  (define p2* (pad-arrow p2 w))
  (define p (inset (vc-append 30 p1* p2*) 20 0))
  (define p* (~> (pin-line p p lc-find (list p1* arrow:in) lc-find)
                 (pin-line p lc-find (list p2* arrow:in) lc-find)
                 (pin-line (list p1* arrow:out) rc-find p rc-find)
                 (pin-line (list p2* arrow:out) rc-find p rc-find)
                 launder))
  (define jw (/ (pict-width joiner) 2))
  (~> (inset p* jw 0)
      (lc-superimpose (inset joiner 0 0 (- jw) 0))
      (rc-superimpose (inset joiner (- jw) 0 0 0))
      (lc-superimpose arrow:in)
      (rc-superimpose arrow:out)))

(define arrow:fork (arrow:fork/choice (arrow:joiner @haskell{*})))
(define arrow:choice (arrow:fork/choice (arrow:joiner @haskell{|})))

;; ---------------------------------------------------------------------------------------------------

(parameterize ([current-title-text-size 80])
  (slide #:title "Introduction to Arrows"))

(parameterize ([current-slide-title "Incrementalization"])
  (slide/staged [s0 s1 s2 s3 s4 s5 s6 s7 s8]
    (vc-append
     50
     (pict-when (at/after s1)
      @haskell{
        foo :: (Integer, Bool) -> String
        foo @(highlight-if (at s2) @haskell{(a, b)}) =
          let c = @(highlight-if (or (at s3) (at/after s7)) @haskell{f a}) @(pict-when (at/after s7) (ct "← really expensive!"))
              d = @(highlight-if (at s4) @haskell{g c b})
          in @(highlight-if (at s5) @haskell{h c d})})
     (pict-when (at/after s8)
       (ct #:sep 5 "Idea: cache result of " @haskell{f a} " if " @haskell{a} " does not change."))))

  (slide/staged [s1 s2 s3 s4 s5]
    (vc-append
     50
     (scale (ct (sbt "How can we implement caching in Haskell?")) 1.5)
     (pict-when (at/after s2)
       (vc-append
        30
        (ct "Simplest answer: take the previous values as an argument.")
        @haskell{
          foo :: @(highlight-if (at s3) @haskell{(Integer, Char)}) -> (Integer, Bool) -> (String, @(highlight-if (at s5) @haskell{Char}))
          foo @(highlight-if (at s3) @haskell{(a_prev, c_prev)}) (a, b) =
            let @(highlight-if (at s4) @haskell{c | a == a_prev = c_prev
                                                  | otherwise   = f a})
                d = g c b
            in (h c d, @(highlight-if (at s5) @haskell{c}))}))))

  (slide/staged [s1 s2.1 s2.2 s2.3 s2.4 s3]
    (vc-append
     50
     (ct (sbt "This approach sucks. :("))
     (vc-append
      30
      (enumerate
       #:stage (- stage s2.1)
       (ct "Leaks implementation details")
       (ct "Hard to read")
       (ct "Doesn’t scale")
       (ct "Easy to screw up")))
     (pict-when (at/after s3)
       (ct (sbt "We want an abstraction!")))))

  (current-slide-title "Abstracting incrementalization")
  (slide/staged [s1 s1.5 s2 s2.1 s2.2 s3]
    (vc-append
     60
     (ct (sbt "Caching monad?"))
     (pict-when (at/after s1.5)
       @haskell{class Monad m => MonadCache m where
                  cache :: m a -> m a})
     (pict-when (at/after s2)
       @haskell{
         foo :: @(highlight-if (at/after s2.1) @haskell{MonadCache m =>}) (Integer, Bool) -> m String
         foo (a, b) = do
           c <- @(highlight-if (at/after s2.2) @haskell{cache}) $ f a
           d <- g c b
           h c d})
     (pict-when (at/after s3)
       (ct "But wait: how does  " @haskell{cache} "  know what changed?"))))

  (slide/staged [s1 s2 s3 s4]
    (vc-append
     50
     @haskell{cache :: MonadCache m => m a -> m a}
     (pict-when (at/after s2)
       (hc-append 10 @haskell{cache :: (MonadCache m, Eq a) => (a -> m b) -> a -> m b} (ct (sbt "?"))))
     (pict-when (at/after s3) @haskell{cache f a})
     (pict-when (at/after s4) @haskell{cache (\() -> f a) ()})))

  (slide/staged [s1 #:! s1.1 s2 s2.1]
    @haskell{
      g a b c = do
        x <- someFunc a b
        y <- anotherFunc b c
        @(pict-cond
          [(at/after s2)
           @haskell{
             flip cache (x, y) $ \(x', y') -> do
               z <- thirdFunc x' y'
               fourthFunc @(highlight-if (at s2.1) @haskell{c}) z}]
          [else
           (highlight-if (at s1.1) @haskell{
             z <- thirdFunc x y
             fourthFunc c z})])})

  (slide/staged [s1 s2 s3]
    (vc-append
     50
     (ct @haskell{Monad} (sbt "  is too powerful."))
     (vc-append
      30
      (pict-when (at/after s2)
        (ct "Can we get away with just  " @haskell{Applicative} " ?"))
      (pict-when (at/after s3)
        (ct "Answer: not really."))))))

(slide (vc-append (ct (bt "We need an abstraction"))
                  (ct (bit "between  ") @haskell{Applicative} (bt "  and  ") @haskell{Monad} (bt "."))))

(slide (inset (bt "Arrows") 30))

(parameterize ([current-slide-title "Arrows"])
  (slide/staged [s1 s2.1 s2.2 s2.3 s3.1 s3.2 s3.3 s3.4]
    (vc-append
     80
     (sbt "What is an arrow?")
     (table 3 (list (pict-when (at/after s2.1) @haskell{Monad m})
                    (pict-when (at/after s2.2) @haskell{m a})
                    (pict-when (at/after s2.3) (~> (vc-append  (ct "A value  " @haskell{a} ",")
                                                               (ct "plus some context in  " @haskell{m} "."))
                                                   (scale 3/4)))

                    (pict-when (at/after s3.1) @haskell{Arrow arr})
                    (pict-when (at/after s3.2)
                      (let ()
                        (define infix (scale (ct "( " @haskell{a `arr` b} " )") 0.8))
                        (vc-append 5 (ghost infix) @haskell{arr a b} (pict-when (at/after s3.4) infix))))
                    (pict-when (at/after s3.3) (~> (vc-append  (ct "A function  " @haskell{a -> b} ",")
                                                               (ct "plus some context in  " @haskell{arr} "."))
                                                   (scale 3/4))))
            cc-superimpose cc-superimpose 100 50)))

  (slide/staged [s0 s1 s2 s3]
    (vc-append
     80
     @haskell{class Monad m => MonadCache m where
                cache :: m a -> m a}
     (pict-when (at/after s1)
       @haskell{class Arrow arr => ArrowCache arr where
                  cache :: @(highlight-if (at/after s3) @haskell{Eq a}) => (@(highlight-if (at/after s2) @haskell{a}) `arr` b) -> (@(highlight-if (at/after s2) @haskell{a}) `arr` b)})))

  (slide/staged [s0 s1 s2 s3]
    (~> (enumerate
         #:sep 40
         #:stage (- stage s1)
         (ct "How do you create an arrow?")
         (ct "How do you run an arrow?")
         (vl-append (ct "How do you compose two")
                    (ct "arrows together?")))
        (inset 30)))

  (slide/staged [s0 s1.1 s1.2 s2.1 s2.2]
    (vc-append
     80
     (vc-append 20 (pict-when (at/after s1.1) (ct "Lifting"))
                (pict-when (at/after s1.1) @haskell{pure :: Monad m => a -> m a})
                (pict-when (at/after s1.2) @haskell{arr :: Arrow arr => (a -> b) -> (a `arr` b)}))
     (vc-append 20 (pict-when (at/after s2.1) (ct "Composition"))
                (pict-when (at/after s2.1) @haskell{(>>=) :: Monad m => m a -> (a -> m b) -> m b})
                (pict-when (at/after s2.2) @haskell{(>>>) :: Arrow arr =>
                                                      (a `arr` b) -> (b `arr` c) -> (a `arr` c)}))))

  (slide/staged [s1 s2.1 s2.2 s3.1 s3.2]
    (vc-append
     100
     (sbt "This interface is very restrictive!")
     (vc-append
      25
      (pict-when (at/after s2.1) @haskell{m1 :: a -> m Bool     m2 :: a -> m b     m3 :: a -> m b})
      (pict-when (at/after s2.2) @haskell{f a = m1 a >>= \b -> if b then m2 a else m3 a}))

     (vc-append
      25
      (pict-when (at/after s3.1) @haskell{a1 :: a `arr` Bool     a2 :: a `arr` b     a3 :: a `arr` b})
      (pict-when (at/after s3.2) @haskell{f a = a1 a >>> ???}))))

  (slide/staged [s1 s1.1 s2 s3.1 s3.2 s3.3]
    (vc-append
     100
     (vc-append 20 (hc-append 20 @haskell{m >>= @(highlight-if (at/after s1.1) @haskell{f})}
                              (pict-when (at/after s1.1) (ct "← black box!")))
                (pict-when (at/after s3.1) (ct "Monads are" (it " higher-order.")))
                           (pict-when (at/after s3.2) @haskell{join :: Monad m => m (m a) -> m a}))
     (vc-append 20 (pict-when (at/after s2) @haskell{a1 >>> a2 >>> a3 >>> a4})
                (pict-when (at/after s3.3) (ct "Arrows are" (it " first-order."))))))

  (current-slide-title "Arrows as graphs")
  (slide/staged [s0 s1 s2]
    (vc-append
     50
     (pict-when (at/after s1) @haskell{a >>> b >>> c >>> d})
     (pict-when (at/after s2)
       (arrow:close (arrow:compose (arrow:expr @haskell{a}) (arrow:expr @haskell{b})
                                   (arrow:expr @haskell{c}) (arrow:expr @haskell{d}))))))

  (current-slide-title "Arrows as graphs: products")
  (slide/staged [s0 s1 s2 s3 s4]
    (vc-append
     80
     (pict-when (at/after s1)
       (vc-append
        20
        @haskell{arr (* 10) :: Integer `arr` Integer}
        @haskell{arr isUpper :: Char `arr` Bool}))
     (pict-when (at/after s2)
       (vc-append
        20
        @haskell{(Integer, Char) `arr` (Integer, Bool)}
        (pict-when (at/after s3)
          (arrow:close (arrow:fork (arrow:expr @haskell{arr (* 10)})
                                   (arrow:expr @haskell{arr isUpper}))))))
     (pict-when (at/after s4)
       @haskell{(***) :: Arrow arr =>
                  (a `arr` b) -> (c `arr` d) -> ((a, c) `arr` (b, d))})))

  (slide/staged [s1 s2]
    (vc-append
     50
     (scale @haskell{a >>> ((b >>> c) *** d) >>> e} 3/4)
     (pict-when (at/after s2)
       (arrow:close (arrow:compose (arrow:expr @haskell{a})
                                   (arrow:fork (arrow:compose (arrow:expr @haskell{b}) (arrow:expr @haskell{c}))
                                               (arrow:expr @haskell{d}))
                                   (arrow:expr @haskell{e}))))))

  (current-slide-title "Arrows as graphs: sums")
  (slide/staged [s1 s2]
    (vc-append
     100
     @haskell{class Arrow arr => ArrowChoice arr where
                (|||) :: (a `arr` b) -> (c `arr` d)
                      -> (Either a c `arr` Either b d)}
     (pict-when (at/after s2)
       (vc-append
        50
        @haskell{a ||| b}
        (~> (arrow:close (arrow:choice (arrow:expr @haskell{a}) (arrow:expr @haskell{b})))
            (scale 1.25))))))

  (current-slide-title "Arrows as graphs")
  (slide/staged [s1 s2 s3]
    (vc-append
     50
     @haskell{  arr :: Arrow arr => (a -> b) -> (a `arr` b)
              (>>>) :: Arrow arr => (a `arr` b) -> (b `arr` c) -> (a `arr` c)
              (***) :: Arrow arr => (a `arr` b) -> (c `arr` d) -> ((a, c) `arr` (b, d))
              (|||) :: ArrowChoice arr =>
                         (a `arr` b) -> (c `arr` d) -> (Either a c `arr` Either b d)}

     (pict-when (at/after s2)
       (arrow:close (arrow:compose (arrow:expr @haskell{a})
                                   (arrow:choice (arrow:fork (arrow:compose (arrow:expr @haskell{b})
                                                                            (arrow:expr @haskell{c}))
                                                             (arrow:expr @haskell{d}))
                                                 (arrow:choice (arrow:expr @haskell{e})
                                                               (arrow:compose (arrow:expr @haskell{f})
                                                                              (arrow:expr @haskell{g})
                                                                              (arrow:expr @haskell{h}))))
                                   (arrow:choice (arrow:expr @haskell{i})
                                                 (arrow:compose (arrow:expr @haskell{j}) (arrow:expr @haskell{k})))
                                   (arrow:expr @haskell{l})
                                   (arrow:expr @haskell{m}))))
     (pict-when (at/after s3)
       @haskell{a >>> (((b >>> c) *** d) ||| (e ||| (f >>> g >>> h)))
                  >>> (i ||| (j >>> k)) >>> l >>> m})))

  (current-slide-title "Arrow notation")
  (slide/staged [s0 s1 s2 s2.1 s3 s4 s5]
    (hc-append
     100
     (pict-when (at/after s1)
       @haskell{foo :: @(pict-if (before s4) @haskell{A -> B -> M C} @haskell{(A, B) -> M C})
                foo @(pict-cond
                      [(before s3) @haskell{a b = do}]
                      [(before s4) @haskell{= \a b -> do}]
                      [else @haskell{= \(a, b) -> do}])
                  x <- f a
                  y <- g @(pict-if (before s4) @haskell{b x} @haskell{(b, x)})
                  h @(pict-if (before s4) @haskell{x y} @haskell{(x, y)})})
     (pict-when (at/after s2)
       (let ()
         (define -< (highlight-if (at/after s5) @haskell{-<}))
         @haskell{foo :: (A, B) `Arr` C
                  foo = @(highlight-if (at/after s2.1) @haskell{proc (a, b) ->}) do
                    x <- f @-< a
                    y <- g @-< (b, x)
                    h @-< (x, y)}))))

  (slide/staged [s1 s1.1 s1.2 s1.3 s1.4 s1.5 s1.6 s2 s2.1 s2.2 s2.3 s3 s3.1 s3.2 s3.3 s3.4 s3.5]
    (vc-append
     90
     @haskell{do { @(highlight-if (at s1.1) @haskell{@;
                     @(highlight-if (at s1.2) @haskell{pat}) @;
                     @(highlight-if (at s1.3) @haskell{<-}) @;
                     @(highlight-if (at s1.4) @haskell{expr})}); @;
                   ...; @(highlight-if (at s1.5) @haskell{expr}) }}
     (vc-append
      40
      (pict-when (at/after s2)
        (let ()
          (define cmd @(highlight-if (at s2.3) @haskell{cmd}))
          @haskell{proc @(highlight-if (at s2.1) @haskell{pat}) -> @;
                     do { @(highlight-if (at s2.2) @haskell{pat <-}) @cmd; ...; @cmd }}))
      (pict-when (at/after s3)
        @haskell{cmd ::= @(highlight-if (at s3.1) @haskell{@;
                           @(highlight-if (at s3.2) @haskell{expr}) @;
                           @(highlight-if (at s3.3) @haskell{-<}) @;
                           @(highlight-if (at s3.4) @haskell{expr})}) @;
                      | ...}))))

  (slide/staged [s1 s2 s3 s4 s5]
    (~> (let ()
          (define (lvar . s) (highlight-if #:color text-secondary-highlight-color (at/after s2) (apply haskell s)))
          (define (lexp . s) (highlight-if #:color text-tertiary-highlight-color (at/after s3) (apply haskell s)))
          (define (aexp . s) (highlight-if (at/after s4) (apply haskell s)))
          @haskell{foo :: (A, B) `Arr` C
                   foo = proc (@lvar{a}, @lvar{b}) -> do
                       @lvar{x} <- @aexp{f} -< @lexp{a}
                       @lvar{y} <- @aexp{g} -< @lexp{(b, x)}
                       @aexp{h} -< @lexp{(x, y)}
                     @(pict-when (at/after s5)
                        @haskell{
                          where
                            @aexp{f} :: A `Arr` D
                            @aexp{f} = proc a -> ...})})))

  (slide/staged [s1 s2]
    (let ()
      (define (lvar . s) (highlight #:color text-secondary-highlight-color (apply haskell s)))
      (define (lexp . s) (highlight #:color text-tertiary-highlight-color (apply haskell s)))
      (define (aexp . s) (highlight (apply haskell s)))
      (hc-append
       80
       @haskell{foo :: (A, B) `Arr` C
                foo = proc (@lvar{a}, @lvar{b}) -> do
                  @lvar{x} <- @aexp{f} -< @lexp{a}
                  @lvar{y} <- @aexp{g} -< @lexp{(b, x)}
                  @aexp{h} -< @lexp{(x, y)}}
       (pict-when (at/after s2)
         (hc-append
          80
          (arrow-line #:line-width 5 #:arrow-size 20)
          (vc-append
           40
           (~> @haskell{let @aexp{a1} = arr (\(@lvar{x}, @lvar{b}) -> @lexp{(x, (b, x))})
                        in (@aexp{f} *** id) >>> @aexp{a1} >>> (id *** @aexp{g}) >>> @aexp{h}}
               (scale 0.8))
           (arrow:close (arrow:compose (arrow:fork (arrow:expr @haskell{f}) (arrow:expr @haskell{id}))
                                       (arrow:expr @haskell{a1})
                                       (arrow:fork (arrow:expr @haskell{id}) (arrow:expr @haskell{g}))
                                       (arrow:expr @haskell{h})))))))))

  (slide/staged [s1 #:! s2 s3 s3.1 #:! s3.2 #:! s4 #:~ s5]
    (hc-append
     50
     (pict-cond
      [(at s1)
       @haskell{cmd ::= expr -< expr
                     |  ...}]
      [(at s2)
       @haskell{cmd ::= expr -< expr
                     |  @(highlight @haskell{do { pat <- cmd; ...; cmd }})
                     |  ...}]
      [(and (at/after s3) (before s4))
       @haskell{cmd ::= expr -< expr
                     |  do { pat <- cmd; ...; cmd }
                     |  @(highlight-if (at s3) @haskell{case expr of { pat -> @(highlight-if (at/after s3.1) @haskell{cmd}); ... }})
                     |  ...}]
      [else
       @haskell{cmd ::= expr -< expr
                     |  do { pat <- cmd; ...; cmd }
                     |  case expr of { pat -> cmd; ... }
                     |  @(highlight-if (at s4) @haskell{if expr then cmd else cmd})
                     |  ...}])
     (pict-cond
      [(at s2)
       @haskell{proc (a, b) -> do
                  x <- do
                    y <- f -< a
                    g -< (y, b)
                  h -< x}]
      [(and (at/after s3) (before s4))
       @haskell{proc (a, b) -> do
                 x <- case b of
                   Just c  -> @(highlight-if (at/after s3.2) @haskell{f -< c})
                   Nothing -> @(highlight-if (at/after s3.2) @haskell{g -< a})
                 h -< x}]
      [(at s4)
       @haskell{proc (a, b) -> do
                  x <- if f b
                    then g -< a
                    else h -< b
                  i -< x}])))

  (current-slide-title "Arrow notation: control operators")
  (slide/staged [s0 s1 s1.1 s2]
    (vc-append
     50
     (hc-append
      150
      (pict-when (at/after s1)
        @haskell{f x = do
                   y <- g x `catchError` \e -> h e x
                   @(pict-if (before s1.1) @haskell{...}
                             @haskell{for y $ \z -> do
                                        ...})})
      (pict-when (at/after s2)
        @haskell{f = proc x -> do
                   y <- ???
                   ???}))))

  (slide/staged [s0 s1 s2 s2.1 s2.2 s3]
    (vc-append
     80
     (pict-when (at/after s1)
       @haskell{class Monad m => MonadError e m | m -> e where
                  throwError :: e -> m a
                  catchError :: m a -> (e -> m a) -> m a})
     (pict-when (at/after s2)
       @haskell{class Arrow arr => ArrowError e arr | arr -> e where
                  throwA :: @(pict-if (before s2.1) @haskell{???}
                                      @haskell{e `arr` a})
                  catchA :: @(pict-if (before s2.2) @haskell{???}
                                      @haskell{(a `arr` b) -> ((a, e) `arr` b) -> (a `arr` b)})})
     (pict-when (at/after s3)
       @haskell{g `catchA` proc (x, e) -> h -< (e, x)})))

  (slide/staged [s1 s2 s2.1 s2.2 s2.3 s2.4 s2.5 s3 s3.1 s3.2 s3.3]
    (let ()
      (define (lvar . s) (highlight-if #:color text-secondary-highlight-color (at/after s3.1) (apply haskell s)))
      @haskell{proc @(highlight-if (at s2) @haskell{(@lvar{a}, @lvar{b})}) -> do
                 x <- @(highlight-if (at s3.2) @haskell{@;
                        (@(highlight-if (or (at s2.1) (at s2.2)) @haskell{g}) @;
                         @(highlight-if (at s2.3) @haskell{@;
                         `catchA` proc @(highlight-if (or (at s2.4) (at s2.5)) @haskell{(_, e)}) -> @;
                           @(highlight-if (at s2.5) @haskell{h -< (@(highlight-if (at/after s3.3) @haskell{b}), e)})}))}) @;
                       -< @(highlight-if (at s2.2) @haskell{a})
                 ...}))

  (slide/staged [s1 s1.1 s1.2]
    @haskell{proc (a, b) -> do
               x <- (@(highlight-if (at s1.1) @haskell{g'}) `catchA` @(highlight-if (at s1.1) @haskell{h'})) @;
                 -< @(highlight-if (at s1.2) @haskell{(a, b)})
               ...
             where
               @(highlight-if (at s1.1) @haskell{@;
                 g' = proc @(highlight-if (at s1.2) @haskell{(a, _)}) -> g -< a})
               @(highlight-if (at s1.1) @haskell{@;
                 h' = proc @(highlight-if (at s1.2) @haskell{((_, b), e)}) -> h -< (b, e)})})

  (slide/staged [s1 #:! s2 s2.1 s2.1.1 s2.2 #:! s3 s3.1 s3.2 s3.3 s3.4 s3.99 #:! s4 s4.1 #:! s5 #:~ s5.99]
    (vc-append
     50
     @haskell{cmd ::= expr -< expr
                   |  do { pat <- cmd; ...; cmd }
                   |  case expr of { pat -> cmd; ... }
                   |  if expr then cmd else cmd
                   |  @(highlight-if (or (at s2) (at s2.1)) @haskell{\pat ... -> @(highlight-if (at s2.1.1) @haskell{cmd})})
                   |  @(highlight-if (or (at s2) (at s2.2)) @haskell{cmd infix_expr cmd})
                   |  @(pict-if
                        (before s4) @haskell{...}
                        (highlight-if (at s4) @haskell{@;
                          @(highlight-if (at s4.1) @haskell{(|}) expr cmd ... @(highlight-if (at s4.1) @haskell{|)})}))
                   @(pict-when (at/after s4) @haskell{|  ...})}
     (pict-cond
      [(and (at/after s3) (before s5))
       @haskell{proc (a, b) -> do
                  x <- @(highlight-if (or (at s3.2) (at s3.3)) @haskell{(g -< a)}) @;
                       @(highlight-if (at s3.1) @haskell{`catchA`}) @;
                       @(highlight-if (or (at s3.2) (at s3.4)) @haskell{\e -> h -< (b, e)})
                  ...}]
      [(at/after s5)
       @haskell{proc (a, b) -> do
                  x <- @(highlight-if (at s5) @haskell{(| catchA (g -< a) (\e -> h -< (b, e)) |)})
                  ...}])))

  (slide/staged [s1 s2 s3 s4 s5 s6]
    (let ()
      (define env (highlight-if (at s2) @haskell{a}))
      (define (arg . ps) (highlight-if (at s6) (apply haskell ps)))
      (vc-append
       50
       @haskell{catchA :: ArrowError e arr =>
                  (@env `arr` b) -> ((@env, @(highlight-if (at s3) @haskell{e})) `arr` b) -> (@env `arr` b)}
       (pict-when (at/after s5)
         @haskell{catchA :: ArrowError e arr =>
                    (@arg{(a, ())} `arr` b) -> (@arg{(a, (e, ()))} `arr` b) -> (@arg{(a, ())} `arr` b)}))))

  (slide
    (let ()
      (define comment-str @~a{
@"{-" Note [Weird control operator types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arrow notation (i.e. `proc`) has support for so-called “custom control operators,” which allow
things like

    proc (x, y) -> do
      z <- foo -< x
      (f -< z) `catchA` \e -> g -< (y, e)

to magically work. What’s so magical about that? Well, note that `catchA` is an ordinary function,
but it’s being given /commands/ as arguments, not expressions. Also note that the arguments to
`catchA` reference the variables `y` and `z`, which are bound earlier in the `proc` expression as
arrow-local variables.

To make this work, GHC has to thread `y` and `z` through `catchA` in the generated code, which will
end up being something like this:

        arr (\(x, y) -> (x, (x, y)))
    >>> first foo
    >>> arr (\(z, (x, y)) -> (z, y))
    >>> catchA (first f)
               (arr (\((_, y), e) -> (y, e)) >>> g)})
      (~>> (string-split comment-str "\n")
           (map tt)
           (apply vl-append (current-code-line-sep)))))

  (slide/staged [s1 s2]
    (rc-superimpose
     (~> (shadow-frame proposal-pr #:shadow-descent 5)
         (inset 0 0 500 0)
         (rotate 0.005))
     (pict-when (at/after s2)
       (~> (shadow-frame typechecking-rules #:shadow-descent 5)
           (inset 500 0 0 0)
           (scale 0.8)
           (rotate -0.02))))))

(parameterize ([current-slide-title "Resources"])
  (slide/staged [s0 s1 s2 s3 s4]
    (vc-append
     100
     (pict-when (at/after s1)
       (vc-append (ct "GHC User’s Guide section on arrow notation")
                  (~> (ct "https://downloads.haskell.org/ghc/8.8.1/docs/html/users_guide/glasgow_exts.html#arrow-notation")
                      (scale 1/2))))
     (pict-when (at/after s2)
       (vc-append (ct "A New Notation for Arrows")
                  (scale (ct "http://www.staff.city.ac.uk/~ross/papers/notation.html") 1/2)))
     (pict-when (at/after s3)
       (vc-append (ct "GHC Proposal: Constraint-based arrow notation")
                  (scale (ct "https://github.com/ghc-proposals/ghc-proposals/pull/303") 1/2)))
     (pict-when (at/after s4)
       (scale (ct "Ask me!") 1.3)))))

(start-at-recent-slide)

;; ---------------------------------------------------------------------------------------------------

(close-output-port pygments-out)
(close-input-port pygments-in)
(set! pygments-cache #f)
