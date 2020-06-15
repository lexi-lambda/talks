#lang racket/base

(require pict
         pict/conditional
         ppict/align
         racket/class
         racket/contract
         racket/draw
         racket/match
         slideshow/base
         threading)

(provide (contract-out [pict-color/c flat-contract?]
                       [pict-finder/c chaperone-contract?]
                       [ppath-cons (-> pict? (or/c pict-path? #f) pict-path?)]
                       [adjust-find (-> pict-finder/c real? real? pict-finder/c)]

                       [pict-when (-> any/c pict? pict?)]
                       [pict-unless (-> any/c pict? pict?)]
                       [picts-take (-> (listof pict?) exact-nonnegative-integer? (listof pict?))]

                       [current-highlight-color (parameter/c pict-color/c)]

                       [line (-> real? real? pict?)]
                       [arrow-line (->* [] [#:arrow-size real?
                                            #:line-length real?
                                            #:line-width real?]
                                        pict?)]
                       [highlight (->* [pict?] [#:bleed real? #:color pict-color/c] pict?)]
                       [highlight-if (->* [any/c pict?] [#:bleed real? #:color pict-color/c] pict?)]
                       [file-icon (-> real? real? pict?)]

                       [em (->* [] [real?] real?)]
                       [one-line (-> pict? pict?)]
                       [indent (->* [pict?] [#:by real?] pict?)]

                       [maybe-colorize (-> pict? (or/c pict-color/c #f) pict?)]
                       [set-smoothing (-> pict? (or/c 'unsmoothed 'smoothed 'aligned) pict?)]
                       [adjust-pen (->* [pict?]
                                        [#:color (or/c string? (is-a?/c color%) #f)
                                         #:width (or/c (real-in 0 255) #f)
                                         #:style (or/c pen-style/c #f)
                                         #:cap (or/c pen-cap-style/c #f)
                                         #:join (or/c pen-join-style/c #f)]
                                        pict?)]
                       [cellophane (-> pict? (real-in 0 1) pict?)]
                       [metrics-frame (-> pict? pict?)]
                       [pip (case-> (-> pict? pict-finder/c pict?)
                                    (-> pict?
                                        (or/c real? pict-path?)
                                        (or/c real? pict-finder/c)
                                        pict?))]
                       [rsvg-isolate (-> pict? pict?)]

                       [line-append (-> pict? pict? ... pict?)]
                       (struct spring ([weight real?]))
                       [hflex (->* [real?]
                                   [#:combine (-> pict? pict? ... pict?)]
                                   #:rest (non-empty-listof (or/c pict? spring?))
                                   pict?)]))

;; -----------------------------------------------------------------------------
;; miscellany

(define pict-color/c (or/c string? (is-a?/c color%) (list/c byte? byte? byte?)))
(define pict-finder/c (-> pict? pict-path? (values real? real?)))

(define (ppath-cons p path)
  (match path
    [#f        p]
    [(? list?) (cons p path)]
    [(? pict?) (list p path)]))

(define (ppath-last path)
  (match path
    [#f             #f]
    [(list _ ... p) p]
    [(? pict? p)    p]))

(define ((adjust-find find dx dy) p path)
  (define-values [x y] (find p path))
  (values (+ x dx) (+ y dy)))

;; -----------------------------------------------------------------------------
;; conditionals

(define (pict-when test then)
  (if test then (ghost then)))
(define (pict-unless test then)
  (if test (ghost then) then))

(define (picts-take ps n)
  (for/list ([p (in-list ps)]
             [i (in-naturals)])
    (if (< i n) p (ghost p))))

;; -----------------------------------------------------------------------------
;; parameters

(define current-highlight-color (make-parameter (make-color #xFF #xB9 #xB5)))

;; -----------------------------------------------------------------------------
;; constructors

(define (line dx dy)
  (dc (λ (dc x y) (send dc draw-line x y (+ x dx) (+ y dy))) dx dy))

(define (arrow-line #:arrow-size [arrow-size 10]
                    #:line-length [line-length 70]
                    #:line-width [line-width 2])
  (panorama (pin-over/align (linewidth line-width (hline line-length line-width))
                            line-length (/ line-width 2) 'c 'c
                            (arrowhead arrow-size 0))))

(define (highlight p #:bleed [bleed 6] #:color [color (current-highlight-color)])
  (define bg (filled-rectangle (+ (pict-width p) (* 2 bleed))
                               (+ (- (pict-height p) (pict-descent p)) (* 2 bleed))
                               #:draw-border? #f
                               #:color color))
  (refocus (cc-superimpose bg p) p))
(define (highlight-if c p #:bleed [bleed 6] #:color [color (current-highlight-color)])
  (pict-if c (highlight p #:bleed bleed #:color color) p))

; Adapted from pict.
(define (file-icon w h)
  (dc (let* ([sw (lambda (x) (* (/ w 110) x))]
             [sh (lambda (y) (* (/ h 150) y))]
             [->pt (lambda (l)
                     (map (lambda (p)
                            (make-object point% 
                              (sw (car p))
                              (sh (cadr p))))
                          l))])
        (lambda (dc x y)
          (send dc draw-polygon 
                (->pt '((0 0)
                        (0 150)
                        (110 150)
                        (110 20)
                        (90 0)))
                x y)
          (send dc draw-line (+ x (sw 90)) (+ y 1) (+ x (sw 90)) (+ y (sh 20)))
          (send dc draw-line (+ x (sw 90)) (+ y (sh 20)) (+ x (sw 110) -1) (+ y (sh 20)))))
   w h))

;; -----------------------------------------------------------------------------
;; sizing / bounding box adjusters

(define (em [n 1]) (* (pict-width (t "M")) n))

; Drops the ascent line to the descent line, making the entire pict behave as a
; single line of text.
(define (one-line p)
  (define ascent (- (pict-height p) (pict-descent p)))
  (pin-over (blank (pict-width p) (pict-height p) ascent (pict-descent p)) 0 0 p))

(define (indent #:by [n (em)] p) (inset p n 0 0 0))

;; -----------------------------------------------------------------------------
;; drawing adjusters

(define (maybe-colorize p color)
  (if color (colorize p color) p))

(define (dc/wrap p proc)
  (define draw-p (make-pict-drawer p))
  (struct-copy
   pict
   (dc (λ (dc dx dy) (proc draw-p dc dx dy))
       (pict-width p)
       (pict-height p)
       (pict-ascent p)
       (pict-descent p))
   [children (list (make-child p 0 0 1 1 0 0))]
   [last (pict-last p)]))

(define (set-smoothing p smoothing)
  (define draw-p (make-pict-drawer p))
  (struct-copy
   pict
   (dc (λ (dc dx dy)
         (define old-smoothing (send dc get-smoothing))
         (send dc set-smoothing smoothing)
         (draw-p dc dx dy)
         (send dc set-smoothing old-smoothing))
       (pict-width p)
       (pict-height p)
       (pict-ascent p)
       (pict-descent p))
   [children (list (make-child p 0 0 1 1 0 0))]
   [last (pict-last p)]))

(define (set-pen #:color [color (make-color 0 0 0)]
                 #:width [width 0]
                 #:style [style 'solid]
                 #:cap [cap 'round]
                 #:join [join 'round]
                 p)
  (dc/wrap p (λ (draw-p dc dx dy)
               (define old-pen (send dc get-pen))
               (send dc set-pen (make-pen #:color color
                                          #:width width
                                          #:style style
                                          #:cap cap
                                          #:join join))
               (draw-p dc dx dy)
               (send dc set-pen old-pen))))

(define (adjust-pen #:color [color #f]
                    #:width [width #f]
                    #:style [style #f]
                    #:cap [cap #f]
                    #:join [join #f]
                    p)
  (define draw-p (make-pict-drawer p))
  (struct-copy
   pict
   (dc (λ (dc dx dy)
         (define old-pen (send dc get-pen))
         (send dc set-pen (make-pen #:color (or color (send old-pen get-color))
                                    #:width (or width (send old-pen get-width))
                                    #:style (or style (send old-pen get-style))
                                    #:cap (or cap (send old-pen get-cap))
                                    #:join (or join (send old-pen get-join))
                                    #:stipple (send old-pen get-stipple)))
         (draw-p dc dx dy)
         (send dc set-pen old-pen))
       (pict-width p)
       (pict-height p)
       (pict-ascent p)
       (pict-descent p))
   [children (list (make-child p 0 0 1 1 0 0))]
   [last (pict-last p)]))

; Like cellophane from pict, but blends the entire pict as a single group.
(define (cellophane p opacity)
  (dc/wrap p (λ (draw-p dc dx dy)
               (define old-alpha (send dc get-alpha))
               (send dc set-alpha 1)
               (send dc push-group)
               (draw-p dc dx dy)
               (send dc set-alpha (* old-alpha opacity))
               (send dc draw-group)
               (send dc set-alpha old-alpha))))

; For debugging: add bounding box lines to the given pict.
(define (metrics-frame p)
  (define metrics-line (hline (pict-width p) 0))
  (define a (pict-ascent p))
  (define b (- (pict-height p) (pict-descent p)))
  (define metrics (~> (rectangle (pict-width p) (pict-height p))
                      (pin-over 0 a (adjust-pen metrics-line #:color "red"))
                      (pin-over 0 b (adjust-pen metrics-line #:color "blue"
                                                #:style (if (< (abs (- a b)) 0.0001)
                                                            'long-dash
                                                            'solid)))
                      (set-pen)))
  (pin-over p 0 0 metrics))

; Convert the pict to a zero-sized pict centered at a particular location.
(define pip
  (case-lambda
    [(p find)
     (pip p p find)]
    [(p a b)
     (define pinhole (blank))
     (refocus (pin-over p a b pinhole) pinhole)]))

(define (rsvg-isolate p)
  (define draw-p (make-pict-drawer p))
  (dc (λ (dc x y)
        ; for reasons I cannot fathom, this prevents rsvg from screwing up the
        ; color of subsequent draw operations
        (define old-pen (send dc get-pen))
        (send dc set-pen (make-pen #:style 'transparent))
        (send dc draw-point -inf.0 -inf.0)
        (send dc set-pen old-pen)
        (draw-p dc x y))
      (pict-width p)
      (pict-height p)
      (pict-ascent p)
      (pict-descent p)))

;; -----------------------------------------------------------------------------
;; combiners

; Combines picts by extending the last line, as determined by pict-last.
(define (line-append p0 . ps)
  (foldl (λ (p2 p1) (line-append/2 p1 p2)) p0 ps))
(define (line-append/2 p1 p2)
  (define draw-p1 (make-pict-drawer p1))
  (define draw-p2 (make-pict-drawer p2))
  ; find the rightmost point on the baseline of (pict-last p1)
  (define-values [last-x last-y] (rbl-find p1 (or (pict-last p1) p1)))

  ; figure out where we’ll place p2 relative to p1, since we want to align the
  ; descent line of (pict-last p1) with the ascent line of p2
  (define p2-y-relative (- last-y (pict-ascent p2)))
  ; if p2-y is negative, that means p2’s ascent peeks out above the top of p1,
  ; so compute how far we need to offset p1/p2 relative to the top of the new pict
  (define p1-y (if (negative? p2-y-relative) (- p2-y-relative) 0))
  (define p2-y (if (negative? p2-y-relative) 0 p2-y-relative))

  ; the x coordinate is simpler, since we don’t have to deal with ascent/descent,
  ; but it’s possible (though unlikely) that last-x is negative, in which case we
  ; want to do a similar adjustment
  (define p1-x (if (negative? last-x) (- last-x) 0))
  (define p2-x (if (negative? last-x) 0 last-x))

  ; compute rightmost point and bottommost point in the new pict’s bounding box
  (define w (max (+ p1-x (pict-width p1))
                 (+ p2-x (pict-width p2))))
  (define h (max (+ p1-y (pict-height p1))
                 (+ p2-y (pict-height p2))))
  ; same for uppermost ascent line and lowermost descent line
  (define a (min (+ p1-y (pict-ascent p1))
                 (+ p2-y (pict-ascent p2))))
  (define d (- h (max (+ p1-y (- (pict-height p1) (pict-descent p1)))
                      (+ p2-y (- (pict-height p2) (pict-descent p2))))))

  ; compute child offsets, which are weird because pict uses an inverted
  ; coordinate system, so these are relative to the lowermost point
  (define p1-dy (- h (+ p1-y (pict-height p1))))
  (define p2-dy (- h (+ p2-y (pict-height p2))))

  ; invent a new, totally unique pict to use as pict-last, in case (pict-last p2)
  ; already exists somewhere in the pict
  (define p2-last (or (ppath-last (pict-last p2)) p2))
  (define-values [p2-last-x p2-last-y] (lt-find p2 (or (pict-last p2) p2)))
  (define last-p (blank (pict-width p2-last)
                        (pict-height p2-last)
                        (pict-ascent p2-last)
                        (pict-descent p2-last)))
  
  (~> (dc (λ (dc dx dy)
            (draw-p1 dc (+ dx p1-x) (+ dy p1-y))
            (draw-p2 dc (+ dx p2-x) (+ dy p2-y)))
          w h a d)
      (struct-copy pict _
                   [children (list (make-child p1 p1-x p1-dy 1 1 0 0)
                                   (make-child p2 p2-x p2-dy 1 1 0 0)
                                   (make-child last-p
                                               (+ p2-x p2-last-x)
                                               (+ p2-dy p2-last-y)
                                               1 1 0 0))]
                   [last last-p])))

(struct spring (weight) #:transparent)
(define (hflex width #:combine [combine hc-append] . elements)
  (define fixed-width (for/sum ([e (in-list elements)] #:unless (spring? e)) (pict-width e)))
  (define flexi-width (- width fixed-width))
  (define total-weight (for/sum ([e (in-list elements)] #:when (spring? e)) (spring-weight e)))
  (define width-per-weight (/ flexi-width total-weight))
  (apply combine (for/list ([element (in-list elements)])
                   (match element
                     [(spring weight) (blank (* weight width-per-weight) 0)]
                     [_               element]))))
