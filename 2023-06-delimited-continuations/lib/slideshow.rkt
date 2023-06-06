#lang racket/base

(require (except-in pict pin-over pin-under cellophane file-icon)
         racket/contract
         racket/match
         slideshow/base
         slideshow/code
         slideshow/text
         threading

         (prefix-in slideshow: slideshow/base)

         "pict.rkt"
         "util.rkt")

(provide (contract-out [current-slide-margin (parameter/c real?)]
                       [make-scaling-slide-assembler (->* [] [#:background-color pict-color/c
                                                              #:annotate (-> pict? pict?)]
                                                          (-> (or/c string? #f)
                                                              exact-nonnegative-integer?
                                                              pict?
                                                              pict?))]

                       [current-text-color (parameter/c (or/c pict-color/c #f))]
                       [tt (-> string? pict?)]

                       [para-spacing/c flat-contract?]
                       [para-align/c flat-contract?]
                       [current-para-spacing (parameter/c para-spacing/c)]
                       [current-para-align (parameter/c para-align/c)]
                       [current-para-fill? (parameter/c any/c)]
                       [current-item-indent (parameter/c (or/c real? (-> real?)) real?)]
                       [para (->* [] [#:width real?
                                      #:align para-align/c
                                      #:spacing para-spacing/c
                                      #:fill? any/c
                                      #:decode? any/c
                                      #:color (or/c pict-color/c #f)]
                                  #:rest (listof para-element/c)
                                  pict?)]
                       [elem (->* [] [#:decode? any/c
                                      #:color (or/c pict-color/c #f)]
                                  #:rest (listof para-element/c)
                                  pict?)]
                       [item (->* [] [#:width real?
                                      #:align para-align/c
                                      #:fill? any/c
                                      #:decode? any/c
                                      #:color (or/c pict-color/c #f)]
                                  #:rest (listof para-element/c)
                                  pict?)]
                       [resolve-para-spacing (->* [] [para-spacing/c] real?)]
                       [paras (->* [] [#:align para-align/c
                                       #:spacing para-spacing/c
                                       #:stage (or/c exact-integer? #f)]
                                   #:rest (listof pict?)
                                   pict?)]))

;; -----------------------------------------------------------------------------
;; slide assembler

(define current-slide-margin (make-parameter 20))

(define ((make-scaling-slide-assembler
          #:background-color [background-color "white"]
          #:annotate [annotate values])
         title-str gap content)
  (define background
    (inset (filled-rectangle (+ client-w (* margin 2))
                             (+ client-h (* margin 2))
                             #:draw-border? #f
                             #:color background-color)
           (- margin)))
  (define title (and title-str (~> ((current-titlet) title-str)
                                   (scale-to-fit client-w title-h))))
  (define content-area
    (~> (if title
            (blank (pict-width background)
                   (- (pict-height background)
                      (pict-height title)
                      gap))
            (ghost background))
        (inset (- (current-slide-margin)))))
  (define bounded-content (scale-to-fit content content-area #:mode 'inset))
  (define title+content (if title (vc-append gap title bounded-content) bounded-content))
  (cc-superimpose background
                  (inset (annotate (blank (+ client-w (* margin 2))
                                          (+ client-h (* margin 2))))
                         (- margin))
                  title+content))

;; -----------------------------------------------------------------------------
;; text and layout

(define current-text-color (make-parameter #f))

(define (tt s)
  (with-font (current-code-font)
    (with-size ((get-current-code-font-size)) (t s))))

(define para-spacing/c (or/c real? (list/c 'lines real?)))
(define para-align/c (or/c 'left 'center 'right))
(define para-element/c (flat-rec-contract elem/c
                         (or/c string? pict? (listof elem/c))))

(define current-para-spacing (make-parameter '(lines 0.2)))
(define current-para-align (make-parameter 'left))
(define current-para-fill? (make-parameter #t (λ (v) (and v #t))))
(define current-item-indent (make-lazy-parameter 0))

(define (para #:width [width (current-para-width)]
              #:align [align (current-para-align)]
              #:spacing [spacing (current-line-sep)]
              #:fill? [fill? (current-para-fill?)]
              #:decode? [decode? #t]
              #:color [color (current-text-color)]
              . elements)
  (parameterize ([current-line-sep (resolve-para-spacing spacing)])
    (~> (apply slideshow:para #:width width #:align align #:fill? fill? #:decode? decode? elements)
        (maybe-colorize color))))

(define (elem #:decode? [decode? #t]
              #:color [color (current-text-color)]
              . elements)
  (apply para #:width +inf.0 #:fill? #f #:decode? decode? #:color color elements))

(define (item #:width [width (current-para-width)]
              #:align [align (current-para-align)]
              #:indent [indent (current-item-indent)]
              #:fill? [fill? (current-para-fill?)]
              #:decode? [decode? #t]
              #:color [color (current-text-color)]
              . elements)
  (define bullet (htl-append (blank indent 0) (elem "→") (blank (em 0.75) 0)))
  (htl-append bullet
              (apply para #:width (- width (pict-width bullet))
                     #:align align #:fill? fill? #:decode? decode? elements)))

(define (resolve-para-spacing [spacing (current-para-spacing)])
  (match spacing
    [(? real?)       spacing]
    [(list 'lines n) (* (current-font-size) n)]))

(define (paras #:align [align (current-para-align)]
               #:spacing [spacing (current-para-spacing)]
               #:stage [stage #f]
               . elements)
  (apply (match align
           ['left vl-append]
           ['center vc-append]
           ['right vr-append])
         (resolve-para-spacing spacing)
         (for/list ([element (in-list elements)]
                    [i (in-naturals)])
           (pict-when (or (not stage) (< i stage)) element))))
