#lang racket/base

(require racket/class
         racket/contract
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         threading

         racket/draw
         (rename-in pict [colorize pict:colorize])
         pict/conditional
         pict/convert
         ppict/align

         (rename-in slideshow/base [current-font-size current-text-size] [slide slideshow:slide])
         slideshow/code)

(provide (all-from-out racket/class racket/contract racket/function racket/list racket/match threading
                       racket/draw pict pict/conditional ppict/align
                       slideshow/base slideshow/code)
         make-lazy-parameter
         align->superimpose
         pict-when
         pict-unless

         (contract-out
          [color (-> color? (or/c #f string? (is-a?/c color%)))]
          [colorize (-> pict-convertible? color? pict?)]

          [current-text-color (parameter/c color?)]
          [current-title-text-color (parameter/c color?)]
          [t (->* [] [#:align halign/c] #:rest (listof (or/c string? pict?)) pict?)]
          [tt (-> string? pict?)]
          [itt (-> string? pict?)]
          [btt (-> string? pict?)]))

;; ---------------------------------------------------------------------------------------------------
;; miscellaneous helpers

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

;; ---------------------------------------------------------------------------------------------------
;; sizing and alignment

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

;; ---------------------------------------------------------------------------------------------------
;; staging

(define (pict-when test then)
  (pict-if test then (blank)))
(define (pict-unless test then)
  (pict-if test (blank) then))

;; ---------------------------------------------------------------------------------------------------
;; colors

(define color?
  (flat-named-contract
   'color?
   (or/c #f string? (is-a? color%) (integer-in 0 #xFFFFFF))))

(define (color v)
  (match v
    [(or #f (? string?)) v]
    [(? exact-integer?) (make-color (bitwise-bit-field v 16 24)
                                    (bitwise-bit-field v 8 16)
                                    (bitwise-bit-field v 0 8))]))

(define (colorize p c) (if color (pict:colorize p (color c)) p))

;; ---------------------------------------------------------------------------------------------------
;; text

(define current-text-color (make-parameter #f))
(define current-title-text-color (make-lazy-parameter current-text-color))

(define (t #:align [align 'l] . args)
  (~>> (for/fold ([lines '()] [line '()] #:result (reverse (cons (reverse line) lines)))
                 ([arg (in-list args)])
         (match arg
           ["\n" (values (cons (reverse line) lines) '())]
           [(? string?) (values lines (~> (text arg (current-main-font) (current-text-size))
                                          (colorize (current-text-color))
                                          (cons line)))]
           [_ (values lines (cons arg line))]))
       (map (λ~>> (apply hbl-append)))
       (apply (halign->vcompose align))))

(define (tt s) (text s (current-code-font) (current-text-size)))
(define (itt s) (text s (cons 'italic (current-code-font)) (current-text-size)))
(define (btt s) (text s (cons 'bold (current-code-font)) (current-text-size)))
