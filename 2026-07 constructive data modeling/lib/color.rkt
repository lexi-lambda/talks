#lang racket/base

(require (for-syntax racket/base
                     racket/struct-info)
         racket/class
         racket/contract
         racket/draw
         racket/match
         threading)

(provide (rename-out [rgb* rgb] [hsv* hsv])
         rgb-red rgb-green rgb-blue rgb-alpha
         hsv-hue hsv-saturation hsv-value hsv-alpha

         (contract-out
          [color? predicate/c]
          [->color% (-> color? (is-a?/c color%))]
          [->rgb (-> color? rgb?)]
          [->hsv (-> color? hsv?)]

          [scale-color-value (-> color? (>=/c 0) color?)]
          [add-color-value (-> color? real? color?)]))

;; -----------------------------------------------------------------------------

(define (fmod x n)
  (define i (floor x))
  (+ (remainder i n) (- x i)))

(struct rgb (red green blue alpha) #:transparent
  #:guard (struct-guard/c (real-in 0 1) (real-in 0 1) (real-in 0 1) (real-in 0 1)))
(struct hsv (hue saturation value alpha) #:transparent
  #:guard (struct-guard/c (real-in 0 1) (real-in 0 1) (real-in 0 1) (real-in 0 1)))

(define (make-rgb r g b [alpha 1.0])
  (rgb r g b alpha))
(define (make-hsv h s v [alpha 1.0])
  (define h* (- h (truncate h)))
  (hsv (if (< h* 0.0) (+ 1.0 h*) h*) s v alpha))

(begin-for-syntax
  (struct struct-info (ctor-id list field-syms) #:transparent
    #:property prop:struct-info (λ (self) (struct-info-list self))
    #:property prop:struct-field-info (λ (self) (struct-info-field-syms self))
    #:property prop:expansion-contexts '(expression)
    #:property prop:procedure
    (λ (self stx)
      (define ctor-id (struct-info-ctor-id self))
      (syntax-case stx ()
        [id (identifier? #'id) ctor-id]
        [(_ . args) (datum->syntax stx (cons ctor-id #'args) stx)])))
  (define (make-custom-ctor-struct-info base-id ctor-id)
    (define base-info (syntax-local-value base-id))
    (define base-list (extract-struct-info base-info))
    (define base-fields (struct-field-info-list base-info))
    (struct-info ctor-id
                 (list* (car base-list) ctor-id (cddr base-list))
                 base-fields)))

(define-syntax rgb* (make-custom-ctor-struct-info #'rgb #'make-rgb))
(define-syntax hsv* (make-custom-ctor-struct-info #'hsv #'make-hsv))

(define (hsv->rgb h s v [alpha 1.0])
  (define (f n)
    (define k (fmod (+ n (* h 6.0)) 6))
    (- v (* v s (max 0.0 (min k (- 4.0 k) 1.0)))))
  (rgb (f 5.0) (f 3.0) (f 1.0) alpha))

(define (rgb->hsv r g b [alpha 1.0])
  (define v (max r g b))
  (define chroma (- v (min r g b)))
  (define h (cond
              [(zero? chroma) 0.0]
              [(= v r) (/        (/ (- g b) chroma)  6.0)]
              [(= v g) (/ (+ 2.0 (/ (- b r) chroma)) 6.0)]
              [else    (/ (+ 4.0 (/ (- r g) chroma)) 6.0)]))
  (define s (if (zero? chroma) 0 (/ chroma v)))
  (hsv h s v alpha))

;; -----------------------------------------------------------------------------

(define (color? v)
  (or (is-a? v color%)
      (rgb? v)
      (hsv? v)
      (and (string? v)
           (send the-color-database find-color v)
           #t)))

(define (find-color% who name)
  (or (send the-color-database find-color name)
      (raise-arguments-error who "no known color with name" "name" name)))

(define (->color% v)
  (match v
    [(? (λ~> (is-a? color%))) v]
    [(rgb r g b a)
     (define (f n) (inexact->exact (round (max 0 (min (* n 255) 255)))))
     (make-color (f r) (f g) (f b) a)]
    [(hsv h s v a)
     (->color% (hsv->rgb h s v a))]
    [(? string?)
     (find-color% '->color% v)]))

(define (->rgb v)
  (match v
    [(? rgb?) v]
    [(? (λ~> (is-a? color%)))
     (rgb (/ (send v red) 255.0)
          (/ (send v green) 255.0)
          (/ (send v blue) 255.0)
          (send v alpha))]
    [(hsv h s v a)
     (hsv->rgb h s v a)]
    [(? string?)
     (->rgb (find-color% '->rgb v))]))

(define (->hsv v)
  (match v
    [(? hsv?) v]
    [(? (λ~> (is-a? color%)))
     (rgb->hsv (/ (send v red) 255.0)
               (/ (send v green) 255.0)
               (/ (send v blue) 255.0)
               (send v alpha))]
    [(rgb r g b a)
     (rgb->hsv r g b a)]
    [(? string?)
     (->hsv (find-color% '->hsv v))]))

;; -----------------------------------------------------------------------------

(define (scale-color-value color fac)
  (match-define (hsv h s v a) (->hsv color))
  (hsv h s (min (* v fac) 1) a))

(define (add-color-value color amt)
  (match-define (hsv h s v a) (->hsv color))
  (hsv h s (max 0 (min (+ v amt) 1)) a))
