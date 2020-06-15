#lang racket/base

(require racket/contract
         racket/function
         racket/math
         syntax/parse/define
         threading)

(provide when~>
         (contract-out [make-lazy-parameter (-> any/c parameter?)]
                       [turns (-> real? real?)]))

(define (make-lazy-parameter val)
  (make-derived-parameter (make-parameter val)
                          identity
                          (λ (v) (if (procedure? v) (v) v))))

(define-simple-macro (when~> e:expr c:expr s:expr ...)
  (let ([v e]) (if c (~> v s ...) v)))

(define (turns n) (* 2 pi n))
