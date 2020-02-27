#lang racket

(require "data-structure.rkt")
(require "scaner.rkt")

(provide (all-defined-out))

(struct success (result rest) #:transparent)
(struct failure (rest) #:transparent)

(define succeed
  (lambda (val)
    (lambda (str)
      (success val str))))

(define @literial
  (lambda (s)
    (lambda (toks)
      (cond
        [(null? toks) (failure '())]
        [else
         (cond
           [(and (token? (car toks))
                 (string=? (Node-elts (car toks)) s))
            (success (list (car toks)) (cdr toks))]
           [else
            (failure toks)])]))))

;; -----------------------
;; parser combinator

(define @!
  (lambda (ps)
    (lambda (toks)
      (match (ps toks)
        [(success result rest)
         (failure toks)]
        [(failure rest)
         (success (list (car toks)) (cdr toks))]))))

(define @seq
  (lambda ps
    (lambda (toks)
      (let loop ([toks toks] [ps ps] [nodes '()])
        (cond
          [(null? ps)
           (success (apply append (reverse nodes)) toks)]
          [else
           (match ((car ps) toks)
             [(failure rest)
              (failure toks)]
             [(success result rest)
              (loop rest (cdr ps) (cons result nodes))])])))))

(define @or
  (lambda ps
    (lambda (toks)
      (let loop ([toks toks] [ps ps])
        (cond
          [(null? ps)
           (failure toks)]
          [else
           (match ((car ps) toks)
             [(failure rest)
              (loop toks (cdr ps))]
             [(success result rest)
              (success result rest)])])))))

(define @and
  (lambda ps
    (lambda (toks)
      (let loop ([ps ps] [res '()])
        (cond
          [(null? ps)
           (first res)]
          [else
           (match ((car ps) toks)
             [(failure rest)
              (failure toks)]
             [(success result rest)
              (loop (cdr ps) (cons (success result rest) res))])])))))
(define @*
  (lambda (p)
    (lambda (toks)
      (let loop ([toks toks] [nodes '()])
        (cond
          [(null? toks)
           (success (apply append (reverse nodes))
                    '())]
          [else
           (match (p toks)
             [(failure rest)
              (success (apply append (reverse nodes))
                       rest)]
             [(success result rest)
              (loop rest (cons result nodes))])])))))
(define @tag
  (lambda (type p)
    (lambda (toks)
      (match (p toks)
        [(success result rest)
         (success (list (Node type
                              (Node-start (first result))
                              (Node-end (last result))
                              (filter (lambda (n) (not (paren? n))) result)
                              #f #f))
                  rest)]
        [(failure rest)
         (failure rest)]))))
;;---------------------------
;; sexp parse


(define-syntax-rule (define-parser name body)
  (define name
    (lambda (toks)
      ;; (pretty-print name)
      ;; (pretty-print toks)
      (body toks))))

(define-parser $start
  (@tag 'paren
        (@literial "(")))

(define-parser $end
  (@tag 'paren
        (@literial ")")))

(define-parser $atom
  (@and (@! $start) (@! $end)))


(define-parser $sexp
  (@or $parens $atom))

(define-parser $parens
  (@tag 'sexp
        (@seq $start (@* $sexp) $end)))

(define parse-sexp
  (lambda (s)
    (let ([toks (scan s)])
      (match ($sexp toks)
        [(success result rest)
         result]
        [(failure rest)
         (failure rest)]))))

(define parse-test
  (lambda (p s)
    (p (scan s))))

;; (parse-sexp "(+ 1 (/ 4 2))")
;; (parse-test $open "(+ 1 2)")
;; (parse-test $close ")")
;; (parse-test $close "())")
;; (parse-test $non-parens "(+ 1 2)")
;; (parse-test $non-parens "2")
;; (parse-test (@or $open $close) "()")
;; (parse-sexp "(+ 1 (/ 4 2))")
;; (parse-test (@* $open) "(((())))")

;; (parse-test (@seq $open $close) "()")

