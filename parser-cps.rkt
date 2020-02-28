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

(define @literal
  (lambda (s)
    (lambda (toks cont)
      (cond
        [(null? toks) (cont (failure '()))]
        [else
         (cond
           [(and (token? (car toks))
                 (string=? (Node-elts (car toks)) s))
            (cont (success (list (car toks)) (cdr toks)))]
           [else
            (cont (failure toks))])]))))

;; -----------------------
;; parser combinator

(define @not
  (lambda (ps)
    (lambda (toks cont)
      (ps toks (lambda (v)
                 (pretty-print "@not-cont")
                 (match v
                   [(success result rest)
                    (cont (failure toks))]
                   [(failure rest)
                    (cont (success (list (car toks))
                                   (cdr toks)))]))))))

(define @seq
  (lambda (ps [nodes '()])
    (lambda (toks cont)
      (cond
        [(null? ps)
         (cont (success (apply append (reverse nodes)) toks))]
        [else
         ((car ps) toks (lambda (v)
                          (pretty-print "@seq-cont")
                          (match v
                            [(success result rest)
                             ((@seq (cdr ps) (cons result nodes))
                              rest cont)]
                            [failure
                             (cont failure)])))]))))

(define @or
  (lambda (ps)
    (lambda (toks cont)
      (cond
        [(null? ps) (cont (failure toks))]
        [else
         ((car ps) toks (lambda (v)
                          (pretty-print "@or-cont")
                          (match v
                            [(success result rest)
                             (cont (success result rest))]
                            [(failure rest)
                             ((@or (cdr ps)) toks cont)])))]))))
(define @and
  (lambda (ps [res '()])
    (lambda (toks cont)
      (cond
        [(null? ps)
         (cont (first res))]
        [else
         ((car ps) toks (lambda (v)
                          (pretty-print "@and-cont")
                          (match v
                            [(success result rest)
                             ((@and (cdr ps) (cons (success result rest) res)) toks cont)]
                            [failure
                             (cont failure)])))]))))
(define @*
  (lambda (p [nodes '()])
    (lambda (toks cont)
      (cond
        [(null? toks)
         (cont (success (apply append (reverse nodes))
                        '()))]
        [else
         (p toks (lambda (v)
                   (pretty-print "@*-cont")
                   (match v
                     [(success result rest)
                      (cons result nodes)
                      ((@* p (cons result nodes)) rest cont)]
                     [(failure rest)
                      (cont (success (apply append (reverse nodes))
                                      rest))])))]))))
(define @tag
  (lambda (type p)
    (lambda (toks cont)
      (p toks (lambda (v)
                (pretty-print "@tag-cont")
                (match v
                  [(success result rest)
                   (cont (success (list (Node type
                                              (Node-start (first result))
                                              (Node-end (last result))
                                              (filter (lambda (n) (not (paren? n))) result)
                                              #f #f))
                                  rest))]
                  [failure
                   (cont failure)]))))))
;;---------------------------
;; sexp parse


(define-syntax-rule (define-parser name body)
  (define name
    (lambda (toks cont)
      ;; (pretty-print name)
      ;; (pretty-print toks)
      (body toks cont))))

(define-parser $start
  (@tag 'paren
        (@literal "(")))

(define-parser $end
  (@tag 'paren
        (@literal ")")))

(define-parser $atom
  (@and (list (@not $start) (@not $end))))


(define-parser $sexp
  (@or (list $parens $atom)))

(define-parser $parens
  (@tag 'sexp
        (@seq (list $start (@* $sexp) $end))))

(define parse-sexp
  (lambda (s)
    (let ([toks (scan s)])
      ($sexp toks (lambda (v)
                    (pretty-print "end-cont")
                    (match v
                      [(success result rest)
                       result]
                      [failure failure]))))))

(define parse-test
  (lambda (p s)
    (p (scan s))))

(parse-sexp "(+ 1 (/ 4 2))")
;; (parse-test $open "(+ 1 2)")
;; (parse-test $close ")")
;; (parse-test $close "())")
;; (parse-test $non-parens "(+ 1 2)")
;; (parse-test $non-parens "2")
;; (parse-test (@or $open $close) "()")
;; (parse-sexp "(+ 1 (/ 4 2))")
;; (parse-test (@* $open) "(((())))")

;; (parse-test (@seq $open $close) "()")

