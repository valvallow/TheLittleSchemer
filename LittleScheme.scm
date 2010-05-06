; little scheme

(define atom? 
  (lambda (x) 
    (and (not (pair? x)) 
         (not (null? x)))))

(define add1 
  (lambda (n) 
    (+ n 1)))

(define sub1 
  (lambda (n) 
    (- n 1)))

(define first 
  (lambda (p) 
    (car p)))

(define second 
  (lambda (p) 
    (cadr p)))

(define third 
  (lambda (p) 
    (caddr p)))

(define build 
  (lambda (s1 s2) 
    (cons s1 (cons s2 '()))))

(define new-entry 
  (lambda (names values) 
    (build names values)))

(define extend-table 
  (lambda (entry table) 
    (cons entry table)))

(define text-of 
  (lambda (sexp) 
    (second sexp)))

(define table-of 
  (lambda (sexp) 
    (first sexp)))

(define formals-of 
  (lambda (sexp) 
    (second sexp)))

(define body-of 
  (lambda (sexp) 
    (third sexp)))

(define question-of 
  (lambda (sexp) 
    (first sexp)))

(define answer-of 
  (lambda (sexp) 
    (second sexp)))

(define cond-lines-of 
  (lambda (sexp) 
    (cdr sexp)))

(define function-of 
  (lambda (sexp) 
    (car sexp)))

(define arguments-of 
  (lambda (sexp) 
    (cdr sexp)))

(define primitive? 
  (lambda (l) 
    (eq? (first l) 'primitive)))

(define non-primitive? 
  (lambda (l) 
    (eq? (first l) 'non-primitive)))

(define else? 
  (lambda (x) 
    (eq? x 'else)))

(define lookup-in-entry 
  (lambda (name entry entry-f) 
    (lookup-in-entry-help name 
                          (first entry) 
                          (second entry) 
                          entry-f)))

(define lookup-in-entry-help 
  (lambda (name names values entry-f) 
    (cond ((null? names)(entry-f name)) 
          ((eq? (car names) name)(car values)) 
          (else (lookup-in-entry-help name 
                                      (cdr names) 
                                      (cdr values) 
                                      entry-f)))))

(define lookup-in-table 
  (lambda (name table table-f) 
    (if (null? table) 
        (table-f name) 
        (lookup-in-entry name 
                         (car table) 
                         (lambda (name) 
                           (lookup-in-table name 
                                            (cdr table) 
                                            table-f))))))

(define expression-to-action 
  (lambda (e) 
    (if (atom? e) 
        (atom-to-action e) 
        (list-to-action e))))

(define atom-to-action 
  (lambda (e) 
    (cond ((number? e) *const) 
          ((eq? e #t) *const) 
          ((eq? e #f) *const) 
          ((eq? e 'cons) *const) 
          ((eq? e 'car) *const) 
          ((eq? e 'cdr) *const) 
          ((eq? e 'null?) *const) 
          ((eq? e 'eq?) *const) 
          ((eq? e 'atom?) *const) 
          ((eq? e 'zero?) *const) 
          ((eq? e 'add1) *const) 
          ((eq? e 'sub1) *const) 
          ((eq? e 'number?) *const) 
          (else *identifer))))

(define list-to-action 
  (lambda (e) 
    (let ((op (car e))) 
      (if (atom? op) 
          (cond ((eq? op 'quote) *quote) 
                ((eq? op 'lambda) *lambda) 
                ((eq? op 'cond) *cond) 
                (else *application)) 
          *application))))

(define value 
  (lambda (e) 
    (meaning e '())))

(define meaning 
  (lambda (e table) 
    ((expression-to-action e) e table)))

(define initial-table 
  (lambda (name) 
    (car '())))

(define *const 
  (lambda (e table) 
    (cond ((number? e) e) 
          ((eq? e #t) #t) 
          ((eq? e #f) #f) 
          (else (build 'primitive e)))))

(define *quote 
  (lambda (e table) 
    (text-of e)))

(define *identifer 
  (lambda (e table) 
    (lookup-in-table e table initial-table)))

(define *lambda 
  (lambda (e table) 
    (build 'non-primitive 
           (cons table (cdr e)))))

(define evcon 
  (lambda (lines table) 
    (let ((front (car lines))) 
      (cond ((else? (question-of front)) 
             (meaning (answer-of front) table)) 
            ((meaning (question-of front) table) 
             (meaning (answer-of front) table)) 
            (else (evcon (cdr lines) table))))))

(define *cond 
  (lambda (e table) 
    (evcon (cond-lines-of e) table)))

(define evlis 
  (lambda (args table) 
    (if (null? args) 
        '() 
        (cons (meaning (car args) table) 
              (evlis (cdr args) table)))))

(define *application 
  (lambda (e table) 
    (*apply  (meaning (function-of e) table) 
             (evlis (arguments-of e) table))))

(define *apply 
  (lambda (fun vals) 
    (cond ((primitive? fun) (apply-primitive (second fun) vals)) 
          ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define apply-primitive 
  (lambda (name vals) 
    (let ((fst (first vals)) 
          (eqn? (lambda (q) 
                  (eq? name q)))) 
      (cond ((eqn? 'car) (car fst)) 
            ((eqn? 'cdr) (cdr fst)) 
            ((eqn? 'null?) (null? fst)) 
            ((eqn? 'atom?) (*atom? fst)) 
            ((eqn? 'zero?) (zero? fst)) 
            ((eqn? 'add1) (add1 fst)) 
            ((eqn? 'sub1) (sub1 fst)) 
            ((eqn? 'number?) (number? fst)) 
            ((eqn? 'cons) (cons fst (second vals))) 
            ((eqn? 'eq?) (eq? fst (second vals)))))))

(define *atom? 
  (lambda (x) 
    (cond ((atom? x) #t) 
          ((null? x) #f) 
          ((eq? (car x) 'primitve) #t) 
          ((eq? (car x) 'non-primitive) #f) 
          (else #f))))

(define apply-closure 
  (lambda (closure vals) 
    (meaning (body-of closure) 
             (extend-table (new-entry (formals-of closure) vals) 
                           (table-of closure)))))


(value '(((lambda (f) 
            (f f)) 
          (lambda (f) 
            (lambda (n) 
              (cond ((zero? n) 1) 
                    (else (((lambda (f) 
                              (f f)) 
                            (lambda (f) 
                              (lambda (n m) 
                                (cond ((zero? m) 0) 
                                      (else (((lambda (f) 
                                                (f f)) 
                                              (lambda (f) 
                                                (lambda (n m) 
                                                  (cond ((zero? m) n) 
                                                        (else ((lambda (n)(add1 n)) ((f f) n ((lambda (n)(sub1 n)) m)))))))) 
                                             n ((f f) n ((lambda (n)(sub1 n)) m)))))))) 
                           n ((f f) ((lambda (n)(sub1 n)) n)))))))) 5))

; y
(lambda (g) 
  ((lambda (f) 
     (f f)) 
   (lambda (f) 
     (g (lambda (x) 
          ((f f) x))))))

; dbl
(value '(((lambda (g) 
            ((lambda (f) 
               (f f)) 
             (lambda (f) 
               (g (lambda (x) 
                    ((f f) x)))))) 
          (lambda (f) 
            (lambda (n) 
              (cond ((zero? n) n) 
                    (else (add1 (add1 (f (sub1 n))))))))) 5))

