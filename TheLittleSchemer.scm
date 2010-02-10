;;; The Little Schemer

; 1.Toys
;   atom, s-expression, car, cdr, cons, null?, eq?
; 2.Do It, Do It, Again, and Again, and Again ...
;   lat?, member?
; 3.Cons the Magnificent
;   rember, firsts, insertR, insertL, subst, subst2, multirember, multiinsertR, multiinsertL, multisubst
; 4.Numbers Games
;   add1, sub1, o+, o-, o*, o/, tup?, addtup, tup+, o>, o<, o=, o^, length, pick, rempick, no-nums, all-nums, egan?, occur, one?
; 5.*Oh My Gawd* : It's Full of Stars
;   rember*, insertR*, insertL*, occur*, subst*, member*, letmost, eqlist?, equal?
; 6.Shadows
;   numbered?, operator, 1st-sub-exp, 2nd-sub-exp, value, sero?, edd1, zub1
; 7.Friends and Relations
;   set?, makeset, subset?, eqset?, intersct?, intersect, union, intersect-all, a-pair?, first, second, third, build, fun?, revrel, revpair, seconds, fullfun?
; 8.Lambda the Ultimate
;   rember-f, insertL-f, insertR-f, insert-g, atom-to-function, multirember-f, multiremberT, multirember&co, multiinsertLR, multiinsertLR&co, even?, evens-onsly*, evens-only*&co
; 9....and Again, and Again, and Again, ...
;   looking, keep-looking, eternity, shift, align, length*, weight*, shuffle, C(collatz), A(ackerman), Y(Y Combinator)
; 10.What Is the Value of All of This ?
; new-entry, lookup-in-entry, lookup-in-entry-help, extend-table, lookup-in-table, expression-to-action, atom-to-action, list-to-action, value, meaning, *const, *quote, text-of, *identifer, initial-table, *lambda, table-of, formals-of, body-of, evcon, else?, question-of, answer-of, *cond, cond-lines-of, evlis, *application, function-of, arguments-of, primitive?, non-primitive?, *apply, apply-primitive, *atom?, apply-closure

;; ------------------------------------------------------------
;; atom?
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

; http://my-snippet.appspot.com/snippet/show/1079
(define atom?
  (lambda (x)
    (not (or (pair? x)
             (null? x)))))

;; chapter 2

;; ------------------------------------------------------------
;; lat?
;; list of atoms ?
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l))
      (lat? (cdr l)))
     (else #f))))

; http://my-snippet.appspot.com/snippet/show/1079
(define lat?
  (lambda (l)
    (or (null? l)
        (and (atom? (car l))
             (lat? (cdr l))))))

(lat? '(a b c d))
(lat? '())
(lat? '(()))
(lat? '(a b c (d e f)))

;; ------------------------------------------------------------
;; member?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? a (car lat))
               (member? a (cdr lat)))))))


; chapter 3


;; ------------------------------------------------------------
;; rember
;; remove member
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) lat)
     ((eq? a (car lat))(cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

;; ------------------------------------------------------------
;; firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) l)
     ((and (list? (car l))(not (null? (car l))))
      (cons (caar l)
            (firsts (cdr l))))
     (else (firsts (cdr l))))))


;; ------------------------------------------------------------
;; insertR
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? old (car lat))
      (cons old
            (cons new
                  (insertR new old (cdr lat)))))
     (else (cons (car lat)
                 (insertR new old (cdr lat)))))))
(insertR 'a 1 '(1 2 3 4 1 2 3 4 1 2 3 4))

;; ------------------------------------------------------------
;; insertL
(define insertL
  (lambda (new old lat)
    (cond ((null? lat) lat)
          ((eq? old (car lat)) (cons new
                                     (cons (car lat)
                                           (insertL new old (cdr lat)))))
          (else (cons (car lat)
                      (insertL new old (cdr lat)))))))

(insertL 'x 'a '(a b c a b c a b c))

;; ------------------------------------------------------------
;; subst
(define subst
  (lambda (new old lat)
    (cond ((null? lat) lat)
          ((eq? old (car lat)) (cons new
                                     (subst new old (cdr lat))))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))

(subst 'a '1 '(1 b c d 1 b c d))

; http://my-snippet.appspot.com/snippet/show/9001
(define (subst new old lat)
  (map (lambda (x)
         (if (eq? x old)
             new
             x)) lat))


;; ------------------------------------------------------------
;; subst2
(define subst2
  (lambda (new o1 o2 lat)
    (if (null? lat)
        lat
        (let ((a (car lat)))
          (cond ((eq? a o1) (cons new (cdr lat)))
                ((eq? a o2) (cons new (cdr lat)))
                (else (cons a (subst2 new o1 o2 (cdr lat)))))))))


(subst2 'a 'b 'c '(a b c d e))
(subst2 'a '1 'c '(a b c d e))
(subst2 'a 'b 'c '(e e e e e))


;; ------------------------------------------------------------
;; multirember
(define multirember
  (lambda (a lat)
    (cond ((null? lat) lat)
          ((eq? a (car lat)) (multirember a (cdr lat)))
          (else (cons (car lat)(multirember a (cdr lat)))))))

(multirember 'a '(a b c))
(multirember 'a '(a b c a b c a b c))


;; ------------------------------------------------------------
;; multiinsertR

;; ------------------------------------------------------------
;; multiinsertL

;; ------------------------------------------------------------
;; multisubst


; chapter 4

;; ------------------------------------------------------------
;; add1
(define add1
  (lambda (n)
    (+ n 1)))

;; ------------------------------------------------------------
;; sub1
(define sub1
  (lambda (n)
    (- n 1)))

;; ------------------------------------------------------------
;; o+
(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (add1 (+o n (sub1 m)))))))

(define o+
  (lambda (n m)
    (cond ((zero? m) n)
          (else (o+ (add1 n)(sub1 m))))))

;; ------------------------------------------------------------
;; o-
(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (sub1 (o- n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond ((zero? m) n)
          (else (o- (sub1 n)(sub1 m))))))


;; ------------------------------------------------------------
;; tup?
;; tup -> tuple -> list of numbers
(define tup?
  (lambda (tup)
    (or (null? tup)
        (and (number? (car tup))
             (tup? (cdr tup))))))

(tup? '(1 2 3 4 5))
(tup? '(1 2 3 4 a))
(tup? '(a))
(tup? '(()))
(tup? '())

;; ------------------------------------------------------------
;; addtup
;; summary of tuple
(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (+ (car tup)
                   (addtup (cdr tup)))))))
(addtup '(1 2 3 4 5 6 7 8 9 10))

(define addtup
  (lambda (tup)
    (apply + tup)))

(addtup '(1 2 3 4 5 6 7 8 9 10))


;; ------------------------------------------------------------
;; o*
(define o*
  (lambda (n m)
    (cond ((zero? m) 0)
          (else (o+ n (o* n (sub1 m)))))))

;; ------------------------------------------------------------
;; tup+
;; tup1 + tup2
(define tup+
  (lambda (tup1 tup2)
    (cond ((null? tup1) tup2)
          ((null? tup2) tup1)
          (else (cons (+ (car tup1)
                         (car tup2))
                      (tup+ (cdr tup1)
                            (cdr tup2)))))))
(tup+ '(1 2 3)'(1 2 3))
(tup+ '(3 6 9 11 4)'(8 5 2 0 7))
(tup+ '()'(1))
(tup+  '(1)'())
(tup+ '() '())


;; ------------------------------------------------------------
;; >
;; o>
(define o>
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (o> (sub1 n)(sub1 m))))))

(o> 1 2)
(o> 2 1)


;; ------------------------------------------------------------
;; <
;; o<
(define o<
  (lambda  (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (o< (sub1 n)(sub1 m))))))

(o< 0 0)
(o< 1 0)
(o< 10 11)


;; ------------------------------------------------------------
;; =
;; o=
(define o=
  (lambda (n m)
    (cond ((o> n m) #f)
          ((o< n m) #f)
          (else #t))))

;; (define o=
;;   (lambda (n m)
;;     (not (and (o> n m)
;;               (o< n m)))))

(o= 10 19)
(o= 0 0)
(o= 10 11)
(o= 11 10)
(o= 5 5)


;; ------------------------------------------------------------
;; o^
;; expt
(define o^
  (lambda (n m)
    (cond ((zero? m) 1)
          (else (o* n (o^ n (sub1 m)))))))

(o^ 2 3)
(o^ 5 3)


;; ------------------------------------------------------------
;; o/
(define o/
  (lambda (n m)
    (cond ((o< n m) 0)
          (else (add1 (o/ (o- n m) m))))))

(o/ 15 4)
(o/ 21 3)


;; ------------------------------------------------------------
;; length
;; count of list elements
(define my-length
  (lambda (ls)
    (cond ((null? ls) 0)
          (else (add1 (my-length (cdr ls)))))))
(my-length '())
(my-length '(1 2 3))


;; ------------------------------------------------------------
;; pick
;; nth element of lat (start 1~)
(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n))(car lat))
          (else (pick (sub1 n)(cdr lat))))))


;; ------------------------------------------------------------
;; rempick
;; remove pick
(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n))(cdr lat))
          (else (cons (car lat)
                      (rempick (sub1 n)
                               (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard))


;; ------------------------------------------------------------
;; no-nums
;; remove numers of lat
(define no-nums
  (lambda (lat)
    (cond ((null? lat) lat)
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat)(no-nums (cdr lat)))))))

(no-nums '(1 2 3 4 5))
(no-nums '(5 pears 6 prunes 9 dates))


;; ------------------------------------------------------------
;; all-nums
;; extract a tup from lat
(define all-nums
  (lambda (lat)
    (cond ((null? lat) lat)
          ((number? (car lat)) (cons (car lat)
                                     (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(all-nums '(1 2 3 4 5))
(all-nums '(1 a 2 b 3 c 4 d 5 e))


;; ------------------------------------------------------------
;; egan?
;; same atom?
(define egan?
  (lambda (a1 a2)
    (cond ((and (number? a1)(number? a2)) (o= a1 a2))
          ((or (number? a1)(number? a2)) #f)
          (else (eq? a1 a2)))))

(egan? 'a 'a)
(egan? 1 2)
(egan? 2 1)
(egan? 1 1)

(o= 0 0)
(o= 1 2)
(o= 2 1)

;; ------------------------------------------------------------
;; occur
;; number of times an atom in a lat
(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eq? a (car lat))(add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

(occur 1 '(1 2 3 1 2 3 1 2 3 1))
(occur 'a '(a b c))


;; ------------------------------------------------------------
;; one?
(define one?
  (lambda (n)
    (= n 1)))


; chapter 5

;; ------------------------------------------------------------
;; rember*
;; remove member star
(define rember*
  (lambda (a l)
    (cond ((null? l) l)
          ((atom? (car l)) (cond ((eq? a (car l))(rember* a (cdr l)))
                                 (else (cons (car l)(rember* a (cdr l))))))
          (else (cons (rember* a (car l))
                      (rember* a (cdr l)))))))

(rember* 'a '(a b c (a b c) (a b c) ((a b c (a b c)) (a b c)) a b c (a b c)))


;; ------------------------------------------------------------
;; insertR*
(define insertR*
  (lambda (new old l)
    (cond ((null? l) l)
          ((atom? (car l)) (cond ((eq? old (car l)) (cons (car l)
                                                          (cons new
                                                                (insertR* new old (cdr l)))))
                                 (else (cons (car l)
                                             (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

(insertR* 'a '1 '((1 2 3 4 (1 2 3 4 (1 2 3 4) 1 2 3 4)(1 2 3 4 (1 2 3 4 (1 2 3 4)))) 1 2 3 4))


;; ------------------------------------------------------------
;; occur*
(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l)) (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
                                 (else (occur* a (cdr l)))))
          (else (o+ (occur* a (car l))
                    (occur* a (cdr l)))))))

(occur* 1 '(1 2 3 4 (1 2 3 4) (1 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4)(1 2 3 4)) 1 2 3 4)) 1 2 3 4 (1 2 3 4)))


;; ------------------------------------------------------------
;; subst*
(define subst*
  (lambda (new old l)
    (cond ((null? l) l)
          ((atom? (car l)) (cond ((eq? old (car l))(cons new
                                                         (subst* new old (cdr l))))
                                 (else (cons (car l)
                                             (subst* new old (cdr l))))))
          (else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))

(subst* 'a 1 '(1 2 3 4 (1 2 3 4) (1 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4)(1 2 3 4)) 1 2 3 4)) 1 2 3 4 (1 2 3 4)))


;; ------------------------------------------------------------
;; insertL*
(define insertL*
  (lambda (new old l)
    (cond ((null? l) l)
          ((atom? (car l))(cond ((eq? old (car l))(cons new
                                                        (cons (car l)
                                                              (insertL* new old (cdr l)))))
                                (else (cons (car l)
                                            (insertL* new old (cdr l))))))
          (else (cons (insertL* new old (car l))
                      (insertL* new old (cdr l)))))))


(insertL* 'a 1 '(1 2 3 4 (1 2 3 4) (1 2 3 4 (1 2 3 4 (1 2 3 4 (1 2 3 4)(1 2 3 4)) 1 2 3 4)) 1 2 3 4 (1 2 3 4)))


;; ------------------------------------------------------------
;; member*
(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l)) (or (eq? a (car l))
                               (member* a (cdr l))))
          (else (or (member* a (car l))
                    (member* a (cdr l)))))))

(member* 1 '(2 2 2 2 (3 3 3 3 3 (4 4 4 4 (5 5 5 (6 6 6)(7 7 7)(8 8 8))) 9 9 9) 10 10 10))
(member* 1 '(2 2 2 2 (3 3 3 3 3 1 (4 4 4 4 (5 5 5 (6 6 6)(7 7 7)(8 8 8))) 9 9 9) 10 10 10))
(member* 1 '(2 2 2 2 (3 3 3 3 3 (4 4 4 4 (5 5 5 (6 6 6)(7 7 1 7)(8 8 8))) 9 9 9) 10 10 10))


;; ------------------------------------------------------------
;; leftmost
(define leftmost
  (lambda (l)
    (cond ((atom? (car l)) (car l))
          (else (leftmost (car l))))))

(leftmost '((potato)(chips ((with) fish)(chips))))


;; ------------------------------------------------------------
;; eqlist?
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1)
                (null? l2)) #t)
          ((or (null? l1)(null? l2)) #f)
          ((and (atom? (car l1))
                (atom? (car l2))) (and (egan? (car l1)(car l2))
                                       (eqlist? (cdr l1)(cdr l2))))
          (else (and (eqlist? (car l1)(car l2))
                     (eqlist? (cdr l1)(cdr l2)))))))

(eqlist? '(1 a b (c d (2 3 4 (5 e f)(g h 6)) 7 i))
         '(1 a b (c d (2 3 4 (5 e f)(g h 6)) 7 i)))


(eqlist? '(1 a b (c d (2 3 4 (5 e f)(g h 6)) 7 i))
         '(1 a b))

(eqlist? '(1) '(2))
(eqlist? '() '(1))


;; ------------------------------------------------------------
;; equal?
;; my-equal?
(define my-equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1)
                (atom? s2)) (egan? s1 s2))
          ((or (atom? s1)(atom? s2) #f))
          (else (eqlist? s1 s2)))))

(my-equal? '() '(1))
(my-equal? '(2) '(1))
(my-equal? '(1 2 3 (4 5 6 (7 8 9) 10))
           '(1 2 3 (4 5 6 (7 8 9) 10)))


;; ------------------------------------------------------------
;; rewrite eqlist?
(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1)
                (null? l2)) #t)
          ((or (null? l1)
               (null? l2)) #f)
          (else (and (equal? (car l1)
                             (car l2))
                     (eqlist? (cdr l1)
                              (cdr l2)))))))


; chapter 6

;; ------------------------------------------------------------
;; numbered?
(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp)(number? aexp))
          (else (and (numbered? (car aexp))
                     (numbered? (caddr aexp)))))))

(numbered? '(3 + (4 * 5)))
(numbered? '(+ 3 (* 4 5)))
(numbered? '(3 + hoge))


;; ------------------------------------------------------------
;; operator, 1st-sub-exp, 2nd-sub-exp
(define operator
  cadr)
(define 1st-sub-exp
  car)
(define 2nd-sub-exp
  caddr)

;; ------------------------------------------------------------
;; value
(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp))
                                       (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp))
                                       (value (2nd-sub-exp nexp))))
          (else (o^ (value (1st-sub-exp nexp))
                    (value (2nd-sub-exp nexp)))))))

(value '(1 + (2 * 5)))
(value '((2 ^ 3) * 5))
(value '(4 + ((2 ^ 3) * 5)))


;; ------------------------------------------------------------
;; sero?
(define sero?
  (lambda (n)
    (null? n)))

(define sero?
  null?)


;; ------------------------------------------------------------
;; edd1
(define edd1
  (lambda (n)
    (cons '() n)))

(edd1 '(()()))


;; ------------------------------------------------------------
;; zub1
(define zub1
  (lambda (n)
    (cdr n)))

(zub1 '(() () ()))



;; ------------------------------------------------------------
;; set?
(define set?
  (lambda (lat)
    (cond ((null? lat) #t)
          ((member? (car lat)(cdr lat)) #f)
          (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))
(set? '(apples peaches pluums))


;; ------------------------------------------------------------
;; makeset
(define makeset
  (lambda (lat)
    (cond ((null? lat) lat)
          (else (cons (car lat)
                      (makeset (multirember (car lat)
                                            (cdr lat))))))))

(makeset '(apple peach  pear peach plum apple lemon peach))


;; ------------------------------------------------------------
;; subset?
(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          ((member? (car set1) set2)(subset? (cdr set1) set2))
          (else #f))))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1)
                              set2)
                     (subset? (cdr set1)
                              set2))))))

(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and light duckling wings))


;; ------------------------------------------------------------
;; eqset?
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(1 2 3) '(3 2 1))


;; ------------------------------------------------------------
;; intersect?
(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) #f)
          (else (or (member? (car set1) set2)
                    (intersect? (cdr set1) set2))))))

(intersect? '(1 2 3) '(4 5))

;; ------------------------------------------------------------
;; intersect
(define intersect
  (lambda (set1 set2)
    (cond ((or (null? set1)(null? set2)) '())
          ((member? (car set1) set2) (cons (car set1)
                                           (intersect (cdr set1)
                                                      set2)))
          (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))


;; ------------------------------------------------------------
;; union
(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))


;; ------------------------------------------------------------
;; intersect-all
(define intersect-all
  (lambda (l-set)
    (cond ((null? (cdr l-set))(car l-set))
          (else (intersect (car l-set)
                           (intersect-all (cdr l-set)))))))

(intersect-all '((6 pears and)
                 (3 peaches and 6 peppers)
                 (8 pears and 6 plums)
                 (and 6 prunes with some apples)))


;; ------------------------------------------------------------
;; a-pair?
(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cddr x)) #t)
          (else #f))))

(define a-pair?
  (lambda (x)
    (and (not (or (atom? x)
                  (null? x)
                  (null? (cdr x))))
         (null? (cddr x)))))

(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2)(pair)))
(a-pair? '(full (house)))
(a-pair? '(a))
(a-pair? '())
(a-pair? '(() 1))


;; ------------------------------------------------------------
;; first, scond, third, build
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


;; ------------------------------------------------------------
;; fun?
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((4 3)(4 2)(7 6)(6 2)(3 4)))
(fun? '((8 3)(4 2)(7 6)(6 2)(3 4)))
(fun? '((d 4)(b 0)(b 9)(e 5)(g 4)))


;; ------------------------------------------------------------
;; revrel
;; reverse relation
(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (build (second (car rel))
                             (first (car rel)))
                      (revrel (cdr rel)))))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (cons (cadar rel)
                            (cons (caar rel)
                                  '()))
                      (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

(define revpair
  (lambda (pair)
    (cons (cadr pair)
          (cons (car pair)
                '()))))

(revpair '(8 a))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (revpair (car rel))
                      (revrel (cdr rel)))))))

(revrel '((8 a)(pumpkin pie)(got sick)))


;; ------------------------------------------------------------
;; fullfun?

(define seconds
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (cadar l)
                      (seconds (cdr l)))))))

(seconds '((8 3)(4 2)(7 6)(6 2)(3 4)))
           
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((8 3)(4 2)(7 6)(6 2)(3 4)))
(fullfun? '((8 3)(4 8)(7 6)(6 2)(3 4)))
(fullfun? '((grape raisin)(plum prune)(stewed prune)))
(fullfun? '((grape rasin)(plum prune)(stewed grape)))


; chapter 8

;; ------------------------------------------------------------
;; rember-f
(define rember-f
  (lambda (test? a l)
    (cond ((null? l) l)
          ((test? a (car l)) (rember-f test? a (cdr l)))
          (else (cons (car l)
                      (rember-f test? a (cdr l)))))))

(rember-f (lambda (x y)
            (= x y)) 5 '(1 2 3 4 5))

(rember-f (lambda (x y)
            (eq? x y)) 'jelly '(jelly beans are good))

(rember-f (lambda (x y)
            (equal? x y)) '(pop corn) '(lemonade (pop corn) and (cake)))


;; ------------------------------------------------------------
;; rewrite rember-f with currying
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) l)
            ((test? a (car l))((rember-f test?) a (cdr l)))
            (else (cons (car l)
                        ((rember-f test?) a (cdr l))))))))
((rember-f (lambda (x y)
             (= x y))) 5 '(1 2 3 4 5 6 7 8 9 10))


;; ------------------------------------------------------------
;; insertL-f
(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) l)
            ((test? old (car l))(cons new l))
            (else (cons (car l)
                        ((insertL-f test?) new old (cdr l))))))))

((insertL-f (lambda (x y)
              (= x y))) 'abc 5 '(1 2 3 4 5 6 7 8 9 10))


;; ------------------------------------------------------------
;; insertR-f
(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) l)
            ((test? old (car l))(cons (car l)
                                      (cons new
                                            (cdr l))))
            (else (cons (car l)
                        ((insertR-f test?) new old (cdr l))))))))

((insertR-f (lambda (x y)
              (= x y))) 'abc 5 '(1 2 3 4 5 6 7 8 9 10))




;; ------------------------------------------------------------
;; insert-g
(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) l)
            ((eq? old (car l))(seq new old (cdr l)))
            (else (cons (car l)
                        ((insert-g seq) new old (cdr l))))))))

; L
((insert-g (lambda (new old l)
             (cons new
                   (cons old l)))) 'a 5 '(1 2 3 4 5 6 7 8 9 10))
; R
((insert-g (lambda (new old l)
             (cons old
                   (cons new l)))) 'a 5 '(1 2 3 4 5 6 7 8 9 10))


; subst
((insert-g (lambda (new old l)
               (cons new l))) 'a 1 '(1 2 3 4 5))

; rember
((insert-g (lambda (new old l)
             l)) #f 1 '(1 2 3 4 5))


;; ------------------------------------------------------------
;; rewrite value
(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+) o+)
          ((eq? x '*) o*)
          (else 'o^))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp))
                 (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))

(value '(1 + (2 * 3)))


;; ------------------------------------------------------------
;; multirember-f
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) lat)
            ((test? a (car lat))((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

((multirember-f (lambda (x y)
                  (= x y))) 1 '(1 2 3 4 5 1 2 3 4 5))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (letrec ((f (multirember-f test?)))
        (cond ((null? lat) lat)
              ((test? a (car lat))(f a (cdr lat)))
              (else (cons (car lat)
                          (f a (cdr lat)))))))))

((multirember-f (lambda (x y)
                  (= x y))) 1 '(1 2 3 1 2 3 1 2 3))


;; ------------------------------------------------------------
;; multiremberT
(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) lat)
          ((test? (car lat))(multiremberT test? (cdr lat)))
          (else (cons (car lat)
                      (multiremberT test? (cdr lat)))))))

(multiremberT (lambda (n)
                (= 5 n)) '(1 2 3 4 5 1 2 3 4 5 1 2 3 4 5))


;; ------------------------------------------------------------
;; multirember&co
;; complecated . . .
(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)(col '()'()))
          ((eq? a (car lat))
           (multirember&co a
                           (cdr lat)
                           (lambda (newlat seen)
                             (col newlat (cons (car lat) seen)))))
          (else (multirember&co a
                                (cdr lat)
                                (lambda (newlat seen)
                                  (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna
                 '(strawberries tuna and swordfish)
                 a-friend)

(multirember&co 'tuna
                '()
                a-friend)
          
(multirember&co 'tuna
                '(tuna)
                a-friend)

; The Little Schemer Fourth Edition P.138
; The name col is short for "collector",
; A collector is sometimes called a "continuation."

(multirember&co 'tuna
                 '(strawberries tuna and swordfish)
                 (lambda (newlist seen)
                   (display newlist)
                   (newline)
                   (display seen)))

(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                (lambda (newlist seen)
                  (append '() newlist seen)))

(define new-friend
  (lambda (newlat seen)
    (a-friend newlat
              (cons 'tuna seen))))

(multirember&co 'tuna
                '(and tuna)
                a-friend)


;; ------------------------------------------------------------
;; multiinsertLR
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) lat)
          ((eq? oldL (car lat))
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? oldR (car lat))
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'a 1 3 '(1 2 3 1 2 3 1 2 3 1 2 3))


;; ------------------------------------------------------------
;; multiinsertLR&co
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? oldL (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons new
                                          (cons oldL newlat))
                                    (add1 L)
                                    R))))
          ((eq? oldR (car lat))
           (multiinsertLR&co new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons oldR
                                          (cons new newlat))
                                    L
                                    (add1 R)))))
          (else (multiinsertLR&co new oldL oldR (cdr lat)
                                  (lambda (newlat R L)
                                    (col (cons (car lat)
                                               newlat)
                                         L
                                         R)))))))
(multiinsertLR&co 'salty
                  'fish
                  'chips
                  '(chips and fish or fish and chips)
                  (lambda (newlat L R)
                    (values newlat L R)))


;; ------------------------------------------------------------
;; evens-only*

(define my-even?
  (lambda (n)
    (= (* #?=(/ n 2) 2) n)))

(my-even? 2)
(my-even? 112)
;(my-even? 111)

(define my-even?
  (lambda (n)
    (o= (o* (o/ n 2) 2) n)))

(my-even? 2)
(my-even? 112)
;(my-even? 111)

(define evens-only*
  (lambda (l)
    (cond ((null? l) l)
          ((atom? (car l))
           (cond ((even? (car l))(cons (car l)
                                       (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(evens-only* '(1 2 3 4 5 6 7 8 9 10))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))


;; ------------------------------------------------------------
;; evens-only*&co
(define evens-only*&co
  (lambda (l col)
    (cond ((null? l)(col '() 1 0))
          ((atom? (car l))
           (cond ((even? (car l))(evens-only*&co (cdr l)
                                                 (lambda (newl p s)
                                                   (col (cons (car l)
                                                              newl)
                                                   (* p (car l))
                                                   s))))
                 (else (evens-only*&co (cdr l)
                                       (lambda (newl p s)
                                         (col newl
                                              p
                                              (+ s (car l))))))))
          (else (evens-only*&co (car l)
                                (lambda (al ap as)
                                  (evens-only*&co (cdr l)
                                                  (lambda (dl dp ds)
                                                    (col (cons al dl)
                                                         (* ap dp)
                                                         (+ as ds))))))))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                (lambda (newl product sum)
                  (cons sum
                        (cons product
                              newl))))


; chapter 9


;; ------------------------------------------------------------
;; looking, keep-looking
;; unnatural recursion

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn) (keep-looking a (pick sorn lat) lat))
          (else (eq? a sorn)))))


(looking 'caviar
         '(6 2 4 caviar 5 7 3))
(looking 'caviar
         '(6 2 grits caviar 5 7 3))


;; ------------------------------------------------------------
;; eternity
(define eternity
  (lambda (x)
    (eternity x)))


;; ------------------------------------------------------------
;; shift

(define shift
  (lambda (pair)
    (cons (caar pair)
          (cons (cons (cadar pair)
                      (cdr pair))
                '()))))

(shift '((a b)(c d)))
(shift '((a b) c))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b)(c d)))
(shift '((a b) c))


;; ------------------------------------------------------------
;; align
;; pora... pair or atom ...?
(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora)) (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))


;; ------------------------------------------------------------
;; length*
;; count atoms *
(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (length* (first pora))
                    (length* (second pora)))))))

(length* '((a b)(c d)))
(length* '((a b) c))


;; ------------------------------------------------------------
;; weight*
(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (o+ (o* (weight* (first pora)) 2)
                    (weight* (second pora)))))))

(weight* '((a b)(c d)))
(weight* '((a b) c))


;; ------------------------------------------------------------
;; shuffle
(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))(shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '(a b))
;(shuffle '((a b)(c d)(e f)))


;; ------------------------------------------------------------
;; C
;; collatz
(define C
  (lambda (n)
    (cond ((one? n) 1)
          ((even? n) (C (o/ n 2)))
          (else (C (add1 (o* 3 n)))))))

(C 10)


;; ------------------------------------------------------------
;; Ackermann
(define A
  (lambda (n m)
    (cond ((zero? n)(add1 m))
          ((zero? m)(A (sub1 n) 1))
          (else (A (sub1 n)
                   (A n (sub1 m)))))))

(A 1 2)
(A 1 0)
(A 2 2)
(A 0 3)
; (A 4 3) -> no answer


;; ------------------------------------------------------------
;; Y Combinator
(define Y
  (lambda (g)
    ((lambda (f)
       (f f))
     (lambda (f)
       (g (lambda (x)
            ((f f) x)))))))

((Y (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n (f (- n 1))))))) 5)


; chapter 10

;; ------------------------------------------------------------
;; new-entry
(define new-entry build)


;; ------------------------------------------------------------
;; lookup-in-entry
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))


;; ------------------------------------------------------------
;; lookup-in-entry-help
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names)(entry-f name))
          ((eq? (car names) name)(car values))
          (else (lookup-in-entry-help name
                                      (cdr names)
                                      (cdr values)
                                      entry-f)))))


;; ------------------------------------------------------------
;; extend-table
(define extend-table cons)


;; ------------------------------------------------------------
;; lookup-in-table
(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table)(table-f name))
          (else (lookup-in-entry name
                                 (car table)
                                 (lambda (name)
                                   (lookup-in-table name
                                                    (cdr table)
                                                    table-f)))))))


;; ------------------------------------------------------------
;; expression-to-action
(define expression-to-action
  (lambda (e)
    (cond ((atom? e)(atom-to-action e))
          (else (list-to-action e)))))


;; ------------------------------------------------------------
;; atom-to-action
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

;; ------------------------------------------------------------
;; list-to-action
(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ((eq? (car e) 'quote) *quote)
                 ((eq? (car e) 'lambda) *lambda)
                 ((eq? (car e) 'cond) *cond)
                 (else *application)))
          (else *application))))


;; ------------------------------------------------------------
;; value
(define value
  (lambda (e)
    (meaning e '())))

(value '(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (quote ())))))))
(value '(lambda (x) (cons x y)))

;; ------------------------------------------------------------
;; meaning
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))


;; ------------------------------------------------------------
;; *const
(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e #t) #t)
          ((eq? e #f) #f)
          (else (build 'primitive e)))))


;; ------------------------------------------------------------
;; *quote
(define *quote
  (lambda (e table)
    (text-of e)))


;; ------------------------------------------------------------
;; text-of
(define text-of second)


;; ------------------------------------------------------------
;; *identifer
(define *identifer
  (lambda (e table)
    (lookup-in-table e table initial-table)))


;; ------------------------------------------------------------
;; initial-table
(define initial-table
  (lambda (name)
    (car '())))


;; ------------------------------------------------------------
;; *lambda
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))

;; ------------------------------------------------------------
(define table-of first)
(define formals-of second)
(define body-of third)


;; ------------------------------------------------------------
(define evcon
  (lambda (lines table)
    (cond ((else? (question-of (car lines)))
           (meaning (answer-of (car lines)) table))
          ((meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines))
                    table))
          (else (evcon (cdr lines) table)))))


;; ------------------------------------------------------------
(define else?
  (lambda (x)
    (cond ((atom? x)(eq? x (quote else)))
          (else #f))))

;; ------------------------------------------------------------
(define question-of first)
(define answer-of second)

;; ------------------------------------------------------------
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

;; ------------------------------------------------------------
(define cond-lines-of cdr)


;; ------------------------------------------------------------
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else (cons (meaning (car args) table)
                 (evlis (cdr args) table))))))


;; ------------------------------------------------------------
(define *application
  (lambda (e table)
    (*apply  (meaning (function-of e) table)
             (evlis (arguments-of e) table))))

;; ------------------------------------------------------------
(define function-of car)
(define arguments-of cdr)

;; ------------------------------------------------------------
(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

;; ------------------------------------------------------------
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote  non-primitive))))

;; ------------------------------------------------------------
(define *apply
  (lambda (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) vals))
          ((non-primitive? fun) (apply-closure (second fun) vals)))))

;; ------------------------------------------------------------
(define apply-primitive
  (lambda (name vals)
    (cond ((eq? name (quote cons))
           (cons (first vals)(second vals)))
          ((eq? name (quote car))
           (car (first vals)))
          ((eq? name (quote cdr))
           (cdr (first vals)))
          ((eq? name (quote null?))
           (null? (first vals)))
          ((eq? name (quote eq?))
           (eq? (first vals) (second vals)))
          ((eq? name (quote atom?))
           (*atom? (frist vals)))
          ((eq? name (quote zero?))
           zero? (first vals))
          ((eq? name (quote add1))
           (add1 (first vals)))
          ((eq? name (quote sub1))
           (sub1 (first vals)))
          ((eq? name (quote number?))
           (number? (first vals))))))

;; ------------------------------------------------------------
(define *atom?
  (lambda (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x)(quote primitve)) #t)
          ((eq? (car x)(quote non-primitive)) #f)
          (else #f))))

;; ------------------------------------------------------------
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table (new-entry (formals-of closure) vals)
                           (table-of closure)))))


(value '(car (quote (a b c))))
(value '(add1 6))
(value 6)
(value '(quote nothing))
(value '((lambda (nothing)
           (cons nothing (quote ())))
         (quote
          (from nothing comes something))))
(value '((lambda (nothing)
           (cond (nothing (quote something))
                 (else (quote nothing))))
         #f))
(value '((lambda (nothing)
           (cond (nothing (quote something))
                 (else (quote nothing))))
         #t))
(value '(((lambda (x)
            (lambda (x)
              (add1 x))) 3) 5))


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