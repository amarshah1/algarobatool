(assert (and (not (is-succ zero)) (is-zero zero)))
(assert (and (not (is-cons null)) (is-null null)))
(assert (= n contrived_variable0))
(assert (= x contrived_variable1))
(assert (= y null))
(assert (= contrived_variable3 contrived_variable2))
(assert (and (= (cons zero null) contrived_variable1)
     (is-cons contrived_variable1)
     (= (car contrived_variable1) zero)
     (= (cdr contrived_variable1) null)))
(assert (let ((a!1 (=> (is-succ m) (and (= (pred m) contrived_variable4)))))
  (and (= (pred m) contrived_variable0) (= (succ contrived_variable4) m) a!1)))
(assert (= (size x) contrived_variable3))
(assert (= (size y) contrived_variable2))
(assert (or (and (not (is-cons contrived_variable1)) (is-null contrived_variable1))
    (and (is-cons contrived_variable1) (not (is-null contrived_variable1)))))
(assert (=> (is-null contrived_variable1) (= contrived_variable1 null)))
(assert (or (and (not (is-cons y)) (is-null y)) (and (is-cons y) (not (is-null y)))))
(assert (=> (is-null y) (= y null)))
(assert (or (and (not (is-cons x)) (is-null x)) (and (is-cons x) (not (is-null x)))))
(assert (=> (is-null x) (= x null)))
(assert (or (and (not (is-succ contrived_variable4)) (is-zero contrived_variable4))
    (and (is-succ contrived_variable4) (not (is-zero contrived_variable4)))))
(assert (=> (is-zero contrived_variable4) (= contrived_variable4 zero)))
(assert (or (and (not (is-succ contrived_variable2)) (is-zero contrived_variable2))
    (and (is-succ contrived_variable2) (not (is-zero contrived_variable2)))))
(assert (=> (is-zero contrived_variable2) (= contrived_variable2 zero)))
(assert (or (and (not (is-succ contrived_variable3)) (is-zero contrived_variable3))
    (and (is-succ contrived_variable3) (not (is-zero contrived_variable3)))))
(assert (=> (is-zero contrived_variable3) (= contrived_variable3 zero)))
(assert (or (and (not (is-succ contrived_variable0)) (is-zero contrived_variable0))
    (and (is-succ contrived_variable0) (not (is-zero contrived_variable0)))))
(assert (=> (is-zero contrived_variable0) (= contrived_variable0 zero)))
(assert (or (and (not (is-succ m)) (is-zero m)) (and (is-succ m) (not (is-zero m)))))
(assert (=> (is-zero m) (= m zero)))
(assert (or (and (not (is-succ n)) (is-zero n)) (and (is-succ n) (not (is-zero n)))))
(assert (=> (is-zero n) (= n zero)))
(assert (=> (is-cons x) (= (cons contrived_variable6 contrived_variable8) x)))
(assert (or (and (not (is-cons x)) (is-null x)) (and (is-cons x) (not (is-null x)))))
(assert (= contrived_variable6 (car x)))
(assert (= contrived_variable8 (cdr x)))
(assert (= (size x) contrived_variable3))
(assert (= contrived_variable5 (= x null)))
(assert (=> contrived_variable5 (= contrived_variable3 zero)))
(assert (=> (not contrived_variable5)
    (= contrived_variable3 (succ (size contrived_variable8)))))
(assert (=> (is-cons y) (= (cons contrived_variable11 contrived_variable13) y)))
(assert (or (and (not (is-cons y)) (is-null y)) (and (is-cons y) (not (is-null y)))))
(assert (= contrived_variable11 (car y)))
(assert (= contrived_variable13 (cdr y)))
(assert (= (size y) contrived_variable2))
(assert (= contrived_variable10 (= y null)))
(assert (=> contrived_variable10 (= contrived_variable2 zero)))
(assert (=> (not contrived_variable10)
    (= contrived_variable2 (succ (size contrived_variable13)))))
(assert (=> (and (is-cons contrived_variable1))
    (not (= (cdr contrived_variable1) contrived_variable1))))
(assert (=> (and (is-cons contrived_variable8))
    (not (= (cdr contrived_variable8) contrived_variable8))))
(assert (=> (and (is-cons contrived_variable13))
    (not (= (cdr contrived_variable13) contrived_variable13))))
(assert (=> (is-cons x) (= (cons contrived_variable6 contrived_variable8) x)))
(assert (or (and (not (is-cons x)) (is-null x)) (and (is-cons x) (not (is-null x)))))
(assert (= contrived_variable6 (car x)))
(assert (= contrived_variable8 (cdr x)))
(assert (= (size x) contrived_variable3))
(assert (= contrived_variable5 (= x null)))
(assert (=> contrived_variable5 (= contrived_variable3 zero)))
(assert (=> (not contrived_variable5)
    (= contrived_variable3 (succ (size contrived_variable8)))))
(assert (=> (is-cons y) (= (cons contrived_variable11 contrived_variable13) y)))
(assert (or (and (not (is-cons y)) (is-null y)) (and (is-cons y) (not (is-null y)))))
(assert (= contrived_variable11 (car y)))
(assert (= contrived_variable13 (cdr y)))
(assert (= (size y) contrived_variable2))
(assert (= contrived_variable10 (= y null)))
(assert (=> contrived_variable10 (= contrived_variable2 zero)))
(assert (=> (not contrived_variable10)
    (= contrived_variable2 (succ (size contrived_variable13)))))
(assert (=> (is-cons contrived_variable8)
    (= (cons contrived_variable16 contrived_variable18) contrived_variable8)))
(assert (or (and (not (is-cons contrived_variable8)) (is-null contrived_variable8))
    (and (is-cons contrived_variable8) (not (is-null contrived_variable8)))))
(assert (= contrived_variable16 (car contrived_variable8)))
(assert (= contrived_variable18 (cdr contrived_variable8)))
(assert (= (size contrived_variable8) contrived_variable9))
(assert (= contrived_variable15 (= contrived_variable8 null)))
(assert (=> contrived_variable15 (= contrived_variable9 zero)))
(assert (=> (not contrived_variable15)
    (= contrived_variable9 (succ (size contrived_variable18)))))
(assert (=> (is-cons contrived_variable13)
    (= (cons contrived_variable21 contrived_variable23) contrived_variable13)))
(assert (or (and (not (is-cons contrived_variable13)) (is-null contrived_variable13))
    (and (is-cons contrived_variable13) (not (is-null contrived_variable13)))))
(assert (= contrived_variable21 (car contrived_variable13)))
(assert (= contrived_variable23 (cdr contrived_variable13)))
(assert (= (size contrived_variable13) contrived_variable14))
(assert (= contrived_variable20 (= contrived_variable13 null)))
(assert (=> contrived_variable20 (= contrived_variable14 zero)))
(assert (=> (not contrived_variable20)
    (= contrived_variable14 (succ (size contrived_variable23)))))
(assert (let ((a!1 (and (not (is-succ (car contrived_variable1)))
                (not (is-cons (cdr contrived_variable1)))))
      (a!2 (and (not (is-succ (car y))) (not (is-cons (cdr y)))))
      (a!3 (and (not (is-succ (car x))) (not (is-cons (cdr x)))))
      (a!4 (and (not (is-succ (car null))) (not (is-cons (cdr null)))))
      (a!5 (=> (is-succ contrived_variable4)
               (not (is-succ (pred contrived_variable4)))))
      (a!6 (=> (is-succ contrived_variable2)
               (not (is-succ (pred contrived_variable2)))))
      (a!7 (=> (is-succ contrived_variable3)
               (not (is-succ (pred contrived_variable3)))))
      (a!8 (=> (is-succ contrived_variable0)
               (not (is-succ (pred contrived_variable0)))))
      (a!9 (=> (is-succ m) (not (is-succ (pred m)))))
      (a!10 (=> (is-succ n) (not (is-succ (pred n)))))
      (a!11 (=> (is-succ zero) (not (is-succ (pred zero))))))
  (and (=> (is-cons contrived_variable1) a!1)
       (=> (is-cons y) a!2)
       (=> (is-cons x) a!3)
       (=> (is-cons null) a!4)
       a!5
       a!6
       a!7
       a!8
       a!9
       a!10
       a!11)))
(assert contrived_variable15)
(assert contrived_variable20)