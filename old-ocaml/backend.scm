(define (compile expr)
  (case (car expr)
    (('int) (compile-int (cdr expr)))
    (else (compile-error (expr)))))


(define (compile-int i)
  

(define (compile-error x)
  (display "Compile error:")
  (display x)
  (exit 1))