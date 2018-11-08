#lang racket

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)

;A
(define (ins_beg element elist)
  (display "ins beg:\n")
  (cons element elist))

;B
(define (ins_end element elist)
  (display "\nins end:\n")
  (append elist (list element)))

;C
(define (cout_top_level list)
  (display "\ncout top level\n")
  (define x 0)
  (define (count elist)
      (if  (empty? elist)
           
           (display "Elements count: ")
           (begin
             (set! x (+ x 1))
             
             (count (cdr elist)))))
  (count list)
   (display x))

;D
(define (count_instances element list)
  (display "\ncount instances\n")
  (define y 0)
  (define (count elist)
  (if (empty? elist)
      
      (display "Instances count: ")
      (begin
        (if (eq? element (car elist))
            (set! y (+ y 1))
            (set! y y))
        (count (cdr elist)))))
  
  (count list)
  (display y))

;E
(define (count_instances_tr element list)
  (display "\ncount instances tr\n")
  (define (count elist total)
    (if (empty? elist)

        (begin
        (display "Instances count: ")
        (display total))
        (begin
          (if (eq? element(car elist))
              
            (count (cdr elist) (+ total 1))
            (count (cdr elist) total)))
        ))
  (count list 0))

;F
(define (count_instances_deep element list)
  (display "\ncount instances deep\n")
  (define x 0)
  (define (count elist)

    (if (empty? elist) 
        (void)
        (begin
          (if (eq? element (car elist))
              (begin
                (set! x (+ x 1))
                (count (cdr elist)))

              (begin
                
                (if (number? (car elist))
                    (count (cdr elist))
                    
                    (begin

                      (count (car elist))
                      (count (cdr elist)))))))))
  (count list)
  (display "Instances count: ")
  (display x))

