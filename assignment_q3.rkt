#lang racket
(provide tree)
(provide to_Sort)
(provide left_child)
(provide right_child)
(provide value)
(provide sortTree)
(provide present)
(provide addItem)
(provide add_list)
(provide higher_order_add_list)
(provide tree_sort)
(provide higher_order_tree_sort)
(provide higher_order_addItem)

(define tree '(((() 5 ()) 9 (() 12 ())) 23 ((() 27 ()) 33 (() 49 ()))))
(define to_Sort '(3 9 122 57 7 16 55 30))

(define (left_child binaryTree)
  (car binaryTree))

(define (right_child binaryTree)
  (caddr binaryTree))

(define (value binaryTree)
  (cadr binaryTree))
;A
(define (sortTree binaryTree)
 (begin(cond [(not (empty?(left_child binaryTree))) (sortTree (left_child  binaryTree))])

       (printf "~a " (value  binaryTree));
   (cond [(not (empty?(right_child  binaryTree))) (sortTree (right_child  binaryTree))])))

;B
(define (present element binaryTree)
(cond
  [(empty? binaryTree) #f]
  [(equal? element (value binaryTree)) #t]
  [(< element (value binaryTree)) (present element (left_child binaryTree))]
  
  [else (present element (right_child binaryTree))]
  )
  )

;C
(define (addItem element binaryTree)
  
  (higher_order_addItem element binaryTree <)
  )


;D
(define (add_list elist binaryTree)
  (if (empty? elist) binaryTree

      (add_list (cdr elist) (addItem (car elist) binaryTree))))

(define (higher_order_add_list elist binaryTree left)
  (if (empty? elist) binaryTree

      (higher_order_add_list (cdr elist) (higher_order_addItem (car elist) binaryTree left) left)))



;E
(define (tree_sort elist)
  (sortTree (add_list elist '())))

(define (higher_order_tree_sort elist orderFunction)
  (sortTree (higher_order_add_list elist '() orderFunction)))



;F
(define (higher_order_addItem item binaryTree left)
  (cond [(empty? binaryTree) (list '() item '())]
        [(equal? item (value binaryTree)) binaryTree]
        [(left item (value binaryTree))
         
         (list (higher_order_addItem item (left_child binaryTree) left) (value binaryTree) (right_child binaryTree))]
        [else (list (left_child binaryTree) (value binaryTree) (higher_order_addItem item (right_child binaryTree) left))]))



(define (ascending_last_digit x y)
  (< (remainder x 10) (remainder y 10)))

(display "display_sorted:\n")
(sortTree tree)

(display "\npresent_in_tree:\n")
(present 9 tree)
(present 222 tree)

(display "add item\n")
(addItem 16 tree)

(display "add_list:\n")
(add_list '(5 60 88 101 68) tree)

(display "tree_sort:\n")
(tree_sort to_Sort)

(display "higher_order_tree_sort:\n")
(display "Ascending:\n")
(higher_order_tree_sort to_Sort <)
(display "\nDescending:\n")
(higher_order_tree_sort to_Sort >)
(display "\nAscending based on final digit:\n")
(higher_order_tree_sort to_Sort ascending_last_digit)
