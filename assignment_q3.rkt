#lang racket
(provide tree)
(provide to_Sort)
(provide left_child)
(provide right_child)
(provide value)
(provide sortTree)
(provide present)
(provide insert_item)
(provide insert_list)
(provide tree_sort)
(provide higher_order_insert_item)
(provide higher_order_insert_list)
(provide higher_order_tree_sort)


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
(define (insert_item element binaryTree)
  
  (higher_order_insert_item element binaryTree <)
  )


;D
(define (insert_list elist binaryTree)
  (if (empty? elist) binaryTree

      (insert_list (cdr elist) (insert_item (car elist) binaryTree))))




;E
(define (tree_sort elist)
  (sortTree (insert_list elist '())))

;F
(define (higher_order_insert_item item binaryTree left)
  (cond [(empty? binaryTree) (list '() item '())]
        [(equal? item (value binaryTree)) binaryTree]
        [(left item (value binaryTree))
         (list (higher_order_insert_item item (left_child binaryTree) left) (value binaryTree) (right_child binaryTree))]
        [else (list (left_child binaryTree) (value binaryTree) (higher_order_insert_item item (right_child binaryTree) left))]))


(define (higher_order_insert_list elist binaryTree left)
  (if (empty? elist) binaryTree
      (higher_order_insert_list (cdr elist) (higher_order_insert_item (car elist) binaryTree left) left)))

(define (higher_order_tree_sort elist orderFunction)
  (sortTree (higher_order_insert_list elist '() orderFunction)))


(define (ascending_last_digit x y)
  (< (remainder x 10) (remainder y 10)))

(display "Part A:Display sorted contents:\n")
(sortTree tree)

(display "\nPart B:Present in tree:\n")
(present 9 tree)
(present 222 tree)

(display "Part C:Insert item\n")
(insert_item 16 tree)

(display "Part D:Insert list to Tree:\n")
(insert_list '(5 60 88 101 68) tree)

(display "Part E:Tree sort alogrithm:\n")
(tree_sort to_Sort)

(display "\nPart F:Higher order tree sort:\n")
(display "Ascending order:\n")
(higher_order_tree_sort to_Sort <)
(display "\nDescending order:\n")
(higher_order_tree_sort to_Sort >)
(display "\nAscending order based on final digit:\n")
(higher_order_tree_sort to_Sort ascending_last_digit)
