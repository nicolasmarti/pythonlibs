import Lisp

definitions = [ """
; this is a function
(defun add1 ((a 0)) ; this is a comment
 "la doc"         
 (+ a 1)   
)
""", """
(defun triangle_using_dotimes (number_of_rows)
   "Using dotimes, add up the number of pebbles in a triangle."
     (let ((total 0))
       (dotimes (number number_of_rows total)
         (setq total (+ total (1+ number))))))
""", """
(defun triangle_recursively (number)
       "Return the sum of the numbers 1 through NUMBER inclusive.
     Uses recursion."
       (if (= number 1)                    ; do-again-test
           1                               ; then-part
         (+ number                         ; else-part
            (triangle_recursively          ; recursive call
             (1- number)))))               ; next-step-expression
""", """
(defun square_each (numbers_list)
       "Square each of a NUMBERS LIST, recursively."
       (if (not numbers_list)                ; do-again-test
           nil
         (cons
          (* (car numbers_list) (car numbers_list))
          (square_each (cdr numbers_list))))) ; next-step-expression
""", """
(defun add_elements (numbers_list)
       "Add the elements of NUMBERS-LIST together."
       (if (not numbers_list)
           0
         (+ (car numbers_list) (add_elements (cdr numbers_list)))))
""", """     
(defun keep_three_letter_words (word_list)
       "Keep three letter words in WORD_LIST."
       (cond
        ;; First do_again_test: stop_condition
        ((not word_list) nil)
     
        ;; Second do_again_test: when to act
        ((eq 3 (length (symbol_name (car word_list))))
         ;; combine acted_on element with recursive call on shorter list
         (cons (car word_list) (keep_three_letter_words (cdr word_list))))
     
        ;; Third do_again_test: when to skip element;
        ;;   recursively call shorter list with next_step expression
        (t (keep_three_letter_words (cdr word_list)))))
""", """
(defun triangle_initialization (number)
       "Return the sum of the numbers 1 through NUMBER inclusive.
     This is the initialization component of a two function
     duo that uses recursion."
       (triangle_recursive_helper 0 0 number))
     (defun triangle_recursive_helper (sum counter number)
      "Return SUM, using COUNTER, through NUMBER inclusive.
     This is the helper component of a two function duo
     that uses recursion."
       (if (> counter number)
           sum
         (triangle_recursive_helper (+ sum counter)  ; sum
                                    (1+ counter)     ; counter
                                    number)))        ; number

""" ]

initial_methods = dir(Lisp)

for x in range(0, 100):
    for d in definitions:
        Lisp.definition( d )

    print [ y for y in dir(Lisp) if not y in initial_methods ]
        
    for d in definitions:
        Lisp.undo_definition()

    print [ y for y in dir(Lisp) if not y in initial_methods ]
        
print "done"
