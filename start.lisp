(require 'functions_used.lisp)

(setf lines (openFile "and_examples.txt"))

(setq specs33 (loop for x in lines collect (split2 x " ")))

(setf state 0)
(setf stack (make-array 1 :adjustable t :fill-pointer 0 ))
(setq specs33 (loop for x in specs33 collect (toListOfStrings x 0)))



(setf different_structures (list :and_ (list ) :or_ (list ) :if_then_else (list ) :if_ (list )))
(loop for x in specs33
	do
	(setf line x)

	(cond
		((and (= (length x) 3) (string= (second x) "and"))

			 (setf different_structures
			 	(list
			 		:and_ (append (getf different_structures :and_) (list (makeSubgroup)) )
			 		:or_ (getf different_structures :or_)
			 		:if_then_else (getf different_structures :if_then_else)
			 		:if_ (getf different_structures :if_))))

		((and (= (length x) 3) (string= (second x) "or"))
			
			 (setf different_structures
			 	(list
			 		:and_  (getf different_structures :and_)
			 		:or_ (append (getf different_structures :or_) (list (makeSubgroup)) )
			 		:if_then_else (getf different_structures :if_then_else)
			 		:if_ (getf different_structures :if_))))

		; for if 5 and 6
		((= (length x) 4)


			(setf different_structures
				(list
					:and_ (getf different_structures :and_)
					:or_ (getf different_structures :or_)
					:if_then_else (getf different_structures :if_then_else)
					:if_ (append (getf different_structures :if_) (list (makeIfElseStatement))))))

		; for if then else statement
		(t

			(setf different_structures
				(list
					:and_ (getf different_structures :and_)
					:or_ (getf different_structures :or_)
					:if_then_else (append (getf different_structures :if_then_else) (list (makeIfElseStatement)))
					:if_ (getf different_structures :if_))))))

(setf output_string "")
(getCodeFromList (getf different_structures :and_))


(getCodeFromList (getf different_structures :or_))

(getCodeFromList (getf different_structures :if_then_else) )

; if condition -> if condition then statement else statement
(setf output_string (concatenate 'string output_string (getCode (getf different_structures :if_)) " " ))
(setf output_string (concatenate 'string output_string (getCode (rest (rest (first (getf different_structures :if_then_else))))) ))
(setf output_string (concatenate 'string output_string "~%~%" ))

(setf output_string (concatenate 'string output_string (getCode (getf different_structures :if_)) " " ))
(setf output_string (concatenate 'string output_string (getCode (rest (rest (second (getf different_structures :if_then_else)))))))
(setf output_string (concatenate 'string output_string "~%~%" ))

(setf output_string (concatenate 'string output_string (getCode (getf different_structures :if_)) " "))
(setf output_string (concatenate 'string output_string (getCode (rest (rest (third (getf different_structures :if_then_else)))))))

(format t "~%output_string~%")
(format t output_string)
(format t "~%~%data structure:~%")
(print different_structures)
