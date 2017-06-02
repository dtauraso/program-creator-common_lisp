(defun openFile(file_name)

	; proble-file returns false if there is not file named by file_name
	; assert will only print out error message when the assert condition is false
	(assert (probe-file file_name) (file_name) "~a doesn't exist (probe-file file_name)" file_name)
	(with-open-file (var file_name )		
			(readLines var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun readLines(var)

	; returns a list with each line from file as elements in the list
	(let ((line (read-line var nil)))

		(if line
			(cons line (readLines var))
			nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stringToVector (string_ vector_ i)

	; string_, vector_, and i have already been initialized
	; i is upper bound
	(cond ((= i 0)
			vector_)
			(t
				(vector-push (char string_ (- i 1)) vector_)

				(stringToVector string_ vector_ (- i 1)))))


(defun lastOnStack (stack)

	; stack is a vector of strings
	(aref stack (- (length stack) 1)))

(defun split2Transitions (i delimiter state state_transition_table not_delimiter delimiter_ input_is_empty)


	)
(defun split2 (string_ delimiter)

	; push from end to start to a vector from the string "alpha and beta g h a j i k"
	; guarantee the last word is collected and added to stack
	; pattern: word space
	(setq string_ (concatenate 'string string_ " "))

	(let* (

			(stack (stringToVector string_ (make-array (length string_) :fill-pointer 0 :element-type 'character) (length string_)))

			; columns/edge
			(not_delimiter 0)
			(delimiter_ 1)
			(input_is_empty 2)


			; non-accepting states
			(collect 0)
			(add_collection 1)
			(collect_nothing 2)
			(error -1)
			; accepting state
			(return_ 3)
			; 		not_delimiter, 	delimiter_, 	input_is_empty
			(state_transition_table (vector

				(vector collect 		add_collection 		return_) ; collect
				(vector collect 			collect_nothing 	error) ; add_collection
				(vector collect 		collect_nothing 	return_) ; collect_nothing
				(vector error			error 				error) ; return_



			))
			(state 0)

 			(string_count (length string_))

			(collection (make-array string_count :fill-pointer 0 :element-type 'character))
			
			(list_of_collections (make-array string_count :fill-pointer 0 :element-type 'string))

			(i 0))

			(loop

				; transitions
				(assert (not (= i 400)) (i) "i is out of bounds ~d (= i 400)" i )

				(cond
					; collect -> return_
					((= (length stack) 0)

						(setq state (aref (aref state_transition_table state) input_is_empty)))

					; collect -> collect
					; collect_nothing -> collect
					((not (string= (lastOnStack stack) delimiter))

						(setq state (aref (aref state_transition_table state) not_delimiter)))

						; collect -> add_collection
						; add_collection -> collect_nothing
						; collect_nothing -> collect_nothing
					((string= (lastOnStack stack) delimiter)

						(setq state (aref (aref state_transition_table state) delimiter_))))

					(cond

						; non-accepting states
						((= state collect)

							(vector-push (aref stack (- (length stack) 1)) collection)
							(vector-pop	stack))

						((= state add_collection)
							
							(vector-push collection list_of_collections)

							(setq collection (make-array string_count :fill-pointer 0 :element-type 'character)))
					
						((= state collect_nothing)
							(vector-pop stack))
					
						
					
						((assert (not (= state error)) (state error) "error state state = ~d, error = ~d, (= state error)" state error)))

					; accepting state
					(cond ((= state return_)
							(return)))

					(setq i (+ i 1)))

			list_of_collections))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toListOfStrings (vector_of_vectors i)

	; vector_of_vectors is a vector holding type list holding vectors holding type strings

	; at last vector
		; collect toStrings((aref vector_of_vectors i))
	; not at last vector
		; (cons (toListOfStrings (aref vector_of_vectors i)))

	(cond ((= (- (length vector_of_vectors) 1) i)
			; so cons doesn't produce dot notation for last cons call
			(list (aref vector_of_vectors i)))

			(t
				(cons (aref vector_of_vectors i) (toListOfStrings vector_of_vectors (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isDigit (char_)

	(if (and (char>= char_ #\0) (char<= char_ #\9))
		t
		nil))

(defun isStringNumeric (string_)

	(loop for x from 0 to (- (length string_) 1)

		do
		(cond ((not (isDigit (aref string_ x ))) (return-from isStringNumeric nil))))
	t)

(defun englishToCode (english)


	(list
		:english english
		:code (first (openFile (concatenate 'string

			(if (isStringNumeric english)
					(format nil "~r" (parse-integer english))
					english)
			

			".txt")))))

(defun makeSubGroup ()

	; do using let*
	(let* (
		
		; edges/columns
		(bool_para 0)
		(bool_operator 1)
		(any_input 2)

		; non-accepting states
		(start 0)
		(get_parameter_1 1)
		(get_operator 2)
		(error -1)
		(get_parameter_2 3)

		; accepting state
		(return_ 4)

		;	 	bool_para, 		bool_operator, 		any_input
		(state_transition_table (vector

		(vector get_parameter_1 	error 				error)  ; start
		(vector error 				get_operator 	 	error)  ; get_parameter_1
		(vector get_parameter_2 	error 				error)  ; get_operator
		(vector error 				error				return_)  ; get_parameter_2
		(vector error 				error 				error)  ; return_

		))

		(state 0)
		(i 0)
		(collection (list )))

		(loop
			do
			(assert (not (= i 4))  (i) "i is out of bounds i = ~d (= i 4) , state = ~d" i state)

			; state transitions
					
			(cond 
				; get_parameter_2 -> return_
				((= state get_parameter_2)

					(setq state (aref (aref state_transition_table state) any_input))

					(assert (not (= (length stack) 0)) (stack) "stack is empty ~a (= (length stack) 0) , state = ~d" stack state)

					; empty stack
					(vector-pop stack))


				; start -> bool_para
				 ((and (not (null line)) (not (string= (first line) "and")) (not (string= (first line) "or")))
				
					(setq state (aref (aref state_transition_table state)  bool_para))

					; can't tell parameter 1 from parameter 2 so an extra case is needed here

					; there will always be something in stack when this is reached
					(if (= state get_parameter_2)
						; get operator off of stack
						(vector-pop stack))

					; push parameter
					(vector-push (first line) stack)

					(setq line (rest line)))

					; bool_para -> bool_operator
					((or (string= (first line) "and") (string= (first line) "or") )

						(setq state (aref (aref state_transition_table state) bool_operator))
						; first operand was not collected so initially empty stack is still empty
						(assert (not (= (length stack) 0)) (stack) "stack is empty ~a (= (length stack) 0)  , state = ~d" stack state )

						(vector-pop stack)
						; push oparator
						(vector-push (first line) stack)

						(setf line (rest line)))


					((assert (not t) () "no transition t fail, state = ~d ~a" state collection)))

			(cond

				((= state get_parameter_1)

						(setq
							collection
							(append
								collection
								(cons (englishToCode (aref stack 0)) ()))))

					((= state get_operator)
				
						(setq
							collection
							(append
								collection
								(cons (englishToCode (aref stack 0)) ()))))

					((= state get_parameter_2)

						(setq
							collection
							(append
								collection
								(cons (englishToCode (aref stack 0)) ()))))

					; accepting
					((= state return_)
						(return))

					((assert (not (= state error) (state error) "error state state = ~d (= state error), state = ~d" state state))))


			(setq i (+ i 1)))

		collection))

(defun makeIfElseStatement ()

	(let* (
			; column names
			(line_is_empty 2)
			(if_statement_word_markers 0)
			(subgroup_element_1 1)

			; non-accepting state names
			(start 0)
			(if_else_append 1)
			(if_else_subgroup 2)
			(then_append 3)
			(then_subgroup 4)

			; accepting state names
			(return_ 5)


			(error -1)
			; if_statement_word_markers, subgroup_element_1, line_is_empty		
			(state_transition_table (vector 

				(vector if_else_append  	error 				error)  ; start
				(vector error 					if_else_subgroup 	error)  ; if_else_append
				(vector then_append 		error 				return_)  ; if_else_subgroup
				(vector error 				then_subgroup 		error)  ; then_append
				(vector if_else_append 		error 				error)  ; then_subgroup
				(vector error 		error 	      				error ) ; return_

				))


			(i 0)
			(collection (list ))
			(state 0))

			(loop 
				do
				(assert (not (= i 13)) (i) "i is out of bounds i = ~d (= i 13), state = ~d" i state)



				(cond 
					; if_else_subgroup -> return_
					((null line)

						; if state != if_else_subgroup there is an error
						(assert (not (not (= state if_else_subgroup))) (state if_else_subgroup) "still on them ~d, ~d (not (= state if_else_subgroup)), state = ~d" state if_else_subgroup state)

						(setq state (aref (aref state_transition_table state)line_is_empty) ))
						

					; specific member of if_statement_word_markers column
					; start -> if_else_append
					((string= (first line) "if")

						(vector-push (first line) stack)

		
						(setq state (aref (aref state_transition_table state) if_statement_word_markers  ))
						(setq line (rest line)))

					; if_else_append -> if_else_subgroup
					((and (not (string= (first line) "if")) (not (string= (first line) "then")) (not (string= (first line) "else")))


						; stack should have nothing on it
						(assert (not (= (length stack) 0)) (stack) "stack is empty ~d (= (length stack) 0), state = ~d" stack state)


						(vector-pop stack)

						(setq state (aref (aref state_transition_table state) subgroup_element_1  )))

					; if_else_subgroup -> then_append
					((string= (first line) "then")

						(vector-push (first line) stack)


						(setq state (aref (aref state_transition_table state) if_statement_word_markers  ))
						(setq line (rest line)))


					; then_append -> then_subgroup
					((and (not (string= (first line) "if")) (not (string= (first line) "then")) (not (string= (first line) "else")) (= state then_append ))

						(assert (not (= (length stack) 0)) (stack) "stack is empty ~a (= (length stack) 0), state = ~d" stack state )

						(vector-pop stack)
						
						(setq state (aref (aref state_transition_table state) subgroup_element_1  )))

						; then_subgroup -> if_else_append
					((string= (first line) "else")
						(vector-push (first line) stack)
						(setq line (rest line))
						(setq state (aref (aref state_transition_table state) if_statement_word_markers)))


		
					; input may be caught before the error state is caught
					; if input is correct error state is never reached
					((assert t () "no transition t, state = ~d" state)))

					; accepting states
					(cond
						((= state return_)
							; breaks out of loop
							(return)))

					; non-accepting states
					(cond
						((= state if_else_append)
							(assert (not (= (length stack) 0)) (stack) "stack is empty ~a (= (length stack) 0), state = ~d" stack state )
							
							(setq collection (append collection (list (englishToCode (aref stack 0))))))

						((= state if_else_subgroup)

							(setq collection (append collection (list (makeSubGroup)))))

					 	((= state then_append)

							(assert (not (= (length stack) 0)) (stack) "stack is empty ~a (= (length stack) 0), state = ~d" stack state )

							(setq collection (append collection (list (englishToCode (aref stack 0))))))

						 	

					  	((= state then_subgroup)

							(setq collection (append collection (list (makeSubGroup)))))
					  	; if assert condtion is true flip bit to print error message
					  	((assert (not (= state error)) (state error) "error state (= state error), state = ~d" state)))

			(setq i (+ i 1)))

				

		
			collection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getCodeFromList (list_)

	(loop for x in list_
		do

		(setf output_string (concatenate 'string output_string "~%" ))
		(setf output_string (concatenate 'string output_string (getCode x ) ))
		(setf output_string (concatenate 'string output_string "~%~%" ))))


(defun getCode (plists)

	; plists is a list
	; the actual plist is at bottom of nested lists

	(cond
		((eq  (first plists) :english )

			(getf plists :code))

		((null (first plists))
			"")

		(t

			(concatenate 'string (getCode (first plists)) (if (null (rest plists))   "" " ") (getCode (rest plists))))))

