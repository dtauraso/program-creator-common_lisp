(require 'common_functions.lisp)

; there are 2 of them because the specs collection has 2 of them (they were in the spec collection beause all possible results for the condition field are required for "if 2 and 4")

; assume the syntax from specs is correct but at least 1 word in field_1
; needs to be checked for incorrect syntax

; assume all numbers are numerals
		; make a linear version of the if statements
		; make the code from the linear versions of the if statements


; calculate posibilities for the python code
; evaluate to true and another one should evaluate to false)
; use the if_evaluator.lisp to break up the groups and calculate possible combinations
; of the boolean

; there are 2 posibilities for result of condition in if statement 0, 1
; the if condtion in compressed_if will be expressed as
; if true do x
; if true(but will be false) do x
; only need 1 representation for each result

(defun evaluateAnd (and_group)

	; assumes all parameters a string versions of single digit numbers
	; runs the python "and" boolean operator

	(setf first_operand (first and_group))
	(setf second_operand (third and_group))

	; assume all operands are in the range of chars from "0" to "9"

			; the "and" will return false
	(cond 	((string= first_operand "0")
				0)

			; the "and" will return false
			((string= second_operand "0")
				0)

			; the "and" will return true
			(t
				(if 	(string> first_operand second_operand)
							(parse-integer first_operand)
							(parse-integer second_operand)))))


(defun createCode (condition_i)

	(setf condition_i (listOfStringsToString condition_i))

	; *and_group_from_compressed_if* must be modified per call to createCode, not modified and saved
	(listOfStringsToString
		(list "if" condition_i ":" (listOfStringsToString *and_group_from_compressed_if*))))


(defun fixParameter? (parameter)

	; replace condition parameter with its numerical representation from the reference file
	; if the parameter is an english word
		; only works if its numerical representation is in the range [0, 9]
	(if 	(or (string> "0" parameter) (string< "9" parameter))
				(setf  parameter (first (openFile (concatenate 'string parameter ".txt"))))
				parameter))

(defun verifyAndFixParameters (condition_)


	(setf parameters (list (first condition_) (third condition_)))

	(setf result (mapcar #'fixParameter? parameters))

	(setf condition_ (list (first result) (second condition_) (second result))))

(defun extractCondition (example)

	(subseq example 1 4))


; open files, get the if then "do something" part, tokenize all specs (split function)

; get compressed_if
(setf compressed_if (openFile "if_unfold.txt"))

; compressed_if is a list containing a string
(setf compressed_if (transform_1 (list #'split compressed_if " ")))

(setf *and_group_from_compressed_if* (subseq (first compressed_if) 1 4))

; get abstract names
(setf abstract_names (openFile "abstract.txt"))
(setf abstract_names (transform_1 (list #'split abstract_names " ")))

(setf examples (openFile "if_examples.txt"))
(setf examples (transform_1 (list #'split examples " ")))


; only want the "and" condition in the if else statement
; each field slot in this case hold a python "and" condition 
; {"if", field_1, "then", field_2, "else" field_3}
; elements [1, 3]
(setf if_conditions (mapcar #'extractCondition examples))


; change parameters from english to numerical if necessary
(setf
		fixed_parameters_conditions
		(mapcar #'verifyAndFixParameters if_conditions))



(setf code_sets (mapcar #'createCode fixed_parameters_conditions))

(format t "~%~%code sets:")
(format t "~%these are 2 out of all diffferent possible if statements for 'if 2 and 4' ")

(terpri)

(format t (concatenate 'string (first code_sets) "~%" (second code_sets)))
