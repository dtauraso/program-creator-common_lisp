
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; not restricted to any of the 5 cases
(defun openFile(file_name)

	(with-open-file (var file_name)		
			(readLines var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun readLines(var)

	; not sure why the references to line on multiple calls of g did not change the value of line
	; setting line as an item in cons is different from the set and not store in addElementAVL
	(setf line (read-line var nil))
	(if 	line
		(cons line (readLines var))
		nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split (string_ delimiter)

	; original design from http://www.lee-mac.com/stringtolist.html

		; find position of delimiter = pos
	(cond 	((setf delimiter_position (search delimiter string_))

			; cut from 0 to right before pos
			(setf new_string_position (+ delimiter_position (length delimiter)))

			(if 	(= delimiter_position 0)

					; pass cut from pos + delimiter length to split (pass in the string after the first delimiter found) and delimiter
					(split (subseq string_ new_string_position) delimiter)

					; the string "" is not in the list split returns unless there are no delimiters?
					(cons
						(subseq string_ 0 delimiter_position)
						(split

							(subseq string_ new_string_position) delimiter))))

		(t
			(if (> (length string_) 0)
				(list string_)

				nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform_1 (parameters)

	; only used with the split function
	(setf (symbol-function 'function_) (nth 0 parameters))
	(setf in_list (nth 1 parameters))
	(setf extra_parameters (nth 2 parameters))

	; works on a single level list
	; assume the last item in list part in_list comes from is a string
	(cond	((typep in_list 'string)

			; if extra_parameters = nil then a list of a string is passed to function_
			(function_ (cons in_list extra_parameters)))
			
	 	(in_list


			(cons
				(function_ (car in_list) extra_parameters)

				(transform_1
					(list 
						(nth 0 parameters)
					
							(cdr in_list)
						extra_parameters))))

		; last case	
		(t
			nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun listOfStringsToString (list_)

	(cond 	(list_
			(concatenate
			'string
				(first list_)
				; insert spaces between the first middles and last strings when concatenating
				(if (second list_) " " "")
				(listOfStringsToString (rest list_))))
		(t
			nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun object (label code children is_concrete)

	(list
		:label label
		:code code
		:children children
		:concrete_status is_concrete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collectKnowledgeParts (list_of_children)

	; 1 column = 1 list containing n plists

	; root's children
	; children = [e_1, e_2, e_3]
	; e_i = [n_1, n_2, n_3]
	; e_i = concrete column = collection of property lists
	; e_i = column of abstract nodes = collection of peroperty lists
	; n_j = property list
	; children = [[n_1, n_2, n_3][n_1, n_2, n_3]]

	; abstract's children
	; children = [e_1, e_2, e_3]
	; e_i = concrete column = collection of property lists
	; e_i = column of abstract nodes = collection of peroperty lists
	(cond 	((eql (first list_of_children) :label)
			(cons
				list_of_children
				(mapcar #'collectKnowledgeParts (getf list_of_children :children))))
		(t
			(mapcar #'collectKnowledgeParts list_of_children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isAbstract2 (plist)
	(if (getf plist :concrete_status) nil plist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten2 (list_)

	; flatten but treat a plist as an atom
	(cond 	((null list_)
			nil)

		; is list_ a plist with :label as one of its keywords
		((eql (first list_) :label)
			(list list_))

		(t
			(append (flatten2 (first list_)) (flatten2 (rest list_))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getListOfChildren (list_of_knowledge_children)


	(setf all_plists_in_knowledge (flatten2 (mapcar #'collectKnowledgeParts list_of_knowledge_children)))
	(setf children_plists_in_knowledge (remove-duplicates (remove-if #'isAbstract2 all_plists_in_knowledge) :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collectMatchingPlists (spec children)

	(cond 	((null children)
			nil)

		((string= spec (getf (first children) :label))
			(cons (first children) (collectMatchingPlists spec (rest children))))

		(t
			(collectMatchingPlists spec (rest children)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find_ (element list_)

	(cond 	((null list_)
			nil)

		((string= element (first list_))
			element)

		(t
			(find_ element (rest list_)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; figureOutCode here for convenience and is independent from case 4
(defun figureOutCode (spec)

	(setf children_plists_in_knowledge (getListOfChildren (getf *knowledge* :children)))
		; is spec known?
	(cond 	((setf match (first (collectMatchingPlists spec children_plists_in_knowledge)))
				;yes then is spec valid python?
			; is the code from match in the valid python keywords list?
			(cond 	((setf code (find_ spec *python_base_elements*))
					; yes then make object and use copy for the code
					; yes then copy add('0')
					(object spec spec nil t))

				(t
					; no then ask for code
					; all of the code in knowledge should be valid python

					; no then there is at least 1 copy of spec already known('one')
					(object spec (getf match :code) nil t))))

		; no then is spec valid python?
		(t
			(cond 	((setf code (find_ spec *python_base_elements*))
				; yes then copy add('or')
				(object spec spec nil t))
				; no then spec is not known and is not valid python
				(t
					; have to get a definition from user("two" refers to the same number as  "2")
					; only add code if it is valid python
					(cond ((setf code (first (openFile (concatenate 'string spec ".txt"))))
					(format t (listOfStringsToString (list "had to ask for help because" spec "is not recognized" "~%")))
					(object spec code nil t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; used in cases 2 and 5
(defun getLabel (plist)

	; plist is inside a list
	(getf plist :label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; will restrict these cases to a depth of 3 levels
; root, concrete and abstract columns, concrete columns in abstract columns
; case 2


(defun collectLabels (concrete_column)

	; works on concrete_column and abstract_column

	; concrete_column has nested lists containing plists
	; the plists have :label as the first element
	(cond 	((null concrete_column)
			"")
		((eql (first concrete_column) :label)
			(getf concrete_column :label))
		(t
			(concatenate 'string (collectLabels (first concrete_column))
			; if last element of concrete_column returns "" then last concatenate
			; should not have an end " " after the last label
			(if (string= (collectLabels (rest concrete_column)) "") "" " ")
			(collectLabels (rest concrete_column))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combineStrings (list_of_strings)

	(cond 	((null list_of_strings)
			"")
		(t
			(concatenate 'string (first list_of_strings)
			; if last element of list_of_strings returns "" then last concatenate
			; should not have an end " " after the last string literal
			; the last item is nil
			(if (null (first (rest list_of_strings)) ) "" "~%")
			(combineStrings (rest list_of_strings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; case 2 controller function
(defun calculateSetForCase2 (specs)


	; prove the first word of specs is the name of 1 abstract column
	; mapcar is applying isAbstract to each item in getf knowledge children
	(setf abstract_groups (remove 'nil (mapcar #'isAbstract (getf *knowledge* :children))))

	(setf *abstract_label* (getf (first abstract_groups) :label))

	(setf rest_of_specs (rest specs))

	; only gets the next abstract label
	; I think it is a coincidence that the next abstract_groups item
	; is in the concrete part of the specs and the second abstract label

	; assume the rest of the words of specs are a memeber of an abstract group
	(cond 	((setf second_abstract_label (getf (second abstract_groups) :label))

			; all children in columns in first abstract_groups
			(setf lines (mapcar #'collectLabels (getf (first abstract_groups) :children)))			
			(setf lines (combineStrings lines)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
; case 3
;;;;;

(defun makeSpecsColumn (specs)

	; convert specs to list of object function pists

	; make a list with the column of object function plists inside
	(cond 	((null specs)
			nil)

		(t
		 	(cons
				(figureOutCode (first specs))
				(makeSpecsColumn (rest specs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun flatten (list_)

	(cond 	((null list_)
			nil)

		((atom (first list_))
			(cons (first list_) (flatten (rest list_))))

		(t
			(append (flatten (first list_)) (flatten (rest list_))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun columnToListOfStrings (column)

	; 1 column = 1 list containing n plists

	; root's children
	; children = [e_1, e_2, e_3]
	; e_i = [n_1, n_2, n_3]
	; e_i = concrete column = collection of property lists
	; e_i = column of abstract nodes = collection of peroperty lists
	; n_j = property list
	; children = [[n_1, n_2, n_3][n_1, n_2, n_3]]

	; abstract's children
	; children = [e_1, e_2, e_3]
	; e_i = concrete column = collection of property lists
	; e_i = column of abstract nodes = collection of peroperty lists

	(cond 	((eql (first column) :label) ; column is a plist
			(getf column :label))

		(t
			(mapcar #'columnToListOfStrings column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isConcrete (column)

	(if (typep (first column) 'list) column nil))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun countPlists (column)

	; assumes column can be either linear or nonlinear
	; returns a nonzero integer if column is second instance of uncreated abstract group

	(cond 	((null column)
			0)

		((eql (first column) :label)
			1)

		(t
			(+ (countPlists (first column)) (countPlists (rest column))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun specsSizedColumns (specs columns)

	
	(cond 	((null columns)
			nil)

		; find number of plists in concrete column
		((= (length specs) (countPlists (first columns)))
			(cons (first columns) (specsSizedColumns specs (rest columns))))

		(t
			(specsSizedColumns specs (rest columns)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isAbstract (column)
	(if (eql (first column) :label) column nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; find if label has an associated verify file
(defun verifyLabel (label)
	(if (load (concatenate 'string label ".lisp") :if-does-not-exist nil)
		label
		nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun	getConcreteColumnIndexOfRoot2 (columns_in_root concrete_column index)


	; keep track of the index of the column so root @index can be extracted

	(cond 	((null columns_in_root)
			nil)

		((isConcrete (first columns_in_root))

			(if (eql (first columns_in_root) concrete_column)
				index))

		(t
			(getConcreteColumnIndexOfRoot2
				(rest columns_in_root)
				concrete_column
				(+ index 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; case 3 controller function
(defun calculateNewAbstractColumnName (specs)

	; specs = list of strings
	; function_names = nil if list_of_concrete_columns = (nil)

	; make set of all children columns from root
	(setf root_children_columns (getf *knowledge* :children))

	; only the concrete children from root are considered
	(setf list_of_concrete_columns (mapcar #'isConcrete root_children_columns))

	; make sure only the concrete columns have same sength as the specs are considered



	; this will be nil if specs is the first instance of abstract group (that hasn't been made yet)
	; list of conrete column having length equal to specs length
	(setf
		specs_length_sized_concrete_columns

		(specsSizedColumns specs list_of_concrete_columns))

	; there can only be 1 column because of current test input
	(setf
		concrete_labels
		(first (mapcar #'columnToListOfStrings specs_length_sized_concrete_columns)))	



	; take the specs and find a single concrete column the specs has at least 1 element in common
	(setf matches (intersection specs concrete_labels :test 'equal))
	; ok with 1 abstract column here
	

	; a list of current abstract_labels must be made so they can be eliminated from
	; matches

	; a set containing nil is not allowed
	(setf abstract_columns (remove 'nil (mapcar #'isAbstract root_children_columns)))

	; is actually a list of a list of abstract_labels
	(setf abstract_labels (mapcar #'columnToListOfStrings abstract_columns))




	; no abstract_labels in knowledge can be in new_abstract_labels
	(setf
		new_abstract_labels
		(set-difference matches abstract_labels :test 'equal))

	(setf
		function_names
		(intersection
			new_abstract_labels
			(remove 'nil (mapcar #'verifyLabel new_abstract_labels))))

	; only consider the first specs_length_sized_concrete_columns
	; wrong comment for this function

	(setf *concrete_column_in_knowledge* (first specs_length_sized_concrete_columns))

	function_names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makeAbstractGroup (specs new_abstract_column_name)

	; 1 abstract group = 1 column that has at least 2 concrete column as children

	; new_abstract_column_name = string
	(setf
		location_of_concrete_column

		(getConcreteColumnIndexOfRoot2
			(getf *knowledge* :children)
			*concrete_column_in_knowledge*
			0))

	(setf
		abstract_node
		(object

	;		label_for_abstract_node
			(first new_abstract_column_name)
			""
			; child field
			nil
			nil))
	(push
		(nth location_of_concrete_column (getf *knowledge* :children))
		(getf abstract_node :children))


	; from case 5 and case 4

	;;;;;;
	; for specs
	; calculate possible nonlinear structure
	; max nonlinear depth = 2

	(setf abstract_groups (remove 'nil (mapcar #'isAbstract (getf *knowledge* :children))))
	(setf
		abstract_labels
		(mapcar #'columnToListOfStrings abstract_groups))

	; first if statement was added using this case
	; abstract_groups is actually the child list of root without any concrete columns

	(initiateGlobalVars abstract_groups)
	(mapcar #'manipulateStack specs)

	; if no groups then set set_of_subsets to nil and return set_of_subsets
	; else return set_of_subsets do below after conditional in process5Cases
	(if (= *groups* 0) (setf set_of_subsets nil) (setf set_of_subsets *stack*))



	(setf local_pointer abstract_node)
	(cond 	(set_of_subsets

			; reguarding makeColumnAndCode
                        	; column is nested in this case
			(makeColumnAndCode #'makeNestedObjectPlists set_of_subsets local_pointer)

			(terpri)
			(terpri))

		(t
			(terpri)
			(terpri)

			; reguarding makeColumnAndCode
                        	; column is linear in this case
			(makeColumnAndCode #'makeSpecsColumn specs local_pointer)

))



	; cut column from its current place
	(setf (nth location_of_concrete_column (getf *knowledge* :children)) nil)

	(push abstract_node (getf *knowledge* :children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun makeColumnAndCode (function_ set_of_subsets local_pointer)

	; column can be nested or not
	(setf column (funcall function_ set_of_subsets))

	(setf code_set (flatten (collectCode column)))
	(format t (listOfStringsToString code_set))

	(push column (getf local_pointer :children)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; case 4





(defun makeNestedObjectPlists (nested_list)

	; nested_list = nested list containing strings
	(cond 	((null nested_list)
			nil)

		((atom (first nested_list))

			(cons
				(figureOutCode (first nested_list))
				(makeNestedObjectPlists (rest nested_list))))

		(t
			(cons
				(makeNestedObjectPlists (first nested_list))
				(makeNestedObjectPlists (rest nested_list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initiateGlobalVars (abstract_groups)

	; assumes there is only 1 element in here
	; only the first abstract_group is needed
	; *abstract_group* is used in calculateListOfIndexGroups, processCase5, and makeAbstractGroup
	(setf *abstract_group* (first abstract_groups))

	; reset and initialize
	(setf *stack* (list))
	(setf *group_elements_in_stack* nil)
	(setf *group_elements_out_of_stack* nil)
	(setf *groups* 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; case 4 controller function
(defun calculateListOfIndexGroups (specs)

	; 1 abstract group = 1 column that has at least 2 concrete column as children

	; abstract_groups is actually the child list of root without any concrete columns
	(setf abstract_groups (remove 'nil (mapcar #'isAbstract (getf *knowledge* :children))))


	(initiateGlobalVars abstract_groups)

	(mapcar #'manipulateStack specs)

	; if no groups then set set_of_subsets to nil and return set_of_subsets
	; else return set_of_subsets do below after conditional in process5Cases
	(if (= *groups* 0) (setf set_of_subsets nil)(setf set_of_subsets *stack*))

	; testing for subgroup in *stack* = specs
	(if (= (length *stack*) 1) (setf set_of_subsets specs))

	; abstract group must have at least 1 concrete column that has the same length of specs
	; the first concrete column must have same length as the specs
	(if (= (length (first (getf *abstract_group* :children))) (length specs))
			set_of_subsets
			nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; case 5

(defun makeIndexBounds (abstract_column)

	; get upper bound and lower bound
	; use the first child in abstract_column's children

	; access the string in plist @ :abstract
	(setf abstract_label (getf abstract_column :label))

	; first_concrete_column is a list of plists
	; access the first concrete column in abstract
	(setf
		first_concrete_column
		(first (getf abstract_column :children)))

	; set and reset *group_elements_in_stack* and *group_elements_out_of_stack*
	(setf
		*group_elements_in_stack*
		(position abstract_label first_concrete_column :test #'string= :key #'getLabel))

	(setf *group_elements_out_of_stack* (- (countPlists first_concrete_column) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun manipulateStack (spec)

		; if spec is an abstract ; spec is in abstract_group
	(cond 	((string= spec (getf *abstract_group* :label))

			; counts how many groups there are
			(setf *groups* (+ *groups* 1))

			; get index bounds
			(makeIndexBounds *abstract_group*)

			; add spec to stack
			(setf *stack* (append *stack* (list spec)))

			; decrement both bounds by 1
			(setf *group_elements_in_stack* (+ *group_elements_in_stack* 1))
			(setf *group_elements_out_of_stack* (- *group_elements_out_of_stack* 1)))

		(t
			; add spec to stack
			(setf *stack* (append *stack* (list spec)))

			; decrement both bounds by 1
			; what happens if this case is the first case?
				; don't care about *lower_bound* and *upper_bound* non nil value
			(cond 	((and *group_elements_in_stack* *group_elements_out_of_stack*)
					(setf *group_elements_in_stack* (+ *group_elements_in_stack* 1))
					(setf *group_elements_out_of_stack* (- *group_elements_out_of_stack* 1))

						; are all of elements from the current group copied from specs and are at the end of *stack* 
					(cond 	((= *group_elements_out_of_stack* 0)

							; length(*stack*) - *lower_bound* = first item from end of stack at *lower_bound*
							(setf group (subseq *stack* (- (length *stack*) *group_elements_in_stack*)))

							; cut out the elements of the group from *stack*
							(setf *stack* (subseq *stack* 0 (- (length *stack*) *group_elements_in_stack*)))

							; (list group) so append will append all elements in (list group) ie. group to the end of *stack*
							(setf *stack* (append *stack* (list group))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun collectCode(column)

			; smallest element is a plist
	(cond ((eql (first column) :label)
		(getf column :code))

		(t
			(cons (collectCode (first column))
				(mapcar #'collectCode (rest column))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun processCase5 (specs)


	(setf abstract_groups (remove 'nil (mapcar #'isAbstract (getf *knowledge* :children))))
	; first if statement was added using this case
	; abstract_groups is actually the child list of root without any concrete columns

	(initiateGlobalVars abstract_groups)

	(mapcar #'manipulateStack specs)

	(setf set_of_subsets *stack*)
	(if (= (length *stack*) 1) (setf set_of_subsets specs))

	; *knowledge* is global and makeColumnAndCode must take a var to *knowledge*
	; so makeColumnAndCode can ve used for all senarios of making the column to add to 
	; *knowledge* and code
	(setf local_pointer *knowledge*)
	(cond 	(set_of_subsets


			; reguarding makeColumnAndCode
				; column is nested in this case
			(makeColumnAndCode #'makeNestedObjectPlists set_of_subsets local_pointer))

		(t

			; reguarding makeColumnAndCode
				; column is linear in this case
			(makeColumnAndCode #'makeSpecsColumn specs local_pointer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saveFile (file_name string_to_write)

	(with-open-file
		(file_stream file_name
			:direction :output
			:if-does-not-exist :create)
		(format file_stream string_to_write)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun saveDataAndRunEvaluator (specs lines)


	(setf abstract_label_unfold (concatenate 'string *abstract_label* "_unfold.txt"))
	(saveFile abstract_label_unfold (listOfStringsToString specs))

	; output specs to file
	; examples in highest abstract_column in specs
	(setf first_specs_examples (concatenate 'string (first specs) "_examples.txt"))

	; output all children of if column to file (in lines)
	(saveFile first_specs_examples lines)


	(setf abstract_groups
			(remove 'nil (mapcar #'isAbstract (getf *knowledge* :children))))

	(setf abstract_names (mapcar #'collectLabels  abstract_groups))			
	(setf abstract_names_string (combineStrings abstract_names))

	; save all abstract labels in file
	(saveFile "abstract.txt" abstract_names_string)
			
	(setf evaluator_file_name (concatenate 'string *abstract_label* "_evaluator.lisp"))

	; load filename for abstract_label "_evaluator.lisp"
	(load evaluator_file_name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; casees 1 - 5 controller
(defun process5Cases (specs)

	; bottom up set of subsets and elements making function names and parameters
	; groups by using the most inner group
	; there is only 1 inner group
	(terpri)
	(terpri)
	(print "specs")
	(print (listOfStringsToString specs))
	(terpri)
	(terpri)

	; case 1
	; assume knowledge is empty
		; case 1
	(cond 	((null (getf *knowledge* :children))

			(setf local_pointer *knowledge*)

			; reguarding makeColumnAndCode
                        ; column is linear in this case
			(makeColumnAndCode #'makeSpecsColumn specs local_pointer))

		; case 2
		; assume data is in the form
		; "[abstract node name] [concrete column already known or unknown]"
		((setf lines (calculateSetForCase2 specs))

			(saveDataAndRunEvaluator specs lines))


		; case 3
		; algorithm is too specific when it comes to multiple abstract columns that are not in the chosen places (possibly as a result of (first result) in case 3 controller function)
		; assume data is unknown and concrete as in a member of an abstract group
		((setf new_abstract_column_name (calculateNewAbstractColumnName specs))

			; second if statement was added using this case
			(makeAbstractGroup specs new_abstract_column_name))

		; case 4
		; algorithm is too specific when it comes to multiple abstract columns that are not in the chosen places (possibly as a result of (first result) in case 4 controller function)

		; assume data contains new or old concrete groups
		; a concrete group is a concrete column but it is a subset of elements in specs
		((setf set_of_subsets (calculateListOfIndexGroups specs))

			; reguarding makeColumnAndCode
	                ; column is nested in this case
			(setf local_pointer *abstract_group*)
			(makeColumnAndCode #'makeNestedObjectPlists set_of_subsets local_pointer))

		; case 5
		; algorithm could be too specific when it comes to multiple abstract columns
		; assume specs does not fall into cases 1-4
		; new data is linear or nonlinear

		(t
			(processCase5 specs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun processEachSpecs (specs)

	(cond 	(specs
				(process5Cases (first specs))

				(processEachSpecs (rest specs)))
		(t nil)))
