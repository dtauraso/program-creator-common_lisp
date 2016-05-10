(require 'common_functions.lisp)
(setf lines (openFile "and_examples.txt"))

(setf specs_collection (transform_1 (list #'split lines " ")))

(setf *python_base_elements* (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "and" "or" "if"))
(setf *knowledge* (object  "" nil nil nil))


(processEachSpecs specs_collection)
(terpri)
(terpri)
(print "knowledge")
(print *knowledge*)


